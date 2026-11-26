# Plan: support more browsers in `get_cookies_from_browser()`

Read `AGENTS.md` first for repo conventions and the common cookie schema.

Goal: `get_cookies_from_browser(browser = ...)` works for Firefox-based
browsers, Chromium-based browsers (Chrome, Chromium, Brave, Edge, Vivaldi,
Opera) and Safari, on Linux, macOS and Windows where feasible, and returns the
common 7-column schema.

Work in phases; each phase should leave `devtools::check()` clean and could be
its own commit/PR. Items marked **VERIFY** are from memory and must be checked
against current browser sources (Chromium `os_crypt`, `browser_cookie3`,
`yt-dlp`'s `cookies.py`) before relying on them.

---

## Phase 1: refactor + fix existing bugs

### Bugs in current `R/browser.r`

1. **macOS never matches.** `find_cookiejar()` compares against
   `Sys.info()["sysname"]`, which is `"Darwin"` on macOS, but `cookie_dirs` uses
   `"macOS"`. Fix in `data-raw/cookie_dirs.R` (use `"Darwin"`) and regenerate
   `R/sysdata.rda`.
2. **LibreWolf can't work.** The S3 class is `tolower(browser)`, so LibreWolf
   dispatches to a non-existent `prep_db.librewolf`. Dispatch on *engine*
   instead (see below).
3. **`flag` is wrong.** It's filled from `sameSite`. It must mean "include
   subdomains": `flag = startsWith(host, ".")` (same as
   `parse_chromote_cookies()` in `R/set.r`).
4. **WAL data ignored + temp file collisions.** Only `cookies.sqlite` is copied
   and all profiles copy to the same `tempdir()/cookies.sqlite`. Copy the db
   plus any `-wal` / `-shm` siblings into a fresh `tempfile()` directory per
   profile; clean up with `on.exit()`/`withr`-style `unlink()`.
5. **Firefox expiry units (VERIFY).** Recent Firefox may store `moz_cookies.expiry`
   in milliseconds. Add a guard: values `> 1e11` are ms, divide by 1000.
6. `DBI::dbDisconnect()` should be in `on.exit()` so it runs on error.

### Refactor

- **`data-raw/cookie_dirs.R`**: add columns
  - `engine`: `"firefox"`, `"chromium"`, `"safari"`
  - `cookie_file`: file name to search for (`"cookies.sqlite"`, `"Cookies"`,
    `"Cookies.binarycookies"`)
  - `keyring_name`: Chromium only, e.g. `"Chrome"`, `"Chromium"`, `"Brave"`,
    `"Microsoft Edge"`, `"Vivaldi"`, `"Opera"` (used for
    `"<name> Safe Storage"` on macOS and `application <name lowercase>` in the
    Linux Secret Service; **VERIFY** exact strings per browser)
  - Fix `os` values to `Sys.info()` names (`"Linux"`, `"Windows"`, `"Darwin"`).
- **`find_cookiejar(browser)`**:
  - Match `browser` case-insensitively against `cookie_dirs$browser`; abort
    with a `cli` message listing supported browsers for this OS if unknown.
  - Return a tibble/list with `path` and `engine` (and `keyring_name`, plus the
    browser's "user data" root dir, needed on Windows for `Local State`).
  - Search for `cookie_file` with `list.files(recursive = TRUE)`. For Chromium
    both `<profile>/Cookies` and `<profile>/Network/Cookies` exist in the wild;
    prefer `Network/Cookies` if both exist in the same profile.
  - Handle `env` rows with `NA` env correctly (current code calls
    `Sys.getenv(NA)`); expand `~` with `path.expand()`.
- **`get_cookies_from_browser()`**: if no cookie file is found, `cli_abort()`
  with the paths searched instead of returning an empty tibble.
- **Dispatch on engine**: `read_cookie_db()` sets
  `class(db) <- c(engine, class(db))`; methods `prep_db.firefox`,
  `prep_db.chromium`, `prep_db.safari`. Each method returns the common schema
  with `expiration` already POSIXct (move the conversion out of
  `get_cookies_from_browser()`).
- Optional: `browser = "all"` reads every browser found on the system and
  combines them. Decide whether to add a `browser` column (would break the
  7-column schema expected by `store_cookies()`; if added, document it and drop
  it in any path that stores cookies). Low priority.

### Cheap additions

Add `cookie_dirs` rows for other Gecko forks (engine `"firefox"`): Zen,
Waterfox, Floorp, Mullvad Browser. Paths from `browser_cookie3` / `yt-dlp`
(**VERIFY**). Deliberately skip Tor Browser.

### Tests (Phase 1)

- Add `tests/testthat/fixtures/firefox_cookies.sqlite`: a tiny db with the
  `moz_cookies` schema and 2–3 fake cookies (one with leading-dot host, one
  session cookie). Create it with a script in `data-raw/` or
  `tests/testthat/fixtures/make_fixtures.R` so it's reproducible.
- Test `prep_db.firefox` against the fixture (schema, `flag`, expiration).
- Test `find_cookiejar()` with a fake profile tree in `withr::local_tempdir()`
  and a mocked `cookie_dirs`/OS (wrap `Sys.info()[["sysname"]]` in a small
  internal `current_os()` so it can be mocked).
- Keep the existing live Firefox test.

---

## Phase 2: Chromium on Linux

Paths (`~/.config/...`): `google-chrome`, `chromium`,
`BraveSoftware/Brave-Browser`, `microsoft-edge`, `vivaldi`, `opera`; plus snap
(`~/snap/chromium/common/chromium`) and flatpak
(`~/.var/app/<id>/config/...`) variants (**VERIFY**).

### Reading

- Table `cookies`. Columns: `host_key`, `name`, `value`, `encrypted_value`
  (BLOB → list of raw), `path`, `expires_utc`, `is_secure`, `is_httponly`,
  `samesite`.
- Mapping: `domain = host_key`, `flag = startsWith(host_key, ".")`,
  `secure = as.logical(is_secure)`,
  `expiration = as.POSIXct(expires_utc / 1e6 - 11644473600)` (microseconds
  since 1601-01-01); `expires_utc == 0` → session cookie, keep as `0`
  (consistent with `is_expired()`).
- `meta` table: read `value` where `key == "version"`; needed for the hash
  prefix below.

### Decryption (`decrypt_chromium_value(blob, key, db_version)`)

- If `value` is non-empty and `encrypted_value` is empty, use `value`.
- Prefix = first 3 bytes of `encrypted_value`:
  - `v10`: key = PBKDF2-HMAC-SHA1(password `"peanuts"`, salt `"saltysalt"`,
    iterations 1, length 16). AES-128-CBC, IV = 16 × `0x20` (space).
    `openssl::aes_cbc_decrypt(data, key, iv)` (handles PKCS7 padding).
  - `v11`: same, but password from the Secret Service / KWallet. Get via
    `system2("secret-tool", c("lookup", "application", tolower(keyring_name)), stdout = TRUE)`
    (**VERIFY** attribute names; some browsers use `xdg:schema`
    `chrome_libsecret_os_crypt_password_v2`). Optional fallback via the
    `keyring` package (add to Suggests). If no password can be obtained, try
    the `"peanuts"` key, then warn and return `NA` for those values.
- If `db_version >= 24` (**VERIFY**), strip the first 32 bytes of the
  plaintext (SHA-256 of `host_key`).
- Implementation note: R's `openssl` probably has no PBKDF2 export
  (**VERIFY**: check `ls("package:openssl")`). If missing, implement
  `pbkdf2_sha1(password, salt, iter, len)` in `R/utils.r` on top of
  `openssl::sha1(x, key = ...)` (HMAC). Unit-test it against RFC 6070 test
  vectors.
- Wrap all external lookups in small internal functions
  (`get_chromium_password(keyring_name)`) so tests can mock them.

### Tests (Phase 2)

- Fixture `chromium_cookies.sqlite` with values encrypted using the `v10`
  "peanuts" key; one db with `meta.version = 23`, one with `24` (hash prefix).
  Generate them in R with `openssl::aes_cbc_encrypt()` in the fixture script.
- Test a `v11` value with `get_chromium_password()` mocked.
- Live test per browser with `skip_if()` not found + `skip_on_cran()`.

---

## Phase 3: Chromium on Windows

- Paths: `%LOCALAPPDATA%\Google\Chrome\User Data`,
  `%LOCALAPPDATA%\Chromium\User Data`,
  `%LOCALAPPDATA%\BraveSoftware\Brave-Browser\User Data`,
  `%LOCALAPPDATA%\Microsoft\Edge\User Data`,
  `%LOCALAPPDATA%\Vivaldi\User Data`,
  `%APPDATA%\Opera Software\Opera Stable` (**VERIFY**).
- Key: `User Data/Local State` (JSON, `jsonlite`) → `os_crypt.encrypted_key`
  → base64 decode → strip leading `"DPAPI"` (5 bytes) → DPAPI-unprotect.
  Do DPAPI via PowerShell to avoid compiled code:
  `Add-Type -AssemblyName System.Security; [Convert]::ToBase64String([Security.Cryptography.ProtectedData]::Unprotect([Convert]::FromBase64String('<b64>'), $null, 'CurrentUser'))`
  in an internal `dpapi_unprotect(raw)` (mockable).
- `v10` values: AES-256-GCM; layout `v10` | 12-byte nonce | ciphertext |
  16-byte tag. Use `openssl::aes_gcm_decrypt()` (**VERIFY** signature/whether
  tag must be appended to the data). Same hash-prefix rule.
- `v20` values (app-bound encryption, Chrome 127+): **not supported**. Detect,
  drop those cookies, and `cli_warn()` once with the count and suggest
  `add_cookies(session = ...)` or a cookies.txt export.
- Locked file: Chrome locks `Cookies` while running → `file.copy()` returns
  `FALSE`. Detect and abort with "close the browser and try again".
- Tests: fixture with AES-GCM values and a known key, `dpapi_unprotect()`
  mocked. A `v20` value must produce the warning.

---

## Phase 4: Chromium on macOS

- Paths under `~/Library/Application Support/`: `Google/Chrome`, `Chromium`,
  `BraveSoftware/Brave-Browser`, `Microsoft Edge`, `Vivaldi`,
  `com.operasoftware.Opera` (**VERIFY**).
- Password:
  `system2("security", c("find-generic-password", "-w", "-s", "<keyring_name> Safe Storage"), stdout = TRUE)`.
  Triggers a macOS Keychain prompt — mention it in the docs and in a
  `cli_inform()` before calling.
- Key: PBKDF2-HMAC-SHA1(password, `"saltysalt"`, **1003** iterations, 16 bytes);
  AES-128-CBC, IV 16 spaces, prefix `v10`. Same hash-prefix rule.
- Tests: mocked password + fixture encrypted with the derived key.

---

## Phase 5: Safari (macOS), lowest priority

- File: `~/Library/Containers/com.apple.Safari/Data/Library/Cookies/Cookies.binarycookies`
  (older: `~/Library/Cookies/Cookies.binarycookies`).
- Not SQLite: `read_cookie_db()` must skip DBI for engine `"safari"` (e.g.
  `read_cookie_db` becomes a generic too, or `prep_db.safari` takes the path).
- Parse with `readBin()`: magic `"cook"`, big-endian page count and page sizes;
  each page little-endian with cookie offsets; each record has flags (secure,
  httpOnly), offsets to url/name/path/value strings, expiry as Mac absolute
  time (seconds since 2001-01-01, add `978307200`). (**VERIFY** layout against
  a published spec.)
- Reading needs Full Disk Access for R/RStudio; catch the permission error and
  explain how to grant it.
- Tests: a small hand-built `.binarycookies` fixture.

---

## Docs / housekeeping (every phase)

- Update the roxygen of `get_cookies_from_browser()`: supported browsers/OS
  table, known limitations (Windows `v20`, macOS Keychain prompt, close Chrome
  on Windows, Safari Full Disk Access), and that values are decrypted in memory
  and only stored encrypted in the jar.
- Update the "Function to read cookies from browser" section of the vignette /
  README if present.
- `NEWS.md` entry per phase.
- `DESCRIPTION`: `jsonlite` already in Suggests; add `keyring` and `withr` only
  if used. No new Imports.
- Run `source("data-raw/cookie_dirs.R")`, `devtools::document()`,
  `devtools::test()`, `devtools::check()`.
- Delete `plan.md` (or move it out) before merging to `main`; `AGENTS.md`
  stays but must be in `.Rbuildignore`.
