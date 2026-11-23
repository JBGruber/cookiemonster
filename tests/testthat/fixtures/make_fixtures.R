# Creates the cookie database fixtures used in test-browser.R. All cookies are
# fake. Run from the package root:
# source("tests/testthat/fixtures/make_fixtures.R")

fixture_dir <- file.path("tests", "testthat", "fixtures")

# Firefox ----------------------------------------------------------------------
moz_schema <- "CREATE TABLE moz_cookies (id INTEGER PRIMARY KEY, originAttributes TEXT NOT NULL DEFAULT '', name TEXT, value TEXT, host TEXT, path TEXT, expiry INTEGER, lastAccessed INTEGER, creationTime INTEGER, isSecure INTEGER, isHttpOnly INTEGER, inBrowserElement INTEGER DEFAULT 0, sameSite INTEGER DEFAULT 0, schemeMap INTEGER DEFAULT 0, isPartitionedAttributeSet INTEGER DEFAULT 0, updateTime INTEGER, CONSTRAINT moz_uniqueid UNIQUE (name, host, path, originAttributes))"

# expiry in seconds; 2^31 - 1 is 2038-01-19 03:14:07 UTC
firefox_cookies <- data.frame(
  name = c("session_id", "theme", "tracker"),
  value = c("abc123", "dark", "xyz"),
  host = c(".example.com", "www.example.com", ".cookies.test"),
  path = c("/", "/", "/account"),
  expiry = c(2147483647, 1700000000, 0),
  isSecure = c(1L, 0L, 1L),
  isHttpOnly = c(1L, 0L, 0L),
  sameSite = c(1L, 2L, 0L)
)

make_firefox_db <- function(file, schema_version, expiry_factor) {
  unlink(file)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, moz_schema)
  DBI::dbExecute(conn, paste0("PRAGMA user_version = ", schema_version))
  cookies <- firefox_cookies
  cookies$expiry <- cookies$expiry * expiry_factor
  DBI::dbAppendTable(conn, "moz_cookies", cookies)
}

# current Firefox (>= 142): expiry in milliseconds
make_firefox_db(file.path(fixture_dir, "firefox_cookies.sqlite"), 17, 1000)
# older Firefox: expiry in seconds
make_firefox_db(file.path(fixture_dir, "firefox_cookies_v15.sqlite"), 15, 1)

# Chromium (Linux) -------------------------------------------------------------
# Keys are PBKDF2-HMAC-SHA1(password, "saltysalt", 1 iteration, 16 bytes),
# computed independently with Python's hashlib.pbkdf2_hmac() so the fixtures
# don't depend on the package's own implementation.
chromium_keys <- list(
  peanuts = "fd621fe5a2b402539dfa147ca9272778",
  empty = "d0d0ec9c7d77d43ac54187fa4818d17f",
  # keyring password "cookie-jar-password" (mocked in the tests)
  keyring = "50c80f78116547d279d8a9ed305f5923"
)
chromium_keys <- lapply(chromium_keys, function(hex) {
  as.raw(strtoi(substring(hex, seq(1, 31, 2), seq(2, 32, 2)), 16L))
})

chromium_schema <- c(
  "CREATE TABLE meta(key LONGVARCHAR NOT NULL UNIQUE PRIMARY KEY, value LONGVARCHAR)",
  "CREATE TABLE cookies(creation_utc INTEGER NOT NULL,host_key TEXT NOT NULL,top_frame_site_key TEXT NOT NULL,name TEXT NOT NULL,value TEXT NOT NULL,encrypted_value BLOB NOT NULL,path TEXT NOT NULL,expires_utc INTEGER NOT NULL,is_secure INTEGER NOT NULL,is_httponly INTEGER NOT NULL,last_access_utc INTEGER NOT NULL,has_expires INTEGER NOT NULL,is_persistent INTEGER NOT NULL,priority INTEGER NOT NULL,samesite INTEGER NOT NULL,source_scheme INTEGER NOT NULL,source_port INTEGER NOT NULL,last_update_utc INTEGER NOT NULL,source_type INTEGER NOT NULL,has_cross_site_ancestor INTEGER NOT NULL)"
)

# key: which key encrypts the value (NA: stored as plain text);
# expiry in seconds since the UNIX epoch, 0 for session cookies
chromium_cookies <- data.frame(
  host_key = c(
    ".example.com",
    "www.example.com",
    ".cookies.test",
    "crumbs.test"
  ),
  name = c("session_id", "theme", "tracker", "consent"),
  value = c("abc123", "dark", "xyz", "yes"),
  key = c("peanuts", NA, "keyring", "empty"),
  prefix = c("v10", NA, "v11", "v10"),
  path = c("/", "/", "/account", "/"),
  expiry = c(2147483647, 0, 1700000000, 2147483647),
  is_secure = c(1L, 0L, 1L, 0L)
)

chromium_encrypt <- function(value, host_key, key, prefix, hash_prefix) {
  plain <- charToRaw(value)
  if (hash_prefix) {
    plain <- c(as.raw(openssl::sha256(charToRaw(host_key))), plain)
  }
  ct <- openssl::aes_cbc_encrypt(plain, key, iv = rep(as.raw(0x20), 16))
  c(charToRaw(prefix), as.raw(ct))
}

make_chromium_db <- function(file, meta_version) {
  unlink(file)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(conn))
  for (sql in chromium_schema) {
    DBI::dbExecute(conn, sql)
  }
  DBI::dbExecute(
    conn,
    "INSERT INTO meta VALUES ('version', ?)",
    params = list(as.character(meta_version))
  )
  ck <- chromium_cookies
  encrypted <- lapply(seq_len(nrow(ck)), function(i) {
    if (is.na(ck$key[i])) {
      return(raw())
    }
    chromium_encrypt(
      ck$value[i],
      ck$host_key[i],
      chromium_keys[[ck$key[i]]],
      ck$prefix[i],
      hash_prefix = meta_version >= 24
    )
  })
  value <- ifelse(is.na(ck$key), ck$value, "")
  expires_utc <- ifelse(
    ck$expiry == 0,
    0,
    (ck$expiry + 11644473600) * 1e6
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO cookies VALUES",
      "(0, ?, '', ?, ?, ?, ?, ?, ?, 0, 0, ?, ?, 1, 0, 2, 443, 0, 0, 0)"
    ),
    params = list(
      ck$host_key,
      ck$name,
      value,
      blob::as_blob(encrypted),
      ck$path,
      bit64::as.integer64(expires_utc),
      ck$is_secure,
      as.integer(ck$expiry != 0),
      as.integer(ck$expiry != 0)
    )
  )
}

# meta version 24 added the SHA-256 hash of host_key to the plain text
make_chromium_db(file.path(fixture_dir, "chromium_cookies_v23.sqlite"), 23)
make_chromium_db(file.path(fixture_dir, "chromium_cookies_v24.sqlite"), 24)

# Chromium (Windows) -----------------------------------------------------------
# AES-256-GCM key: 32 bytes 0x00..0x1f (used as a known test key; in
# production this is DPAPI-unprotected from Local State).
chromium_windows_key <- as.raw(0:31)

chromium_windows_cookies <- data.frame(
  host_key  = c(".example.com",  "www.example.com", ".blocked.test"),
  name      = c("session_id",    "theme",           "v20_cookie"),
  value     = c("secret",        "light",           "blocked"),
  prefix    = c("v10",           NA,                "v20"),
  path      = c("/",             "/",               "/"),
  expiry    = c(2147483647,      0,                 2147483647),
  is_secure = c(1L,              0L,                0L)
)

chromium_gcm_encrypt <- function(value, host_key, key, hash_prefix) {
  plain <- charToRaw(value)
  if (hash_prefix) {
    plain <- c(as.raw(openssl::sha256(charToRaw(host_key))), plain)
  }
  nonce <- as.raw(1:12)  # fixed nonce so the fixture is reproducible
  ct <- openssl::aes_gcm_encrypt(plain, key, iv = nonce)
  # aes_gcm_encrypt() returns ciphertext with the 16-byte tag as attr("tag");
  # Chromium stores the layout as: prefix | nonce | ciphertext | tag
  c(charToRaw("v10"), nonce, as.raw(ct), attr(ct, "tag"))
}

make_chromium_windows_db <- function(file, meta_version) {
  unlink(file)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(conn))
  for (sql in chromium_schema) {
    DBI::dbExecute(conn, sql)
  }
  DBI::dbExecute(
    conn,
    "INSERT INTO meta VALUES ('version', ?)",
    params = list(as.character(meta_version))
  )
  ck <- chromium_windows_cookies
  encrypted <- lapply(seq_len(nrow(ck)), function(i) {
    if (is.na(ck$prefix[i])) {
      return(raw())
    }
    if (ck$prefix[i] == "v20") {
      return(c(charToRaw("v20"), as.raw(rep(0L, 10L))))
    }
    chromium_gcm_encrypt(
      ck$value[i],
      ck$host_key[i],
      chromium_windows_key,
      hash_prefix = meta_version >= 24
    )
  })
  value <- ifelse(is.na(ck$prefix), ck$value, "")
  expires_utc <- ifelse(
    ck$expiry == 0,
    0,
    (ck$expiry + 11644473600) * 1e6
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO cookies VALUES",
      "(0, ?, '', ?, ?, ?, ?, ?, ?, 0, 0, ?, ?, 1, 0, 2, 443, 0, 0, 0)"
    ),
    params = list(
      ck$host_key,
      ck$name,
      value,
      blob::as_blob(encrypted),
      ck$path,
      bit64::as.integer64(expires_utc),
      ck$is_secure,
      as.integer(ck$expiry != 0),
      as.integer(ck$expiry != 0)
    )
  )
}

make_chromium_windows_db(
  file.path(fixture_dir, "chromium_cookies_windows_v24.sqlite"),
  24
)
