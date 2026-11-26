#' Get Cookies from Browser
#'
#' Raids the cookie jar of a browser installed on your computer: reads the
#' browser's own cookie database (from all profiles found) and returns the
#' cookies in the same format as the other cookie sources of this package, ready
#' for [store_cookies()].
#'
#' @section Supported browsers:
#'
#' | Browser   | Linux | macOS | Windows |
#' |-----------|:-----:|:-----:|:-------:|
#' | Firefox   | yes   | yes   | yes     |
#' | LibreWolf | yes   | yes   | yes     |
#' | Zen       | yes   | yes   | yes     |
#' | Waterfox  | yes   | yes   | yes     |
#' | Floorp    | yes   | yes   | yes     |
#' | Chrome    | yes   | yes   | yes     |
#' | Chromium  | yes   | yes   | yes     |
#' | Brave     | yes   | yes   | yes     |
#' | Edge      | yes   | yes   | yes     |
#' | Vivaldi   | yes   | yes   | yes     |
#' | Opera     | yes   | yes   | yes     |
#' | Safari    | -     | yes   | -       |
#'
#' If your browser is missing or its cookies can't be read, export a
#' `cookies.txt` file or use `add_cookies(session = ...)` instead.
#'
#' The browser database is copied to a temporary directory before it is read,
#' so the browser can usually stay open. Cookie values are only held in memory;
#' they are encrypted once you put them in the jar with [store_cookies()].
#'
#' @section Chromium-based browsers on Windows:
#'
#' Chromium-based browsers encrypt their cookies with an AES-256-GCM key that
#' is stored DPAPI-protected in `User Data/Local State`. The key is unprotected
#' via PowerShell and held in memory only. Close the browser before reading its
#' cookies: Chrome locks the `Cookies` file while running.
#'
#' Cookies encrypted with app-bound encryption (prefix `v20`, Chrome 127+ on
#' Windows) cannot be decrypted. Those cookies are dropped with a warning; the
#' rest of the cookie jar is returned. Export a `cookies.txt` file or use
#' `add_cookies(session = ...)` to work around this.
#'
#' @section Chromium-based browsers on Linux:
#'
#' Chromium-based browsers encrypt their cookies with a password that is kept
#' in the desktop's keyring. It is read with `secret-tool` (GNOME Keyring and
#' other Secret Service providers, usually in the `libsecret` package of your
#' distribution) or `kwallet-query` (KDE), so one of them needs to be
#' installed. Your keyring might ask you to unlock it. Cookies that can't be
#' decrypted are dropped with a warning.
#'
#' @section Chromium-based browsers on macOS:
#'
#' Chromium-based browsers encrypt their cookies with a password that is kept
#' in the macOS Keychain (e.g., "Chrome Safe Storage"). It is read with the
#' `security` command line tool, which makes macOS ask you whether R (or the
#' app you run it in) may access the entry; choose "Always Allow" to not be
#' asked every time. Each browser has its own entry, so you are asked once
#' per browser. Cookies that can't be decrypted are dropped with a warning.
#'
#' @section Safari:
#'
#' Safari's cookie file is protected by macOS. To read it, the app you run R in
#' (e.g., RStudio, Positron or Terminal) needs Full Disk Access: open System
#' Settings > Privacy & Security > Full Disk Access, add the app and restart it.
#'
#' @param browser A character string specifying the browser to get cookies from
#'   (case-insensitive, see the section "Supported browsers"). Defaults to "Firefox".
#'
#' @return A tibble containing the cookies from the specified browser, with the
#'   columns `domain`, `flag` (include subdomains), `path`, `secure`,
#'   `expiration`, `name` and `value`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cookies <- get_cookies_from_browser("Firefox")
#' store_cookies(cookies)
#' }
get_cookies_from_browser <- function(browser = "Firefox") {
  jars <- find_cookiejar(browser)
  if (nrow(jars) == 0L) {
    searched <- attr(jars, "searched")
    cli::cli_abort(c(
      "No cookie database for {.val {browser}} found.",
      "i" = "Searched in: {.path {searched}}",
      if (tolower(browser) == "safari") {
        c(
          "i" = "Safari's cookies are protected by macOS. Give the app you run
          R in Full Disk Access under System Settings > Privacy & Security >
          Full Disk Access, then restart it."
        )
      }
    ))
  }

  if (any(jars$engine != "safari")) {
    rlang::check_installed(c("DBI", "RSQLite"))
  }
  # Some RSQLite binaries (e.g., from Posit Package Manager) ship their own,
  # older libcurl and make its symbols global on dbConnect(). If curl is loaded
  # after that, it binds to the wrong libcurl and every httr2/curl request fails
  # with "An unknown option was passed in to libcurl". Loading curl first avoids
  # this.
  requireNamespace("curl", quietly = TRUE)

  # profiles of one browser share a password: ask the keyring only once
  passwords <- new.env(parent = emptyenv())
  cookies <- lapply(seq_len(nrow(jars)), function(i) {
    read_cookie_db(
      jars$path[i],
      engine = jars$engine[i],
      keyring_name = jars$keyring_name[i],
      root = jars$root[i],
      passwords = passwords
    )
  })
  vctrs::vec_unique(vctrs::vec_rbind(!!!cookies))
}


#' Look up the cookie directories of a browser
#'
#' @param browser browser name, matched case-insensitively against
#'   `dirs$browser`.
#' @param dirs table of browser directories (see `data-raw/cookie_dirs.R`).
#' @param os operating system as returned by `Sys.info()[["sysname"]]`.
#' @param call environment reported in errors.
#'
#' @return A tibble with one row per cookie database found and the columns
#'   `browser`, `engine`, `keyring_name`, `root` (the browser's user data
#'   directory) and `path` (the cookie database). The directories that were
#'   searched are attached as attribute `"searched"`.
#' @noRd
find_cookiejar <- function(
  browser,
  dirs = cookie_dirs,
  os = current_os(),
  call = rlang::caller_env()
) {
  dirs <- dirs[dirs$os == os, ]
  sel <- dirs[tolower(dirs$browser) == tolower(browser), ]
  if (nrow(sel) == 0L) {
    supported <- unique(dirs$browser)
    cli::cli_abort(
      c(
        "Browser {.val {browser}} is not supported on {os}.",
        "i" = "Supported browsers: {.val {supported}}."
      ),
      call = call
    )
  }

  base <- ifelse(is.na(sel$env), "", Sys.getenv(sel$env, unset = ""))
  # rows relative to an unset environment variable can't be searched
  keep <- is.na(sel$env) | base != ""
  sel <- sel[keep, ]
  base <- base[keep]
  sel$root <- ifelse(
    is.na(sel$env),
    path.expand(sel$path),
    file.path(base, sel$path)
  )

  found <- lapply(seq_len(nrow(sel)), function(i) {
    files <- list.files(
      sel$root[i],
      pattern = paste0(
        "^",
        gsub(".", "\\.", sel$cookie_file[i], fixed = TRUE),
        "$"
      ),
      recursive = TRUE,
      full.names = TRUE
    )
    tibble::tibble(
      browser = rep(sel$browser[i], length(files)),
      engine = sel$engine[i],
      keyring_name = sel$keyring_name[i],
      root = sel$root[i],
      path = files
    )
  })
  out <- vctrs::vec_rbind(
    tibble::tibble(
      browser = character(),
      engine = character(),
      keyring_name = character(),
      root = character(),
      path = character()
    ),
    !!!found
  )
  out <- prefer_network_dir(out)
  # Safari has a single cookie file: the legacy location can hold a stale copy
  safari <- which(out$engine == "safari")
  if (length(safari) > 1L) {
    out <- out[-safari[-1L], ]
  }
  attr(out, "searched") <- sel$root
  out
}


#' Chromium moved the cookie database from `<profile>/Cookies` to
#' `<profile>/Network/Cookies`, but the old file can linger. Keep only the
#' latter if both exist in the same profile.
#' @noRd
prefer_network_dir <- function(jars) {
  in_network <- basename(dirname(jars$path)) == "Network"
  profile <- ifelse(in_network, dirname(dirname(jars$path)), dirname(jars$path))
  has_network <- profile %in% profile[in_network]
  jars[in_network | !has_network, ]
}


#' Copy a cookie database to a temporary directory and read it
#'
#' The database is copied together with its `-wal`/`-shm` files, so cookies the
#' browser has not yet checkpointed into the main file are included.
#'
#' @param db_file path to the cookie database.
#' @param engine browser engine, used to dispatch `prep_db()`.
#' @param keyring_name Chromium only: name of the browser's keyring entry.
#' @param root Chromium on Windows only: the browser's User Data directory.
#' @param passwords `NULL` or an environment that caches keyring passwords
#'   (see `lookup_password()`).
#' @return a tibble in the common cookie schema.
#' @noRd
read_cookie_db <- function(
  db_file,
  engine,
  keyring_name = NA_character_,
  root = NA_character_,
  passwords = NULL
) {
  tmp <- tempfile("cookiemonster")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  files <- paste0(db_file, c("", "-wal", "-shm"))
  files <- files[file.exists(files)]
  copied <- suppressWarnings(file.copy(files, tmp))
  if (!isTRUE(copied[1])) {
    if (identical(engine, "safari")) {
      cli::cli_abort(c(
        "Could not copy the cookie database {.path {db_file}}.",
        "i" = "Safari's cookies are protected by macOS. Give the app you run R
        in (e.g., RStudio, Positron or Terminal) Full Disk Access under System
        Settings > Privacy & Security > Full Disk Access, then restart it."
      ))
    }
    cli::cli_abort(c(
      "Could not copy the cookie database {.path {db_file}}.",
      "i" = "If the browser is running, close it and try again."
    ))
  }
  local_file <- file.path(tmp, basename(db_file))

  # Safari's cookie file is not an SQLite database
  conn <- NULL
  if (!identical(engine, "safari")) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), local_file, bigint = "numeric")
    on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  }

  db <- structure(
    list(
      conn = conn,
      file = local_file,
      path = db_file,
      keyring_name = keyring_name,
      root = root,
      passwords = passwords
    ),
    class = c(engine, "cookie_db")
  )
  prep_db(db)
}


#' Convert a browser's cookie database into the common cookie schema
#' @param db a list of class `c(<engine>, "cookie_db")` with elements `conn`
#'   (a DBI connection, `NULL` for Safari), `file` (path to the temporary
#'   copy), `path` (path to the original database), `keyring_name` and `root`.
#' @noRd
prep_db <- function(db) {
  UseMethod("prep_db")
}


#' @exportS3Method
#' @noRd
prep_db.default <- function(db) {
  cli::cli_abort(
    "Reading cookies from {.cls {class(db)[1]}} browsers is not supported."
  )
}


#' @exportS3Method
#' @noRd
prep_db.firefox <- function(db) {
  cookies <- DBI::dbGetQuery(
    db$conn,
    "SELECT host, name, value, path, expiry, isSecure FROM moz_cookies"
  )
  schema_version <- DBI::dbGetQuery(db$conn, "PRAGMA user_version")[[1]]

  expiry <- as.numeric(cookies$expiry)
  # Firefox 142 (schema version 16) switched expiry from seconds to
  # milliseconds. Values above 1e11 seconds (year 5138) must be milliseconds.
  ms <- !is.na(expiry) & (schema_version >= 16 | expiry > 1e11)
  expiry[ms] <- expiry[ms] / 1000

  new_cookie_tbl(
    domain = cookies$host,
    flag = startsWith(cookies$host, "."),
    path = cookies$path,
    secure = as.logical(cookies$isSecure),
    expiration = expiry,
    name = cookies$name,
    value = cookies$value
  )
}


#' @exportS3Method
#' @noRd
prep_db.chromium <- function(db) {
  meta_version <- DBI::dbGetQuery(
    db$conn,
    "SELECT value FROM meta WHERE key = 'version'"
  )$value
  meta_version <- as.integer(c(meta_version, 0L)[1])
  # very old databases call the column "secure"
  fields <- DBI::dbListFields(db$conn, "cookies")
  secure_col <- if ("is_secure" %in% fields) "is_secure" else "secure"
  cookies <- DBI::dbGetQuery(
    db$conn,
    paste(
      "SELECT host_key, name, value, encrypted_value, path, expires_utc,",
      secure_col,
      "AS is_secure FROM cookies"
    )
  )

  if (current_os() == "Windows") {
    value <- decrypt_chromium_values_windows(
      encrypted = cookies$encrypted_value,
      value = cookies$value,
      host_key = cookies$host_key,
      meta_version = meta_version,
      root = db$root,
      path = db$path
    )
  } else {
    value <- decrypt_chromium_values(
      encrypted = cookies$encrypted_value,
      value = cookies$value,
      host_key = cookies$host_key,
      meta_version = meta_version,
      keyring_name = db$keyring_name,
      passwords = db$passwords
    )
    failed <- is.na(value)
    if (any(failed)) {
      store <- if (current_os() == "Darwin") "macOS Keychain" else "keyring"
      cli::cli_warn(c(
        "{sum(failed)} cookie{?s} in {.path {db$path}} could not be decrypted and {?was/were} dropped.",
        "i" = "The browser's password could not be read from the {store} or did not fit."
      ))
    }
  }

  # microseconds since 1601-01-01; 0 marks session cookies
  expires <- as.numeric(cookies$expires_utc)
  expiration <- ifelse(expires == 0, 0, expires / 1e6 - 11644473600)

  new_cookie_tbl(
    domain = cookies$host_key,
    flag = startsWith(cookies$host_key, "."),
    path = cookies$path,
    secure = as.logical(cookies$is_secure),
    expiration = expiration,
    name = cookies$name,
    value = value
  )[!is.na(value), ]
}


#' @exportS3Method
#' @noRd
prep_db.safari <- function(db) {
  data <- readBin(db$file, "raw", n = file.size(db$file))
  parse_binarycookies(data, path = db$path)
}


#' Parse Safari's `Cookies.binarycookies` file
#'
#' The format is undocumented; this follows the layout used by yt-dlp
#' (`_parse_safari_cookies()`) and other readers:
#'
#' - header: signature `"cook"`, number of pages (uint32, big-endian), then the
#'   size of each page (uint32, big-endian), followed by the pages and a
#'   checksum/footer that is ignored.
#' - page: signature `00 00 01 00`, number of cookies (uint32, little-endian),
#'   then one offset per cookie record (uint32, little-endian, relative to the
#'   start of the page).
#' - record (all little-endian, offsets relative to the start of the record):
#'   size (uint32), unknown (4 bytes), flags (uint32; bit 1: secure, bit 3:
#'   HttpOnly), unknown (4 bytes), offsets of the domain, name, path and value
#'   strings (uint32 each), unknown (8 bytes), expiry and creation time
#'   (doubles, seconds since 2001-01-01 UTC), then the NUL-terminated strings.
#'
#' @param data raw vector with the file's content.
#' @param path path of the file, used in error messages.
#' @return a tibble in the common cookie schema.
#' @noRd
parse_binarycookies <- function(data, path = "Cookies.binarycookies") {
  bad <- function(what) {
    cli::cli_abort(
      "{.path {path}} is not a valid Safari cookie file ({what}).",
      call = NULL
    )
  }
  uint <- function(x, at, endian = "little") {
    if (at + 4 > length(x)) {
      bad("unexpected end of data")
    }
    out <- readBin(x[at + 1:4], "integer", size = 4, endian = endian)
    # readBin() reads signed integers
    if (out < 0) out + 2^32 else out
  }

  if (length(data) < 8L || !identical(data[1:4], charToRaw("cook"))) {
    bad("missing signature")
  }
  n_pages <- uint(data, 4, "big")
  page_sizes <- vapply(
    seq_len(n_pages),
    function(i) uint(data, 4 + 4 * i, "big"),
    numeric(1)
  )
  page_starts <- 8 + 4 * n_pages + cumsum(c(0, page_sizes))[seq_len(n_pages)]
  if (8 + 4 * n_pages + sum(page_sizes) > length(data)) {
    bad("unexpected end of data")
  }

  records <- list()
  for (i in seq_len(n_pages)) {
    page <- data[page_starts[i] + seq_len(page_sizes[i])]
    if (length(page) < 8L || !identical(page[1:4], as.raw(c(0, 0, 1, 0)))) {
      bad("missing page signature")
    }
    n_cookies <- uint(page, 4)
    offsets <- vapply(
      seq_len(n_cookies),
      function(j) uint(page, 4 + 4 * j),
      numeric(1)
    )
    for (offset in offsets) {
      size <- uint(page, offset)
      if (size < 56 || offset + size > length(page)) {
        bad("invalid cookie record")
      }
      records[[length(records) + 1L]] <- parse_binarycookies_record(
        page[offset + seq_len(size)],
        bad
      )
    }
  }

  col <- function(name, type) vapply(records, `[[`, type, name)
  domain <- col("domain", character(1))
  new_cookie_tbl(
    domain = domain,
    flag = startsWith(domain, "."),
    path = col("path", character(1)),
    secure = col("secure", logical(1)),
    expiration = col("expiration", numeric(1)),
    name = col("name", character(1)),
    value = col("value", character(1))
  )
}


#' @noRd
parse_binarycookies_record <- function(rec, bad) {
  uint <- function(at) {
    out <- readBin(rec[at + 1:4], "integer", size = 4, endian = "little")
    if (out < 0) out + 2^32 else out
  }
  cstring <- function(at) {
    if (at >= length(rec)) {
      bad("invalid string offset")
    }
    rest <- rec[(at + 1):length(rec)]
    end <- match(as.raw(0), rest)
    if (is.na(end)) {
      bad("unterminated string")
    }
    out <- rawToChar(rest[seq_len(end - 1L)])
    Encoding(out) <- "UTF-8"
    out
  }
  flags <- uint(8)
  # seconds since 2001-01-01 00:00:00 UTC
  expiry <- readBin(rec[41:48], "double", size = 8, endian = "little")
  list(
    domain = cstring(uint(16)),
    name = cstring(uint(20)),
    path = cstring(uint(24)),
    value = cstring(uint(28)),
    secure = bitwAnd(flags, 1L) > 0L,
    expiration = expiry + 978307200
  )
}


#' Decrypt the values of Chromium cookies (Linux and macOS)
#'
#' Encrypted values start with a version prefix. On Linux, `v10` values use a
#' key derived from the hard-coded password "peanuts", `v11` values a key
#' derived from a password in the keyring. Both fall back to a key derived from
#' an empty password, which Chromium uses when the keyring is unavailable. On
#' macOS, `v10` values use a key derived from a password in the Keychain (with
#' 1003 PBKDF2 iterations instead of 1). See `components/os_crypt` in the
#' Chromium sources.
#'
#' @param encrypted list of raw vectors (the `encrypted_value` column).
#' @param value character vector (the `value` column), used where nothing is
#'   encrypted.
#' @param host_key character vector, needed to check the hash prefix.
#' @param meta_version version of the database (`meta` table). From version 24
#'   on, the plain text starts with the SHA-256 hash of `host_key`.
#' @param keyring_name name of the keyring entry.
#' @param os operating system, as returned by `current_os()`.
#' @param passwords `NULL` or an environment that caches keyring passwords.
#' @return character vector of values; `NA` where decryption failed.
#' @noRd
decrypt_chromium_values <- function(
  encrypted,
  value,
  host_key,
  meta_version,
  keyring_name,
  os = current_os(),
  passwords = NULL
) {
  value <- as.character(value)
  prefix <- vapply(
    encrypted,
    function(x) if (length(x) >= 3) rawToChar(x[1:3]) else "",
    character(1)
  )
  todo <- which(lengths(encrypted) > 0 & (is.na(value) | value == ""))
  if (length(todo) == 0L) {
    return(value)
  }

  if (os == "Darwin") {
    keys <- list()
    if (any(prefix[todo] == "v10")) {
      password <- lookup_password(
        get_chromium_password_mac,
        keyring_name,
        passwords
      )
      if (!is.null(password)) {
        keys$v10 <- list(chromium_mac_key(password))
      }
    }
  } else {
    empty_key <- chromium_linux_key("")
    keys <- list(v10 = list(chromium_linux_key("peanuts"), empty_key))
    if (any(prefix[todo] == "v11")) {
      password <- lookup_password(
        get_chromium_password,
        keyring_name,
        passwords
      )
      keys$v11 <- c(
        if (!is.null(password)) list(chromium_linux_key(password)),
        list(empty_key)
      )
    }
  }

  for (i in todo) {
    value[i] <- NA_character_
    for (key in keys[[prefix[i]]]) {
      plain <- decrypt_chromium_value(
        encrypted[[i]][-(1:3)],
        key = key,
        host_key = host_key[i],
        hash_prefix = meta_version >= 24
      )
      if (!is.null(plain)) {
        value[i] <- plain
        break
      }
    }
  }
  value
}


#' Decrypt one AES-128-CBC encrypted Chromium value
#'
#' @param ciphertext raw vector without the version prefix.
#' @param key raw vector.
#' @param host_key cookie domain.
#' @param hash_prefix whether the plain text starts with the SHA-256 hash of
#'   `host_key`.
#' @return a string, or `NULL` if the key does not fit.
#' @noRd
decrypt_chromium_value <- function(ciphertext, key, host_key, hash_prefix) {
  plain <- tryCatch(
    openssl::aes_cbc_decrypt(ciphertext, key, iv = rep(as.raw(0x20), 16)),
    error = function(e) NULL
  )
  if (is.null(plain)) {
    return(NULL)
  }
  if (hash_prefix) {
    hash <- as.raw(openssl::sha256(charToRaw(host_key)))
    if (length(plain) < 32 || !identical(plain[1:32], hash)) {
      return(NULL)
    }
    plain <- plain[-(1:32)]
  }
  out <- tryCatch(rawToChar(plain), error = function(e) NULL)
  if (is.null(out) || !validUTF8(out)) {
    return(NULL)
  }
  out
}


#' Decrypt the values of Chromium cookies on Windows
#'
#' On Windows, Chromium stores an AES-256-GCM key DPAPI-protected in the
#' browser's `Local State` file. `v10` values use this key; `v20` values use
#' app-bound encryption that cannot be decrypted without browser internals.
#'
#' @inheritParams decrypt_chromium_values
#' @param root path to the browser's User Data directory (contains
#'   `Local State`).
#' @param path path to the cookie database (for warning messages only).
#' @return character vector of values; `NA` where decryption failed or was
#'   not possible (v20).
#' @noRd
decrypt_chromium_values_windows <- function(
  encrypted,
  value,
  host_key,
  meta_version,
  root,
  path
) {
  value <- as.character(value)
  prefix <- vapply(
    encrypted,
    function(x) if (length(x) >= 3L) rawToChar(x[1:3]) else "",
    character(1)
  )

  v20_idx <- which(prefix == "v20")
  if (length(v20_idx) > 0L) {
    cli::cli_warn(c(
      "{length(v20_idx)} cookie{?s} use app-bound encryption (v20) and {?was/were} dropped.",
      "i" = "Export a {.file cookies.txt} file or use {.fn add_cookies} with {.arg session} instead."
    ))
    value[v20_idx] <- NA_character_
  }

  todo <- which(
    lengths(encrypted) > 0L & (is.na(value) | value == "") & prefix == "v10"
  )
  if (length(todo) == 0L) {
    return(value)
  }

  key <- get_chromium_key_windows(root)

  for (i in todo) {
    value[i] <- NA_character_
    plain <- decrypt_chromium_value_gcm(
      ciphertext = encrypted[[i]][-(1:3)],
      key = key,
      host_key = host_key[i],
      hash_prefix = meta_version >= 24L
    )
    if (!is.null(plain)) {
      value[i] <- plain
    }
  }

  other_failed <- is.na(value) & !seq_along(value) %in% v20_idx
  if (any(other_failed)) {
    cli::cli_warn(c(
      "{sum(other_failed)} cookie{?s} in {.path {path}} could not be decrypted and {?was/were} dropped.",
      "i" = "The browser's encryption key could not be read."
    ))
  }

  value
}


#' Read and unprotect the AES-256-GCM cookie key from a Chromium `Local State`
#'
#' @param root path to the browser's User Data directory.
#' @return a 32-byte raw vector.
#' @noRd
get_chromium_key_windows <- function(root) {
  rlang::check_installed("jsonlite")
  local_state <- file.path(root, "Local State")
  if (!file.exists(local_state)) {
    cli::cli_abort(c(
      "Cannot find Chromium encryption key: {.path {local_state}} does not exist.",
      "i" = "Make sure the browser's User Data directory is correct."
    ))
  }
  ls <- jsonlite::fromJSON(local_state, simplifyVector = TRUE)
  b64 <- ls$os_crypt$encrypted_key
  if (is.null(b64) || !nzchar(b64)) {
    cli::cli_abort("No encryption key found in {.path {local_state}}.")
  }
  raw_key <- openssl::base64_decode(b64)
  if (length(raw_key) < 5L || rawToChar(raw_key[1:5]) != "DPAPI") {
    cli::cli_abort(
      "Unexpected format in {.path {local_state}}: missing DPAPI prefix."
    )
  }
  dpapi_unprotect(raw_key[-(1:5)])
}


#' Unprotect a DPAPI-protected blob via PowerShell
#'
#' Writes a small `.ps1` file and calls `powershell.exe`, which handles the
#' DPAPI call without requiring compiled code.
#'
#' @param raw_bytes a raw vector holding the DPAPI-protected blob.
#' @return a raw vector of the unprotected bytes.
#' @noRd
dpapi_unprotect <- function(raw_bytes) {
  b64_in <- openssl::base64_encode(raw_bytes, linebreaks = FALSE)
  ps <- paste0(
    "Add-Type -AssemblyName System.Security\n",
    "$e = [Convert]::FromBase64String('",
    b64_in,
    "')\n",
    "$d = [System.Security.Cryptography.ProtectedData]::Unprotect(",
    "$e, $null, [System.Security.Cryptography.DataProtectionScope]::CurrentUser)\n",
    "[Convert]::ToBase64String($d)\n"
  )
  tmp <- tempfile(fileext = ".ps1")
  on.exit(unlink(tmp))
  writeLines(ps, tmp)
  out <- run_powershell_script(tmp)
  if (length(out) == 0L || !nzchar(out[1L])) {
    cli::cli_abort("DPAPI decryption of the Chromium key failed.")
  }
  openssl::base64_decode(out[1L])
}


#' Run a PowerShell script file and return stdout lines
#' @noRd
run_powershell_script <- function(path) {
  suppressWarnings(system2(
    "powershell",
    c(
      "-NoProfile",
      "-NonInteractive",
      "-ExecutionPolicy",
      "Bypass",
      "-File",
      shQuote(path)
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
}


#' Decrypt one AES-256-GCM encrypted Chromium value (Windows)
#'
#' @param ciphertext raw vector without the version prefix (`v10`).
#'   Layout: 12-byte nonce | encrypted data | 16-byte GCM tag.
#' @param key 32-byte raw vector (AES-256 key).
#' @param host_key cookie domain.
#' @param hash_prefix whether the plain text starts with the SHA-256 hash of
#'   `host_key` (Chromium database version >= 24).
#' @return a string, or `NULL` if decryption failed.
#' @noRd
decrypt_chromium_value_gcm <- function(ciphertext, key, host_key, hash_prefix) {
  if (length(ciphertext) < 29L) {
    return(NULL)
  }
  nonce <- ciphertext[1:12]
  rest <- ciphertext[-(1:12)]
  n <- length(rest)
  if (n < 17L) {
    return(NULL)
  }
  # aes_gcm_encrypt() appends the 16-byte authentication tag; split it off
  tag <- rest[(n - 15L):n]
  data <- rest[seq_len(n - 16L)]
  plain <- tryCatch(
    openssl::aes_gcm_decrypt(data, key, iv = nonce, tag = tag),
    error = function(e) NULL
  )
  if (is.null(plain)) {
    return(NULL)
  }
  if (hash_prefix) {
    hash <- as.raw(openssl::sha256(charToRaw(host_key)))
    if (length(plain) < 32L || !identical(plain[1:32], hash)) {
      return(NULL)
    }
    plain <- plain[-(1:32)]
  }
  out <- tryCatch(rawToChar(plain), error = function(e) NULL)
  if (is.null(out) || !validUTF8(out)) {
    return(NULL)
  }
  out
}


#' @noRd
chromium_linux_key <- function(password) {
  pbkdf2_sha1(password, "saltysalt", iterations = 1, key_length = 16)
}


#' Look up a keyring password, at most once per cache
#'
#' @param fun function that takes `keyring_name` and returns the password or
#'   `NULL`.
#' @param keyring_name name of the keyring entry.
#' @param passwords `NULL` (no caching) or an environment. Failed lookups
#'   (`NULL`) are cached too, so a refused prompt is not repeated.
#' @noRd
lookup_password <- function(fun, keyring_name, passwords = NULL) {
  if (is.null(passwords)) {
    return(fun(keyring_name))
  }
  if (!exists(keyring_name, envir = passwords, inherits = FALSE)) {
    assign(keyring_name, fun(keyring_name), envir = passwords)
  }
  get(keyring_name, envir = passwords, inherits = FALSE)
}


#' @noRd
chromium_mac_key <- function(password) {
  pbkdf2_sha1(password, "saltysalt", iterations = 1003, key_length = 16)
}


#' Read a Chromium browser's password from the macOS Keychain
#'
#' The entry is a generic password with service "<keyring_name> Safe Storage"
#' and account "<keyring_name>". macOS asks the user to allow access.
#'
#' @param keyring_name name of the Keychain entry, e.g. "Chrome".
#' @return the password as a string, or `NULL` if none was found.
#' @noRd
get_chromium_password_mac <- function(keyring_name) {
  service <- paste(keyring_name, "Safe Storage")
  cli::cli_inform(c(
    "i" = "Reading the password {.val {service}} from the macOS Keychain.
    macOS might ask you to allow this (choose {.emph Always Allow} to not be
    asked again)."
  ))
  run_quietly(
    "security",
    c(
      "find-generic-password",
      "-w",
      "-a",
      shQuote(keyring_name),
      "-s",
      shQuote(service)
    )
  )
}


#' Read a Chromium browser's password from the Linux keyring
#'
#' Tries the Secret Service (GNOME Keyring, KeePassXC, ...) via `secret-tool`
#' and KWallet via `kwallet-query`, in the order that fits the desktop.
#'
#' @param keyring_name name of the keyring entry, e.g. "Chrome".
#' @return the password as a string, or `NULL` if none was found.
#' @noRd
get_chromium_password <- function(keyring_name) {
  lookups <- list(
    secret_service = function() secret_service_password(keyring_name),
    kwallet = function() kwallet_password(keyring_name)
  )
  if (grepl("KDE", Sys.getenv("XDG_CURRENT_DESKTOP"), ignore.case = TRUE)) {
    lookups <- rev(lookups)
  }
  for (lookup in lookups) {
    password <- lookup()
    if (!is.null(password)) {
      return(password)
    }
  }
  NULL
}


#' @noRd
secret_service_password <- function(keyring_name) {
  if (!has_command("secret-tool")) {
    return(NULL)
  }
  app <- tolower(keyring_name)
  queries <- list(
    c(
      "xdg:schema",
      "chrome_libsecret_os_crypt_password_v2",
      "application",
      app
    ),
    c("xdg:schema", "chrome_libsecret_os_crypt_password", "application", app),
    c("application", app)
  )
  for (query in queries) {
    password <- run_quietly("secret-tool", c("lookup", query))
    if (!is.null(password)) {
      return(password)
    }
  }
  NULL
}


#' @noRd
kwallet_password <- function(keyring_name) {
  if (!has_command("kwallet-query")) {
    return(NULL)
  }
  password <- run_quietly(
    "kwallet-query",
    c(
      "--read-password",
      shQuote(paste(keyring_name, "Safe Storage")),
      "--folder",
      shQuote(paste(keyring_name, "Keys")),
      "kdewallet"
    )
  )
  if (is.null(password) || startsWith(tolower(password), "failed to read")) {
    return(NULL)
  }
  password
}


#' Run a command and return its first line of output, or `NULL` on failure
#' @noRd
run_quietly <- function(command, args) {
  out <- tryCatch(
    suppressWarnings(system2(command, args, stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL
  )
  status <- attr(out, "status")
  if (length(out) == 0L || (!is.null(status) && status != 0L)) {
    return(NULL)
  }
  out[1]
}


#' @noRd
has_command <- function(command) {
  nzchar(Sys.which(command))
}


#' Build a tibble in the common cookie schema
#' @param expiration seconds since the UNIX epoch (or POSIXct)
#' @noRd
new_cookie_tbl <- function(
  domain,
  flag,
  path,
  secure,
  expiration,
  name,
  value
) {
  tibble::tibble(
    domain = as.character(domain),
    flag = as.logical(flag),
    path = as.character(path),
    secure = as.logical(secure),
    expiration = .POSIXct(as.numeric(expiration)),
    name = as.character(name),
    value = as.character(value)
  )
}


#' @noRd
current_os <- function() {
  Sys.info()[["sysname"]]
}
