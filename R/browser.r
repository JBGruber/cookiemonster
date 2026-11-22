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
#' | Chrome    | yes   | no    | no      |
#' | Chromium  | yes   | no    | no      |
#' | Brave     | yes   | no    | no      |
#' | Edge      | yes   | no    | no      |
#' | Vivaldi   | yes   | no    | no      |
#' | Opera     | yes   | no    | no      |
#'
#' Chromium-based browsers on macOS and Windows and Safari are not supported
#' yet. Until then, export a `cookies.txt` file or use
#' `add_cookies(session = ...)`.
#'
#' The browser database is copied to a temporary directory before it is read,
#' so the browser can stay open. Cookie values are only held in memory; they
#' are encrypted once you put them in the jar with [store_cookies()].
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
  rlang::check_installed(c("DBI", "RSQLite"))
  # Some RSQLite binaries (e.g., from Posit Package Manager) ship their own,
  # older libcurl and make its symbols global on dbConnect(). If curl is loaded
  # after that, it binds to the wrong libcurl and every httr2/curl request fails
  # with "An unknown option was passed in to libcurl". Loading curl first avoids
  # this.
  requireNamespace("curl", quietly = TRUE)

  jars <- find_cookiejar(browser)
  if (nrow(jars) == 0L) {
    searched <- attr(jars, "searched")
    cli::cli_abort(c(
      "No cookie database for {.val {browser}} found.",
      "i" = "Searched in: {.path {searched}}"
    ))
  }

  cookies <- lapply(seq_len(nrow(jars)), function(i) {
    read_cookie_db(
      jars$path[i],
      engine = jars$engine[i],
      keyring_name = jars$keyring_name[i]
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
#' @return a tibble in the common cookie schema.
#' @noRd
read_cookie_db <- function(db_file, engine, keyring_name = NA_character_) {
  tmp <- tempfile("cookiemonster")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  files <- paste0(db_file, c("", "-wal", "-shm"))
  files <- files[file.exists(files)]
  copied <- file.copy(files, tmp)
  if (!isTRUE(copied[1])) {
    cli::cli_abort(c(
      "Could not copy the cookie database {.path {db_file}}.",
      "i" = "If the browser is running, close it and try again."
    ))
  }

  conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    file.path(tmp, basename(db_file)),
    bigint = "numeric"
  )
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)

  db <- structure(
    list(conn = conn, path = db_file, keyring_name = keyring_name),
    class = c(engine, "cookie_db")
  )
  prep_db(db)
}


#' Convert a browser's cookie database into the common cookie schema
#' @param db a list with elements `conn` (a DBI connection), `path` (path to
#'   the original database) and `keyring_name` of class
#'   `c(<engine>, "cookie_db")`.
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

  value <- decrypt_chromium_values(
    encrypted = cookies$encrypted_value,
    value = cookies$value,
    host_key = cookies$host_key,
    meta_version = meta_version,
    keyring_name = db$keyring_name
  )
  failed <- is.na(value)
  if (any(failed)) {
    cli::cli_warn(c(
      "{sum(failed)} cookie{?s} in {.path {db$path}} could not be decrypted and {?was/were} dropped.",
      "i" = "The browser's password could not be read from the keyring or did not fit."
    ))
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
  )[!failed, ]
}


#' Decrypt the values of Chromium cookies
#'
#' Encrypted values start with a version prefix. On Linux, `v10` values use a
#' key derived from the hard-coded password "peanuts", `v11` values a key
#' derived from a password in the keyring. Both fall back to a key derived from
#' an empty password, which Chromium uses when the keyring is unavailable. See
#' `components/os_crypt` in the Chromium sources.
#'
#' @param encrypted list of raw vectors (the `encrypted_value` column).
#' @param value character vector (the `value` column), used where nothing is
#'   encrypted.
#' @param host_key character vector, needed to check the hash prefix.
#' @param meta_version version of the database (`meta` table). From version 24
#'   on, the plain text starts with the SHA-256 hash of `host_key`.
#' @param keyring_name name of the keyring entry.
#' @return character vector of values; `NA` where decryption failed.
#' @noRd
decrypt_chromium_values <- function(
  encrypted,
  value,
  host_key,
  meta_version,
  keyring_name
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

  empty_key <- chromium_linux_key("")
  keys <- list(v10 = list(chromium_linux_key("peanuts"), empty_key))
  if (any(prefix[todo] == "v11")) {
    password <- get_chromium_password(keyring_name)
    keys$v11 <- c(
      if (!is.null(password)) list(chromium_linux_key(password)),
      list(empty_key)
    )
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


#' @noRd
chromium_linux_key <- function(password) {
  pbkdf2_sha1(password, "saltysalt", iterations = 1, key_length = 16)
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
