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
#'
#' Chromium-based browsers (Chrome, Edge, Brave, ...) and Safari are not
#' supported yet. Until then, export a `cookies.txt` file or use
#' `add_cookies(session = ...)`.
#'
#' The browser database is copied to a temporary directory before it is read,
#' so the browser can stay open. Cookie values are only held in memory; they
#' are encrypted once you put them in the jar with [store_cookies()].
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
    read_cookie_db(jars$path[i], engine = jars$engine[i])
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
find_cookiejar <- function(browser, dirs = cookie_dirs, os = current_os(),
                           call = rlang::caller_env()) {
  dirs <- dirs[dirs$os == os, ]
  sel <- dirs[tolower(dirs$browser) == tolower(browser), ]
  if (nrow(sel) == 0L) {
    supported <- unique(dirs$browser)
    cli::cli_abort(c(
      "Browser {.val {browser}} is not supported on {os}.",
      "i" = "Supported browsers: {.val {supported}}."
    ), call = call)
  }

  base <- ifelse(is.na(sel$env), "", Sys.getenv(sel$env, unset = ""))
  # rows relative to an unset environment variable can't be searched
  keep <- is.na(sel$env) | base != ""
  sel <- sel[keep, ]
  base <- base[keep]
  sel$root <- ifelse(is.na(sel$env), path.expand(sel$path), file.path(base, sel$path))

  found <- lapply(seq_len(nrow(sel)), function(i) {
    files <- list.files(
      sel$root[i],
      pattern = paste0("^", gsub(".", "\\.", sel$cookie_file[i], fixed = TRUE), "$"),
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
    tibble::tibble(browser = character(), engine = character(),
                   keyring_name = character(), root = character(),
                   path = character()),
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
#' @return a tibble in the common cookie schema.
#' @noRd
read_cookie_db <- function(db_file, engine) {
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

  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, basename(db_file)),
                         bigint = "numeric")
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)

  db <- structure(list(conn = conn, path = db_file),
                  class = c(engine, "cookie_db"))
  prep_db(db)
}


#' Convert a browser's cookie database into the common cookie schema
#' @param db a list with element `conn` (a DBI connection) and `path` (path to
#'   the original database) of class `c(<engine>, "cookie_db")`.
#' @noRd
prep_db <- function(db) {
  UseMethod("prep_db")
}


#' @exportS3Method
#' @noRd
prep_db.default <- function(db) {
  cli::cli_abort("Reading cookies from {.cls {class(db)[1]}} browsers is not supported.")
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


#' Build a tibble in the common cookie schema
#' @param expiration seconds since the UNIX epoch (or POSIXct)
#' @noRd
new_cookie_tbl <- function(domain, flag, path, secure, expiration, name, value) {
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
