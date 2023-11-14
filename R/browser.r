#' Get Cookies from Browser
#'
#' Retrieves all cookies from a Browser
#'
#' @param browser A character string specifying the browser to get cookies from.
#'   Defaults to "Firefox".
#'
#' @return A tibble containing the cookies from the specified browser.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' get_cookies_from_browser("Firefox")
#' }
get_cookies_from_browser <- function(browser = "Firefox") {

  rlang::check_installed(c("DBI", "RSQLite", "dplyr"))

  cookie_db_file <- find_cookiejar(browser)

  lapply(cookie_db_file, read_cookie_db, browser = browser) |>
    dplyr::bind_rows() |>
    dplyr::distinct(.data$domain, .data$flag, .data$path, .data$secure,
                    .data$expiration, .data$name, .data$value)
}


#' @importFrom rlang .data
#' @noRd
find_cookiejar <- function(browser) {
  sel <- cookie_dirs[cookie_dirs$browser == browser &
                       cookie_dirs$os == Sys.info()["sysname"], ]
  paths <- paste0(sel$env[!is.na(sel$env)], sel$path)

  # TODO: only works for Firefox
  list.files(paths, "^cookies.sqlite$", recursive = TRUE, full.names = TRUE)
}

#' @importFrom rlang .data
#' @noRd
read_cookie_db <- function(db_file, browser) {
  data_loc <- db_file
  temp_data_loc <- file.path(tempdir(), basename(data_loc))
  file.copy(data_loc, temp_data_loc)
  db <- DBI::dbConnect(RSQLite::SQLite(), temp_data_loc)
  class(db) <- c(tolower(browser), class(db))
  prep_db(db)
}


#' @importFrom rlang .data
#' @noRd
prep_db <- function(db_conn) {
  UseMethod("prep_db")
}


#' @importFrom rlang .data
#' @noRd
prep_db.firefox <- function(db_conn) {
  cookies <- DBI::dbReadTable(db_conn, "moz_cookies") |>
    tibble::as_tibble()
  dplyr::rename(cookies,
                domain = .data$host,
                flag = .data$sameSite,
                secure = .data$isSecure,
                expiration = .data$expiry
  )
}

