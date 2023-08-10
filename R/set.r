add_cookies <- function(cookiefile, cookiestring, host = NULL) {

  if (!missing(cookiefile) & !missing(cookiefile)) {
    cli::cli_abort("This function can either handle a cookie file or string, not both")
  } else if (!missing(cookiefile)) {
    cookies <- read_cookiefile(cookiefile)
  } else if (!missing(cookiestring)) {
    cookies <- parse_cookiestring(cookiestring, host = host)
  } else {
    cli::cli_abort("You must provide either cookie file or string.")
  }

  store_cookies(cookies)
}

store_cookies <- function(cookies, jar = jar()) {
  dir.create(jar, showWarnings = FALSE)
  f <- file.path(jar, paste0("cookies.rds"))
  if (file.exists(f)) {
    cookies_old <- readRDS(f)
    # replace old cookies
    cookies_old <- cookies_old[!cookies_old$host %in% cookies$host, ]
    cookies <- vctrs::vec_rbind(cookies_old, cookies)
  }
  saveRDS(cookies, f, compress = FALSE)
}

jar <- function() {
  if (!is.null(getOption("cookie_dir"))) {
    return(getOption("cookie_dir"))
  } else {
    rappdirs::user_cache_dir("r_cookies")
  }
}

#' read a cookie file
#' @noRd
read_cookiefile <- function(cookiefile) {

  lines <- readLines(cookiefile, warn = FALSE)
  if (!any(grepl("HTTP Cookie File", lines, fixed = TRUE))) {
    cli::cli_alert_danger("This does not seem to be a valid cookiefile")
  }
  df <- utils::read.delim(text = lines[grep("\t", lines)], header = FALSE)
  colnames(df) <- c(
    "host", "subdomains", "path", "is_secure", "expiry", "name", "value"
  )
  df$host <- sub("^\\.", "", df$host)
  df$expiry <- as.POSIXct.numeric(df$expiry, origin = "1970-01-01")
  return(tibble::as_tibble(df))

}

#' parse a string containing cookies
#' @noRd
parse_cookiestring <- function(cookiestring, host) {
  if (is.null(host)) {
    cli::cli_abort("When parsing cookie strings, you need to provide a host")
  }
  cookiestring <- stringi::stri_replace_first_regex(cookiestring, "^Cookie:\\s*", "")
  cookiestring <- stringi::stri_split_fixed(cookiestring, pattern = "; ")[[1]]
  tibble::tibble(
    host = host,
    name = stringi::stri_extract_first_regex(cookiestring, "^.*?(?==)"),
    value = stringi::stri_extract_first_regex(cookiestring, "(?<==).*$")
  )
}

