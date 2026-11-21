#' @noRd
url_get_domain <- function(x) {
  # Netscape cookie files mark HttpOnly cookies with a "#HttpOnly_" prefix,
  # which can be followed by a domain with or without a leading dot
  x <- sub("^#HttpOnly_", "", x)
  x <- sub("^\\.", "", x)
  urltools::domain(x)
}


#' Find cookies that are past their expiration date
#'
#' Session cookies have no expiration date: they are stored as `NA` (cookie
#' strings), `0` (Netscape cookie files) or `-1` (chromote sessions) and never
#' count as expired.
#' @noRd
is_expired <- function(expiration, now = Sys.time()) {
  exp <- as.numeric(expiration)
  !is.na(exp) & exp > 0 & exp < as.numeric(now)
}
