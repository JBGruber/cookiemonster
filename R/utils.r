#' @noRd
url_get_domain <- function(x) {
  # Netscape cookie files mark HttpOnly cookies with a "#HttpOnly_" prefix,
  # which can be followed by a domain with or without a leading dot
  x <- sub("^#HttpOnly_", "", x)
  x <- sub("^\\.", "", x)
  urltools::domain(x)
}


#' Warn if any of the cookies are expired
#'
#' Session cookies have no expiration date: they are stored as `NA` (cookie
#' strings), `0` (Netscape cookie files) or `-1` (chromote sessions) and never
#' trigger the warning.
#' @noRd
warn_expired <- function(cookies, now = Sys.time()) {
  exp <- as.numeric(cookies$expiration)
  expired <- !is.na(exp) & exp > 0 & exp < as.numeric(now)
  if (any(expired)) {
    nms <- cookies$name[expired]
    doms <- unique(cookies$domain[expired])
    cli::cli_warn(c(
      "{length(nms)} cookie{?s} for {.emp {doms}} {cli::qty(length(nms))}{?has/have} expired: {.val {nms}}.",
      "i" = "Use {.fn add_cookies} to put fresh cookies in the jar."
    ))
  }
  invisible(expired)
}
