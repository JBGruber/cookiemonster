#' Retrieve cookies from a jar
#'
#' Imagine you're reaching into a magical jar overflowing with those scrumptious
#' digital delights from websites you've visited. The flavour? Up to you! Just
#' select your desired output format.
#'
#' @param domain A character string of the domain to retrieve cookies for.
#' @param jar A character string of the path to the cookie jar (the default is
#'   to use \code{default_jar()} to get a suitable directory).
#' @param as A character string of the type of output to return.
#' @returns Depending on the value of \code{as}, returns either a data frame, a
#'   character string, or a named vector.
#'
#' @details The function returns cookies in one of three formats:
#'
#' \itemize{
#'   \item{\strong{data.frame:}} is how cookies are stored internally and can be used for manual inspection.
#'   \item{\strong{string:}} is used by \code{curl} and \code{httr2}.
#'   \item{\strong{vector:}} is used by \code{httr}.
#' }
#'
#' See \code{vignette("cookies", "cookiemonster")} for how to use cookies with
#' these packages.
#'
#' @note Your cookies are saved in an encrypted file. See \link{encrypt_vec} for
#'   more info.
#'
#'
#' @export
#'
#' @examples
#' # put some cookies in the jar
#' add_cookies(cookiestring = "chococookie=delicious", domain = "example.com")
#' # Reach into your cookie jar and enjoy!
#' get_cookies("example.com")
#' @seealso \code{\link{add_cookies}}
get_cookies <- function(domain, jar = default_jar(), as = c("data.frame", "string", "vector")) {

  as <- match.arg(as)
  f <- file.path(jar, paste0("cookies.rds"))
  if (!file.exists(f)) {
   cli::cli_abort(paste(
     "The directory {jar} does not contain any cookies yet. Use {.fn add_cookies} to",
     "store cookies for a website (see {.code vignette(\"cookies\", \"cookiemonster\")} for details)."
   ))
  }
  cookies <- readRDS(f)
  sel <- stringi::stri_detect_regex(cookies$domain, paste0(urltools::domain(domain), "$"))
  out <- cookies[sel, ]
  out$value <- decrypt_vec(out$value)

  switch (
    as,
    "data.frame" = return(out),
    "string" = return(prep_cookies(out)),
    "vector" = return(prep_cookies(out, as_list = TRUE))
  )

}


#' prepare the cookie df for usage in different packages
#' @noRd
prep_cookies <- function(tbl, as_list = FALSE) {
  if (nrow(tbl) > 0) {
    if (!as_list) {
      return(paste0(tbl$name, "=", tbl$value, collapse = "; "))
    } else {
      return(stats::setNames(tbl$value, tbl$name))
    }
  }
}
