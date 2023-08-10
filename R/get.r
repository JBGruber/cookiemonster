get_cookies <- function(domain, jar = jar(), as_string = FALSE) {

  f <- file.path(jar, paste0("cookies.rds"))
  if (file.exists(f)) {
   cli::cli_abort("The directory {jar} does not contain any cookies yet. Use {.fn add_cookies} to store cookies for a website (see {.code vignette(\"cookies\", \"cookiemonster\")} for details).")
  }
  cookies <- readRDS(f)
  sel <- stringi::stri_detect_regex(cookies$host, paste0(domain, "$"))
  out <- cookies[sel, ]
  if (as_string) out <- prep_cookies(out)
  return(out)
}

prep_cookies <- function(tbl) {
  paste0(tbl$name, "=", tbl$value, collapse = "; ")
}
