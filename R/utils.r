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


#' PBKDF2 with HMAC-SHA1 (RFC 8018)
#'
#' The openssl package does not export PBKDF2, so it is built here on top of
#' `openssl::sha1(key = )` (HMAC). Only meant for the few iterations Chromium
#' uses to derive its cookie keys.
#'
#' @param password,salt character or raw.
#' @param iterations number of iterations.
#' @param key_length length of the derived key in bytes.
#' @return a raw vector of length `key_length`.
#' @noRd
pbkdf2_sha1 <- function(password, salt, iterations, key_length) {
  if (is.character(password)) {
    password <- charToRaw(password)
  }
  if (is.character(salt)) {
    salt <- charToRaw(salt)
  }
  hmac <- function(x) as.raw(openssl::sha1(x, key = password))
  n_blocks <- ceiling(key_length / 20)
  blocks <- lapply(seq_len(n_blocks), function(i) {
    # block index as 32-bit big-endian integer
    u <- hmac(c(salt, as.raw(c(i %/% 2^24, i %/% 2^16, i %/% 2^8, i) %% 256)))
    out <- u
    for (j in seq_len(iterations - 1)) {
      u <- hmac(u)
      out <- xor(out, u)
    }
    out
  })
  unlist(blocks)[seq_len(key_length)]
}
