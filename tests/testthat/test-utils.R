test_that("pbkdf2_sha1() matches the RFC 6070 test vectors", {
  hex <- function(x) paste(as.character(x), collapse = "")
  expect_equal(
    hex(pbkdf2_sha1("password", "salt", 1, 20)),
    "0c60c80f961f0e71f3a9b524af6012062fe037a6"
  )
  expect_equal(
    hex(pbkdf2_sha1("password", "salt", 2, 20)),
    "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957"
  )
  expect_equal(
    hex(pbkdf2_sha1("password", "salt", 4096, 20)),
    "4b007901b765489abead49d926f721d065a429c1"
  )
  expect_equal(
    hex(pbkdf2_sha1(
      "passwordPASSWORDpassword",
      "saltSALTsaltSALTsaltSALTsaltSALTsalt",
      4096,
      25
    )),
    "3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038"
  )
  expect_equal(
    hex(pbkdf2_sha1(
      as.raw(c(charToRaw("pass"), 0, charToRaw("word"))),
      as.raw(c(charToRaw("sa"), 0, charToRaw("lt"))),
      4096,
      16
    )),
    "56fa6aa75548099dcc37d7f03425e0c3"
  )
})

test_that("pbkdf2_sha1() derives Chromium's Linux key", {
  expect_equal(
    paste(pbkdf2_sha1("peanuts", "saltysalt", 1, 16), collapse = ""),
    "fd621fe5a2b402539dfa147ca9272778"
  )
})
