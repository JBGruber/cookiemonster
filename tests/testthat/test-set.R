test_that("setting cookies works", {
  expect_equal(
    default_jar(),
    rappdirs::user_cache_dir("r_cookies")
  )

  jar <- options(cookie_dir = tempdir())
  withr::defer(options(jar))
  withr::defer(unlink(c(file.path(tempdir(), paste0("cookies.rds")),
                        "test.txt")))

  expect_equal(default_jar(), tempdir())

  expect_error(
    add_cookies(cookiefile = system.file("extdata", "cookies.txt", package = "cookiemonster"),
                cookiestring = "test=true; success=yes"),
    "not both"
  )

  expect_error(
    add_cookies(),
    "either cookie file or string"
  )

  expect_gte({
    add_cookies(cookiefile = system.file("extdata", "cookies.txt", package = "cookiemonster"))
    file.size(file.path(tempdir(), paste0("cookies.rds")))
  }, 622)

  expect_message({
    l <- readLines(system.file("extdata", "cookies.txt", package = "cookiemonster"))
    writeLines(l[5:7], "test.txt")
    add_cookies(cookiefile = "test.txt")
  }, "not seem to be a valid cookiefile")

  expect_error(add_cookies(cookiestring = "test=true; success=yes"),
               "provide a domain")

  expect_gte({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests.com")
    file.size(file.path(tempdir(), paste0("cookies.rds")))
  }, 754)
})
