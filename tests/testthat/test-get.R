test_that("getting cookies works", {
  jar <- options(cookie_dir = tempdir())
  withr::defer(options(jar))
  withr::defer(unlink(file.path(tempdir(), paste0("cookies.rds"))))

  expect_error(get_cookies("tests.com"),
               "does not contain any cookies yet")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests.com", confirm = TRUE)
    out <- get_cookies("tests.com", as = "data.frame")
    c(class(out), colnames(out), out$domain, out$value)
  }, c("tbl_df", "tbl", "data.frame", "domain", "flag", "path", "secure",
       "expiration", "name", "value", "tests.com", "tests.com", "true",
       "yes"))

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests.com")
    get_cookies("tests.com", as = "string")
  }, "test=true; success=yes")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "more.tests.com")
    get_cookies("tests.com", as = "string")
  }, "test=true; success=yes; test=true; success=yes")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "more.tests.com")
    get_cookies("^tests.com", as = "string")
  }, "test=true; success=yes")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "more.tests.com")
    get_cookies("^tests.com", key = "success", as = "string")
  }, "success=yes")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests.com")
    get_cookies("^tests.com", as = "vector")
  }, c(test = "true", success = "yes"))
})


