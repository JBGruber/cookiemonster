test_that("getting cookies works", {
  jar <- options(cookie_dir = tempdir())

  unlink(tempdir(), recursive = TRUE)
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

  expect_equal({
    add_cookies(cookiestring = "test=true", domain = "tests.com")
    get_cookies("^tests.com", as = "list")
  }, list(list(domain = "tests.com", flag = NA, path = NA,
               secure = NA, name = "test", value = "true", httpOnly = FALSE,
               expires = NA_integer_)))

  expect_equal({
    get_cookies("^$", as = "vector")
  }, list())

  expect_equal({
    get_cookies("^$", as = "string")
  }, "")

})

test_that("fixed selection works", {
  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests_nl.com")
    get_cookies(domain = "tests.nl", key = "succe*", as = "string", fixed = "domain")
  }, "")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests_nl.com")
    get_cookies(domain = "tests.nl", key = "succe*", as = "string", fixed = FALSE)
  }, "success=yes")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests_nl.com")
    get_cookies(domain = "tests_nl", key = "succe*", as = "string", fixed = "key")
  }, "")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests_nl.com")
    get_cookies(domain = "tests_nl", key = "succe*", as = "string", fixed = "domain")
  }, "success=yes")
})


test_that("expired cookies trigger a warning", {
  jar <- file.path(tempdir(), "expired_jar")
  on.exit(unlink(jar, recursive = TRUE))
  cookies <- tibble::tibble(
    domain = "expired.com",
    flag = TRUE,
    path = "/",
    secure = TRUE,
    expiration = as.POSIXct.numeric(
      c(1, 0, -1, NA, as.numeric(Sys.time()) + 3600), origin = "1970-01-01"
    ),
    name = c("stale", "session_file", "session_chromote", "session_string", "fresh"),
    value = "x"
  )
  store_cookies(cookies, jar = jar, confirm = TRUE)

  expect_warning(get_cookies("expired.com", jar = jar), "1 cookie.+has expired")
  expect_warning(get_cookies("expired.com", jar = jar), "stale")
  expect_no_warning(get_cookies("expired.com", key = "session|fresh", jar = jar))
})
