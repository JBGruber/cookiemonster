test_that("multiplication works", {
  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests.com")
    get_cookies("^tests.com", as = "string")
  }, "test=true; success=yes")

  expect_equal({
    add_cookies(cookiestring = "test=true; success=yes", domain = "tests.com")
    delete_cookies("^tests.com", ask = FALSE)
    get_cookies("^tests.com", as = "string")
  }, "")
})
