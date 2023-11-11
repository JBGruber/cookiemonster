test_that("getting cookies works", {
  jar <- options(cookie_dir = tempdir())
  withr::defer(options(jar))

  expect_false(file.exists(file.path(tempdir(), paste0("cookies.rds"))))
  add_cookies(cookiestring = "test=true", domain = "https://create.com", confirm = TRUE)
  expect_true(file.exists(file.path(tempdir(), paste0("cookies.rds"))))
})
