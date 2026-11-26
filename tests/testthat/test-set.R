jar <- options(cookie_dir = tempdir())

test_that("setting cookies works", {
  expect_equal(default_jar(), tempdir())

  expect_error(
    add_cookies(
      cookiefile = system.file(
        "extdata",
        "cookies.txt",
        package = "cookiemonster"
      ),
      cookiestring = "test=true; success=yes"
    ),
    "not both"
  )

  expect_error(
    add_cookies(),
    "You.must.provide.either"
  )

  expect_gte(
    {
      add_cookies(
        cookiefile = system.file(
          "extdata",
          "cookies.txt",
          package = "cookiemonster"
        )
      )
      file.size(file.path(tempdir(), paste0("cookies.rds")))
    },
    622
  )

  expect_message(
    {
      l <- readLines(system.file(
        "extdata",
        "cookies.txt",
        package = "cookiemonster"
      ))
      writeLines(l[5:7], "test.txt")
      add_cookies(cookiefile = "test.txt")
      unlink("test.txt")
    },
    "not seem to be a valid cookiefile"
  )

  expect_error(
    add_cookies(cookiestring = "test=true; success=yes"),
    "provide a domain"
  )

  expect_gte(
    {
      add_cookies(cookiestring = "test=true; success=yes", domain = "tests.com")
      file.size(file.path(tempdir(), paste0("cookies.rds")))
    },
    754
  )
})

test_that("old cookies are replaced", {
  expect_equal(default_jar(), tempdir())
  expect_equal(
    {
      add_cookies(cookiestring = "test=true", domain = "https://tests.com")
      add_cookies(cookiestring = "chococookie=delicious", domain = "tests.com")
      get_cookies(domain = "^tests.com")$value
    },
    "delicious"
  )
})

test_that("HttpOnly cookies from cookie files get a proper domain", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines(
    c(
      "# Netscape HTTP Cookie File",
      "#HttpOnly_.httponly.com\tTRUE\t/\tTRUE\t0\twith_dot\ta",
      "#HttpOnly_www.httponly.com\tFALSE\t/\tTRUE\t0\twithout_dot\tb",
      ".httponly.com\tTRUE\t/\tTRUE\t0\tplain\tc"
    ),
    f
  )
  expect_equal(
    read_cookiefile(f)$domain,
    c("httponly.com", "www.httponly.com", "httponly.com")
  )
})

test_that("cookies from live sessions are parsed without optional fields", {
  # mimics sess$session$Network$getCookies() from a current Chrome, which no
  # longer sends sameParty
  fake_sess <- structure(
    list(
      session = list(
        Network = list(
          getCookies = function() {
            list(
              cookies = list(
                list(
                  name = "domain_cookie",
                  value = "a",
                  domain = ".example.com",
                  path = "/",
                  expires = 1700000000,
                  size = 14,
                  httpOnly = TRUE,
                  secure = TRUE,
                  session = FALSE,
                  sameSite = "Lax",
                  priority = "Medium",
                  sourceScheme = "Secure",
                  sourcePort = 443L
                ),
                list(
                  name = "host_cookie",
                  value = "b",
                  domain = "www.example.com",
                  path = "/",
                  expires = -1,
                  size = 12,
                  httpOnly = FALSE,
                  secure = FALSE,
                  session = TRUE,
                  priority = "Medium",
                  sourceScheme = "Secure",
                  sourcePort = 443L
                )
              )
            )
          }
        )
      )
    ),
    class = "LiveHTML"
  )

  cookies <- parse_session(fake_sess)
  expect_named(
    cookies,
    c("domain", "flag", "path", "secure", "expiration", "name", "value")
  )
  expect_equal(cookies$flag, c(TRUE, FALSE))
  expect_equal(cookies$secure, c(TRUE, FALSE))
  expect_equal(as.numeric(cookies$expiration), c(1700000000, -1))
  expect_s3_class(cookies$expiration, "POSIXct")
  expect_equal(cookies$name, c("domain_cookie", "host_cookie"))
  expect_equal(cookies$value, c("a", "b"))

  # a missing field becomes NA instead of an error
  no_secure <- parse_chromote_cookies(list(list(
    name = "x",
    value = "y",
    domain = "example.com",
    path = "/",
    expires = -1
  )))
  expect_identical(no_secure$secure, NA)

  # sessions without cookies give an empty table with the usual columns
  empty <- parse_chromote_cookies(list())
  expect_equal(nrow(empty), 0L)
  expect_named(empty, names(cookies))

  expect_error(parse_session(list()), "read_html_live")
})
