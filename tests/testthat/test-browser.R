fixture <- function(...) test_path("fixtures", ...)

cookie_cols <- c(
  "domain",
  "flag",
  "path",
  "secure",
  "expiration",
  "name",
  "value"
)

test_that("Firefox cookie databases are read into the common schema", {
  for (f in c("firefox_cookies.sqlite", "firefox_cookies_v15.sqlite")) {
    cookies <- read_cookie_db(fixture(f), engine = "firefox")
    expect_named(cookies, cookie_cols)
    expect_s3_class(cookies$expiration, "POSIXct")
    expect_type(cookies$flag, "logical")
    expect_type(cookies$secure, "logical")
    expect_equal(cookies$flag, c(TRUE, FALSE, TRUE))
    expect_equal(cookies$secure, c(TRUE, FALSE, TRUE))
    # milliseconds (v17) and seconds (v15) end up the same
    expect_equal(as.numeric(cookies$expiration), c(2147483647, 1700000000, 0))
    expect_equal(cookies$name, c("session_id", "theme", "tracker"))
  }
})

test_that("Firefox expiry in milliseconds is detected without schema version", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  file.copy(fixture("firefox_cookies_v15.sqlite"), db)
  conn <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(
    conn,
    "UPDATE moz_cookies SET expiry = 2147483647000 WHERE name = 'theme'"
  )
  DBI::dbDisconnect(conn)
  cookies <- read_cookie_db(db, engine = "firefox")
  expect_equal(as.numeric(cookies$expiration), c(2147483647, 2147483647, 0))
})

test_that("cookies only in the WAL file are read and the copy is cleaned up", {
  dir <- withr::local_tempdir()
  db <- file.path(dir, "cookies.sqlite")
  file.copy(fixture("firefox_cookies.sqlite"), db)
  conn <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(conn, "PRAGMA wal_autocheckpoint = 0")
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO moz_cookies (name, value, host, path, expiry, isSecure)",
      "VALUES ('wal_only', 'yes', '.wal.test', '/', 0, 0)"
    )
  )
  expect_true(file.exists(paste0(db, "-wal")))

  before <- list.files(tempdir())
  cookies <- read_cookie_db(db, engine = "firefox")
  expect_true("wal_only" %in% cookies$name)
  expect_setequal(list.files(tempdir()), before)
})

test_that("unknown engines and failed copies give informative errors", {
  expect_error(
    read_cookie_db(fixture("firefox_cookies.sqlite"), "netscape"),
    "not supported"
  )
  expect_error(
    read_cookie_db(fixture("does_not_exist.sqlite"), "firefox"),
    "close it and try again"
  )
})

test_that("Chromium cookie databases are read and decrypted (Linux AES-CBC)", {
  local_mocked_bindings(current_os = function() "Linux")
  local_mocked_bindings(
    get_chromium_password = function(keyring_name) {
      expect_equal(keyring_name, "Chrome")
      "cookie-jar-password"
    }
  )
  for (f in c("chromium_cookies_v23.sqlite", "chromium_cookies_v24.sqlite")) {
    cookies <- read_cookie_db(fixture(f), "chromium", keyring_name = "Chrome")
    expect_named(cookies, cookie_cols)
    expect_s3_class(cookies$expiration, "POSIXct")
    expect_equal(
      cookies$domain,
      c(".example.com", "www.example.com", ".cookies.test", "crumbs.test")
    )
    expect_equal(cookies$flag, c(TRUE, FALSE, TRUE, FALSE))
    expect_equal(cookies$secure, c(TRUE, FALSE, TRUE, FALSE))
    expect_equal(cookies$path, c("/", "/", "/account", "/"))
    # session cookies keep 0
    expect_equal(
      as.numeric(cookies$expiration),
      c(2147483647, 0, 1700000000, 2147483647)
    )
    expect_equal(cookies$name, c("session_id", "theme", "tracker", "consent"))
    # v10 ("peanuts"), plain text, v11 (keyring), v10 (empty password)
    expect_equal(cookies$value, c("abc123", "dark", "xyz", "yes"))
  }
})

test_that("Chromium cookies that can't be decrypted are dropped with a warning", {
  local_mocked_bindings(current_os = function() "Linux")
  local_mocked_bindings(get_chromium_password = function(keyring_name) NULL)
  expect_warning(
    cookies <- read_cookie_db(
      fixture("chromium_cookies_v24.sqlite"),
      "chromium",
      keyring_name = "Chrome"
    ),
    "1 cookie .* could not be decrypted"
  )
  expect_equal(cookies$name, c("session_id", "theme", "consent"))

  # a wrong password is no better
  local_mocked_bindings(get_chromium_password = function(keyring_name) "nope")
  expect_warning(
    read_cookie_db(
      fixture("chromium_cookies_v24.sqlite"),
      "chromium",
      "Chrome"
    ),
    "could not be decrypted"
  )
})

test_that("the keyring is only asked if there are v11 values", {
  local_mocked_bindings(current_os = function() "Linux")
  db <- withr::local_tempfile(fileext = ".sqlite")
  file.copy(fixture("chromium_cookies_v24.sqlite"), db)
  conn <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(conn, "DELETE FROM cookies WHERE name = 'tracker'")
  DBI::dbDisconnect(conn)
  local_mocked_bindings(
    get_chromium_password = function(keyring_name) stop("keyring asked")
  )
  cookies <- read_cookie_db(db, "chromium", keyring_name = "Chrome")
  expect_equal(cookies$value, c("abc123", "dark", "yes"))
})

test_that("Chromium Windows databases are decrypted with AES-256-GCM", {
  local_mocked_bindings(current_os = function() "Windows")
  local_mocked_bindings(get_chromium_key_windows = function(root) as.raw(0:31))
  expect_warning(
    cookies <- read_cookie_db(
      fixture("chromium_cookies_windows_v24.sqlite"),
      "chromium",
      root = "/fake/User Data"
    ),
    "v20"
  )
  expect_named(cookies, cookie_cols)
  expect_s3_class(cookies$expiration, "POSIXct")
  expect_equal(cookies$domain, c(".example.com", "www.example.com"))
  expect_equal(cookies$flag, c(TRUE, FALSE))
  expect_equal(cookies$secure, c(TRUE, FALSE))
  expect_equal(cookies$name, c("session_id", "theme"))
  expect_equal(cookies$value, c("secret", "light"))
  expect_equal(as.numeric(cookies$expiration), c(2147483647, 0))
})

test_that("v20 cookies produce a warning and are dropped", {
  local_mocked_bindings(current_os = function() "Windows")
  local_mocked_bindings(get_chromium_key_windows = function(root) as.raw(0:31))
  expect_warning(
    cookies <- read_cookie_db(
      fixture("chromium_cookies_windows_v24.sqlite"),
      "chromium",
      root = "/fake/User Data"
    ),
    "app-bound encryption.*v20"
  )
  expect_false("v20_cookie" %in% cookies$name)
})

test_that("dpapi_unprotect() calls PowerShell and decodes the result", {
  local_mocked_bindings(
    run_powershell_script = function(path) "aGVsbG8="  # base64("hello")
  )
  result <- dpapi_unprotect(as.raw(1:4))
  expect_type(result, "raw")
  expect_equal(result, charToRaw("hello"))
})

test_that("get_chromium_key_windows() reads and strips the DPAPI prefix", {
  dir <- withr::local_tempdir()
  local_state <- list(os_crypt = list(
    encrypted_key = openssl::base64_encode(
      c(charToRaw("DPAPI"), as.raw(1:4)),
      linebreaks = FALSE
    )
  ))
  jsonlite::write_json(local_state, file.path(dir, "Local State"))
  local_mocked_bindings(dpapi_unprotect = function(raw_bytes) {
    expect_equal(raw_bytes, as.raw(1:4))
    as.raw(rep(0L, 32L))
  })
  key <- get_chromium_key_windows(dir)
  expect_type(key, "raw")
  expect_length(key, 32L)
})

test_that("decrypt_chromium_value_gcm() checks the hash prefix", {
  key <- as.raw(0:31)
  nonce <- as.raw(1:12)
  plain_with_hash <- c(
    as.raw(openssl::sha256(charToRaw("a.test"))),
    charToRaw("crumb")
  )
  raw_ct <- openssl::aes_gcm_encrypt(plain_with_hash, key, iv = nonce)
  ct <- c(nonce, as.raw(raw_ct), attr(raw_ct, "tag"))
  expect_equal(decrypt_chromium_value_gcm(ct, key, "a.test", TRUE), "crumb")
  expect_null(decrypt_chromium_value_gcm(ct, key, "b.test", TRUE))
  expect_null(decrypt_chromium_value_gcm(ct, as.raw(rep(0L, 32L)), "a.test", TRUE))
})

test_that("decrypt_chromium_value() checks the hash prefix", {
  key <- chromium_linux_key("peanuts")
  ct <- openssl::aes_cbc_encrypt(
    c(as.raw(openssl::sha256(charToRaw("a.test"))), charToRaw("crumb")),
    key,
    iv = rep(as.raw(0x20), 16)
  )
  expect_equal(decrypt_chromium_value(ct, key, "a.test", TRUE), "crumb")
  expect_null(decrypt_chromium_value(ct, key, "b.test", TRUE))
  expect_null(
    decrypt_chromium_value(ct, chromium_linux_key(""), "a.test", TRUE)
  )
})

test_that("Chromium passwords are looked up in the keyring", {
  calls <- list()
  local_mocked_bindings(
    run_quietly = function(command, args) {
      calls[[length(calls) + 1]] <<- c(command, args)
      if (command == "kwallet-query") "secret" else NULL
    }
  )
  local_mocked_bindings(has_command = function(command) TRUE)

  withr::local_envvar(XDG_CURRENT_DESKTOP = "GNOME")
  expect_equal(get_chromium_password("Chrome"), "secret")
  # Secret Service is tried first, with Chromium's schema
  expect_equal(calls[[1]][1], "secret-tool")
  expect_true(all(
    c("chrome_libsecret_os_crypt_password_v2", "application", "chrome") %in%
      calls[[1]]
  ))
  expect_equal(calls[[length(calls)]][1], "kwallet-query")
  expect_true(shQuote("Chrome Safe Storage") %in% calls[[length(calls)]])
  expect_true(shQuote("Chrome Keys") %in% calls[[length(calls)]])

  calls <- list()
  withr::local_envvar(XDG_CURRENT_DESKTOP = "KDE")
  expect_equal(get_chromium_password("Chromium"), "secret")
  expect_length(calls, 1L)
  expect_equal(calls[[1]][1], "kwallet-query")
})

test_that("missing keyring tools give no password", {
  local_mocked_bindings(has_command = function(command) FALSE)
  expect_null(get_chromium_password("Chrome"))
})

test_that("find_cookiejar() finds cookie databases", {
  root <- withr::local_tempdir()
  for (d in c(
    "ff/abc.default",
    "ff/def.work",
    "appdata/Wolf/x.default",
    "chr/Default/Network",
    "chr/Profile 1"
  )) {
    dir.create(file.path(root, d), recursive = TRUE)
  }
  for (f in c(
    "ff/abc.default/cookies.sqlite",
    "ff/def.work/cookies.sqlite",
    "appdata/Wolf/x.default/cookies.sqlite",
    "chr/Default/Cookies",
    "chr/Default/Network/Cookies",
    "chr/Profile 1/Cookies"
  )) {
    file.create(file.path(root, f))
  }
  withr::local_envvar(COOKIEMONSTER_TEST_APPDATA = file.path(root, "appdata"))
  dirs <- tibble::tribble(
    ~os      , ~browser , ~engine    , ~env                         , ~path                   , ~cookie_file     , ~keyring_name ,
    "Linux"  , "Fox"    , "firefox"  , NA                           , file.path(root, "ff")   , "cookies.sqlite" , NA            ,
    "Linux"  , "Fox"    , "firefox"  , NA                           , file.path(root, "nope") , "cookies.sqlite" , NA            ,
    "Linux"  , "Wolf"   , "firefox"  , "COOKIEMONSTER_TEST_APPDATA" , "Wolf"                  , "cookies.sqlite" , NA            ,
    "Linux"  , "Wolf"   , "firefox"  , "COOKIEMONSTER_TEST_UNSET"   , "Wolf"                  , "cookies.sqlite" , NA            ,
    "Linux"  , "Shiny"  , "chromium" , NA                           , file.path(root, "chr")  , "Cookies"        , "Shiny"       ,
    "Darwin" , "Fox"    , "firefox"  , NA                           , file.path(root, "ff")   , "cookies.sqlite" , NA
  )

  # case-insensitive, multiple profiles, missing dirs are fine
  jars <- find_cookiejar("fox", dirs = dirs, os = "Linux")
  expect_equal(nrow(jars), 2L)
  expect_equal(unique(jars$browser), "Fox")
  expect_equal(unique(jars$engine), "firefox")
  expect_setequal(basename(dirname(jars$path)), c("abc.default", "def.work"))
  expect_equal(attr(jars, "searched"), file.path(root, c("ff", "nope")))

  # paths relative to environment variables; unset variables are skipped
  jars <- find_cookiejar("Wolf", dirs = dirs, os = "Linux")
  expect_equal(nrow(jars), 1L)
  expect_equal(jars$root, file.path(root, "appdata", "Wolf"))

  # Network/Cookies is preferred over Cookies in the same profile
  jars <- find_cookiejar("Shiny", dirs = dirs, os = "Linux")
  expect_setequal(
    jars$path,
    file.path(root, c("chr/Default/Network/Cookies", "chr/Profile 1/Cookies"))
  )
  expect_equal(unique(jars$keyring_name), "Shiny")

  expect_error(
    find_cookiejar("Shiny", dirs = dirs, os = "Darwin"),
    "not supported on Darwin"
  )
  expect_error(find_cookiejar("Shiny", dirs = dirs, os = "Darwin"), "Fox")

  # no database found
  jars <- find_cookiejar("Fox", dirs = dirs[2, ], os = "Linux")
  expect_equal(nrow(jars), 0L)
  expect_named(jars, c("browser", "engine", "keyring_name", "root", "path"))
})

test_that("cookie_dirs uses Sys.info() names and known engines", {
  expect_true(all(cookie_dirs$os %in% c("Linux", "Windows", "Darwin")))
  expect_true(all(cookie_dirs$engine %in% c("firefox", "chromium", "safari")))
  for (engine in unique(cookie_dirs$engine)) {
    expect_true(is.function(getS3method("prep_db", engine)))
  }
})

test_that("get_cookies_from_browser() combines profiles", {
  local_mocked_bindings(find_cookiejar = function(browser) {
    tibble::tibble(
      browser = "Firefox",
      engine = "firefox",
      keyring_name = NA_character_,
      root = fixture(),
      path = fixture(c("firefox_cookies.sqlite", "firefox_cookies_v15.sqlite"))
    )
  })
  cookies <- get_cookies_from_browser("Firefox")
  expect_named(cookies, cookie_cols)
  # both fixtures hold the same cookies
  expect_equal(nrow(cookies), 3L)
})

test_that("get_cookies_from_browser() fails if no database is found", {
  local_mocked_bindings(find_cookiejar = function(browser) {
    structure(tibble::tibble(path = character()), searched = "/no/such/dir")
  })
  expect_error(get_cookies_from_browser("Firefox"), "No cookie database")
  expect_error(get_cookies_from_browser("Firefox"), "/no/such/dir")
})

test_that("unknown browsers give an informative error", {
  expect_error(get_cookies_from_browser("Netscape Navigator"), "not supported")
})

test_that("get cookies from Firefox", {
  skip_on_cran()
  skip_if(nrow(find_cookiejar("Firefox")) == 0L)
  cookies <- get_cookies_from_browser()
  expect_named(cookies, cookie_cols)
  expect_gt(nrow(cookies), 1L)
})


for (b in c("Chrome", "Chromium", "Brave", "Edge", "Vivaldi", "Opera")) {
  test_that(paste("get cookies from", b, "(Linux)"), {
    skip_on_cran()
    skip_if_not(current_os() == "Linux")
    skip_if(nrow(find_cookiejar(b)) == 0L)
    cookies <- get_cookies_from_browser(b)
    expect_named(cookies, cookie_cols)
    expect_false(anyNA(cookies$value))
  })
}

for (b in c("Chrome", "Chromium", "Brave", "Edge", "Vivaldi", "Opera")) {
  test_that(paste("get cookies from", b, "(Windows)"), {
    skip_on_cran()
    skip_if_not(current_os() == "Windows")
    skip_if(nrow(find_cookiejar(b)) == 0L)
    cookies <- get_cookies_from_browser(b)
    expect_named(cookies, cookie_cols)
  })
}
