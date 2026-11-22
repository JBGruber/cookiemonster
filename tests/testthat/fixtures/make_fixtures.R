# Creates the cookie database fixtures used in test-browser.R. All cookies are
# fake. Run from the package root:
# source("tests/testthat/fixtures/make_fixtures.R")

fixture_dir <- file.path("tests", "testthat", "fixtures")

# Firefox ----------------------------------------------------------------------
moz_schema <- "CREATE TABLE moz_cookies (id INTEGER PRIMARY KEY, originAttributes TEXT NOT NULL DEFAULT '', name TEXT, value TEXT, host TEXT, path TEXT, expiry INTEGER, lastAccessed INTEGER, creationTime INTEGER, isSecure INTEGER, isHttpOnly INTEGER, inBrowserElement INTEGER DEFAULT 0, sameSite INTEGER DEFAULT 0, schemeMap INTEGER DEFAULT 0, isPartitionedAttributeSet INTEGER DEFAULT 0, updateTime INTEGER, CONSTRAINT moz_uniqueid UNIQUE (name, host, path, originAttributes))"

# expiry in seconds; 2^31 - 1 is 2038-01-19 03:14:07 UTC
firefox_cookies <- data.frame(
  name = c("session_id", "theme", "tracker"),
  value = c("abc123", "dark", "xyz"),
  host = c(".example.com", "www.example.com", ".cookies.test"),
  path = c("/", "/", "/account"),
  expiry = c(2147483647, 1700000000, 0),
  isSecure = c(1L, 0L, 1L),
  isHttpOnly = c(1L, 0L, 0L),
  sameSite = c(1L, 2L, 0L)
)

make_firefox_db <- function(file, schema_version, expiry_factor) {
  unlink(file)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, moz_schema)
  DBI::dbExecute(conn, paste0("PRAGMA user_version = ", schema_version))
  cookies <- firefox_cookies
  cookies$expiry <- cookies$expiry * expiry_factor
  DBI::dbAppendTable(conn, "moz_cookies", cookies)
}

# current Firefox (>= 142): expiry in milliseconds
make_firefox_db(file.path(fixture_dir, "firefox_cookies.sqlite"), 17, 1000)
# older Firefox: expiry in seconds
make_firefox_db(file.path(fixture_dir, "firefox_cookies_v15.sqlite"), 15, 1)
