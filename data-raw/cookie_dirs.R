## Where browsers keep their cookie databases.
##
## Columns:
## - os:           value of Sys.info()[["sysname"]] ("Linux", "Windows", "Darwin")
## - browser:      user-facing browser name (matched case-insensitively)
## - engine:       which prep_db() method reads the file ("firefox", "chromium",
##                 "safari")
## - env:          environment variable the path is relative to (NA: path is
##                 absolute or starts with "~")
## - path:         directory that is searched recursively for `cookie_file`
## - cookie_file:  file name of the cookie database
## - keyring_name: Chromium only, name used for the OS keyring entry
##
## Sources: https://github.com/borisbabic/browser_cookie3 and
## https://github.com/yt-dlp/yt-dlp/blob/master/yt_dlp/cookies.py
## (Firefox, LibreWolf). Zen, Waterfox and Floorp use the default locations of
## their profile managers. Tor Browser is skipped on purpose; Mullvad Browser is
## skipped as it does not keep cookies between sessions by default and has no
## fixed install location.

gecko <- function(browser, linux = character(), windows = character(),
                  darwin = character()) {
  rows <- list(
    tibble::tibble(os = "Linux", env = NA_character_, path = linux),
    tibble::tibble(
      os = "Windows",
      env = sub("\\\\.*", "", windows),
      path = sub("^[^\\\\]+\\\\", "", windows)
    ),
    tibble::tibble(os = "Darwin", env = NA_character_, path = darwin)
  )
  out <- do.call(rbind, rows)
  out$browser <- browser
  out$engine <- "firefox"
  out$cookie_file <- "cookies.sqlite"
  out$keyring_name <- NA_character_
  out[, c("os", "browser", "engine", "env", "path", "cookie_file", "keyring_name")]
}

cookie_dirs <- rbind(
  gecko(
    "Firefox",
    linux = c(
      # FF147+ follows the XDG base directory specification
      "~/.config/mozilla/firefox",
      "~/.mozilla/firefox",
      "~/.var/app/org.mozilla.firefox/config/mozilla/firefox",
      "~/.var/app/org.mozilla.firefox/.mozilla/firefox",
      "~/snap/firefox/common/.mozilla/firefox"
    ),
    windows = c(
      "APPDATA\\Mozilla\\Firefox",
      "LOCALAPPDATA\\Mozilla\\Firefox",
      # Microsoft Store version
      "LOCALAPPDATA\\Packages\\Mozilla.Firefox_n80bbvh6b1yt2\\LocalCache\\Roaming\\Mozilla\\Firefox"
    ),
    darwin = "~/Library/Application Support/Firefox"
  ),
  gecko(
    "LibreWolf",
    linux = c(
      "~/snap/librewolf/common/.librewolf",
      "~/.librewolf",
      "~/.var/app/io.gitlab.librewolf-community/.librewolf"
    ),
    windows = c("APPDATA\\librewolf", "LOCALAPPDATA\\librewolf"),
    darwin = "~/Library/Application Support/librewolf"
  ),
  gecko(
    "Zen",
    linux = c("~/.zen", "~/.var/app/app.zen_browser.zen/.zen"),
    windows = "APPDATA\\zen",
    darwin = "~/Library/Application Support/zen"
  ),
  gecko(
    "Waterfox",
    linux = "~/.waterfox",
    windows = "APPDATA\\Waterfox",
    darwin = "~/Library/Application Support/Waterfox"
  ),
  gecko(
    "Floorp",
    linux = c("~/.floorp", "~/.var/app/one.ablaze.floorp/.floorp"),
    windows = "APPDATA\\Floorp",
    darwin = "~/Library/Application Support/Floorp"
  )
)

stopifnot(
  all(cookie_dirs$os %in% c("Linux", "Windows", "Darwin")),
  all(cookie_dirs$engine %in% c("firefox", "chromium", "safari")),
  !anyNA(cookie_dirs$path),
  !anyNA(cookie_dirs$cookie_file)
)

usethis::use_data(cookie_dirs, overwrite = TRUE, internal = TRUE)
