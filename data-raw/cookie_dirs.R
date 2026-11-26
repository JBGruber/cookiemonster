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
## (Firefox, LibreWolf, Chromium-based browsers and their keyring names). Zen, Waterfox and Floorp use the default locations of
## their profile managers. Tor Browser is skipped on purpose; Mullvad Browser is
## skipped as it does not keep cookies between sessions by default and has no
## fixed install location.

gecko <- function(
  browser,
  linux = character(),
  windows = character(),
  darwin = character()
) {
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
  out[, c(
    "os",
    "browser",
    "engine",
    "env",
    "path",
    "cookie_file",
    "keyring_name"
  )]
}

# keyring_name is the name used for the keyring entry
# ("<keyring_name> Safe Storage" on macOS; "application <name>" on Linux).
# windows_keyring defaults to linux_keyring; Windows decryption uses DPAPI and
# does not look up a named keyring, but the column must be non-NA.
# darwin_keyring is the macOS Keychain service "<darwin_keyring> Safe Storage"
# (account "<darwin_keyring>"), as used by yt-dlp; it defaults to
# windows_keyring, which already holds the macOS names.
chromium <- function(
  browser,
  linux = character(),
  linux_keyring = NA_character_,
  windows = character(),
  windows_keyring = linux_keyring,
  darwin = character(),
  darwin_keyring = windows_keyring
) {
  rows <- list()
  if (length(linux) > 0) {
    rows$linux <- tibble::tibble(
      os = "Linux",
      browser = browser,
      engine = "chromium",
      env = NA_character_,
      path = linux,
      cookie_file = "Cookies",
      keyring_name = linux_keyring
    )
  }
  if (length(windows) > 0) {
    rows$windows <- tibble::tibble(
      os = "Windows",
      browser = browser,
      engine = "chromium",
      env = sub("\\\\.*", "", windows),
      path = sub("^[^\\\\]+\\\\", "", windows),
      cookie_file = "Cookies",
      keyring_name = windows_keyring
    )
  }
  if (length(darwin) > 0) {
    rows$darwin <- tibble::tibble(
      os = "Darwin",
      browser = browser,
      engine = "chromium",
      env = NA_character_,
      path = darwin,
      cookie_file = "Cookies",
      keyring_name = darwin_keyring
    )
  }
  do.call(rbind, rows)
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
  ),
  chromium(
    "Chrome",
    linux = c(
      "~/.config/google-chrome",
      "~/.var/app/com.google.Chrome/config/google-chrome"
    ),
    linux_keyring = "Chrome",
    windows = "LOCALAPPDATA\\Google\\Chrome\\User Data",
    darwin = "~/Library/Application Support/Google/Chrome"
  ),
  chromium(
    "Chromium",
    linux = c(
      "~/.config/chromium",
      "~/snap/chromium/common/chromium",
      "~/.var/app/org.chromium.Chromium/config/chromium",
      "~/.var/app/io.github.ungoogled_software.ungoogled_chromium/config/chromium"
    ),
    linux_keyring = "Chromium",
    windows = "LOCALAPPDATA\\Chromium\\User Data",
    darwin = "~/Library/Application Support/Chromium"
  ),
  chromium(
    "Brave",
    linux = c(
      "~/.config/BraveSoftware/Brave-Browser",
      "~/.var/app/com.brave.Browser/config/BraveSoftware/Brave-Browser"
    ),
    linux_keyring = "Brave",
    windows = "LOCALAPPDATA\\BraveSoftware\\Brave-Browser\\User Data",
    darwin = "~/Library/Application Support/BraveSoftware/Brave-Browser"
  ),
  chromium(
    "Edge",
    linux = c(
      "~/.config/microsoft-edge",
      "~/.var/app/com.microsoft.Edge/config/microsoft-edge"
    ),
    linux_keyring = "Chromium",
    windows = "LOCALAPPDATA\\Microsoft\\Edge\\User Data",
    windows_keyring = "Microsoft Edge",
    darwin = "~/Library/Application Support/Microsoft Edge"
  ),
  chromium(
    "Vivaldi",
    linux = c(
      "~/.config/vivaldi",
      "~/.var/app/com.vivaldi.Vivaldi/config/vivaldi"
    ),
    linux_keyring = "Chrome",
    windows = "LOCALAPPDATA\\Vivaldi\\User Data",
    windows_keyring = "Vivaldi",
    darwin = "~/Library/Application Support/Vivaldi"
  ),
  chromium(
    "Opera",
    linux = c(
      "~/.config/opera",
      "~/.var/app/com.opera.Opera/config/opera"
    ),
    linux_keyring = "Chromium",
    windows = "APPDATA\\Opera Software\\Opera Stable",
    windows_keyring = "Opera",
    darwin = "~/Library/Application Support/com.operasoftware.Opera"
  ),
  # Safari keeps one cookie file. The sandboxed location is used since macOS
  # 10.14; the legacy location is only searched if the sandboxed one is absent
  # (see find_cookiejar()).
  tibble::tibble(
    os = "Darwin",
    browser = "Safari",
    engine = "safari",
    env = NA_character_,
    path = c(
      "~/Library/Containers/com.apple.Safari/Data/Library/Cookies",
      "~/Library/Cookies"
    ),
    cookie_file = "Cookies.binarycookies",
    keyring_name = NA_character_
  )
)

stopifnot(
  all(cookie_dirs$os %in% c("Linux", "Windows", "Darwin")),
  all(cookie_dirs$engine %in% c("firefox", "chromium", "safari")),
  !anyNA(cookie_dirs$path),
  !anyNA(cookie_dirs$cookie_file),
  !anyNA(cookie_dirs$keyring_name[cookie_dirs$engine == "chromium"])
)

usethis::use_data(cookie_dirs, overwrite = TRUE, internal = TRUE)
