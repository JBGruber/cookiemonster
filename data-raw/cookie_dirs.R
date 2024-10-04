## locations taken from https://github.com/borisbabic/browser_cookie3
cookie_dirs <- tibble::tribble(
  ~os,       ~browser,  ~env,           ~path,
  "Linux",   "Firefox", NA_character_,  "~/snap/firefox/common/.mozilla/firefox",
  "Linux",   "Firefox", NA_character_,  "~/.mozilla/firefox",
  "Windows", "Firefox", "APPDATA",      "Mozilla\\Firefox",
  "Windows", "Firefox", "LOCALAPPDATA", "Mozilla\\Firefox",
  "macOS",   "Firefox", NA_character_,  "~/Library/Application Support/Firefox",

  "Linux",   "LibreWolf", NA_character_,  "~/snap/librewolf/common/.librewolf",
  "Linux",   "LibreWolf", NA_character_,  "~/.librewolf",
  "Windows", "LibreWolf", "APPDATA",      "librewolf",
  "Windows", "LibreWolf", "LOCALAPPDATA", "librewolf",
  "macOS",   "LibreWolf", NA_character_,  "~/Library/Application Support/librewolf",
)

usethis::use_data(cookie_dirs, overwrite = TRUE, internal = TRUE)
