# cookiemonster (development version)

* `get_cookies_from_browser()` got a makeover:
  * now works on macOS (the cookie jar was never found there)
  * LibreWolf works (it failed before) and Zen, Waterfox and Floorp are supported, plus Firefox installed via Flatpak, in `~/.config/mozilla` (Firefox 147+) or from the Microsoft Store
  * the browser name is case-insensitive, and unknown browsers or missing cookie databases produce an informative error instead of an empty result
  * `flag` now means "include subdomains", as everywhere else in the package (it was filled with the `sameSite` attribute)
  * expiration dates of Firefox 142+ (stored in milliseconds) are converted correctly
  * cookies the browser has not yet written to the main database file (SQLite WAL) are included, and profiles no longer overwrite each other's temporary copy
  * `dplyr` is no longer needed
  * Chromium-based browsers (Chrome, Chromium, Brave, Edge, Vivaldi and Opera) are supported on Linux, including Snap and Flatpak installs. Encrypted cookies are decrypted with the browser's password from the Secret Service (`secret-tool`) or KWallet (`kwallet-query`)
  * Chromium-based browsers are supported on Windows. The cookie key is unprotected via PowerShell (DPAPI); cookies with app-bound encryption (`v20`, Chrome 127+) can't be decrypted and are dropped with a warning
  * Chromium-based browsers are supported on macOS, with the browser's password from the macOS Keychain (macOS asks for permission)
  * Safari is supported (needs Full Disk Access for the app you run R in)

* `get_cookies()` (and therefore `req_cookiemonster_set()`) no longer returns cookies that are past their expiration date, just like a browser would not send them anymore. Use `include_expired = TRUE` to get them anyway. Session cookies are not affected
* fixes cookies with a `#HttpOnly_` prefix but no leading dot getting an `NA` domain (such cookies could not be retrieved and replaced each other across websites)
* fixes `add_cookies(session = )` failing with "Column `sameParty` doesn't exist" since Chrome stopped sending that field. `flag` now means "include subdomains" (domain starts with a dot), as in cookie files, and missing fields become `NA` instead of an error. This also removes the hidden dependency on `dplyr` (which is only suggested)

# cookiemonster 0.1.0

* Fixes issues in tests

# cookiemonster 0.0.4

* adds support for cookies from rvest::read_html_live sessions
* support to read cookies from Firefox
* adds req_cookiemonster_set to use with httr2

# cookiemonster 0.0.3

* fixes issue with "HTTPonly" cookies

# cookiemonster 0.0.2

* adds delete cookies function
* improves filtering cookies taken from the jar

# cookiemonster 0.0.1

* Initial release of the package
