# cookiemonster (development version)

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
