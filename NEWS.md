# cookiemonster (development version)

* `get_cookies()` (and therefore `req_cookiemonster_set()`) now warns when retrieved cookies are past their expiration date (session cookies are not affected)
* fixes cookies with a `#HttpOnly_` prefix but no leading dot getting an `NA` domain (such cookies could not be retrieved and replaced each other across websites)

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
