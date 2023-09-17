
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cookiemonster

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/cookiemonster)](https://CRAN.R-project.org/package=cookiemonster)
[![R-CMD-check](https://github.com/JBGruber/cookiemonster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JBGruber/cookiemonster/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/JBGruber/cookiemonster/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JBGruber/cookiemonster?branch=main)
<!-- badges: end -->

Welcome to the `cookiemonster` package, your friendly solution to
managing browser cookies in R! 🍪 Browser cookies are a way for browsers
to recognise a user and their settings throughout sessions (e.g., if you
accept a site’s terms of service, this acceptance is saved as a cookie).
The focus of `cookiemonster` lies on making it possible to use these
cookies from `R`to make request to the site (e.g., for web-scraping or
automation). If you are looking for a way to use cookies in shiny apps
you can check out the [cookies
package](https://cran.r-project.org/web/packages/cookies/index.html).

## Installation

You can install the development version of `cookiemonster` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JBGruber/cookiemonster")
```

## Reading and storing cookies

Welcome to the `cookiemonster` package, a one-stop solution to help you
navigate the delicious world of browser cookies! In this vignette, we
will explain what browser cookies are, how to read and store them using
the `cookiemonster` package, and interact with them using the modern
`httr2` package, the legacy `httr` package, and the powerful `curl`
package.

## What are Browser Cookies?

This package helps you manage browser cookies in R, making it easy to
work with cookies when sending HTTP requests. Before we dive into the
functions and features of the package, let’s briefly discuss what
browser cookies are. Browser cookies are small pieces of data that
websites send to your browser to store and later retrieve. They help
websites remember your preferences, login information, or even the items
in your shopping cart. Cookies play a crucial role in making your
browsing experience smooth and personalized. In `R` browser cookies come
in handy when working with tasks that involve web interactions, like web
scraping, browsing automation, website testing, or API calls that
require authentication. They allow your scripts to efficiently mimic
browser behavior and maintain sessions as well as user-specific data.

## Reading and Storing Cookies

Using the `cookiemonster` package, we can easily read and store cookies
for further use. First, let’s load the package:

``` r
library(cookiemonster)
```

To use cookies with the `cookiemonster`, you will need to export the
necessary cookies from your browser after visiting or logging into a
website. To do this, you can use browser extensions like [“Get
cookies.txt”](https://chrome.google.com/webstore/detail/get-cookiestxt-locally/cclelndahbckbenkjhflpdbgdldlbecc)
for Chromium-based browsers or
[“cookies.txt”](https://addons.mozilla.org/en-US/firefox/addon/cookies-txt/)
for Firefox.

Let’s import an example cookie file provided by the `cookiemonster`
package:

``` r
file.copy(
  from = system.file("extdata", "cookies.txt", package = "cookiemonster"),
  to = "."
)
```

Now, let’s add the cookies from the file to our cookie jar:

``` r
add_cookies(cookiefile = "cookies.txt")
```

## Default Cookie Storage

The `cookiemonster` package stores cookies in a default location. To see
this location, you can use:

``` r
default_jar()
```

If you want to change the default cookie storage location, you can set
the `cookie_dir` option:

``` r
options(cookie_dir = tempdir())
default_jar()
```

To revert back to the original cookie storage location:

``` r
options(cookie_dir = NULL)
default_jar()
```

To retrieve cookies for a specific domain:

``` r
get_cookies("https://hb.cran.dev")
```

## Using `cookiemonster` with other packages

### Using Stored Cookies with `httr2`

Now let’s see how to use stored cookies with the `httr2` package:

``` r
library(httr2)
resp <- request("https://hb.cran.dev/cookies/set") |> # start a request
  req_options(cookie = get_cookies("https://hb.cran.dev", as = "string")) |> # add cookies to be sent with it
  req_perform() # perform the request

resp |> 
  resp_body_json()
```

As you can see, the individual cookie values we see above are returned
correctly. This is because the server at <https://hb.cran.dev> is
configured to echo requests send to it. It shows us that the correct
cookies were send (hooray!).

### Using Stored Cookies with `httr`

To use stored cookies with the legacy `httr` package:

``` r
library(httr)
GET("https://hb.cran.dev/cookies/set", set_cookies(get_cookies("https://hb.cran.dev", as = "vector")))
```

This code uses the ‘httr’ library to set cookies from the
‘<https://hb.cran.dev>’ website (a test website for development). The
`GET` function is used to set the cookies, and the `set_cookies`
function add cookies to the request.

### Using Stored Cookies with `curl`

`curl` is the backbone of both `httr` and `httr2`, which provide a more
straightforward interface for it. You can also use `curl` directly
though (which is only recommended for advanced users though). To make
the same request as above, we can use this code:

``` r
library(curl)
h <- new_handle()
handle_setopt(h, cookie = get_cookies("https://hb.cran.dev", as = "string"))
resp <- curl_fetch_memory("https://hb.cran.dev/cookies/set", handle = h)
jsonlite::fromJSON(rawToChar(resp$content))
```

If the `curl` response contains new cookies:

``` r
h2 <- new_handle()
resp <- curl_fetch_memory("https://hb.cran.dev/cookies/set?new_cookies=moo", handle = h2)
handle_cookies(h2)
```

Use `store_cookies` to store them in your jar:

``` r
new_cookies <- handle_cookies(h2)
store_cookies(new_cookies)
get_cookies("https://hb.cran.dev")
```

Keep in mind that adding cookies for a domain will replace all
previously stored cookies for that domain by default.

Now that you have an understanding of how the `cookiemonster` package
can be used with `httr2`, `httr`, and `curl`, you’re ready to take
control of browser cookies in your R projects! Happy coding and stay
sharp, cookie monsters!
