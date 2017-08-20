apiPKG
================

A demo API package created by [apitools](..)

### Document package

``` r
devtools::document()
```

### Call various APIs

``` r
library(apiPKG)
api_call("somethingcool")
```

    ## [1] "https://api.PKG.com/v1.0/somethingcool"

``` r
api_call("articles/headlines")
```

    ## [1] "https://api.PKG.com/v1.0/articles/headlines"

### Along with any number of parameters

``` r
api_call("articles/headlines", n = 500, filter = "en")
```

    ## [1] "https://api.PKG.com/v1.0/articles/headlines?n=500&filter=en"

### Integrate API keys/tokens

``` r
api_call(
  "articles/headlines", 
  n = 500, 
  filter = "en", 
  token = apiPKG_token()
)
```

    ## [1] "https://api.PKG.com/v1.0/articles/headlines?n=500&filter=en&token=thisistheapikeyiwasprovided"

### Send the actual request

``` r
rurl <- api_call(
  "articles/headlines", 
  n = 500, 
  filter = "en", 
  token = apiPKG_token()
)
r <- httr::GET(rurl)
```
