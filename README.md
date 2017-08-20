
apitools
--------

Tools for making API wrapper packages

Description
-----------

Creates a package with authorization (keys and tokens) and basic functions used to compose URL queries to interact with web APIs.

Demo
----

In an interactive session, use the `new_api_pkg` function.

``` r
## execute function
new_api_pkg()
```

You'll be then be prompted with questions as your API package is being built to fit your needs.

``` r
## What's the name of the site hosting the API?(what do you want to name the package?)
apiPKG

## Do you need a key or token to access the API? (select appropriate number)

## 1: Key (a single alphanumeric string)
## 2: Token (multiple keys/secrets)
## 3: None (no authorization method)
1

## What's your key?
thisistheapikeyiwasprovided

## Which scheme does the API use?

## 1: http
## 2: https
## 3: Unsure
2

## What's the base URL? (e.g., api.twitter.com)
api.PKG.com

## What's the version? (e.g., v2.1)
v1.0
```

See the demo for yourself [here](apiPKG)
