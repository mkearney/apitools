
api_base <- function() {
  baseurl <- getOption("apiPKGbaseurl")
  if (baseurl == "" || is.null(baseurl)) {
    options(
      apiPKGbaseurl = list(
        scheme = "https",
        base = "api.PKG.com",
        version = "v1.0"
      )
    )
  }
  baseurl <- getOption("apiPKGbaseurl")
  paste0(baseurl$scheme, "://", baseurl$base, "/", baseurl$version)
}


#' update base url
#'
#' @param scheme http or https
#' @param base base api, e.g., api.twitter.com
#' @param version version string, e.g., v1.1
update_api_base_url <- function(scheme, base, version) {
  abu <- getOption("apiPKGbaseurl")
  abu[["scheme"]] <- scheme
  abu[["base"]] <- base
  abu[["version"]] <- version
  options(apiPKGbaseurl = abu)
}


#' api_call
#'
#' Composes API requests
#'
#' @param path Specific API hosted at base site.
#' @param ... Other named args are converted as query parameters.
#' @export
#' @noRd
api_call <- function(path, ...) {
  ## base
  base <- api_base()
  ## params
  params <- c(...)
  params <- params[names(params) != ""]
  if (length(params) > 0L) {
    params <- paste(names(params), params, sep = "=")
    params <- paste(params, collapse = "&")
    params <- paste0("?", params)
  }
  ## build complete request
  paste0(base, "/", path, params)
}
