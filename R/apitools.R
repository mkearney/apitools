


#' new_api_pkg
#'
#' Automates the creation of new API packages.
#' @export
new_api_pkg <- function() {
  pkg <- readline_(
    "What's the name of the site hosting the API?",
    "(what do you want to name the package?)"
  )
  dirs <- list.dirs(full.names = FALSE, recursive = FALSE)
  if (pkg %in% dirs) {
    stop("There's already a directory with that name.", call. = FALSE)
  }
  pkg_path <- file.path(normalizePath("."), pkg)
  dir.create(pkg_path)
  devtools::create(pkg_path)
  choices <- c(
    "Key (a single alphanumeric string)",
    "Token (multiple keys/secrets)",
    "None (no authorization method)"
  )
  abc <- menuline(
    "Do you need a key or token to access the API? (select appropriate number)",
    choices
  )
  home_dir <- normalizePath("~")
  renv_pat <- file.path(home_dir, ".Renviron")
  check_renv(renv_pat)

  ## if one single key
  if (abc == 1L) {
    ## get key
    key <- readline_("What's your key?")
    KEY_PAT <- paste0(toupper(pkg), "_KEY")
    ## set key
    .Internal(Sys.setenv(KEY_PAT, key))
    new_env_var <- paste0(KEY_PAT, "=", key)
    ## save key
    cat(
      new_env_var,
      file = renv_pat,
      fill = TRUE,
      append = TRUE
    )
    ## make key function
    keyfun <- make_key_function(pkg)
    cat(
      keyfun,
      file = file.path(pkg_path, "R", "auth.R")
    )
  } else if (abc == 2L) {
    ## get number of keys
    nkeys <- readline_("How many keys?")
    for (i in seq_len(nkeys)) {
      ## get key
      key <- readline_(paste0("Enter key #", i))
      KEY_PAT <- paste0(toupper(pkg), "_KEY_", i)
      ## set key
      .Internal(Sys.setenv(KEY_PAT, key))
      new_env_var <- paste0(KEY_PAT, "=", key)
      ## save key
      cat(
        new_env_var,
        file = renv_pat,
        fill = TRUE,
        append = TRUE
      )
    }
  } else {
    KEY_PAT <- paste0(toupper(pkg), "_KEY")
    .Internal(Sys.setenv(KEY_PAT, ""))
    new_env_var <- paste0(KEY_PAT, "=", "")
    ## save key
    cat(
      new_env_var,
      file = renv_pat,
      fill = TRUE,
      append = TRUE
    )
  }
  cat(
    utils_funs(),
    file = file.path(pkg_path, "R", "utils.R")
  )

  ## api calls functions
  ## ask for scheme
  abc <- menuline(
    "Which scheme does the API use?",
    c("http", "https", "Unsure")
  )
  scheme <- c("http", "https", "http")[abc]

  ## ask for base api url
  base <- readline_("What's the base URL? (e.g., api.twitter.com)")

  ## ask for version
  version <- readline_("What's the version? (e.g., v2.1)")

  ## create pkg base url
  pkg_base_url <- paste0(pkg, "_base_url")

  ## api call funs text
  acf <- api_call_funs(pkg, scheme, base, version)

  ## save api call funs
  cat(acf, file = file.path(pkg_path, "R", "api-call.R"))

  message("Package created!")
}



utils_funs <- function() {
'
menuline <- function(q, a) {
  message(q)
  menu(a)
}

readline_ <- function(...) {
  input <- readline(paste(c(...), collapse = ""))
  gsub("^\\"|\\"$", "", input)
}

check_renv <- function(path) {
  con <- file(path)
  x <- readLines(con, warn = FALSE)
  close(con)
  x <- paste(x, collapse = "\\n")
  cat(x, file = path, fill = TRUE)
  invisible()
}
'
}


##
make_key_function <- function(pkg) {
  funtext <- "
#' token
#'
#' Executes authorization method(s).
#'
#' @export
_pkg__token <- function() {
  PKG_KEY <- paste0(toupper(\"_pkg_\"), \"_KEY\")
  if (!PKG_KEY %in% names(Sys.getenv())) {
    ## check renv file
    home_dir <- normalizePath(\"~\")
    renv_pat <- file.path(home_dir, \".Renviron\")
    check_renv(renv_pat)
    key <- readline_(\"Please enter your API key below:\")
    KEY_PAT <- paste0(toupper(\"_pkg_\"), \"_KEY\")
    ## set key
    .Internal(Sys.setenv(KEY_PAT, key))
    new_env_var <- paste0(KEY_PAT, \"=\", key)
    ## save key
    cat(
      new_env_var,
      file = renv_pat,
      fill = TRUE,
      append = TRUE
    )
  }
  Sys.getenv(PKG_KEY)
}
"
  gsub("\\_pkg\\_", pkg, funtext)
}


api_call_funs <- function(pkg, scheme, base, version) {
  base_fun <- '
api_base <- function() {
  baseurl <- getOption("_pkg_baseurl")
  if (baseurl == "" || is.null(baseurl)) {
    options(
      _pkg_baseurl = list(
        scheme = "_scheme_",
        base = "_base_",
        version = "_version_"
      )
    )
  }
  baseurl <- getOption("_pkg_baseurl")
  paste0(baseurl$scheme, \"://\", baseurl$base, \"/\", baseurl$version)
}
'
  base_fun <- gsub("\\_pkg\\_", pkg, base_fun)
  base_fun <- gsub("\\_scheme\\_", scheme, base_fun)
  base_fun <- gsub("\\_base\\_", base, base_fun)
  base_fun <- gsub("\\_version\\_", version, base_fun)

  update_base_fun <- "
#' update base url
#'
#' @param scheme http or https
#' @param base base api, e.g., api.twitter.com
#' @param version version string, e.g., v1.1
update_api_base_url <- function(scheme, base, version) {
  abu <- getOption(\"_pkg_baseurl\")
  abu[[\"scheme\"]] <- scheme
  abu[[\"base\"]] <- base
  abu[[\"version\"]] <- version
  options(_pkg_baseurl = abu)
}
"
  update_base_fun <- gsub("\\_pkg\\_", pkg, update_base_fun)

  api_call_fun <- "
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
  params <- params[names(params) != \"\"]
  if (length(params) > 0L) {
    params <- paste(names(params), params, sep = \"=\")
    params <- paste(params, collapse = \"&\")
    params <- paste0(\"?\", params)
  }
  ## build complete request
  paste0(base, \"/\", path, params)
}
"
  paste(
    c(base_fun, update_base_fun, api_call_fun),
    collapse = "\n"
  )
}



