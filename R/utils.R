#' Temporary directory for running treeppl
#'
#' @description
#' `tp_tempdir` returns a normalized path for a temporary directory where the
#' executables can read and write temporary files.
#'
#' @param temp_dir NULL, or a path to be used; if NULL, R's [base::tempdir]
#' is used.
#' @param sep Better ignored; non-default values are passed to
#' [base::normalizePath].
#' @param sub Extension for defining a sub-directory within the directory
#' defined by [base::tempdir].
#'
#' @return Normalized path with system-dependent terminal separator.
#' @export

tp_tempdir <- function(temp_dir = NULL,
                       sep = NULL,
                       sub = NULL) {
  if (is.null(sep)) {
    sep <- sep()
    if (is.null(temp_dir))
      temp_dir <- tempdir()
    temp_dir <- normalizePath(temp_dir, sep)
    if (substring(temp_dir, nchar(temp_dir)) != sep) {
      temp_dir <- paste0(temp_dir, sep)
    }
    if (!is.null(sub))
      temp_dir <- paste0(temp_dir, sub, sep)
    if (!dir.exists(temp_dir))
      dir.create(temp_dir)
    temp_dir
  }
}

# Platform-dependent separator character
sep <- function() {
  .Platform$file.sep
}

# Platform-dependent treeppl self contain installation
installing_treeppl <- function() {
  if(Sys.info()['sysname'] == "Linux") {
    linux_path <- system.file("linux_treeppl", package = "treepplr")
    # Test if tpplc is already here
    file_name <- sub('\\.tar\\.gz$', '',list.files(path=linux_path,
                                                   full.names=FALSE))
    tpplc_path <- paste0("/tmp/",file_name,"/tpplc")
    if(!file.exists(tpplc_path)) {
      utils::untar(list.files(path=linux_path, full.names=TRUE),
                   exdir="/tmp")
    }
    tpplc_path
  } else if (Sys.info()['sysname'] == "Windows") {
    # No self container for Windows, need to install it manually
    "tpplc"
  } else { #Mac OS have a lot of different name
    mac_path <- system.file("mac_treeppl", package = "treepplr")
    file_name <- sub('\\.tar\\.gz$', '',list.files(path=mac_path,
                                                   full.names=FALSE))
    tpplc_path <- paste0("/tmp/",file_name,"/tpplc")
    if(!file.exists(tpplc_path)) {
      utils::untar(list.files(path=mac_path, full.names=TRUE),
                   exdir="/tmp")
    }
    tpplc_path
  }
}

# Find model and data files for model_name
find_file <- function(model_name, exten) {
  if (exten == "tppl") {
    readr::read_file(paste0(
      system.file("extdata", package = "treepplr"),
      sep(),
      model_name,
      ".",
      exten
    ))
  } else if (exten == "json") {
    jsonlite::fromJSON(paste0(
      system.file("extdata", package = "treepplr"),
      sep(),
      model_name,
      ".",
      exten
    ))
  }
}

#' Model file names stored by user in [base::tempdir] using
#' [treepplr::tp_write]
#'
#' @description Provides a list of all the model file names currently
#' stored in [base::tempdir]. To verify if there is a
#' matching data file, see [treepplr::tp_stored_data].
#'
#' @return A list of model file names.
#' @export

tp_stored_model <- function() {
  stored_files("tppl")
}

#' Data file names stored by user in [base::tempdir] using
#' [treepplr::tp_write]
#'
#' @description Provides a list of all the data file names currently
#' stored in [base::tempdir]. To verify if there is a
#' matching model file, see [treepplr::tp_stored_model].
#'
#' @return A list of data file names.
#' @export

tp_stored_data <- function() {
  res <- stored_files("json")
  list_na <- stringr::str_extract(res, "^((?!_out).)*$")
  list <- list_na[!is.na(list_na)]
  list
}

#' List of compiled models in [base::tempdir]
#'
#' @description Provides a list of all the compiled model file names currently
#' stored in [base::tempdir].
#'
#' @return A list of compiled model file names.
#' @export

tp_stored_compiled <- function() {
  stored_files("exe")
}

#Provides file names already provided by user, stored in [base::tempdir]
stored_files <- function(exten) {
  tmp <- list.files(tp_tempdir())
  list_na <-
    stringr::str_extract(tmp, paste0(".*(?=\\.", exten, ")"))
  list <- stringr::str_sort(list_na[!is.na(list_na)])
  list
}

#' Model names supported by treepplr
#'
#' @description Provides a list of all model names supported by treepplr.
#' The names can also be used to find data for these models
#' (see [treepplr::tp_data]).
#'
#' @return A list of model names.
#' @export
tp_model_names <- function() {
  list(
    custom = "custom",
    coin = "coin",
    hostrep3states = "hostrep3states",
    hostrep2states = "hostrep2states",
    treeinference = "treeinference",
    crbd = "crbd",
    clads = "clads"
  )
}

#' Create a flat list
#'
#' @description
#' `tp_list` takes a variable number of arguments and returns a list.
#'
#' @param ... Variadic arguments (see details).
#'
#' @details
#' This function takes a variable number of arguments, so that users can pass as
#' arguments either independent lists, or a single structured
#' list of list (name_arg = value_arg).
#'
#' @return A list.
#' @export
#'
tp_list <- function(...) {
  dotlist <- list(...)

  if (length(dotlist) == 1L && is.list(dotlist[[1]])) {
    dotlist <- dotlist[[1]]
  }

  dotlist
}
