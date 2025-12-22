#' Fetch the latest version of treeppl
#' @export

tp_fp_fetch <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    # no self container for Windows, need to install it manually
    0.0
  } else {
    # get repo info
    repo_info <- gh::gh("GET /repos/treeppl/treeppl/releases")
    # Check for Linux
    if (Sys.info()["sysname"] == "Linux") {
      # assets[[2]] because releases are in alphabetical order (1 = Mac, 2 = Linux)
      asset <- repo_info[[1]]$assets[[2]]
      folder_name <- "treeppl-linux"
    } else {
      asset <- repo_info[[1]]$assets[[1]]
      folder_name <- "treeppl-mac"
    }

    # online hash
    online_hash <- asset$digest
    # local hash
    file_name <- list.files(path = system.file(folder_name, package = "treepplr"), full.names = TRUE)
    # download file if file_name is empty
    if (length(file_name) == 0) {
      # create destination folder
      dest_folder <- paste(system.file(package = "treepplr"), folder_name, sep = "/")
      system(paste("mkdir", dest_folder))
      # download
      fn <- paste(dest_folder, asset$name, sep = "/")
      curl::curl_download(
        asset$browser_download_url,
        destfile = fn,
        quiet = FALSE
      )
    } else {
      local_hash <- paste0("sha256:", cli::hash_file_sha256(file_name))
      # compare local and online hash and download the file if they differ
      if (!identical(local_hash, online_hash)) {
        # remove old file
        file.remove(file_name)
        # download
        fn <- paste(system.file(package = "treepplr"), folder_name, asset$name, sep = "/")
        curl::curl_download(
          asset$browser_download_url,
          destfile = fn,
          quiet = FALSE
        )
      }
    }
  }
  repo_info[[1]]$tag_name
}

# Platform-dependent treeppl self contain installation
installing_treeppl <- function() {
  tag <- tp_fp_fetch()
  if (Sys.info()['sysname'] == "Windows") {
    # No self container for Windows, need to install it manually
    "tpplc"
  } else if(Sys.info()['sysname'] == "Linux") {
    path <- system.file("treeppl-linux", package = "treepplr")
    file_name <- paste0("treeppl-",substring(tag, 2))
  } else {#Mac OS have a lot of different name
    path <- system.file("treeppl-mac", package = "treepplr")
    file_name <- paste0("treeppl-",substring(tag, 2))
  }
  # Test if tpplc is already here
  tpplc_path <- paste0("/tmp/",file_name,"/tpplc")
  if(!file.exists(tpplc_path)) {
    utils::untar(list.files(path=path, full.names=TRUE),
                 exdir="/tmp")
  }
  tpplc_path
}



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
    tree_inference = "tree_inference",
    crbd = "crbd",
    clads = "clads"
  )
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



