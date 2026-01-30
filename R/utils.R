# Platform-dependent treeppl self-contained installation
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


# Fetch the latest version of treeppl
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



#### Code below needs updating - Stenio is on it ####


#' Model names supported by treepplr
#'
#' @description Provides a list of all models in the TreePPL model library.
#'
#' @return A list of model names.
#' @export
tp_model_library <- function() {

  # take whatever treeppl version is in the tmp
  fd <- list.files("/tmp", pattern = "treeppl", full.names = TRUE)
  # make sure you get the most recent version if you have more than one treeppl folder in the tmp
  fd <- sort(fd, decreasing = TRUE)[1]
  # go to the right treeppl folder, whatever it is called
  fd <- list.files(fd, pattern = "treeppl", full.names = TRUE)
  # add the rest of the path
  fd <- paste0(fd, "/lib/mcore/treeppl/models")
  # model names
  mn <- list.files(fd, full.names = TRUE, recursive = TRUE, pattern = "\\.tppl$")
  subcategory <- grepl(".*models/([^/]+)/([^/]+)/([^/]+)\\.tppl$", mn)
  no_sub <- mn[!subcategory]

  # results in a data frame
  rs <- data.frame(
    "category" = sub(".*models/([^/]+)/.*", "\\1", no_sub),
    "model_name" = sub(".*/([^/]+)\\.tppl$", "\\1", no_sub)
  )
  # order by category then by model name
  rs <- rs[order(rs$category, rs$model_name, decreasing = FALSE), ]
  rownames(rs) <- NULL
  rs

}


# Find model for model_name
tp_find_model <- function(model_name) {

  # take whatever treeppl version is in the tmp
  version <- list.files("/tmp", pattern = "treeppl", full.names = FALSE)
  # make sure you get the most recent version if you have more than one treeppl folder in the tmp
  version <- sort(version, decreasing = TRUE)[1]

  res = system(paste0("find /tmp/", version," -name ", model_name, ".tppl"),
         intern = T)
}

# Find data for model_name
tp_find_data <- function(model_name) {

  # take whatever treeppl version is in the tmp
  version <- list.files("/tmp", pattern = "treeppl", full.names = FALSE)
  # make sure you get the most recent version if you have more than one treeppl folder in the tmp
  version <- sort(version, decreasing = TRUE)[1]

  system(paste0("find /tmp/", version ," -name testdata_", model_name, ".json"),
         intern = T)
}



#### Do we need these? ####

#' #' Model file names stored by user in [base::tempdir] using
#' #' [treepplr::tp_write]
#' #'
#' #' @description Provides a list of all the model file names currently
#' #' stored in [base::tempdir]. To verify if there is a
#' #' matching data file, see [treepplr::tp_stored_data].
#' #'
#' #' @return A list of model file names.
#' #' @export
#'
#' tp_stored_model <- function() {
#'   stored_files("tppl")
#' }
#'
#' #' Data file names stored by user in [base::tempdir] using
#' #' [treepplr::tp_write]
#' #'
#' #' @description Provides a list of all the data file names currently
#' #' stored in [base::tempdir]. To verify if there is a
#' #' matching model file, see [treepplr::tp_stored_model].
#' #'
#' #' @return A list of data file names.
#' #' @export
#'
#' tp_stored_data <- function() {
#'   res <- stored_files("json")
#'   list_na <- stringr::str_extract(res, "^((?!_out).)*$")
#'   list <- list_na[!is.na(list_na)]
#'   list
#' }
#'
#' #' List of compiled models in [base::tempdir]
#' #'
#' #' @description Provides a list of all the compiled model file names currently
#' #' stored in [base::tempdir].
#' #'
#' #' @return A list of compiled model file names.
#' #' @export
#'
#' tp_stored_compiled <- function() {
#'   stored_files("exe")
#' }
#'
#' #Provides file names already provided by user, stored in [base::tempdir]
#' stored_files <- function(exten) {
#'   tmp <- list.files(tp_tempdir())
#'   list_na <-
#'     stringr::str_extract(tmp, paste0(".*(?=\\.", exten, ")"))
#'   list <- stringr::str_sort(list_na[!is.na(list_na)])
#'   list
#' }
#'



