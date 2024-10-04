#' Import input for TreePPL program
#'
#' @description
#' `tp_import` takes a directory path and out an model/data to be check by `tp_check()`.
#'
#' @param dir_path a character vector giving the directory name.
#' @param project_name a character vector giving the project name.
#'
#' @details
#' This function takes TreePPL code (.tppl) and data (.json) files and import them.
#'
#' `dir_path` : The directory that contains the TreePPL code and data file. If NULL, use [tp_tempdir()] output.
#'
#' `project_name` : Name of the TreePPL code and data file in dir_path (without extension).
#'
#' @return Structured list : TreePPL model (string) and TreePPL data (Structured list)
#' @export
#'
tp_import <- function(dir_path, project_name){
  dir_path <-tp_tempdir(dir_path)
  model <- tp_import_model(dir_path, project_name)
  data <- tp_import_data(dir_path, project_name)
  out <- list(model, data)
  names(out) <- c("model","data")
  out
}

#' @export
#'
tp_import_model <- function(dir_path, model_name){
  dir_path <-tp_tempdir(dir_path)
  model <- readr::read_file(paste0(dir_path, model_name, ".tppl"))
  model
}
#' @export
#'
tp_import_data <- function(dir_path, data_name){
  dir_path <-tp_tempdir(dir_path)
  data <- jsonlite::fromJSON(paste0(dir_path, data_name, ".json"))
  data
}
