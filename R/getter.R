#' Import input for inference with TreePPL
#'
#' This function takes a directory path and out an model/data to be check by `tp_check()`.
#'
#' @param file_path The complete path to the directory where the TreePPL model and TreePPL JSON datafile where.
#'
#' @return tuple : TreePPL model (string) and TreePPL data (r_object)
#' @export
#'
#' @examples
tp_import <- function(file_path, project_name){
  file_path <-tp_tempdir(file_path)
  model <- tp_import_model(file_path, project_name)
  data <- tp_import_data(file_path, project_name)
  out <- list(model, data)
  names(out) <- c("model","data")
  out
}

tp_import_model <- function(file_path, model_name){
  file_path <-tp_tempdir(file_path)
  model <- readr::read_file(paste0(file_path, model_name, ".tppl"))
  model
}

tp_import_data <- function(file_path, data_name){
  file_path <-tp_tempdir(file_path)
  data <- jsonlite::fromJSON(paste0(file_path, data_name, ".json"))
  data
}
