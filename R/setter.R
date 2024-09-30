#' Prepare input for inference with TreePPL
#'
#' This function takes R objects and writes out an JSON file to be used by `tp_run()`.
#'
#' @param model_string A string contain the TreePPL model to be included in the trppl input file.
#' @param data_treeppl_input A list of R objects contain the TreePPL data to be included in the TreePPL JSON input file.
#'
#' @return tuple : path to TreePPL file / path to JSON file
#' @export
#'
#' @examples
tp_write <- function(model_data_strlist){

  dir <- tp_tempdir()

  cat(model_data_strlist[[1]], file = paste0(dir, "input.tppl"))

  # write json with input data
  input_json <- jsonlite::toJSON(model_data_strlist[[2]], dataframe = "columns")

  write(input_json, paste0(dir, "input.json"))
}
