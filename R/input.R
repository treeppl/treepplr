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
tp_input <- function(model_string, data_treeppl_input){

  sep <- .sep()
  dir <- tp_tempdir()

  # process one element at a time?

  # figure out the class and if anything special is necessary

  # write json with input data
  input_json <- jsonlite::toJSON(data, dataframe = "columns")
  write(input_json, paste0(dir, sep ,"input.json"))
}
