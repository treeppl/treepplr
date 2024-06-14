#' Prepare input for inference with TreePPL
#'
#' This function takes R objects and writes out an JSON file to be used by `run_treeppl()`.
#'
#' @param object_list A list of R objects to be included in the JSON input file.
#' @param file_path The complete path to where the JSON file is to be written.
#'
#' @return Path to a JSON file
#' @export
#'
#' @examples
treeppl_input <- function(object_list, file_path){

  sep <- .sep()

  # process one element at a time?

  # figure out the class and if anything special is necessary

  # write json with input data
  input_json <- jsonlite::toJSON(data, dataframe = "columns")
  write(input_json, paste0(dir, sep ,"input.json"))
}
