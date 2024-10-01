#' Prepare input for [tp_go()]
#'
#' @description
#' `tp_write` writes an JSON file to be used by [tp_go()].
#'
#' @param model_data_strlist a structured list coming from [tp_import()].
#'
#' @details
#' This function takes a structured list of TreePPL code and data files (more details in [tp_import()]) and write 2 files from them.
#'
#' @return None.
#' @export

tp_write <- function(model_data_strlist){

  dir <- tp_tempdir()

  cat(model_data_strlist[[1]], file = paste0(dir, "input.tppl"))

  # write json with input data
  input_json <- jsonlite::toJSON(model_data_strlist[[2]], dataframe = "columns")

  write(input_json, paste0(dir, "input.json"))
}
