#' Import model for TreePPL program
#'
#' @description
#' `tp_model` takes TreePPL programm code and out an model to becheck by
#' [treepplr::tp_treeppl()].
#'
#' @param model_input polymorphic parameter (see below).
#'
#' @details
#' This function takes TreePPL programm code and import them.
#'
#' `model_input` : The full path of the model file that contains the TreePPL
#'  programm code OR
#' `model_input` : The model name of one model supported by treepplr package
#' (see [treepplr::tp_model_name()] for model supported) OR
#' `model_input` : The full string containing TreePPL programm code.
#'
#' @return A TreePPL model (S3). A structured list with a 1
#' string as a representation of the TreePPL model and a class
#' object = ([treepplr::tp_model_name()] or "custom")
#' @export
#'
tp_model <- function(model_input) {
  class_model <- NULL
  res <- try(file.exists(model_input), silent = TRUE)
  # If path exist import model from film
  if (!is(res, "try-error") && res) {
    model <- readr::read_file(model_input)
    class_model <- "custom"
    # If not path exist
  } else if (assertthat::is.string(model_input)) {
    res <-
      try(get(model_input, treepplr::tp_model_name()), silent = TRUE)
    # but model_input as the name of a know model
    if (!is(res, "try-error")) {
      model <- find_file(res, "tppl")
      class_model <- res
      # OR model_input is a string
      # (need to be verify as a appropriate model later)
    } else {
      model <- model_input
      class_model <- "custom"
    }
  }

  if (!is.null(class_model)) {
    class(model) <- class_model
    model
  } else {
    stop("Unknow R type (not a valid path, a know data model or a R string)")
  }
}

#' Import data for TreePPL program
#'
#' @description
#' `tp_data` takes TreePPL program data and out an data to be use by
#' [treepplr::tp_treeppl()].
#'
#' @param data_input polymorphic parameter (see below).
#'
#' @details
#' This function takes TreePPL program data and import them.
#'
#' `data_input` : The full path of the data file that contains
#' the TreePPL program data OR
#' `data_input` : The model name of one model supported by treepplr package
#' (see [treepplr::tp_model_name()] for exemple data supported) OR
#' `data_input` : The list (or strucutred list) containing TreePPL program data.
#'
#' @return A json data (S3), see [treepplr::tp_json()] for further
#' details.
#' @export
#'
tp_data <- function(data_input) {
  res <- try(file.exists(data_input), silent = TRUE)
  # If path exist import data from film
  if (!is(res, "try-error") && res) {
    data <- tp_json(jsonlite::fromJSON(data_input))
    # If not path exist
  } else if (assertthat::is.string(data_input)) {
    res <-
      try(get(data_input, treepplr::tp_model_name()), silent = TRUE)
    # but data_input as the name of a know model
    if (!is(res, "try-error")) {
      data <- tp_json(find_file(res, "json"))
    }
    # OR data_input is a list (or a strucutred list)
    # (need to be verify as an coherant json class later)
  } else if (is.list(data_input)) {
    data <- tp_json(data_input)
  }

  if (is(data, "json")) {
    data
  } else {
    stop("Unknow R type (not a valid path, known data model, or json S3)")
  }
}
