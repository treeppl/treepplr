#' Import a TreePPL model
#'
#' @description
#' `tp_model` takes TreePPL code and prepares it to be used by
#' [treepplr::tp_treeppl()].
#'
#' @param model_input One of tree options:
#'   * The full path of the model file that contains the TreePPL code, OR
#'   * A string with the name of a model supported by treepplr
#' (see [treepplr::tp_model_names()]), OR
#'   * A string containing the entire TreePPL code.
#'
#' @return A TreePPL model (S3). A structured list with the
#' string containing the TreePPL model and a class
#' ([treepplr::tp_model_names()] or "custom")
#' @export
#'
tp_model <- function(model_input) {
  class_model <- NULL
  res <- try(file.exists(model_input), silent = TRUE)
  # If path exists, import model from file
  if (!is(res, "try-error") && res) {
    model <- readr::read_file(model_input)
    class_model <- "custom"
    # If path doesn't exist
  } else if (assertthat::is.string(model_input)) {
    res <-
      try(get(model_input, treepplr::tp_model_names()), silent = TRUE)
    # model_input has the name of a known model
    if (!is(res, "try-error")) {
      model <- find_file(res, "tppl")
      class_model <- res
      # OR model_input is a string
      # (needs to be verified as an appropriate model later)
    } else {
      model <- model_input
      class_model <- "custom"
    }
  }

  if (!is.null(class_model)) {
    class(model) <- class_model
    model
  } else {
    stop("Unknow R type (not a valid path, a known data model, or a model)")
  }
}

#' Import data for TreePPL program
#'
#' @description
#' `tp_data` takes data and prepares it to be used by
#' [treepplr::tp_treeppl()].
#'
#' @param data_input One of tree options:
#'   * The full path of the JSON file that contains the data, OR
#'   * A string with the name of a model supported by treepplr
#' (see [treepplr::tp_model_names()]), OR
#'   * A list (or structured list) containing TreePPL data.
#'
#' @return a list, see [treepplr::tp_check_input()] for further details.
#' @export
#'
tp_data <- function(data_input) {
  res <- try(file.exists(data_input), silent = TRUE)
  # If path exists, import data from file
  if (!is(res, "try-error") && res) {
    data <- tp_list(jsonlite::fromJSON(data_input))
    # If path doesn't exist
  } else if (assertthat::is.string(data_input)) {
    res <- try(get(data_input, treepplr::tp_model_names()), silent = TRUE)
    # data_input has the name of a known model
    if (!is(res, "try-error")) {
      data <- tp_list(find_file(res, "json"))
    }
    # OR data_input is a list (or a structured list)
  } else if (is.list(data_input)) {
    data <- tp_list(data_input)
  }

  if (is(data, "list")) {
    data
  } else {
    stop("Unknow R type (not a valid path, known data model, or list)")
  }
}
