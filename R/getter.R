#' Import model for TreePPL program
#'
#' @description
#' `tp_model` takes TreePPL model code and out an model to be check by [treepplr::tp_check()].
#'
#' @param model polymorphic parameter (see below).
#'
#' @details
#' This function takes TreePPL model code and import them.
#'
#' `model` : The full path of the model file that contains the TreePPL model code OR
#' `model` : The model name of one model supported by treepplr package (see [treepplr::tp_model_name()] for model supported) OR
#' `model` : The full string contains the TreePPL model code.
#'
#' @return A TreePPL model (S3)
#' @export
#'
tp_model <- function(model) {
  class_model <- NULL
  res <- try(file.exists(model), silent = TRUE)
  if (class(res) != "try-error" && res) {
    model <- readr::read_file(model)
    class_model <- "custom"
  } else if (assertthat::is.string(model)) {
    res <- try(get(model, treepplr::tp_model_name()), silent = TRUE)
    if (class(res) != "try-error") {
      model <- treepplr:::find_file(res, "tppl")
      class_model <- res
    } else {
      class_model <- "custom"
    }
  }
  if (!is.null(class_model)) {
    class(model) <- class_model
    return(model)
  } else {
    stop("Unknow R type (not valid path, a know data model or a R string)")
  }
}

#' Import data for TreePPL program
#'
#' @description
#' `tp_data` takes TreePPL data and out an data to be check by [treepplr::tp_check()].
#'
#' @param data polymorphic parameter (see below).
#'
#' @details
#' This function takes TreePPL data and import them.
#'
#' `data` : The full path of the data file that contains the TreePPL data OR
#' `data` : The model name of one model supported by treepplr package (see [treepplr::tp_model_name()] for exemple data supported) OR
#' `data` : The full string contains the TreePPL data.
#'
#' @return A TreePPL data (S3)
#' @export
#'
tp_data <- function(data) {
  res <- try(file.exists(data), silent = TRUE)
  if (class(res) != "try-error" && res) {
    data <- tp_phyjson(jsonlite::fromJSON(data))
  } else if (assertthat::is.string(data)) {
    res <- try(get(data, treepplr::tp_model_name()), silent = TRUE)
    if (class(res) != "try-error") {
      data <- tp_phyjson(treepplr:::find_file(res, "json"))
    }
  }
  if (class(data) == "phyjson") {
    return(data)
  } else {
    stop("Unknow R type (not valid path, a know data model or a R string)")
  }
}
