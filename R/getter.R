#' Import input for TreePPL program
#'
#' @description
#' `tp_import` takes a directory path and out an model/data to be check by `tp_check()`.
#'
#' @param dir_path a character vector giving the directory name.
#' @param model_name a character vector giving the model name.
#' @param data_name a character vector giving the data name.
#'
#' @details
#' This function takes TreePPL code (.tppl) and data (.json) files and import them.
#'
#' `dir_path` : The directory that contains the TreePPL code and data file. If NULL, use [tp_tempdir()] output.
#'
#' `model_name` : Name of the TreePPL code in dir_path (without extension).
#'
#' `data_name` : Name of the TreePPL data file in dir_path (without extension).
#'
#' @return Structured list : TreePPL model (string) and TreePPL data (Structured list)
#' @export
#'
tp_import <- function(dir_path, model_name, data_name) {
  dir_path <- tp_tempdir(dir_path)
  model <- tp_import_model(dir_path, model_name)
  data <- tp_import_data(dir_path, data_name)
  out <- list(model, data)
  names(out) <- c("model", "data")
  out
}

#' Import model for TreePPL program
#'
#' @description
#' `tp_import_model` takes a directory path and out an model to be check by `tp_check()`.
#'
#' @param dir_path a character vector giving the directory name.
#' @param model_name a character vector giving the model name.
#'
#' @details
#' This function takes TreePPL code (.tppl) files and import them.
#'
#' `dir_path` : The directory that contains the TreePPL code. If NULL, use [tp_tempdir()] output.
#'
#' `model_name` : Name of the TreePPL code in dir_path (without extension).
#'
#' @return A string of TreePPL model
#' @export
#'
tp_import_model <- function(dir_path, model_name) {
  dir_path <- tp_tempdir(dir_path)
  model <- readr::read_file(paste0(dir_path, model_name, ".tppl"))
  model
}

#' Import model for TreePPL program
#'
#' @description
#' `tp_import_data` takes a directory path and out an data to be check by `tp_check()`.
#'
#' @param dir_path a character vector giving the directory name.
#' @param data_name a character vector giving the data file name.
#'
#' @details
#' This function takes TreePPL data (.json) files and import them.
#'
#' `dir_path` : The directory that contains the TreePPL data If NULL, use [tp_tempdir()] output.
#'
#' `data_name` : Name of the TreePPL data in dir_path (without extension).
#'
#' @return Structured list of TreePPL data
#' @export
#'
tp_import_data <- function(dir_path, data_name) {
  dir_path <- tp_tempdir(dir_path)
  data <- jsonlite::fromJSON(paste0(dir_path, data_name, ".json"))
  data
}

#' Create a phyjson tree object
#'
#' @description
#' `tp_phyjson_tree` takes a variable number of argument and return a phyjson tree object.
#'
#' @details
#' This function takes a variable number of argument, so that users can pass as arguments
#'  either independent lists for each parameter or a single structured list of list.
#' it's use like a list (name_arg = value_arg, etc)
#'
#' @return A phyjson tree object (S3)
#' @export
#'
tp_phyjson_tree <- function(...) {
  dotlist <- list(...)

  if (length(dotlist) == 1L && is.list(dotlist[[1]])) {
    dotlist <- dotlist[[1]]
  }

  class(dotlist) <- "phyjson_tree"

  dotlist
}

#' Create a phyjson_tree from a phylo object
#'
#' @description
#' `tp_phylo_2_phyjson_tree` takes an object of class "phylo" and return a phyjson_tree object.
#'
#' @param phylo_tree an object of class [ape::phylo].
#'
#' @return A phyjson tree object (S3)
#' @export
#'

tp_phylo_2_phyjson_tree <- function(phylo_tree) {
  name <- deparse(substitute(phylo_tree))

  df_ <- as_tibble(phylo_tree)

  tree <- data.frame(matrix(
    c(NA, NA, 0.0, NA, NA),
    ncol = 5,
    nrow = nrow(df_),
    byrow = TRUE
  ))
  colnames(tree) <- c("Type", "Label", "Age", "Right", "Left")

  num_leaf <- nrow(df_) - phylo_tree$Nnode

  for (i in 1:nrow(df_)) {
    row <- df_[i, ]
    tree[i, "Label"] = row$node
    tree[row$parent, "Age"] <- row$branch.length + tree[i, "Age"]

    if (row$parent == row$node) {
      tree[i, "Type"] = "Root"
      root_index <- i
    } else {
      if (is.na(tree[row$parent, "Right"])) {
        tree[row$parent, "Right"] <- row$node
      } else {
        tree[row$parent, "Left"] <- row$node
      }
      if (i <= num_leaf) {
        tree[i, "Type"] = "Leaf"
      } else {
        tree[i, "Type"] = "Node"
      }
    }
  }

  tp_phyjson_tree(name = name,
                  tree = tree,
                  root_index = root_index)
}
