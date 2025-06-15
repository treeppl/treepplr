#' Create a json object
#'
#' @description
#' `tp_json` takes a variable number of argument and return a json object.
#'
#' @param ... Variadic arguments (see details).
#'
#' @details
#' This function takes a variable number of argument, so that users can pass as
#' arguments either independent lists for each parameter or a single structured
#' list of list. It's use like a list (name_arg = value_arg, etc).
#'
#' @return A json object (S3).
#' @export
#'
tp_json <- function(...) {
  dotlist <- list(...)

  if (length(dotlist) == 1L && is.list(dotlist[[1]])) {
    dotlist <- dotlist[[1]]
  }

  class(dotlist) <- "json"

  dotlist
}

#' Create a from a phylo object
#'
#' @description
#' `tp_phylo_2_json` takes an object of class "phylo" and return a json
#' object.
#'
#' @param phylo_tree an object of class [ape::phylo].
#'
#' @return A json object (S3)
#' @export
#'

tp_phylo_2_json <- function(phylo_tree) {
  name <- deparse(substitute(phylo_tree))

  df_ <- tidytree::as_tibble(phylo_tree)

  tree <- data.frame(matrix(
    c(NA, NA, 0.0, NA, NA),
    ncol = 5,
    nrow = nrow(df_),
    byrow = TRUE
  ))
  colnames(tree) <- c("Type", "Label", "Age", "Right", "Left")

  num_leaf <- nrow(df_) - phylo_tree$Nnode

  for (i in seq_len(nrow(df_))) {
    row <- df_[i, ]
    tree[i, "Label"] <- row$node
    tree[row$parent, "Age"] <- row$branch.length + tree[i, "Age"]

    if (row$parent == row$node) {
      tree[i, "Type"] <- "Node"
      root_index <- i
    } else {
      if (is.na(tree[row$parent, "Right"])) {
        tree[row$parent, "Right"] <- row$node
      } else {
        tree[row$parent, "Left"] <- row$node
      }
      if (i <= num_leaf) {
        tree[i, "Type"] <- "Leaf"
      } else {
        tree[i, "Type"] <- "Node"
      }
    }
  }

  json_df <- tp_json(name = name,
          tree = tree,
          root_index = root_index)

  tp_json_list(json_df)

}

#' Convert a json object to json list
#'
#' @description
#' `tp_json` takes an object of class "json" and return a json list
#' ready to be export as a JSON
#'
#' @param json an object of class "json".
#'
#' @return A structured list how contain a json
#' @export
#'
tp_json_list <- function(json) {
  pjs_list <- list(rec_json_list(json$tree, json$root_index))
  names(pjs_list) <- json$name

  pjs_list
}

rec_json_list <- function(tree, row_index) {
  row <- tree[row_index, ]

  sub_pjs_list <- list("label" = row$Label, "age" = row$Age)
  if (row$Type != "Leaf") {
    sub_pjs_list <- c(sub_pjs_list,
                      list(
                        "left" = rec_json_list(tree, row$Left),
                        "right" = rec_json_list(tree, row$Right)
                      ))
  }

  pjs_list <- list("__constructor__" = row$Type, "__data__" = sub_pjs_list)

  pjs_list
}
