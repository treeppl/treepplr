#' Create a phyjson object
#'
#' @description
#' `tp_phyjson` takes a variable number of argument and return a phyjson object.
#'
#' @param ... Variadic arguments (see details).
#'
#' @details
#' This function takes a variable number of argument, so that users can pass as
#' arguments either independent lists for each parameter or a single structured
#' list of list. It's use like a list (name_arg = value_arg, etc).
#'
#' @return A phyjson object (S3).
#' @export
#'
tp_phyjson <- function(...) {
  dotlist <- list(...)

  if (length(dotlist) == 1L && is.list(dotlist[[1]])) {
    dotlist <- dotlist[[1]]
  }

  class(dotlist) <- "phyjson"

  dotlist
}

#' Create a from a phylo object
#'
#' @description
#' `tp_phylo_2_phyjson` takes an object of class "phylo" and return a phyjson
#' object.
#'
#' @param phylo_tree an object of class [ape::phylo].
#'
#' @return A phyjson object (S3)
#' @export
#'

tp_phylo_2_phyjson <- function(phylo_tree) {
  name <- deparse(substitute(phylo_tree))

  df_ <- tibble::as_tibble(phylo_tree)

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

  tp_phyjson(name = name,
             tree = tree,
             root_index = root_index)
}

#' Convert a phyjson object to phyjson list
#'
#' @description
#' `tp_phyjson` takes an object of class "phyjson" and return a phyjson list
#' ready to be export as a JSON
#'
#' @param phyjson an object of class "phyjson".
#'
#' @return A structured list how contain a phyjson
#' @export
#'
tp_phyjson_list <- function(phyjson) {
  pjs_list <- list(rec_phyjson_list(phyjson$tree, phyjson$root_index))
  names(pjs_list) <- phyjson$name

  pjs_list
}

rec_phyjson_list <- function(tree, row_index) {
  row <- tree[row_index, ]

  sub_pjs_list <- list("label" = row$Label, "age" = row$Age)
  if (row$Type != "Leaf") {
    sub_pjs_list <- c(sub_pjs_list,
                      list(
                        "left" = rec_phyjson_list(tree, row$Left),
                        "right" = rec_phyjson_list(tree, row$Right)
                      ))
  }

  pjs_list <- list("__constructor__" = row$Type, "__data__" = sub_pjs_list)

  pjs_list
}
