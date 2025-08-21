#' Convert phylo obj to TreePPL tree
#'
#' @description
#' `tp_phylo_2_TreePPL` takes an object of class `phylo` and returns
#' a TreePPL json str ready to print in a data file for TreePPL use.
#'
#' @param phylo_tree an object of class [ape::phylo].
#' @param age a string that determine the way the age of the node are
#' calculated (default "branch-length").
#'
#' * "branch-length" : Root's age = NA, branch-length from the parent branch
#' * "top-down" : Root's age = 0.0, cumulative branch-length from root
#' * "down-top" : Leaf's age = 0.0, cumulative branch-length from leaf
#'
#' @return A TreePPL json str
#'
#' @export

tp_phylo_2_TreePPL <- function(phylo_tree, age = "") {

  name <- "tree"
  root_tree <- tp_phylo_2_tppl_tree(phylo_tree)

  if (age != "branch-length") {
    tree <- tree_age_cumul(root_tree[[2]], root_tree[[1]], age)
  }

  json_df <- tp_list(name = name,
                     tree = tree,
                     root_index = root_tree[[1]])

  tp_TreePPL_json(json_df)
}

#' Convert phylo to a tppl_tree
#'
#' @description
#' `tp_phylo_2_tppl_tree` takes an object of class `phylo` and returns
#' a tppl_tree.
#'
#' @param phylo_tree an object of class [ape::phylo].
#'
#' @return A pair (root index, tppl_tree)
#'

tp_phylo_2_tppl_tree <- function(phylo_tree) {
  name <- deparse(substitute(phylo_tree))

  df_ <- tidytree::as_tibble(phylo_tree)

  tree <- data.frame(matrix(
    c(NA, NA, 0.0, NA, NA),
    ncol = 5,
    nrow = nrow(df_),
    byrow = TRUE
  ))
  colnames(tree) <- c("Type", "Label", "Age", "Left", "Right")

  num_leaf <- nrow(df_) - phylo_tree$Nnode

  for (i in seq_len(nrow(df_))) {
    row <- df_[i, ]
    tree[row$node, "Label"] <- row$node
    tree[row$node, "Age"] <- row$branch.length

    if (row$parent == row$node) {
      tree[row$node, "Type"] <- "Node"
      root_index <- row$node
    } else {
      if (i <= num_leaf) {
        tree[row$node, "Type"] <- "Leaf"
      } else {
        tree[row$node, "Type"] <- "Node"
      }
      if (is.na(tree[row$parent, "Left"])) {
        tree[row$parent, "Left"] <- row$node
      } else {
        tree[row$parent, "Right"] <- row$node
      }
    }
  }
  list(root_index,tree)
}

#' Calculate age in a tppl_tree
#'
#' @description
#' `tree_age_cumul` takes an tppl_tree and returns a json with
#' branch length accumulation as node's age.
#'
#' @param tree an tppl_tree create with [treepplr::tp_phylo_2_tppl_tree].
#' @param root_index the index of the root in the tppl_tree.
#' @param age a string that determine the way the age of the node are
#' calculated (default "branch-length").
#'
#' * "top-down" : Root's age = 0.0, cumulative branch-length from root
#' * "down-top" : Leaf's age = 0.0, cumulative branch-length from leaf
#'
#' @return A tppl_tree

tree_age_cumul <- function(tree, root_index, age = "branch-length") {
  age_cumul <- rep(length(tree$Type), 0.0)
  down <- TRUE
  going_left <- c(TRUE, NA)
  i <- root_index
  if (age == "top-down") {
    tree[i, "Age"] <- 0.0
    age_cumul[i] <- 0.0
  }
  parent <- c()
  while (down || !is.na(going_left[1]) || (i != root_index)) {
    if (down) {
      if (tree[i, "Type"] == "Node") {
        parent <- c(i, parent)
        if (going_left[1]) {
          i <- tree[i, "Left"]
        } else {
          i <- tree[i, "Right"]
        }
        going_left <- c(TRUE, going_left)
        if (age == "top-down") {
          age_cumul[i] <- age_cumul[parent[1]] + tree[i, "Age"]
        }
      } else {
        down <- FALSE
        going_left <- going_left[-1]
        if (age == "down-top") {
          age_cumul[i] <- 0.0
        }
      }
    } else {
      i <- parent[1]
      parent <- parent[-1]
      if (going_left[1]) {
        down <- TRUE
        going_left[1] <- FALSE
        if (age == "down-top") {
          age_cumul[i] <- age_cumul[tree[i, "Left"]] + tree[tree[i, "Left"], "Age"]
        }
      } else {
        down <- FALSE
        going_left <- going_left[-1]
      }
    }
  }
  tree$Age <- age_cumul
  tree
}

#' Convert a tppl_tree to TreePPL json str
#'
#' @description
#' `tp_TreePPL_json` takes an tppl_tree create with [treepplr::tp_phylo_2_tppl_tree]
#'  and returns a list ready to be exported with JSON
#'
#' @param tree an object of class json.
#'
#' @return A TreePPL json str
#' @export
#'
tp_TreePPL_json <- function(tree) {
  pjs_list <- list(rec_tree_list(tree$tree, tree$root_index))
  names(pjs_list) <- tree$name

  pjs_list
}

rec_tree_list <- function(tree, row_index) {
  row <- tree[row_index, ]

  sub_pjs_list <- list("label" = row$Label, "age" = row$Age)
  if (row$Type != "Leaf") {
    sub_pjs_list <- c(sub_pjs_list,
                      list(
                        "left" = rec_tree_list(tree, row$Left),
                        "right" = rec_tree_list(tree, row$Right)
                      ))
  }

  pjs_list <- list("__constructor__" = row$Type, "__data__" = sub_pjs_list)

  pjs_list
}
