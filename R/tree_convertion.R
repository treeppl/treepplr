#' Convert phylo obj to TreePPL tree
#'
#' @description
#' `tp_phylo_to_tpjson` takes an object of class `phylo` and returns
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

tp_phylo_to_tpjson <- function(phylo_tree, age = "") {

  name <- "tree"
  root_tree <- tp_phylo_to_tppl_tree(phylo_tree)

  if (age != "branch-length") {
    tree <- tree_age_cumul(root_tree[[2]], root_tree[[1]], age)
  }

  json_df <- tp_list(name = name,
                     tree = tree,
                     root_index = root_tree[[1]])

  tp_treeppl_json(json_df)
}

#' Convert phylo to a tppl_tree
#'
#' @description
#' `tp_phylo_to_tppl_tree` takes an object of class `phylo` and returns
#' a tppl_tree.
#'
#' @param phylo_tree an object of class [ape::phylo].
#'
#' @return A pair (root index, tppl_tree)
#'

tp_phylo_to_tppl_tree <- function(phylo_tree) {
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
#' @param tree an tppl_tree create with [treepplr::tp_phylo_to_tppl_tree].
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
#' `tp_treeppl_json` takes an tppl_tree create with [treepplr::tp_phylo_to_tppl_tree]
#'  and returns a list ready to be exported with JSON
#'
#' @param tree an object of class json.
#'
#' @return A TreePPL json str
#' @export
#'
tp_treeppl_json <- function(tree) {
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



#' Convert TreePPL multi-line JSON to R phylo/multiPhylo object with associated
#' weights
#'
#' @description
#' `tp_json_to_phylo` takes the path to a TreePPL json output and returns an
#' object of class `phylo`.
#'
#' @param json_out One of two options:
#'   * A list of TreePPL output in parsed JSON format (output from
#'   [treepplr::tp_treeppl()]), OR
#'   * The full path of the json file containing the TreePPL output.
#'
#' @return A list with two elements:
#'         $trees: A 'phylo' object (if one tree) or 'multiPhylo' object (if multiple).
#'         $weights: A numeric vector of sample weights.
#' @export
tp_json_to_phylo <- function(json_out) {

  res <- try(file.exists(json_out), silent = TRUE)
  # If path exists, import output from file
  if (!is(res, "try-error") && res) {

    ## Read lines and parse each line as a separate JSON object
    raw_lines <- readLines(json_out, warn = FALSE)

    # Filter out empty lines just in case
    raw_lines <- raw_lines[raw_lines != ""]

    json_list <- lapply(raw_lines, function(x) {
      jsonlite::fromJSON(x, simplifyVector = FALSE)})

    # If path doesn't exist, then it should be a list
  } else if (is.list(json_out)) {
    json_list <- json_out
  } else {
    stop("Incorrect input format")
  }

  ## Process each tree in the list
  newick_strings <- sapply(json_list, function(sweep) {

    # loop over all particles within the sweep
    samples <- sweep$samples

    sweep_string <- sapply(samples, function(particle) {

      # Extract the root age
      root_data <- particle[[1]]$`__data__`
      root_age  <- root_data$age

      # Start recursion from the children of the root.
      # The root itself does not have a branch length in standard Newick,
      # but it provides the 'parent_age' for its immediate children.

      left_str  <- build_newick_node(root_data$left, root_age)
      right_str <- build_newick_node(root_data$right, root_age)

      # Construct final Newick string: (ChildL, ChildR);
      paste0("(", left_str, ",", right_str, ");")
    })

  })

  # 4. Convert to phylo
  trees <- ape::read.tree(text = newick_strings)

  # 5. Get weight for each tree
  nweight_matrix <- sapply(json_list, function(sweep) {

    nconst <- sweep$normConst
    logweights <- unlist(sweep$weights)
    log_nw <- nconst + logweights

  })

  nweights <- as.vector(nweight_matrix)
  norm_weights <- exp(nweights - max(nweights))

  return(list(trees = trees, weights = norm_weights))
}


# 2. Recursive function to build Newick string
# 'parent_age' is passed down from the caller
build_newick_node <- function(node, parent_age) {

  type <- node[["__constructor__"]]
  data <- node[["__data__"]]

  # Ensure ages are numeric
  parent_age <- as.numeric(parent_age)

  if (type == "Leaf") {
    # Rule: Leaf branch length is the age of its parent node
    len <- parent_age

    if (is.null(data$label)){
      label <- data$index
    } else {
      label <- data$label
    }

    # Return "Label:Length"
    return(paste0(label, ":", len))

  } else if (type == "Node") {
    # Rule: Internal node branch length = Parent Age - Own Age
    current_age <- as.numeric(data$age)
    len <- parent_age - current_age

    # Recursively process children, passing CURRENT age as their 'parent_age'
    left_str <- build_newick_node(data$left, current_age)
    right_str <- build_newick_node(data$right, current_age)

    # Return "(Left,Right):Length"
    return(paste0("(", left_str, ",", right_str, "):", len))
  }
}

#' Find the Maximum A Posteriori (MAP) Tree from weighted samples
#'
#' @param trees_out The list returned by [treepplr::tp_json_to_phylo]
#' (containing $trees and $weights)
#'
#' @returns
#' @export
#'
#' @examples
tp_map_tree <- function(trees_out) {

  trees <- trees_out$trees
  weights <- trees_out$weights

  # Handle Log-Weights (Optional Check)
  # If weights are negative (log-scale), convert them to probabilities first
  if (any(weights < 0)) {
    message("Log-weights detected. converting to relative probabilities...")
    # Subtract max to avoid underflow/overflow issues
    weights <- exp(weights - max(weights))
  }

  # Identify unique topologies
  trees_ready <- lapply(trees, function(tree) {

    # normalize edge lengths for the tip reordering
    tree$edge.length <- tree$edge.length/max(tree$edge.length)
    # Ladderize to fix edge indices
    tree_lad <- ladderize_tree(tree)
    # Order tip labels as similarly as possible
    tree_ord <- bnpsd::tree_reorder(tree_lad, sort(tree_lad$tip.label))
    # Remove edge lengths to only focus on topology
    tree_ord$edge.length <- NULL
    return(tree_ord)

  })

  # This compresses the list into unique tree topologies
  unique_topologies <- ape::unique.multiPhylo(trees_ready, use.edge.length = FALSE)

  # Map every original tree to a unique topology index
  #match_indices <- match(trees_ready, unique_topologies)
  match_indices <- attr(unique_topologies, "old.index")

  # Sum weights for each unique topology
  # tapply splits the weights by the index and sums them
  topology_probs <- tapply(weights, match_indices, sum)

  # Identify the Best Topology
  best_index <- as.numeric(names(which.max(topology_probs)))

  # Calculate posterior probability of this MAP topology
  map_prob <- max(topology_probs) / sum(topology_probs)

  # Compute Mean Branch Lengths for the MAP Topology
  # We take all samples that matched the MAP topology...
  matching_indices <- which(match_indices == best_index)
  matching_trees   <- trees[matching_indices]
  matching_weights <- weights[matching_indices]

  # ...and compute a consensus to average their branch lengths.
  # Ideally, we should do a weighted average of the lengths,
  # but ape::consensus uses simple mean. For most purposes, this is sufficient.
  final_map <- map <- phangorn::allCompat(matching_trees, rooted=TRUE) |>
    phangorn::add_edge_length(matching_trees,
                              fun = function(x) weighted.mean(x, matching_weights))

  print(paste("MAP Topology found"))
  print(paste("Posterior Probability:", round(map_prob, 4)))
  print(paste("Based on the topology of", length(matching_indices),
              "samples out of", length(trees)))

  return(final_map)
}



# Function to ladderize tree and correct tip label sequence
ladderize_tree <- function(tree, temp_file = "temp", orientation = "left"){
  if(file.exists(paste0("./", temp_file))){
    stop("The chosen temporary file exists! Please choose an other temp_file name")
  }
  if(orientation == "left"){
    right <- FALSE
  }else{
    right <- TRUE
  }
  tree_temp <- ladderize(tree, right = right)
  write.tree(tree_temp, file = paste0("./", temp_file, ".tre"))
  tree_lad <- read.tree(paste0("./", temp_file, ".tre"))
  file.remove(paste0("./", temp_file, ".tre"))
  return(tree_lad)
}

