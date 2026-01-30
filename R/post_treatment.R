#' Parse simple TreePPL json SMC output
#'
#' @description
#' `tp_parse_smc` takes TreePPL json SMC output and returns a data.frame
#'
#' @param treeppl_out a character vector giving the TreePPL json output
#' produced by [tp_run] using an SMC method.
#'
#' @return A data frame with the output from inference in TreePPL.
#' @export
tp_parse_smc <- function(treeppl_out) {

  result_df <- list()

  for (i in seq_along(treeppl_out)) {

    samples_c <- unlist(treeppl_out[[i]]$samples)
    log_weight_c <- unlist(treeppl_out[[i]]$weights)

    if(is.null(names(samples_c))){
      result_df <- rbind(result_df,
                         data.frame(run = i,
                                    samples = samples_c,
                                    log_weight = log_weight_c,
                                    norm_const = treeppl_out[[i]]$normConst)
      )
    } else {
      result_df <- rbind(result_df,
                         data.frame(run = i,
                                    parameter = names(samples_c),
                                    samples = samples_c,
                                    log_weight = log_weight_c,
                                    norm_const = treeppl_out[[i]]$normConst)
      )
    }
  }

  result_df <- result_df |>
    dplyr::mutate(total_lweight = .data$log_weight + .data$norm_const) |>
    dplyr::mutate(norm_weight = exp(.data$total_lweight - max(.data$total_lweight))) |>
    dplyr::select(-"total_lweight")

  return(result_df)
}

#' Parse simple TreePPL json MCMC output
#'
#' @description
#' `tp_parse_mcmc` takes TreePPL json MCMC output and returns a data.frame
#'
#' @param treeppl_out a character vector giving the TreePPL json output
#' produced by [tp_run] using an MCMC method.
#'
#' @return A data frame with the output from inference in TreePPL.
#' @export
tp_parse_mcmc <- function(treeppl_out) {

  result_df <- list()

  for (i in seq_along(treeppl_out)) {

    samples_c <- unlist(treeppl_out[[i]]$samples)
    log_weight_c <- unlist(treeppl_out[[i]]$weights)

    if(is.null(names(samples_c))){
      result_df <- rbind(result_df,
                         data.frame(run = i,
                                    samples = samples_c,
                                    log_weight = log_weight_c,
                                    norm_const = treeppl_out[[i]]$normConst)
      )
    } else {
      result_df <- rbind(result_df,
                         data.frame(run = i,
                                    parameter = names(samples_c),
                                    samples = samples_c,
                                    log_weight = log_weight_c,
                                    norm_const = treeppl_out[[i]]$normConst)
      )
    }
  }

  return(result_df)
}


#' Parse TreePPL json output for host repertoire model
#'
#' @description
#' `tp_parse_host_rep` takes TreePPL json output from inference with the
#' model of host repertoire evolution and returns a data.frame
#'
#' @param treeppl_out a character vector giving the TreePPL json output
#' produced by [tp_run].
#'
#' @return A list (n = n_runs) of data frames with the output from inference
#' in TreePPL under the host repertoire evolution model.
#' @export

tp_parse_host_rep <- function(treeppl_out) {

  result_list <- list()

  for (index in seq_along(treeppl_out)) {
    output_trppl <- treeppl_out[[index]]

    nbr_lam <- length(output_trppl[1][[1]][[1]][[1]]$lambda)
    nbr_col <- 14 + nbr_lam
    name_lam <- c()

    for (i in 1:nbr_lam) {
      name_lam <- c(name_lam, paste0("lambda", i))
    }

    result <- data.frame(matrix(ncol = nbr_col, nrow = 0))

    colnames(result) <- c(
      "iteration",
      "log_weight",
      "log_norm_const",
      "mu",
      "beta",
      name_lam,
      "node_index",
      "branch_start_time",
      "branch_end_time",
      "start_state",
      "end_state",
      "transition_time",
      "parent_index",
      "child1_index",
      "child2_index"
    )

    #for (i in seq_along(output_trppl[1][[1]])) {
    for (i in seq_along(output_trppl$samples)) {
      res <- data.frame(matrix(ncol = nbr_col, nrow = 0))
      colnames(res) <- c(
        "iteration",
        "log_weight",
        "log_norm_const",
        "mu",
        "beta",
        name_lam,
        "node_index",
        "branch_start_time",
        "branch_end_time",
        "start_state",
        "end_state",
        "transition_time",
        "parent_index",
        "child1_index",
        "child2_index"
      )

      tree <- output_trppl[1][[1]][[i]][[1]]$tree$`__data__`

      state <- paste(tree$repertoire, collapse = "")

      lambda <- output_trppl[1][[1]][[i]][[1]]$lambda

      names(lambda) <- name_lam

      res <- peel_tree(
        tree,
        i,
        pindex = NA,
        output_trppl$weights[i],
        output_trppl$normConst,
        output_trppl[1][[1]][[i]][[1]]$mu,
        output_trppl[1][[1]][[i]][[1]]$beta,
        lambda,
        prev_age = NA,
        state,
        res
      )
      result <- rbind(result, res)
    }
    result_list[[index]] <- result
  }
  return(result_list)
}

#Recursive function to go deep in the tree
peel_tree <- function(subtree,
                       index,
                       pindex,
                       lweight,
                       lnorm_const,
                       mu,
                       beta,
                       lambda,
                       prev_age,
                       start_state,
                       result) {
  base <- c(
    iteration = as.numeric(index - 1),
    log_weight = as.numeric(lweight),
    log_norm_const = as.numeric(lnorm_const),
    mu = as.numeric(mu),
    beta = as.numeric(beta),
    lambda,
    node_index = as.numeric(subtree$label - 1),
    branch_start_time = as.numeric(prev_age),
    branch_end_time = as.numeric(subtree$age),
    start_state = as.numeric(start_state),
    end_state = NA,
    transition_time = NA,
    parent_index = as.numeric(pindex),
    child1_index = NA,
    child2_index = NA
  )

  if (!is.null(subtree$left)) {
    base[["child1_index"]] <-
      as.numeric(subtree$left$`__data__`$label - 1)
    base[["child2_index"]]  <-
      as.numeric(subtree$right$`__data__`$label - 1)
  }

  base[["end_state"]]  <- base[["start_state"]]

  chang_nbr <- length(subtree$history)
  if (chang_nbr != 0) {
    df <- data.frame(matrix(ncol = 2, nrow = chang_nbr))
    for (i in 1:chang_nbr) {
      #"end_state"
      df[i, 1] <-
        as.numeric(paste(subtree$history[[i]]$`__data__`$repertoire,
                         collapse = ""))
      #"transition_time"
      df[i, 2] <- as.numeric(subtree$history[[i]]$`__data__`$age)
    }
    df <- df[order(-df$X2), ]
    for (j in 1:chang_nbr) {
      base[["start_state"]]  <- base[["end_state"]]
      base[["end_state"]]  <- df[j, 1]
      base[["transition_time"]]  <- df[j, 2]
      result[nrow(result) + 1, ] <- base
    }
  } else {
    result[nrow(result) + 1, ] <- base
  }

  if (!is.null(subtree$left)) {
    result <- peel_tree(
      subtree$left$`__data__`,
      index,
      subtree$label - 1,
      lweight,
      lnorm_const,
      mu,
      beta,
      lambda,
      subtree$age,
      base[["end_state"]],
      result
    )
    result <- peel_tree(
      subtree$right$`__data__`,
      index,
      subtree$label - 1,
      lweight,
      lnorm_const,
      mu,
      beta,
      lambda,
      subtree$age,
      base[["end_state"]],
      result
    )
  }
  result
}


#' Check for convergence across multiple SMC sweeps/runs
#'
#' @param treeppl_out a character vector giving the TreePPL json output
#' produced by [tp_run].
#'
#' @returns Variance in the normalizing constants across SMC sweeps.
#' @export
#'
tp_smc_convergence <- function(treeppl_out) {

  output <- tp_parse_smc(treeppl_out)
  zs <- output |>
    dplyr::slice_head(n = 1, by = .data$run) |>
    dplyr::pull(.data$norm_const)

  return(var(zs))
}



#' Find the Maximum A Posteriori (MAP) Tree from weighted samples
#'
#' @param trees_out The list returned by [treepplr::tp_json_to_phylo]
#' (containing $trees and $weights)
#'
#' @returns The MAP tree as a phylo object
#' @export
#'
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
