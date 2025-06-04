#' Parse TreePPL json coin output
#'
#' @description
#' `tp_parse_coin` take TreePPL json output and return a data.frame
#'
#' @param treeppl_out a character vector giving the TreePPL json output.
#' @param n_runs a [base::integer] giving the number of run (mcmc)/sweap (smc).
#'
#' @details
#' This function takes a TreePPL json output and write a revBeyes data.fram
#' format.
#'
#' `treeppl_out` : A TreePPL json output coming from [tp_treeppl].
#'
#' `n_runs` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' @return List (n=n_runs) RevBayes dataframe format.
#' @export
tp_parse_coin <- function(treeppl_out,
                          n_runs = 1) {
  if (n_runs == 1) {
    treeppl_out <- list(treeppl_out)
  }

  result_list <- list()

  for (index in seq_along(treeppl_out)) {
    result_list <-
      rbind(result_list, as.data.frame(treeppl_out[[index]]))
  }

  return(result_list)
}

#' Parse TreePPL json output
#'
#' @description
#' `tp_parse` take TreePPL json output and return a data.frame
#'
#' @param treeppl_out a character vector giving the TreePPL json output.
#' @param n_runs a [base::integer] giving the number of run (mcmc)/sweap (smc).
#'
#' @details
#' This function takes a TreePPL json output and write a revBeyes data.fram
#' format.
#'
#' `treeppl_out` : A TreePPL json output coming from [tp_treeppl].
#'
#' `n_runs` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' @return List (n=n_runs) RevBayes dataframe format.
#' @export

tp_parse <- function(treeppl_out, n_runs = 1) {
  if (n_runs == 1) {
    treeppl_out <- list(treeppl_out)
  }

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

    for (i in seq_along(output_trppl[1][[1]])) {
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
