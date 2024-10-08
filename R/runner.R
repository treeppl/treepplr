#' Compile and run a TreePPL program
#'
#' @description
#' `tp_go` take paths, execute TreePPL and return current [base::tempdir] path.
#'
#' @param dir_path a character vector giving the directory name.
#' @param project_name a character vector giving the project name.
#' @param src_name a character vector giving the output name.
#' @param method a character vector giving the inference method name.
#' @param samples a [base::integer] giving the number of samples (mcmc) or particules (smc).
#' @param run a [base::integer] giving the number of run (mcmc) or sweap (smc).
#'
#' @details
#' This function takes TreePPL code (.tppl) and data (.json) files, compile TreePPL code using [tp_compile()]
#' and writing TreePPL output using [tp_run()].
#'
#' TreePPL need to be install on your computer and the PATH set for R/RSTUDIO (see [install](https://www.google.com) manual).
#' The executable and the output files will be written in R's [base::tempdir()].
#'
#' `dir_path` : The directory that contains the TreePPL code and data file. If NULL, use [tp_tempdir()] output.
#'
#' `project_name` : Name of the TreePPL code and data file in dir_path (without extension).
#'
#' `src_name` : File name for TreePPL program and TreePPL output.
#'
#' `method` : Inference method to be used (smc, mcmc, etc).
#'
#' `samples` : The number of samples (mcmc) / particules (smc) during inference.
#'
#' `run` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' @return Path to the tmp R directory where output file was written.
#' @export

tp_go <- function(dir_path = NULL,
                  project_name = "input",
                  src_name = "out",
                  method = "smc-bpf",
                  samples = 1000,
                  run = 1) {
  tp_compile(dir_path, project_name, src_name, method)
  return(tp_run(dir_path, project_name, src_name, method, samples, run))
}

#' Compile a TreePPL program
#'
#' @description
#' `tp_compile` take paths, compile TreePPL code.
#'
#' @param dir_path a character vector giving the TreePPL code directory name.
#' @param project_name a character vector giving TreePPL code name.
#' @param src_name a character vector giving the compiled file name.
#' @param method a character vector giving the inference method name.
#'
#' @details
#' This function takes TreePPL code (.tppl) files and compile it.
#'
#' TreePPL need to be install on your computer and the PATH set for R/RSTUDIO (see [install](https://www.google.com) manual).
#' The executable file will be written in R's [base::tempdir()].
#'
#' `dir_path` : The directory that contains the TreePPL code and data file. If NULL, use [tp_tempdir()] output.
#'
#' `project_name` : Name of the TreePPL code (.tppl) in dir_path (without extension).
#'
#' `src_name` : File name for TreePPL program output.
#'
#' `method` : Inference method to be used.
#'
#' @return None.
#' @export

tp_compile <- function(dir_path = NULL,
                       project_name = "input",
                       src_name = "out",
                       method = "smc-bpf") {
  # smc-apf

  # if dir = NULL return temp_dir, if not return dir
  temp_dir <- tp_tempdir(dir_path)

  argum <- c(
    paste0(temp_dir, project_name, ".tppl"),
    paste("-m", method),
    paste0("--output ", temp_dir, src_name)
  )

  # Compile program
  system2("tpplc", args = argum)
}

#' Run a TreePPL program
#'
#' @description
#' `tp_run` take paths, execute compiled TreePPL code with data and return current [base::tempdir] path.
#'
#' @param dir_path a character vector giving the TreePPL data directory name.
#' @param project_name a character vector giving TreePPL data name.
#' @param src_name a character vector giving the TreePPL output name.
#' @param method a character vector giving the inference method name.
#' @param samples a [base::integer] giving the number of samples (mcmc) or particules (smc).
#' @param run a [base::integer] giving the number of run (mcmc) or sweap (smc).
#'
#' @details
#' This function takes compiled TreePPL code from [tp_compile()] and data (.json) files
#' and writing TreePPL json output.
#'
#' TreePPL need to be install on your computer and the PATH set for R/RSTUDIO (see [install](https://www.google.com) manual).
#' The executable and the output files will be written in R's [base::tempdir()].
#'
#' `dir_path` : The directory that contains TreePPL data file. If NULL, use [tp_tempdir()] output.
#'
#' `project_name` : Name of the TreePPL data file in dir_path (without extension).
#'
#' `src_name` : File name of TreePPL json output.
#'
#' `method` : Inference method to be used (smc, mcmc, etc).
#'
#' `samples` : The number of samples (mcmc) / particules (smc) during inference.
#'
#' `run` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' @return Path to the tmp R directory where output file was written.
#' @export

tp_run <- function(dir_path = NULL,
                   project_name = "input",
                   src_name = "out",
                   method = "smc-bpf",
                   samples = 1000,
                   run = "1") {
  # check inputs
  if (method == "smc-apf")
    samples <- samples + 1

  # if dir_path = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir(dir_path)

  if (is.null(src_name))
    src_name <- "out"

  if (is.null(project_name))
    project_name <- "input"

  # run
  system2(
    command = paste0(dir_path, src_name),
    args = c(paste0(dir_path, project_name, ".json"), paste(samples, run)),
    stdout = paste0(dir_path, src_name, ".json")
  )

}

#' Parse TreePPL json output
#'
#' @description
#' `tp_parse` take TreePPL json output and return a data.frame
#'
#' @param dir_path a character vector giving the TreePPL json output directory name.
#' @param src_name a character vector giving data.frame output name.
#' @param run a [base::integer] giving the number of run (mcmc) or sweap (smc).
#'
#' @details
#' This function takes a TreePPL json output file compile and write a revBeyes data.fram format.
#'
#' `dir_path` : The directory that contains TreePPL json output coming from [tp_run]. If NULL, use [tp_tempdir()] output.
#'
#' `project_name` : Name of the TreePPL json output file in dir_path (without extension).
#'
#' `run` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' @return RevBayes dataframe format.
#' @export

tp_parse <- function(dir_path = NULL,
                     src_name = "out",
                     run = 1) {
  # if dir_path = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir(dir_path)

  output_trppl <- RJSONIO::fromJSON(paste0(dir_path, src_name, ".json"))
  nbr_lam <- length(output_trppl[1][[1]][[1]][[1]]$lambda)
  nbr_col <- 14 + nbr_lam
  name_lam <- c()

  for(i in 1:nbr_lam)
  {
    name_lam <- c(name_lam, paste0("lambda",i))
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

  for (i in 1:length(output_trppl[1][[1]])) {
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

    res <- .peel_tree(
      tree,
      i,
      pindex = NA,
      output_trppl$weights[i],
      output_trppl$normConst,
      output_trppl[1][[1]][[i]][[1]]$mu,
      output_trppl[1][[1]][[i]][[1]]$beta,
      lambda,
      prevAge = NA,
      state,
      res
    )
    result <- rbind(result, res)
  }
  return(result)
}

.peel_tree <- function(subtree,
                       i,
                       pindex,
                       lweight,
                       lnorm_const,
                       mu,
                       beta,
                       lambda,
                       prevAge,
                       startState,
                       result)
{
  base <- c(
    iteration = as.numeric(i - 1),
    log_weight = as.numeric(lweight),
    log_norm_const = as.numeric(lnorm_const),
    mu = as.numeric(mu),
    beta = as.numeric(beta),
    lambda,
    node_index = as.numeric(subtree$label - 1),
    branch_start_time = as.numeric(prevAge),
    branch_end_time = as.numeric(subtree$age),
    start_state = as.numeric(startState),
    end_state = NA,
    transition_time = NA,
    parent_index = as.numeric(pindex),
    child1_index = NA,
    child2_index = NA
  )

  if (!is.null(subtree$left)) {
    base[["child1_index"]] <- as.numeric(subtree$left$`__data__`$label - 1)
    base[["child2_index"]]  <- as.numeric(subtree$right$`__data__`$label - 1)
  }

  base[["end_state"]]  <- base[["start_state"]]

  if (length(subtree$history) != 0) {
    for (j in 1:length(subtree$history)) {
      base[["start_state"]]  <- as.numeric(base[["end_state"]])
      base[["end_state"]]  <-  as.numeric(paste(subtree$history[[length(subtree$history) - j +
                                                                   1]]$`__data__`$repertoire, collapse = ""))
      base[["transition_time"]]  <- as.numeric(subtree$history[[length(subtree$history) - j +
                                                                  1]]$`__data__`$age)
      result[nrow(result) + 1, ] <- base
    }
  } else {
    result[nrow(result) + 1, ] <- base
  }

  if (!is.null(subtree$left)) {
    result <- .peel_tree(
      subtree$left$`__data__`,
      i,
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
    result <- .peel_tree(
      subtree$right$`__data__`,
      i,
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
  return(result)
}
