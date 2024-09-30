#' Compile and run a TreePPL program
#'
#' @param dir_path The directory that contains the TreePPL code and input file. The executable and the output files will be written here.
#' @param src_name Name of file with TreePPL program.
#' @param method Inference method to be used.
#' @param data_path Path to JSON file with input data.
#' @param samples The number of samples during inference.
#'
#' @return A data frame with sampled values, log weights, normalized weights, and the normalizing constant for all samples.
#' @export
#'
#' @examples
#' \dontrun{
#'   coinflips <- tibble(coinflips = sample(c(TRUE, FALSE), 20, replace = TRUE))
#'   output <- tp_run(dir_path = system.file("extdata", package = "treepplr"),
#'                         src_name = "coin.tppl", data = coinflips, samples = 10)
#'   if(rlang::is_installed("ggplot2")) {
#'     ggplot2::ggplot(output) +
#'     ggplot2::geom_col(ggplot2::aes(.data$samples, .data$nweights), width = 0.005) +
#'     ggplot2::theme_bw()
#'   }
#' }

tp_go <- function(dir_path = NULL,
                  project_name = NULL,
                  src_name = NULL,
                  method = "smc-bpf",
                  samples = 1000) {
  # smc-apf

  tp_compile(dir_path, project_name, src_name, method)
  return(tp_run(dir_path, project_name, src_name, method, samples))
}

#' Compile a TreePPL program
#'
#' @param dir_path The directory that contains the TreePPL code and input file. The executable and the output files will be written here.
#' @param src_name Name of file with TreePPL program.
#' @param method Inference method to be used.
#' @param data_path Path to JSON file with input data.
#' @param samples The number of samples during inference.
#'
#' @return A data frame with sampled values, log weights, normalized weights, and the normalizing constant for all samples.
#' @export
#'
#' @examples
#' \dontrun{
#'   coinflips <- tibble(coinflips = sample(c(TRUE, FALSE), 20, replace = TRUE))
#'   output <- tp_run(dir_path = system.file("extdata", package = "treepplr"),
#'                         src_name = "coin.tppl", data = coinflips, samples = 10)
#'   if(rlang::is_installed("ggplot2")) {
#'     ggplot2::ggplot(output) +
#'     ggplot2::geom_col(ggplot2::aes(.data$samples, .data$nweights), width = 0.005) +
#'     ggplot2::theme_bw()
#'   }
#' }

tp_compile <- function(dir_path = NULL,
                       project_name = NULL,
                       src_name = NULL,
                       method = "smc-bpf") {
  # smc-apf

  # if dir = NULL return temp_dir, if not return dir
  temp_dir <- tp_tempdir(dir_path)

  if (is.null(src_name))
    src_name <- "out"

  if (is.null(project_name))
    project_name <- "input"

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
#' @param dir_path The directory that contains the TreePPL code and input file. The executable and the output files will be written here.
#' @param src_name Name of file with TreePPL program.
#' @param method Inference method to be used.
#' @param data_path Path to JSON file with input data.
#' @param samples The number of samples during inference.
#'
#' @return A data frame with sampled values, log weights, normalized weights, and the normalizing constant for all samples.
#' @export
#'
#' @examples
#' \dontrun{
#'   coinflips <- tibble(coinflips = sample(c(TRUE, FALSE), 20, replace = TRUE))
#'   output <- tp_run(dir = system.file("extdata", package = "treepplr"),
#'                         src_name = "coin.tppl", data = coinflips, samples = 10)
#'   if(rlang::is_installed("ggplot2")) {
#'     ggplot2::ggplot(output) +
#'     ggplot2::geom_col(ggplot2::aes(.data$samples, .data$nweights), width = 0.005) +
#'     ggplot2::theme_bw()
#'   }
#' }

tp_run <- function(dir_path = NULL,
                   project_name = NULL,
                   src_name = NULL,
                   method = "smc-bpf",
                   samples = 1000) {
  # smc-apf

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
    args = c(paste0(dir_path, project_name, ".json"), paste(samples, "1")),
    stdout = paste0(dir_path, src_name, ".json")
  )

  return(as.data.frame(fromJSON(paste0(
    dir_path, src_name, ".json"
  ))))
}

#' Parse a TreePPL output
#'
#' @param dir_path The directory that contains the TreePPL code and input file. The executable and the output files will be written here.
#' @param src_name Name of file with TreePPL program.
#'
#' @return A data frame with sampled values, log weights, normalized weights, and the normalizing constant for all samples.
#' @export
#'
#' @examples
#' \dontrun{
#'   coinflips <- tibble(coinflips = sample(c(TRUE, FALSE), 20, replace = TRUE))
#'   output <- tp_run(dir = system.file("extdata", package = "treepplr"),
#'                         src_name = "coin.tppl", data = coinflips, samples = 10)
#'   if(rlang::is_installed("ggplot2")) {
#'     ggplot2::ggplot(output) +
#'     ggplot2::geom_col(ggplot2::aes(.data$samples, .data$nweights), width = 0.005) +
#'     ggplot2::theme_bw()
#'   }
#' }

tp_parse <- function(dir_path = NULL,
                      src_name = NULL) {
  # if dir_path = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir(dir_path)

  if (is.null(src_name))
    src_name <- "out"

  output_trppl <- fromJSON(paste0(dir_path, src_name, ".json"))

  result <- data.frame(matrix(ncol = 12, nrow = 0))
  colnames(result) <- c(
    "iteration",
    "log_weight",
    "log_norm_const",
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
    res <- data.frame(matrix(ncol = 12, nrow = 0))
    colnames(res) <- c(
      "iteration",
      "log_weight",
      "log_norm_const",
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

    res <- .peel_tree(
      tree,
      i,
      pindex = NA,
      output_trppl$weights[i],
      output_trppl$normConst,
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
                       prevAge,
                       startState,
                       result)
{
  base <- c(
    iteration = as.numeric(i - 1),
    log_weight = as.numeric(lweight),
    log_norm_const = as.numeric(lnorm_const),
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
      subtree$age,
      base[["end_state"]],
      result
    )
  }
  return(result)
}
