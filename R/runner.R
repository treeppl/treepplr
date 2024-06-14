#' Compile and run a TreePPL program
#'
#' @param dir The directory that contains the TreePPL code and input file. The executable and the output files will be written here.
#' @param source Name of file with TreePPL program.
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
#'   output <- run_treeppl(dir = system.file("extdata", package = "treepplr"),
#'                         source = "coin.tppl", data = coinflips, samples = 10)
#'   if(rlang::is_installed("ggplot2")) {
#'     ggplot2::ggplot(output) +
#'     ggplot2::geom_col(ggplot2::aes(.data$samples, .data$nweights), width = 0.005) +
#'     ggplot2::theme_bw()
#'   }
#' }
run_treeppl <- function(dir = NULL, source = NULL, method = "smc-bpf", data_path = NULL, samples = 1000) { # smc-apf

  # check inputs

  if(method == "smc-apf") samples <- samples + 1

  tmp_dir <- treeppl_tempdir(NULL)
  sep <- .sep()

  # Compile program
  system2(command = "tpplc", args = c(paste0(dir, sep, source),
                                      paste0("-m ", method),
                                      paste0("--output ", tmp_dir, sep, "out")))

  # run
  system2(command = paste0(tmp_dir, sep, "out"),
          args = c(data_path,
                   paste0(samples, " 1")),
          stdout = paste0(tmp_dir, sep, "stdout.json")
  )

  # read output
  output <- read_treeppl_output(paste0(tmp_dir, sep, "stdout.json"))

  return(output)
}

