#' Compile and run a TreePPL program
#'
#' @param dir The directory that contains the TreePPL code and input file. The executable and the output files will be written here.
#' @param source Name of file with TreePPL program.
#' @param method Inference method to be used.
#' @param data Name of JSON file with input data.
#' @param samples The number of samples during inference.
#'
#' @return A data frame with sampled values, log weights, normalized weights, and the normalizing constant for all samples.
#' @export
#'
#' @examples
#' output <- run_treeppl(dir = system.file("extdata", package = "treepplr"), source = "coin.tppl", data = "coin.json", samples = 10)
#' if(rlang::is_installed("ggplot2")) {
#'  ggplot2::ggplot(output) +
#'  ggplot2::geom_col(ggplot2::aes(.data$samples, .data$nweights), width = 0.005) +
#'  ggplot2::theme_bw()
#' }

run_treeppl <- function(dir = NULL, source = NULL, method = "smc-bpf", data = NULL, samples = 1000) { # smc-apf

  # Compile program
  system2(command = "tpplc", args = c(paste0(dir,"/", source),
                                      paste0("-m ", method)))
  # which arguments are necessary other than method?
  # should the executable go to a temporary folder and be delete afterwards?

  # run
  system2(command = "./out", args = c(paste0(dir,"/", data),
                                      paste0(samples, " 1")), stdout = "stdout.json")

  # read output
  output <- jsonlite::fromJSON("stdout.json") %>%
    as.data.frame() %>%
    dplyr::mutate(nweights = norm_weights(.data$weights))

  return(output)
}

# Function to get normalize weights from log weights
norm_weights <- function(weights_vector) { # input log weights from TreePPL
  lweights <- as.numeric(weights_vector) # making sure weights are numeric vector
  weights <- exp(lweights - max(lweights)) # exponentiate the difference between the weight and the maximum of the weights
  weights / sum(weights) # normalising weights
}
