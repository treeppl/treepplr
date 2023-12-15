run_treeppl <- function(source = NULL, method = "smc-bpf", data = NULL, samples = 1000) { # smc-apf

  # Compile program
  system2(command = "tpplc", args = c(source, paste0("-m ", method)))
  # which arguments are necessary other than method?
  # should the executable go to a temporary folder and be delete afterwards?

  # run
  system2(command = "./out", args = c(data, paste0(samples, " 1")), stdout = "stdout.json")

  # read output
  output <- jsonlite::fromJSON("stdout.json") %>%
    as.data.frame() %>%
    dplyr::mutate(nweights = norm_weights(.data$weights))

  return(output)
}

norm_weights <- function(weights_vector) { #input log weights from TreePPL
  lweights <- as.numeric(weights_vector) #making sure weights are numeric vector
  weights <- exp(lweights - max(lweights)) #exponentiate the difference between the weight and the maximum of the weights
  weights / sum(weights) #Normalising weights
}
