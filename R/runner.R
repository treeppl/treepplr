#' Compile and run a TreePPL program
#'
#' @description
#' `tp_treeppl` executes TreePPL and returns the inference output (string JSON format).
#'
#' @inheritParams tp_write
#' @inheritParams tp_compile
#' @inheritParams tp_run
#' @param compile_model a [base::logical]. Compile the model?
#'
#'
#' @details
#'
#' This function takes a TreePPL object (the model) and a json object (the data),
#' compiles the TreePPL model, runs it with the data and returns TreePPL output.
#'
#' TreePPL has to be installed on your computer
#' (see [install](https://treeppl.org/getting-started/installation) manual).
#' The executable and the output files will be written in R's [base::tempdir].
#'
#' `model` : A TreePPL model (S3), see [treepplr::tp_model] for further details.
#' Use 'NULL' if you have previously provided a model. Check already provided
#' models with [treepplr::tp_stored_model].
#'
#' `model_file_name` : a string given to [treepplr::tp_treeppl] as
#' a model name.  Use a [treepplr::tp_stored_data] name if you have already
#' written your model with [treepplr::tp_treeppl].
#'
#' `data` : A json object (S3), see [treepplr::tp_json] for further
#' details. Use 'NULL' if you have previously provided data. Check already
#' provided data with [treepplr::tp_stored_data].
#'
#' `data_file_name` : a string given to [treepplr::tp_treeppl] as
#' a data name. Use a [treepplr::tp_stored_data] name if you have already written
#' your data with [treepplr::tp_treeppl].
#'
#' `compile_model` : a [base::logical] telling if the model needs to be compiled.
#' Can be used to avoid compiling the same model again in R's [base::tempdir].
#' Check already compiled models with [treepplr::tp_stored_compiled].
#'
#' `samples` : The number of samples (MCMC) / particles (SMC) during inference.
#'
#' `seed` : The random seed to use. Using 'NULL' initializes it randomly.
#'
#' `n_runs` : The number of runs (MCMC) / sweeps (SMC) used for the inference.
#'
#' `method` : Inference method to be used.
#' The supported methods are: is-lw, smc-bpf, smc-apf, mcmc-lightweight,
#' mcmc-trace, mcmc-naive, pmcmc-pimh.
#'
#'
#'
#' **The following options are only applicable to certain methods:**
#'
#' `align` : Whether or not to align the model.
#'
#' `cps` : Configuration of CPS transformation.
#' The supported options are: none, partial, and full.
#'
#' `delay` : The model is transformed to an efficient representation if
#' possible. The supported options are: static or dynamic. Use 'NULL' to ignore.
#'
#' `kernel` : The value of the driftScale for driftKernel in MCMC. Use 'NULL'
#' to ignore. Use together with `method` mcmc-lightweight".
#' Use 'NULL' to ignore.
#'
#' `mcmc_lw_gprob` : The probability of performing a global MH step
#' (non-global means only modify a single sample in the previous trace).
#'  Use together with `method` mcmc-lightweight". Use 'NULL' to ignore.
#'
#' `pmcmc_particles` : The number of particles for the SMC proposal computation.
#' This option is used if one of the following methods are used: pmcmc-*.
#' Use 'NULL' to ignore.
#'
#' `prune` : The model is pruned if possible.
#'
#' `subsample` : The number of draws to subsample from the posterior
#' distribution. Use together with `method` smc-apf or smc-bpf.
#' Use 'NULL' to ignore.
#'
#' `resample`: The selected resample placement method. The supported methods are:
#' likelihood (resample immediately after all likelihood updates),
#' align (resample after aligned likelihood updates, forces --align),
#' and manual (sample only at manually defined resampling locations).
#' Use 'NULL' to ignore.
#'
#' @return TreePPL output in JSON format.
#' @export

tp_treeppl <-
  function(model = NULL,
           model_file_name = "tmp_model_file",
           data = NULL,
           data_file_name = "tmp_data_file",
           compile_model = TRUE,
           samples = 1000,
           seed = NULL,
           n_runs = 1,
           method = "smc-bpf",
           align = FALSE,
           cps = "none",
           delay = NULL,
           kernel = NULL,
           mcmc_lw_gprob = NULL,
           pmcmc_particles =  NULL,
           prune = FALSE,
           subsample =  NULL,
           resample = NULL) {

    tp_write(model, model_file_name, data, data_file_name)
    if (compile_model) {
      tp_compile(
        model_file_name,
        seed,
        method,
        align,
        cps,
        delay,
        kernel,
        mcmc_lw_gprob,
        pmcmc_particles,
        prune,
        subsample,
        resample
      )
    }
    return(tp_run(model_file_name, data_file_name, samples, n_runs))
  }

#' Prepare input for [tp_compile]
#'
#' @description
#' This function takes a TreePPL object (the model) and/or a json object (the data) and writes
#' them in [base::tempdir] as a .tppl and a .json file, respectively.
#'
#' @param model a TreePPL model (S3).
#' @param model_file_name a string giving a name to the temporary model file.
#' @param data a json object with all data needed for inference (S3).
#' @param data_file_name a string giving a name to the temporary data file.
#'
#' @export
tp_write <- function(model = NULL,
                     model_file_name = "tmp_model_file",
                     data = NULL,
                     data_file_name = "tmp_data_file") {
  dir <- tp_tempdir()

  if (!is.null(model)) {
    cat(model, file = paste0(dir, model_file_name, ".tppl"))
  }

  # write json with input data
  if (!is.null(data)) {
    input_json <- RJSONIO::toJSON(data)
    write(input_json, file = paste0(dir, data_file_name, ".json"))
  }
}

#' Compile the TreePPL program
#'
#' @description
#' `tp_compile` compiles a TreePPL model to be used by [treepplr::tp_run].
#'
#' @param model_file_name a string giving a name to the temporary model file.
#' @param samples a [base::integer] giving the number of samples (MCMC) /
#' particles (SMC).
#' @param seed a [base::numeric] to use as a random seed.
#' @param method a string giving the inference method name.
#' @param align a [base::logical]. Whether or not to align the model.
#' @param cps a string giving the configuration of CPS transformation.
#' @param delay a string giving the configuration of delayed sampling.
#' @param kernel a [base::numeric] value giving the driftScale for driftKernel
#' in MCMC.
#' @param mcmc_lw_gprob a [base::numeric] probability of performing a global
#' MCMC step.
#' @param pmcmc_particles a [base::integer] number of particles for the SMC
#' proposal computation.
#' @param prune a [base::logical]. Whether or not to prune the model.
#' @param subsample a [base::integer] number of draws to subsample from the
#' posterior distribution.
#' @param resample a string giving the selected resample placement method.
#'
#' @return The directory whreŕe the compiled file is stored.
#' @export

tp_compile <- function(model_file_name = "tmp_model_file",
                       seed = NULL,
                       method = "smc-bpf",
                       align = FALSE,
                       cps = "none",
                       delay = NULL,
                       kernel = NULL,
                       mcmc_lw_gprob = NULL,
                       pmcmc_particles =  NULL,
                       prune = FALSE,
                       subsample =  NULL,
                       resample = NULL) {

  dir_path <- tp_tempdir()

  argum <- c(
    paste0(dir_path, model_file_name, ".tppl"),
    paste0("-m ", method),
    paste0("--output ", dir_path, model_file_name, ".exe")
  )

  if (cps != "none") {
    argum <- c(argum, paste0("--cps ", cps))
  }

  if (!is.null(seed)) {
    argum <- c(argum, paste0("--seed ", seed))
  }

  if (align) {
    argum <- c(argum, "--align ")
  }

  if (!is.null(delay)) {
    if (delay == "static") {
      argum <- c(argum, "--static-delay ")
    }
    if (delay == "dynamic") {
      argum <- c(argum, "--dynamic-delay ")
    }
  }

  if (!is.null(kernel)) {
    argum <- c(argum, paste0("--kernel --drift ", kernel))
  }

  if (!is.null(mcmc_lw_gprob)) {
    argum <- c(argum, paste0("--mcmc_lw_gprob ", mcmc_lw_gprob))
  }

  if (!is.null(pmcmc_particles)) {
    argum <- c(argum, paste0("--pmcmcParticles ", pmcmc_particles))
  }

  if (prune) {
    argum <- c(argum, "--prune ")
  }

  if (!is.null(subsample)) {
    argum <- c(argum, paste0("--subsample -n ", subsample))
  }

  if (!is.null(resample)) {
    argum <- c(argum, paste0("--resample ", resample))
  }

  # Compile program

  system2("tpplc", args = argum)

  return(dir_path)
}

#' Run a TreePPL program
#'
#' @description
#' `tp_treeppl` executes TreePPL and returns the inference output in JSON format.
#'
#' @inheritParams tp_write
#' @param samples a [base::integer] giving the number of samples (MCMC) /
#' particles (SMC).
#' @param n_runs a [base::integer] giving the number of runs (MCMC) / sweeps (SMC).
#'
#' @return TreePPL output in JSON format.
#' @export

tp_run <- function(model_file_name = "tmp_model_file",
                   data_file_name = "tmp_data_file",
                   samples = 1000,
                   n_runs = "1") {
  # if dir_path = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir()

  # n_runs
  system2(
    command = paste0(dir_path, model_file_name, ".exe"),
    args = c(
      paste0(dir_path, data_file_name, ".json"),
      paste(samples, n_runs)
    ),
    stdout = paste0(dir_path, model_file_name, "_out.json")
  )

  return(RJSONIO::fromJSON(paste0(dir_path, model_file_name, "_out.json")))
}
