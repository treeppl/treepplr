#' Compile and run a TreePPL program
#'
#' @description
#' `tp_treeppl` execute TreePPL and return TreePPL output (string JSON format).
#'
#' @param model a TreePPL model (S3).
#' @param model_file_name a character vector giving a model name.
#' @param data a phyjson object (S3).
#' @param data_file_name a character vector giving a data name.
#' @param compile_model a [base::logical] to tell if the model need to be
#' compile
#' @param samples a [base::integer] giving the number of samples (mcmc) or
#' particules (smc).
#' @param seed a [base::numeric] to use as a random seed.
#' @param n_runs a [base::integer] giving the number of run (mcmc)/sweap (smc).
#' @param method a character vector giving the inference method name.
#' @param align a [base::logical] to tell if need to align the model.
#' @param cps a character vector giving the configuration of CPS transformation.
#' @param delay a character vector giving the configuration of delayed sampling.
#' @param kernel a [base::numeric] value giving the driftScale for driftKernel
#' in MCMC.
#' @param mcmc_lw_gprob a [base::numeric] probability of performing a global
#' MCMC step.
#' @param pmcmc_particles a [base::integer] number of particles for the smc
#' proposal computation
#' @param prune a [base::logical] to tell if the model will try to be pruned.
#' @param subsample a [base::integer] number of draw to subsample from the
#' posterior distribution.
#' @param resample a character vector giving the selected resample placement
#' method
#'
#' @details
#' This function takes TreePPL object (S3) and phyjson object (S3),
#' compile TreePPL model, run it with data and returning TreePPL output.
#'
#' TreePPL need to be install on your computer and the PATH set for R/RSTUDIO
#' (see [install](https://treeppl.org/docs/Howtos) manual).
#' The executable and the output files will be written in R's [base::tempdir()].
#'
#' `model` : A TreePPL model (S3), see [treepplr::tp_model] for further details.
#' Use 'NULL' if you have previously provide an model. Check already provide
#' model with [treepplr::tp_stored_model].
#'
#' `model_file_name` : a character vector giving to [treepplr::tp_treeppl] as
#' a model name.  Use a [treepplr::tp_stored_data] name if you have already
#' write your model with [treepplr::tp_treeppl].
#'
#' `data` : A list, see [treepplr::tp_check_input()] for further
#' details. Use 'NULL' if you have previously provide data. Check already
#' provide data with [treepplr::tp_stored_data].
#'
#' `data_file_name` : a character vector giving to [treepplr::tp_treeppl]
#' a data name. Use a [treepplr::tp_stored_data] name if you have already write
#' your data with [treepplr::tp_treeppl].
#'
#' `compile_model` : a [base::logical] telling if the model need to be compiled.
#' Can be use to avoid to compile a model again in R's [base::tempdir()]
#' if you have already compile a `model` in a previous call of
#' [treepplr::tp_treeppl]. Check already compile model
#' with [treepplr::tp_stored_compiled].
#'
#' `samples` : The number of samples (mcmc) / particules (smc) during inference.
#'
#' `seed` : The random seed to use. Using 'NULL' initialized randomly.
#'
#' `n_runs` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' `method` : Inference method to be used. The selected inference method.
#' The supported methods are: is-lw, smc-bpf, smc-apf, mcmc-lightweight,
#' mcmc-trace, mcmc-naive, pmcmc-pimh.
#'
#' The following options are highly dependable of the method used.
#' Check \[not implemented yet\] for more information.
#'
#' `align` : Whether or not to align the model for certain inference algorithms.
#'
#' `cps` : Configuration of CPS transformation (only applicable to certain
#'  inference algorithms). The supported options are: none, partial, and full.
#'
#' `delay` : The model is transformed to an efficient representation if
#' possible. The supported options are: static or dynamic. Use 'NULL' to ignore.
#'
#' `kernel` : The value of the driftScale for driftKernel in MCMC. Use 'NULL'
#' to ignore. Use in conjuction with `method` mcmc-lightweight".
#' Use 'NULL' to ignore
#'
#' `mcmc_lw_gprob` : The probability of performing a global MH step
#' (non-global means only modify a single sample in the previous trace).
#'  Use in conjuction with `method` mcmc-lightweight". Use 'NULL' to ignore
#'
#' `pmcmc_particles` : The number of particles for the smc proposal computation.
#' This option is used if one of the following methods are used: pmcmc-*.
#' Use 'NULL' to ignore
#'
#' `prune` : The model is pruned if possible.
#'
#' `subsample` : The number of draw to subsample from the posterior
#' distribution. Use in conjuction with `method` smc-apf or smc-bpf.
#' Use 'NULL' to ignore.
#'
#' `resample`: The selected resample placement method, for inference algorithms
#' where applicable. The supported methods are:
#' likelihood (resample immediately after all likelihood updates),
#' align (resample after aligned likelihood updates, forces --align),
#' and manual (sample only at manually defined resampling locations).
#' Use 'NULL' to ignore.
#'
#' @return A list of TreePPL output in parsed JSON format.
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
        samples,
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

    return(tp_run(model_file_name, data_file_name, n_runs))
  }

#' Prepare input for [tp_compile()]
#'
#' @description
#' `tp_write` writes an JSON file to be used by [tp_compile()].
#'
#' @param model a TreePPL model (S3).
#' @param model_file_name a character vector giving a model name.
#' @param data a phyjson object (S3).
#' @param data_file_name a character vector giving a data name.
#'
#' @details
#' This function takes TreePPL object (S3) and phyjson object (S3) and write
#' them in [base::tempdir()].
#'
#' `model` : A TreePPL model (S3), see [treepplr::tp_model] for further details.
#' Use 'NULL' if you have previously provide an model. Check already provide
#' model with [treepplr::tp_stored_model].
#'
#' `model_file_name` : a character vector giving to [treepplr::tp_treeppl] as
#' a model name.  Use a [treepplr::tp_stored_data] name if you have already
#' write your model with [treepplr::tp_write].
#'
#' `data` : A list, see [treepplr::tp_check_input()] for further
#' details. Use 'NULL' if you have previously provide data. Check already
#' provide data with [treepplr::tp_stored_data].
#'
#' `data_file_name` : a character vector giving to [treepplr::tp_treeppl]
#' a data name. Use a [treepplr::tp_stored_data] name if you have already write
#' your data with [treepplr::tp_write].
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
    input_json <- jsonlite::toJSON(data, auto_unbox=TRUE)
    write(input_json, file = paste0(dir, data_file_name, ".json"))
  }
}

#' Compile for [tp_run()]
#'
#' @description
#' `tp_compile` compile a TreePPL model to use by [treepplr::tp_run].
#'
#' @param model_file_name a character vector giving a model name.
#' @param samples a [base::integer] giving the number of samples (mcmc) or
#' particules (smc).
#' @param seed a [base::numeric] to use as a random seed.
#' @param method a character vector giving the inference method name.
#' @param align a [base::logical] to tell if need to align the model.
#' @param cps a character vector giving the configuration of CPS transformation.
#' @param delay a character vector giving the configuration of delayed sampling.
#' @param kernel a [base::numeric] value giving the driftScale for driftKernel
#' in MCMC.
#' @param mcmc_lw_gprob a [base::numeric] probability of performing a global
#' MCMC step.
#' @param pmcmc_particles a [base::integer] number of particles for the smc
#' proposal computation
#' @param prune a [base::logical] to tell if the model will try to be pruned.
#' @param subsample a [base::integer] number of draw to subsample from the
#' posterior distribution.
#' @param resample a character vector giving the selected resample placement
#' method.
#'
#' @details
#'
#' `model_file_name` : a character vector giving to [treepplr::tp_treeppl] as
#' a model name.  Use a [treepplr::tp_stored_data] name if you have already
#' write your model with [treepplr::tp_treeppl].
#'
#' `seed` : The random seed to use. Using 'NULL' initialized randomly.
#'
#' `method` : Inference method to be used. The selected inference method.
#' The supported methods are: is-lw, smc-bpf, smc-apf, mcmc-lightweight,
#' mcmc-trace, mcmc-naive, pmcmc-pimh.
#'
#' The following options are highly dependable of the method used.
#' Check \[not implemented yet\] for more information.
#'
#' `align` : Whether or not to align the model for certain inference algorithms.
#'
#' `cps` : Configuration of CPS transformation (only applicable to certain
#'  inference algorithms). The supported options are: none, partial, and full.
#'
#' `delay` : The model is transformed to an efficient representation if
#' possible. The supported options are: static or dynamic. Use 'NULL' to ignore.
#'
#' `kernel` : The value of the driftScale for driftKernel in MCMC. Use 'NULL'
#' to ignore. Use in conjuction with `method` mcmc-lightweight".
#' Use 'NULL' to ignore
#'
#' `mcmc_lw_gprob` : The probability of performing a global MH step
#' (non-global means only modify a single sample in the previous trace).
#'  Use in conjuction with `method` mcmc-lightweight". Use 'NULL' to ignore
#'
#' `pmcmc_particles` : The number of particles for the smc proposal computation.
#' This option is used if one of the following methods are used: pmcmc-*.
#' Use 'NULL' to ignore
#'
#' `prune` : The model is pruned if possible.
#'
#' `subsample` : The number of draw to subsample from the posterior
#' distribution. Use in conjuction with `method` smc-apf or smc-bpf.
#' Use 'NULL' to ignore.
#'
#' `resample`: The selected resample placement method, for inference algorithms
#' where applicable. The supported methods are:
#' likelihood (resample immediately after all likelihood updates),
#' align (resample after aligned likelihood updates, forces --align),
#' and manual (sample only at manually defined resampling locations).
#' Use 'NULL' to ignore.
#'
#' @return The R's [base::tempdir()] whreŕe the compile file is stored.
#' @export

tp_compile <- function(model_file_name = "tmp_model_file",
                       samples = 1000,
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
  # if dir = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir()
  options(scipen=999)
  argum <- paste(
    paste0(dir_path, model_file_name, ".tppl"),
    paste("-m", method),
    paste("--particles", samples),
    paste0("--output ", dir_path, model_file_name, ".exe")
  )

  if (cps != "none") {
    argum <- paste(argum, paste0("--cps ", cps))
  }

  if (!is.null(seed)) {
    argum <- paste(argum, paste0("--seed ", seed))
  }

  if (align) {
    argum <- paste(argum, "--align ")
  }

  if (!is.null(delay)) {
    if (delay == "static") {
      argum <- paste(argum, "--static-delay ")
    }
    if (delay == "dynamic") {
      argum <- paste(argum, "--dynamic-delay ")
    }
  }

  if (!is.null(kernel)) {
    argum <- paste(argum, paste0("--kernel --drift ", kernel))
  }

  if (!is.null(mcmc_lw_gprob)) {
    argum <- paste(argum, paste0("--mcmc_lw_gprob ", mcmc_lw_gprob))
  }

  if (!is.null(pmcmc_particles)) {
    argum <- paste(argum, paste0("--pmcmcParticles ", pmcmc_particles))
  }

  if (prune) {
    argum <- paste(argum, "--prune ")
  }

  if (!is.null(subsample)) {
    argum <- paste(argum, paste0("--subsample -n ", subsample))
  }

  if (!is.null(resample)) {
    argum <- paste(argum, paste0("--resample ", resample))
  }

  # Preparing the command line program
  tpplc_path <- installing_treeppl()
  command <- paste(tpplc_path, argum)

  # Compile program
  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self container
  system(paste0("LD_LIBRARY_PATH= MCORE_LIBS= ", command))

  return(dir_path)
}

#' Run a TreePPL program
#'
#' #'
#' @description
#' `tp_treeppl` execute TreePPL and return TreePPL output (string JSON format).
#'
#' @param model_file_name a character vector giving a model name.
#' @param data_file_name a character vector giving a data name.
#' @param n_runs a [base::integer] giving the number of runs (mcmc)/sweaps (smc).
#'
#' @details
#'
#' `model_file_name` : a character vector giving to [treepplr::tp_treeppl] as
#' a model name.  Use a [treepplr::tp_stored_data] name if you have already
#' write your model with [treepplr::tp_treeppl].
#'
#' `data_file_name` : a character vector giving to [treepplr::tp_treeppl]
#' a data name. Use a [treepplr::tp_stored_data] name if you have already write
#' your data with [treepplr::tp_treeppl].
#'
#' `n_runs` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' @return A list of TreePPL output in parsed JSON format.
#' @export

tp_run <- function(model_file_name = "tmp_model_file",
                   data_file_name = "tmp_data_file",
                   n_runs = 1) {

  n_runs_string <- paste0("--sweeps ", n_runs, " ")

  # if dir_path = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir()

  # n_runs
  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self container
  command <- paste0("LD_LIBRARY_PATH= MCORE_LIBS= ", dir_path, model_file_name, ".exe ",
                    dir_path, data_file_name, ".json ", n_runs_string,
                    paste(">", paste0(dir_path, model_file_name, "_out.json")
                    ))
  system(command)

  json_out <- readLines(paste0(dir_path, model_file_name, "_out.json")) %>%
    lapply(jsonlite::fromJSON, simplifyVector = FALSE)

  return(json_out)
}
