#' Compile and run a TreePPL program
#'
#' @description
#' `tp_treeppl` execute TreePPL and return TreePPL output (string JSON format).
#'
#' @param model a TreePPL model (S3).
#' @param model_name a character vector giving a model name.
#' @param data a phyjson object (S3).
#' @param data_name a character vector giving a data name.
#' @param compile_model a [base::logical] telling if the model need to be compile again.
#' @param method a character vector giving the inference method name.
#' @param samples a [base::integer] giving the number of samples (mcmc) or particules (smc).
#' @param subsample a [base::logical] telling if  ...
#' @param run a [base::integer] giving the number of run (mcmc) or sweap (smc).
#'
#' @details
#' This function takes TreePPL model code and data, compile TreePPL model, run it with data
#' and returning TreePPL output.
#'
#' TreePPL need to be install on your computer and the PATH set for R/RSTUDIO (see [install](https://treeppl.org/docs/Howtos) manual).
#' The executable and the output files will be written in R's [base::tempdir()].
#'
#' `model` A TreePPL model (S3), see [treepplr::tp_model] for further details.
#'
#' `model_name`a character vector giving to [treepplr::tp_treeppl] a model name. Can be use to avoid if you have already provide
#' `model` in a previous call of [treepplr::tp_treeppl] to rewrite it.
#' Can be also use to avoid to write on alredy existing model.
#'
#' `data` A phyjson object (S3), see [treepplr::tp_data] for further details.
#'
#' `data_name` a character vector giving to [treepplr::tp_treeppl] a data name. Can be use to avoid if you have already provide
#' `data` in a previous call of [treepplr::tp_treeppl] to rewrite it.
#' Can be also use to avoid to write on alredy existing data.
#'
#' `compile_model` : If you want to avoid to recompile.
#'
#' `method` : Inference method to be used (smc, mcmc, etc).
#'
#' `samples` : The number of samples (mcmc) / particules (smc) during inference.
#'
#' `subsample` ...
#'
#' `run` : The number of run (mcmc) / sweap (smc) used for the inference.
#'
#' @return Path to R directory where output file was written.
#' @export

tp_treeppl <-
  function(model = NULL,
           model_name = "input",
           data = NULL,
           data_name = "input",
           compile_model = TRUE,
           method = "smc-bpf",
           samples = 1000,
           subsample = NULL,
           run = 1) {
    model_data <- tp_write(model, model_name, data, data_name)
    if (compile_model) {
      tp_compile(model_name, method, subsample)
    }
    return(tp_run(model_name, data_name, method, samples, run))
  }

#' Prepare input for [tp_treeppl()]
#'
#' @description
#' `tp_write` writes an JSON file to be used by [tp_treeppl()].
#'
#' @param model_data_strlist a structured list coming from [tp_import()].
#'
#' @details
#' This function takes a structured list of TreePPL code and data files (more details in [tp_import()]) and write 2 files from them.

tp_write <- function(model = NULL,
                     model_name = "input",
                     data = NULL,
                     data_name = "input") {
  dir <- tp_tempdir()

  if (!is.null(model)) {
    cat(model, file = paste0(dir, model_name, ".tppl"))
  }

  # write json with input data
  if (!is.null(data)) {
    input_json <- RJSONIO::toJSON(data)
    if (class(model) == "hostrep3states" ||
        class(model) == "hostrep2states") {
      input_json <- treepplr:::cor_export_num(input_json)
    }
    write(input_json, file = paste0(dir, data_name, ".json"))
  }
}

#' Specific correctif for hostrep to be able to export float value

cor_export_num <- function(JSON_str) {
  JSON_str <- stringr::str_replace_all(JSON_str, "age\":", "age\":!")
  JSON_str <- stringr::str_split_fixed(JSON_str, "!", n = Inf)
  JSON_res <- JSON_str[1]
  for (i in 2:length(JSON_str)) {
    if (!stringr::str_detect(stringr::str_sub(JSON_str[i], 1, 10), "\\.")) {
      JSON_str[i] <-
        stringr::str_c(
          stringr::str_sub(JSON_str[i], 1, 9),
          ".0",
          stringr::str_sub(JSON_str[i], start = 10)
        )
    }
    JSON_res <- stringr::str_c(JSON_res, JSON_str[i])
  }
  JSON_mtx <-
    stringr::str_replace_all(JSON_res, "host_distances\":", "host_distances\":!")
  JSON_mtx <- stringr::str_split_fixed(JSON_mtx, "!", n = Inf)
  JSON_juk <- stringr::str_replace_all(JSON_mtx[2], "]", "]!")
  JSON_juk <- stringr::str_split_fixed(JSON_juk, "!", n = Inf)
  JSON_juk[1] <- stringr::str_replace_all(JSON_juk[1], "0,", "0.0,")
  JSON_juk[1] <- stringr::str_replace_all(JSON_juk[1], "0 ", "0.0 ")
  JSON_juk <- stringr::str_c(JSON_juk[1], JSON_juk[2])
  JSON_mtx <- stringr::str_c(JSON_mtx[1], JSON_juk)
}

#' Compile a TreePPL program

tp_compile <- function(model_name = "input",
                       method = "smc-bpf",
                       subsample = NULL) {
  # if dir = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir()

  argum <- c(
    paste0(dir_path, model_name, ".tppl"),
    paste("-m", method),
    paste0("--output ", dir_path, model_name)
  )

  if (!is.null(subsample)) {
    argum <- c(argum, paste("--subsample -n", subsample))
  }

  # Compile program
  system2("tpplc", args = argum)

  return(dir_path)
}

#' Run a TreePPL program

tp_run <- function(model_name = "input",
                   data_name = "input",
                   method = "smc-bpf",
                   samples = 1000,
                   run = "1") {

  # check inputs
  if (method == "smc-apf")
    samples <- samples + 1

  # if dir_path = NULL return temp_dir, if not return dir
  dir_path <- tp_tempdir()

  # run
  system2(
    command = paste0(dir_path, model_name),
    args = c(paste0(dir_path, data_name, ".json"), paste(samples, run)),
    stdout = paste0(dir_path, model_name, "_out.json")
  )

  return(RJSONIO::fromJSON(paste0(dir_path, model_name, "_out.json")))
}
