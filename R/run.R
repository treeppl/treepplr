#' Run a TreePPL program
#'
#' @description
#' Run TreePPL and return output.
#'
#' @param sampler a [treepplr::sampler_T] outputted by [treepplr::tp_compile].
#' @param data a [base::character] with the full path to the data file in TreePPL
#' JSON format (as outputted by [treepplr::tp_data]).
#' @param dir a [base::character] with the full path to the directory where you
#' want to save the output. Default is [base::tempdir()].
#' @param out_file_name a [base::character] with the name of the output file in
#' JSON format. Default is "out".
#' @param n_runs a [base::numeric] giving the numbers of sweeps(SMC)/runs(MCMC).
#' @param n_processes a [base::numeric], number of parallel processes to use.
#' Can't be superior to n_runs.
#' @param ... See [treepplr::tp_runtime_options] for all supported arguments.
#'
#'
#' @return A list of TreePPL output in parsed JSON format.
#' @export
#'
#' @examples
#' \dontrun{
#' # When using SMC
#' # compile model and create SMC inference machinery
#' exe_path <- tp_compile(model = "coin", method = "smc-bpf", particles = 2000)
#'
#' # prepare data
#' data_path <- tp_data(data_input = "coin")
#'
#' # run TreePPL
#' result <- tp_run(exe_path, data_path, n_runs = 2)
#'
#'
#' # When using MCMC
#' # compile model and create MCMC inference machinery
#' exe_path <- tp_compile(model = "coin", method = "mcmc-naive", iterations = 2000)
#'
#' # prepare data
#' data_path <- tp_data(data_input = "coin")
#'
#' # run TreePPL
#' result <- tp_run(exe_path, data_path)
#' }

tp_run <- function(sampler,
                   data,
                   dir = NULL,
                   out_file_name = "out",
                   n_runs = 1,
                   n_processes = 3,
                   ...) {

  if (is.null(dir)) {
    dir_path <- tp_tempdir()
  } else {
    dir_path <- dir
  }

  listFiles <- list.files(path = dir_path,
                          pattern = out_file_name,
                          full.names = TRUE)
  if(length(listFiles) != 0) {
    file.remove(listFiles)
  }

  output_path <- paste0(dir_path, out_file_name, ".json")

  #If a list have multiple time the same key
  # list[[key]] will return the first key
  # Exemple
  #> lis <- list(method = "mcmc", method = "smc")
  #> lis[["method"]] => "mcmc"
  # So the user list have priority
  tpplc_options <- list_to_options(tp_list(...))

  if (length(tpplc_options[["compile"]]) != 0) {
    stop("Can't give compile time options here")
  }
  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self container
  command <- paste(
    "LD_LIBRARY_PATH= ",
    sampler$exe_path,
    data,
    options_to_string(tpplc_options[["runtime"]]),
    paste(">", output_path)
  )

  if (n_runs > 1) {
    if (n_processes > n_runs) {
      warning("n_processes reduce to be equal to n_runs.")
      n_processes <- n_runs
    }
    future::plan(future::multisession, workers = n_processes)
    future.apply::future_sapply(
      1:n_runs,
      FUN = function(i) {
        system(paste0(command, i))
      }
    )
  } else {
    system(command)
  }

  listFiles <- list.files(path = dir_path,
                          pattern = out_file_name,
                          full.names = TRUE)

  json_out <- readr::read_lines(listFiles) |>
    lapply(jsonlite::fromJSON, simplifyVector = FALSE)

  return(json_out)
}
