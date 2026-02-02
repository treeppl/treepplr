temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-run : Running TreePPL.\n"))

test_that("Test-run_1a : tp_run", {
  cat("\tTest-run_1a : tp_run \n")

  model <- treepplr::tp_model("coin")
  exe <- treepplr::tp_compile(model, method = "smc-bpf", particles = 2)
  data <- treepplr::tp_data("coin")

  treepplr::tp_run(exe, data, n_sweeps = 1)

  expect_no_error(readBin(paste0(temp_dir, "out.json"), "raw", 10e6))

})


test_that("Test-run_1a : tp_run custom name", {
  cat("\tTest-run_1a : tp_run \n")

  model <- treepplr::tp_model("coin")
  exe <- treepplr::tp_compile(model, method = "smc-bpf", particles = 2)
  data <- treepplr::tp_data("coin")

  treepplr::tp_run(exe, data, n_sweeps = 1, out_file_name = "test_out")

  expect_no_error(readBin(paste0(temp_dir, "test_out.json"), "raw", 10e6))

})
