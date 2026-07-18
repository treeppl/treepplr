temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-run : Running TreePPL.\n"))

test_that("Test-run_1a : tp_run", {
  cat("\tTest-run_1a : tp_run \n")

  sampler <- treepplr::tp_compile("crbd", method = "smc-apf", particles = 2)
  data <- treepplr::tp_data("crbd")

  result <- treepplr::tp_run(sampler, data, sweeps = 1)

  expect_equal(2, length(result[[1]]$samples))

})

test_that("Test-run_1a : tp_run custom name", {
  cat("\tTest-run_1a : tp_run \n")

  sampler <- treepplr::tp_compile("crbd", method = "smc-apf", particles = 2)
  data <- treepplr::tp_data("crbd")

  result <-treepplr::tp_run(sampler, data, sweeps = 1, out_file_name = "test_out", particles = 5)

  expect_equal(5, length(result[[1]]$samples))
})

test_that("Test-run_1c : tp_run threading", {
  cat("\tTest-run_1c : tp_run \n")

  sampler <- treepplr::tp_compile("crbd", method = "smc-apf", particles = 2)
  data <- treepplr::tp_data("crbd")

  result <-treepplr::tp_run(sampler, data, sweeps = 1, out_file_name = "test_out", particles = 5, , n_runs = 10)

  expect_equal(10, length(result))
})
