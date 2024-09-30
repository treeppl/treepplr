temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)

cat(crayon::yellow("\nTest-runner : Compile and run.\n"))


testthat::test_that("Test-runner_1 : tp_compile", {
  cat("\tTest-runner_1 : tp_compile without option \n")

  model_right <- "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n * Compilation:\n *   tpplc models/lang/coin.tppl models/data/examples.mc out.mc && mi compile out.mc\n * Execution: ./out 100 1\n */\n\n/**\n * Conditions the likelihood of the computation \n *   on an observed datapoint to come from a particular Bernoulli experiment \n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior: \n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) : Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  data_right <- list(
    coinflips = c(
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  model_data_right <- list(model = model_right, data = data_right)

  tp_write(model_data_right)

  tp_compile()

  expect_no_error(readBin(paste0(temp_dir, "out"), "raw", 10e6))
})

testthat::test_that("Test-runner_2 : tp_compile", {
  cat("\tTest-runner_2 : tp_compile with option \n")

  dir_path <- system.file("extdata", package = "treepplr")

  tp_compile(dir_path = dir_path,
             project_name = "coin",
             src_name = "coincoin",
             method = "smc-bpf")

  expect_no_error(readBin(paste0(dir_path, "/coincoin"), "raw", 10e6))

})

testthat::test_that("Test-runner_3 : tp_run", {
  cat("\tTest-runner_3 : tp_run without option \n")

  model_right <- "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n * Compilation:\n *   tpplc models/lang/coin.tppl models/data/examples.mc out.mc && mi compile out.mc\n * Execution: ./out 100 1\n */\n\n/**\n * Conditions the likelihood of the computation \n *   on an observed datapoint to come from a particular Bernoulli experiment \n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior: \n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) : Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  data_right <- list(
    coinflips = c(
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  model_data_right <- list(model = model_right, data = data_right)

  tp_write(model_data_right)

  tp_compile()
  tp_run()

  expect_no_error(readBin(paste0(temp_dir, "out.json"), "raw", 10e6))
})

testthat::test_that("Test-runner_4 : tp_run", {
  cat("\tTest-runner_4 : tp_run with option \n")

  dir_path <- system.file("extdata", package = "treepplr")

  tp_compile(dir_path = dir_path,
             project_name = "coin",
             src_name = "coincoin",
             method = "smc-bpf")

  tp_run(dir_path = dir_path,
         project_name = "coin",
         src_name = "coincoin",
         method = "smc-bpf",
         samples = 1000)

  expect_no_error(readBin(paste0(dir_path, "/coincoin"), "raw", 10e6))

})

testthat::test_that("Test-runner_5 : tp_parse", {
  cat("\tTest-runner_4 : tp_parse with option \n")

  dir_path <- system.file("extdata", package = "treepplr")
  source_name <- "hostrep"

  pars <- tp_parse(dir_path = dir_path,
             src_name = source_name)

  dec = "."
  test <- read.csv(paste0(dir_path, "/hostrep.csv"), dec=dec, stringsAsFactors = FALSE, strip.white = TRUE)

  expect_equal(pars, test)
})
