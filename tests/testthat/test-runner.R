temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-runner : Write, compile and run.\n"))

testthat::test_that("Test-setter_0: tp_write", {
  cat("\tTest-setter_0 : tp_write\n")

  model_right <-
    "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n * Compilation:\n *   tpplc models/lang/coin.tppl models/data/examples.mc out.mc && mi compile out.mc\n * Execution: ./out 100 1\n */\n\n/**\n * Conditions the likelihood of the computation \n *   on an observed datapoint to come from a particular Bernoulli experiment \n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior: \n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) : Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  class(model_right) <- "custom"
  data_right <-
    list(
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
  class(data_right) <- "json"

  treepplr:::tp_write(model = model_right, data = data_right)

  model <- tp_model(paste0(temp_dir, "tmp_model_file.tppl"))
  data <- tp_data(paste0(temp_dir, "tmp_data_file.json"))

  expect_equal(model, model_right)
  expect_equal(data, data_right)
})

testthat::test_that("Test-runner_1a : tp_compile", {
  cat("\tTest-runner_1a : tp_compile without option \n")

  model_right <-
    "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n * Compilation:\n *   tpplc models/lang/coin.tppl models/data/examples.mc out.mc && mi compile out.mc\n * Execution: ./out 100 1\n */\n\n/**\n * Conditions the likelihood of the computation \n *   on an observed datapoint to come from a particular Bernoulli experiment \n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior: \n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) : Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  class(model_right) <- "custom"
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
  class(data_right) <- "json"

  treepplr:::tp_write(model = model_right, data = data_right)

  treepplr:::tp_compile()

  expect_no_error(readBin(paste0(temp_dir, "tmp_model_file.exe"), "raw", 10e6))
})

testthat::test_that("Test-runner_1b : tp_compile", {
  cat("\tTest-runner_1b : tp_compile with option \n")

  model <- tp_model("coin")
  data <- tp_data("coin")

  treepplr:::tp_write(
    model = model,
    model_file_name = "coin",
    data = data,
    data_file_name = "coin"
  )
  treepplr:::tp_compile(model_file_name = "coin",
                        method = "smc-bpf")

  expect_no_error(readBin(paste0(temp_dir, "coin.exe"), "raw", 10e6))

})

testthat::test_that("Test-runner_2a : tp_run", {
  cat("\tTest-runner_2a : tp_run without option \n")

  model_right <-
    "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n * Compilation:\n *   tpplc models/lang/coin.tppl models/data/examples.mc out.mc && mi compile out.mc\n * Execution: ./out 100 1\n */\n\n/**\n * Conditions the likelihood of the computation \n *   on an observed datapoint to come from a particular Bernoulli experiment \n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior: \n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) : Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  class(model_right) <- "custom"
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
  class(data_right) <- "json"

  treepplr:::tp_write(model = model_right, data = data_right)

  treepplr:::tp_compile()

  out <- treepplr:::tp_run()

  expect_no_error(readBin(paste0(temp_dir, "tmp_model_file_out.json"), "raw", 10e6))
})

testthat::test_that("Test-runner_2b : tp_run", {
  cat("\tTest-runner_2b : tp_run with option \n")

  model <- tp_model("coin")
  data <- tp_data("coin")

  treepplr:::tp_write(
    model = model,
    model_file_name = "coin",
    data = data,
    data_file_name = "coin"
  )

  treepplr:::tp_compile(model_file_name = "coin",
                        method = "smc-bpf")

  out <- treepplr:::tp_run(
    model_file_name = "coin",
    data_file_name = "coin",
    samples = 1000,
    n_runs = 1
  )

  expect_no_error(readBin(paste0(temp_dir, "coin_out.json"), "raw", 10e6))
})

testthat::test_that("Test-runner_3a : tp_treppl", {
  cat("\tTest-runner_3a : tp_treppl with hostrep model \n")

  model <- tp_model("hostrep3states")
  data <- tp_data("hostrep3states")

  out <- tp_treeppl(
    model = model,
    model_file_name = "hostrep",
    data = data,
    data_file_name = "hostrep",
    samples = 2
  )

  expect_no_error(tp_parse_host_rep(out, n_runs = 1))
})
