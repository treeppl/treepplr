temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-getter : Import data and model.\n"))

testthat::test_that("Test-getter_1a : tp_model model path", {
  cat("\tTest-getter_1a : tp_model\n")

  model <-
    tp_model(paste0(
      system.file("extdata", package = "treepplr"),
      treepplr:::sep(),
      "coin.tppl"
    ))
  model_right <-
    "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n */\n\n/**\n * Conditions the likelihood of the computation\n *   on an observed datapoint to come from a particular Bernoulli experiment\n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior:\n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) => Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  class(model_right) <- "custom"

  testthat::expect_equal(model, model_right)
})

testthat::test_that("Test-getter_1b : tp_model model name ", {
  cat("\tTest-getter_1b : tp_model\n")

  model <- tp_model("coin")
  model_right <-
    "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n */\n\n/**\n * Conditions the likelihood of the computation\n *   on an observed datapoint to come from a particular Bernoulli experiment\n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior:\n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) => Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  class(model_right) <- "coin"

  testthat::expect_equal(model, model_right)
})

testthat::test_that("Test-getter_1c : tp_model model string ", {
  cat("\tTest-getter_1c : tp_model\n")

  model_right <-
    "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n */\n\n/**\n * Conditions the likelihood of the computation\n *   on an observed datapoint to come from a particular Bernoulli experiment\n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior:\n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) => Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  model <- tp_model(model_right)
  class(model_right) <- "custom"

  testthat::expect_equal(model, model_right)
})

testthat::test_that("Test-getter_2a : tp_data data path", {
  cat("\tTest-getter_2a : tp_data\n")

  data <-
    tp_data(paste0(
      system.file("extdata", package = "treepplr"),
      treepplr:::sep(),
      "coin.json"
    ))
  data_right <-
    tp_list(
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

  testthat::expect_equal(data, data_right)
})

testthat::test_that("Test-getter_2b : tp_data data name", {
  cat("\tTest-getter_2b : tp_data\n")

  data <- tp_data("coin")
  data_right <-
    tp_list(
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

  testthat::expect_equal(data, data_right)
})

testthat::test_that("Test-getter_2a : tp_data data string", {
  cat("\tTest-getter_2c : tp_data\n")

  data <-
    tp_data(list(
      c(
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
    ))

  data_right <-
    tp_list(
      c(
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

  testthat::expect_equal(data, data_right)
})

