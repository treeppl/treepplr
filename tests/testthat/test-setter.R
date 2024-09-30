temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)

cat(crayon::yellow("\nTest-setter : Export data and model.\n"))


testthat::test_that("Test-setter_1 : tp_write", {
  cat("\tTest-setter_1 : tp_write\n")

  model_right <- "/*\n * File: coin.tppl\n * Description: Simplest meaningful probabilistic program. Evaluates how likely it is that a coin is fair, given data.\n * Compilation:\n *   tpplc models/lang/coin.tppl models/data/examples.mc out.mc && mi compile out.mc\n * Execution: ./out 100 1\n */\n\n/**\n * Conditions the likelihood of the computation \n *   on an observed datapoint to come from a particular Bernoulli experiment \n * Parameters:\n *   datapoint: Real\n *   probability: Real in (0, 1), the probability of True in the Bernoulli experiment\n * Returns: nothing\n * Side-effects: reweighs the computation\n */\nfunction flip(datapoint: Bool, probability: Real) {\n  observe datapoint ~ Bernoulli(probability);\n}\n\n/*\n * Model function\n * Data:\n *   coinflips: Bool[]\n * Prior: \n *   p ~ Beta(2, 2)\n * Posterior:\n *   p | coinflips\n */\nmodel function coinModel(coinflips: Bool[]) : Real  {\n  // Uncomment if you want to test the input\n  //printLn(\"Input:\");\n  //let coinStr = apply(bool2string, coinflips);\n  //printLn(join(coinStr));\n  assume p ~ Beta(2.0, 2.0); // prior\n  let n = length(coinflips);\n  for i in 1 to n {\n    flip(coinflips[i], p); // likelihood\n  }\n  return(p); // posterior\n}\n"
  data_right <- list(coinflips=c(FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE))
  model_data_right <-list(model=model_right, data=data_right)

  tp_write(model_data_right)

  model_data <- tp_import(file_path = temp_dir, project_name = "input")

  expect_equal(model_data, model_data_right)
})
