temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-data : Import and convert data.\n"))

test_that("Test-data_1a : tp_data name", {
  cat("\tTest-data_1a \n")

  version <- list.files("/tmp", pattern = "treeppl", full.names = FALSE)
  version <- sort(version, decreasing = TRUE)[1]

  data_right <- paste0("/tmp/treeppl-0.2/y5b8qlyn9qgk61jxcdzq6gmv65-", version,"/lib/mcore/treeppl/models/lang/data/testdata_coin.json")

  data <- treepplr::tp_data("coin")

  expect_equal(data, data_right)

})


test_that("Test-data_1b : tp_data content", {
  cat("\tTest-data_1b \n")

  data_right <-
    tp_list(coinflips =
      c(
        TRUE,
        TRUE,
        TRUE,
        FALSE,
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        TRUE,
        FALSE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        FALSE
      )
    )

  path <- treepplr::tp_data("coin")
  content <- readLines(path, warn = FALSE)
  data <- jsonlite::fromJSON(content)

  expect_equal(data_right, data)

})


test_that("Test-data_2a : tp_write_data path", {
  cat("\tTest-data_2a \n")

  path_right <- paste0(temp_dir, "tmp_data_file.json")
  data <- list(test = "test_data")
  data_list <- tp_list(list(data))
  path <- treepplr::tp_write_data(data_list)

  expect_equal(path, path_right)

})
