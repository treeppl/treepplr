temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-utils : Test utilitary function for treepplr.\n"))

options(warn=-1)
tmp <- list.files(tp_tempdir())
list_na <- do.call(rbind,lapply(c("exe","tppl","json") , function(x) {
  paste0(stringr::str_extract(tmp, paste0(".*(?=\\.", x, ")")),".",x)}))
  list <- list_na[!is.na(list_na)]
do.call(file.remove, list(list))
options(warn=0)

lis <-
  c(
    "test.json",
    "test_out.json",
    "testt_out.json",
    "test1.json",
    "test1_out.json",
    "test.tppl",
    "test1.tppl",
    "testx.exe",
    "testx1.exe"
  )
sapply(lis, function(x) write(".", file = x))

testthat::test_that("Test-utils_1a : model_data_stored", {
  cat("\tTest-utils_1a : model_data_stored with tppl\n")

  res <- treepplr:::model_data_stored("tppl")

  print(res)

  testthat::expect_equal(res, c("test", "test1"))
})

testthat::test_that("Test-utils_1b : model_data_stored", {
  cat("\tTest-utils_1b : model_data_stored with json\n")

  res <- treepplr:::model_data_stored("json")

  testthat::expect_equal(res, c("test", "test1", "test1_out",
                                "test_out", "testt_out"))
})

testthat::test_that("Test-utils_1c : model_data_stored", {
  cat("\tTest-utils_1c : model_data_stored with exe\n")

  res <- treepplr:::model_data_stored("exe")

  testthat::expect_equal(res, c("testx", "testx1"))
})

testthat::test_that("Test-utils_2 : tp_model_stored", {
  cat("\tTest-utils_2 : tp_model_stored with tppl\n")

  res <- treepplr::tp_model_stored()

  testthat::expect_equal(res, c("test", "test1"))
})

testthat::test_that("Test-utils_3 : tp_data_stored", {
  cat("\tTest-utils_3 : tp_data_stored\n")

  res <- treepplr::tp_data_stored()

  testthat::expect_equal(res, c("test", "test1"))
})

testthat::test_that("Test-utils_4 : tp_compile_stored", {
  cat("\tTest-utils_4 : tp_compile_stored\n")

  res <- treepplr::tp_compile_stored()

  testthat::expect_equal(res, c("testx", "testx1"))
})
