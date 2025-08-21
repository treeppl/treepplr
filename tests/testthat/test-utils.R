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

testthat::test_that("Test-utils_1a : stored_files", {
  cat("\tTest-utils_1a : stored_files with tppl\n")

  res <- treepplr:::stored_files("tppl")

  testthat::expect_equal(res, c("test", "test1"))
})

testthat::test_that("Test-utils_1b : stored_files", {
  cat("\tTest-utils_1b : stored_files with json\n")

  res <- treepplr:::stored_files("json")

  testthat::expect_equal(res, c("test", "test_out", "test1", "test1_out",
                                "testt_out"))
})

testthat::test_that("Test-utils_1c : stored_files", {
  cat("\tTest-utils_1c : stored_files with exe\n")

  res <- treepplr:::stored_files("exe")

  testthat::expect_equal(res, c("testx", "testx1"))
})

testthat::test_that("Test-utils_2 : tp_stored_model", {
  cat("\tTest-utils_2 : tp_stored_model with tppl\n")

  res <- treepplr::tp_stored_model()

  testthat::expect_equal(res, c("test", "test1"))
})

testthat::test_that("Test-utils_3 : tp_stored_data", {
  cat("\tTest-utils_3 : tp_stored_data\n")

  res <- treepplr::tp_stored_data()

  testthat::expect_equal(res, c("test", "test1"))
})

testthat::test_that("Test-utils_4 : tp_stored_compiled", {
  cat("\tTest-utils_4 : tp_stored_compiled\n")

  res <- treepplr::tp_stored_compiled()

  testthat::expect_equal(res, c("testx", "testx1"))
})
