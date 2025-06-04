# Copyright (CeCILL-2) 2024 RTF authors (gspace2infr package)

if (require("testthat", quietly = TRUE)) {
  pkg <- "treepplr"
  require(pkg, character.only = TRUE, quietly = TRUE)
  if (interactive()) {
    while (dev.cur() > 1L) {
      dev.off()
    }
    oldask <- devAskNewPage(ask = FALSE)

    testfiles <-
      dir(system.file("/tests/testthat/", package = "treepplr"),
          full.names = TRUE)
    timings <- t(sapply(testfiles, function(fich) {
      system.time(source(fich))
    }))
    print(timings)
    print(colSums(timings))
    devAskNewPage(oldask)
    ## fixme: pb if dev.new has been called in-between
  } else {
    ## for devtools or R CMD check,
    # cf ?test_check for using library() here:
    library("testthat")
    library(pkg, character.only = TRUE)

    report <- test_check(pkg)
    print("DONE")
    print(warnings())
  } #TODO? catch most of these by expect_warning(..)
} else {
  cat("package 'testthat' not available, cannot run unit tests\n")
}
