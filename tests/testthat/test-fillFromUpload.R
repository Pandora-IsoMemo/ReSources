test_that("test compileRunModel - blackBearData with default inputs", {
  testEmptyUpload <-
    readRDS(testthat::test_path("dataUpload_emptyInput.rds"))
  testCompleteUpload <-
    readRDS(testthat::test_path("dataUpload_complete.rds"))
  
  # expect 3 missing tables after filling values
  expect_length(suppressWarnings(checkForEmptyTables(fillValuesFromUpload(testEmptyUpload))), 3)
  
  # expect 1 missing table after filling values
  expect_length(suppressWarnings(checkForEmptyTables(fillValuesFromUpload(testCompleteUpload))), 1)
})
