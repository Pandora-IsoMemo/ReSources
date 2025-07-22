test_that("test compileRunModel - blackBearData with default inputs", {
  data_path <- testthat::test_path("testdata_large", "upload_empty.rds")
  skip_if_not(file.exists(data_path), "Skipping large data test on CI or for devtools:check()")
  
  testEmptyUpload <- readRDS(data_path)
  testCompleteUpload <-
    readRDS(testthat::test_path("testdata_large", "upload_complete.rds"))
  
  # expect 3 missing tables after filling values
  expect_length(suppressWarnings(checkForEmptyTables(fillValuesFromUpload(testEmptyUpload))), 3)
  
  # expect 1 missing table after filling values
  expect_length(suppressWarnings(checkForEmptyTables(fillValuesFromUpload(testCompleteUpload))), 1)
})
