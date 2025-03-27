test_that("test compileRunModel - blackBearData with default inputs", {
  skip_if_not(Sys.getenv("RUN_LOCAL_TESTS") == "true", "Skipping large data test on CI")
  
  testEmptyUpload <-
    readRDS(testthat::test_path("testdata_large", "upload_empty.rds"))
  testCompleteUpload <-
    readRDS(testthat::test_path("testdata_large", "upload_complete.rds"))
  
  # expect 3 missing tables after filling values
  expect_length(suppressWarnings(checkForEmptyTables(fillValuesFromUpload(testEmptyUpload))), 3)
  
  # expect 1 missing table after filling values
  expect_length(suppressWarnings(checkForEmptyTables(fillValuesFromUpload(testCompleteUpload))), 1)
})
