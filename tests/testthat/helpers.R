# This script is run automatically when testthat::test_dir(testthat::test_path()) is called.

compileTestModels <- function(testData, testModelTypes, priors = NULL, userEstimates = NULL) {
  testResultsVec <- list()
  
  # new column added: values[["optimalPrior"]], default value == TRUE
  testData$optimalPrior <- TRUE
  
  for (i in 1:length(testModelTypes)) {
    typeName <- names(testModelTypes)[i]
    
    # set specific modelType
    testData$modelType <- testModelTypes[i]
    
    print(paste("Compiling modelType:", typeName))
    startTime <- Sys.time()
    testResultsVec[[typeName]] <-
      try({
        compileRunModel(
          fruitsObj = shinyInputToClass(testData,
                                        priors = as.list(priors),
                                        userEstimates = as.list(userEstimates)),
          progress = FALSE,
          userDefinedAlphas = testData$userDefinedAlphas
        )
      },
      silent = TRUE)
    
    print(difftime(Sys.time(), startTime))
  }
  
  testResultsVec
}

safe_read_xlsx <- function(xlsxFile) {
  tryCatch(
    read.xlsx(xlsxFile),
    error = function(e) {
      testthat::skip(paste("Skipping xlsx test because the source could not be read:", conditionMessage(e)))
    }
  )
}
