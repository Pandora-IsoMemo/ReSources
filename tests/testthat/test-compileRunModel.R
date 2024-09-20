# blackBearData ----
test_that("test compileRunModel - blackBearData with default inputs", {
  testData <-
    readRDS(testthat::test_path("blackBearData_default.rds"))
  testDataBaseline <-
    readRDS(testthat::test_path("blackBearData_baselineModel.rds"))
  
  print("Testing with blackBearData ----------")
  
  diffIndex <-
    sapply(1:length(testData), function(i)
      ! identical(testData[[i]], testDataBaseline[[i]]))
  
  expect_equal(
    names(testData[diffIndex]),
    c(
      "concentrationCovariance",
      "sourceUncert",
      "sourceCovariance",
      "sourceOffsetUncert",
      "concentration",
      "modelType",
      "source",
      "concentrationUncert",
      "sourceOffset"
    )
  )
  
  testModelTypes <- list(
    "Update model (all info shared)" = "1",
    "Individual targets (partially shared info)" = "2",
    "Individual targets (no shared info)" = "4"
  )
  
  testBaselineModelTypes <- list(
    "Baseline model (partially shared info)" = "3",
    "Baseline model (no shared info)" = "5"
  )
  
  testResultsVec <- compileTestModels(testData, testModelTypes)
  testResultsBaselineVec <-
    compileTestModels(testDataBaseline, testBaselineModelTypes)
  
  for (typeName in c(
    "Update model (all info shared)",
    "Individual targets (partially shared info)",
    "Individual targets (no shared info)"
  )) {
    print(paste("Testing modelType:", typeName))
    expect_false(inherits(testResultsVec[[typeName]], "try-error"))
    expect_equal(
      names(testResultsVec[[typeName]]),
      c(
        "parameters",
        "userEstimateSamples",
        "wAIC",
        "pValue",
        "BIC"
      )
    )
    expect_equal(testResultsVec[[typeName]]$userEstimateSamples, NULL)
    expect_gt(min(testResultsVec[[typeName]]$parameters), -30)
    expect_lt(max(testResultsVec[[typeName]]$parameters), 101)
    expect_gt(testResultsVec[[typeName]]$wAIC, 295)
    expect_lt(testResultsVec[[typeName]]$wAIC, 360)
    expect_gt(min(testResultsVec[[typeName]]$pValue), 0.45)
    expect_lt(max(testResultsVec[[typeName]]$pValue), 0.9999)
    expect_gt(testResultsVec[[typeName]]$BIC, 950)
    expect_lt(testResultsVec[[typeName]]$BIC, 9030)
  }
  
  for (typeName in c("Baseline model (partially shared info)",
                     "Baseline model (no shared info)")) {
    print(paste("Testing modelType:", typeName))
    expect_false(inherits(testResultsBaselineVec[[typeName]], "try-error"))
    expect_equal(
      names(testResultsBaselineVec[[typeName]]),
      c(
        "parameters",
        "userEstimateSamples",
        "wAIC",
        "pValue",
        "BIC"
      )
    )
    expect_equal(testResultsBaselineVec[[typeName]]$userEstimateSamples, NULL)
    expect_gt(min(testResultsBaselineVec[[typeName]]$parameters), -30)
    expect_lt(max(testResultsBaselineVec[[typeName]]$parameters), 101)
    expect_gt(testResultsBaselineVec[[typeName]]$wAIC, 340)
    expect_lt(testResultsBaselineVec[[typeName]]$wAIC, 360)
    expect_gt(min(testResultsBaselineVec[[typeName]]$pValue), 0.55)
    expect_lt(max(testResultsBaselineVec[[typeName]]$pValue), 0.9999)
    expect_gt(testResultsBaselineVec[[typeName]]$BIC, 7500)
    expect_lt(testResultsBaselineVec[[typeName]]$BIC, 9500)
  }
  
  rm(testData,
     testDataBaseline,
     diffIndex,
     testResultsVec,
     testResultsBaselineVec)
})

# brownBearData ----
test_that("test compileRunModel - brownBearData with default inputs", {
  testData <-
    readRDS(testthat::test_path("brownBearData_default.rds"))
  
  print("Testing with brownBearData ----------")
  
  testModelTypes <- list(
    "Update model (all info shared)" = "1",
    "Individual targets (partially shared info)" = "2",
    "Baseline model (partially shared info)" = "3",
    "Individual targets (no shared info)" = "4",
    "Baseline model (no shared info)" = "5"
  )
  
  testResultsVec <- compileTestModels(testData, testModelTypes)
  
  
  for (typeName in c(
    "Update model (all info shared)",
    "Baseline model (partially shared info)",
    "Baseline model (no shared info)"
  )) {
    print(paste("Testing modelType:", typeName))
    expect_true(inherits(testResultsVec[[typeName]], "try-error"))
  }
  
  for (typeName in c("Individual targets (partially shared info)",
                     "Individual targets (no shared info)")) {
    print(paste("Testing modelType:", typeName))
    expect_false(inherits(testResultsVec[[typeName]], "try-error"))
    expect_equal(
      names(testResultsVec[[typeName]]),
      c(
        "parameters",
        "userEstimateSamples",
        "wAIC",
        "pValue",
        "BIC"
      )
    )
    expect_equal(testResultsVec[[typeName]]$userEstimateSamples, NULL)
    expect_gt(min(testResultsVec[[typeName]]$parameters), -40)
    expect_lt(max(testResultsVec[[typeName]]$parameters), 101)
    expect_gt(testResultsVec[[typeName]]$wAIC, 130)
    expect_lt(testResultsVec[[typeName]]$wAIC, 650)
    expect_gt(min(testResultsVec[[typeName]]$pValue), 0.55)
    expect_lt(max(testResultsVec[[typeName]]$pValue), 1)
    expect_gt(testResultsVec[[typeName]]$BIC, 1153)
    expect_lt(testResultsVec[[typeName]]$BIC, 2900)
  }
  
  rm(testData, testResultsVec)
})

# fiveSourcesData ----
test_that("test compileRunModel - fiveSourcesData with default inputs", {
  testData <-
    readRDS(testthat::test_path("fiveSourcesData_default.rds"))
  
  print("Testing with fiveSourcesData ----------")
  
  testModelTypes <- list(
    "Update model (all info shared)" = "1",
    "Individual targets (partially shared info)" = "2",
    "Baseline model (partially shared info)" = "3",
    "Individual targets (no shared info)" = "4",
    "Baseline model (no shared info)" = "5"
  )
  
  testResultsVec <- compileTestModels(testData, testModelTypes)
  
  for (typeName in c(
    "Update model (all info shared)",
    "Baseline model (partially shared info)",
    "Baseline model (no shared info)"
  )) {
    print(paste("Testing modelType:", typeName))
    expect_true(inherits(testResultsVec[[typeName]], "try-error"))
  }
  
  for (typeName in c("Individual targets (partially shared info)",
                     "Individual targets (no shared info)")) {
    print(paste("Testing modelType:", typeName))
    expect_false(inherits(testResultsVec[[typeName]], "try-error"))
    expect_equal(
      names(testResultsVec[[typeName]]),
      c(
        "parameters",
        "userEstimateSamples",
        "wAIC",
        "pValue",
        "BIC"
      )
    )
    expect_equal(testResultsVec[[typeName]]$userEstimateSamples, NULL)
    expect_gt(min(testResultsVec[[typeName]]$parameters), -35)
    expect_lt(max(testResultsVec[[typeName]]$parameters), 101)
    expect_gt(testResultsVec[[typeName]]$wAIC, 20)
    expect_lt(testResultsVec[[typeName]]$wAIC, 100)
    expect_gt(min(testResultsVec[[typeName]]$pValue), 0.55)
    expect_lt(max(testResultsVec[[typeName]]$pValue), 0.9999)
    expect_gt(testResultsVec[[typeName]]$BIC, 350)
    expect_lt(testResultsVec[[typeName]]$BIC, 900)
  }
  
  rm(testData, testResultsVec)
})

# romanData ----
test_that("test compileRunModel - romanData with default inputs", {
  testData <-
    readRDS(testthat::test_path("romanData_default.rds"))
  
  print("Testing with romanData ----------")
  
  testModelTypes <- list(
    #"Update model (all info shared)" = "1",             # this takes too long for automated tests!
    #"Individual targets (partially shared info)" = "2", # this takes too long for automated tests!
    "Baseline model (partially shared info)" = "3",
    #"Individual targets (no shared info)" = "4",        # this takes too long for automated tests!
    "Baseline model (no shared info)" = "5"
  )
  
  testResultsVec <- compileTestModels(testData, testModelTypes)
  
  for (typeName in c("Baseline model (partially shared info)",
                     "Baseline model (no shared info)")) {
    print(paste("Testing modelType:", typeName))
    expect_true(inherits(testResultsVec[[typeName]], "try-error"))
  }
  
  # for (typeName in c(
  #   "Update model (all info shared)",
  #   "Individual targets (partially shared info)",
  #   "Individual targets (no shared info)"
  # )) {
  #   print(paste("Testing modelType:", typeName))
  #   expect_false(inherits(testResultsVec[[typeName]], "try-error"))
  #   expect_equal(
  #     names(testResultsVec[[typeName]]),
  #     c("parameters", "userEstimateSamples", "wAIC", "pValue", "BIC")
  #   )
  #   expect_equal(testResultsVec[[typeName]]$userEstimateSamples, NULL)
  #   expect_gt(min(testResultsVec[[typeName]]$parameters), -40)
  #   expect_lt(max(testResultsVec[[typeName]]$parameters), 101)
  #   expect_gt(testResultsVec[[typeName]]$wAIC, 4270)
  #   expect_lt(testResultsVec[[typeName]]$wAIC, 115000)
  #   expect_gt(min(testResultsVec[[typeName]]$pValue), 0.45)
  #   expect_lt(max(testResultsVec[[typeName]]$pValue), 1.01)
  #   expect_gt(testResultsVec[[typeName]]$BIC, 15570)
  #   expect_lt(testResultsVec[[typeName]]$BIC, 322000)
  # }
  
  rm(testData, testResultsVec)
})

# dataWithPriors ----
test_that("test compileRunModel - dataWithPriors with default inputs", {
  testData <-
    readRDS(testthat::test_path("dataWithPriors_default.rds"))
  
  print("Testing with dataWithPriors ----------")
  
  testPriors <-
    c(
      "[Ovicaprid]>[Cattle]",
      "[Pig]>[Ovicaprid]",
      "[Pig]>[Poultry]",
      "[Pig]>[Poultry]",
      "[C3]>[C4]",
      "[C3]+[C4]>[Cattle]+[Ovicaprid]+[Pig]+[Poultry]+[Marine]",
      "(([C3]*(([C3-Protein]*0.04)+([C3-LipidsCarbohydrates]*0.04)))+([C4]*(([C4-Protein]*0.04)+([C4-LipidsCarbohydrates]*0.04))))/(([C3]*(([C3-Protein]*0.04)+([C3-LipidsCarbohydrates]*0.04)))+([C4]*(([C4-Protein]*0.04)+([C4-LipidsCarbohydrates]*0.04)))+([Cattle]*(([Cattle-Protein]*0.04)+([Cattle-LipidsCarbohydrates]*0.09)))+([Ovicaprid]*(([Ovicaprid-Protein]*0.04)+([Ovicaprid-LipidsCarbohydrates]*0.09)))+([Pig]*(([Pig-Protein]*0.04)+([Pig-LipidsCarbohydrates]*0.09)))+([Poultry]*(([Poultry-Protein]*0.04)+([Poultry-LipidsCarbohydrates]*0.09)))+([Marine]*(([Marine-Protein]*0.04)+([Marine-LipidsCarbohydrates]*0.09))))>0.57"
    )
  
  testModelTypes <- list(
    "Update model (all info shared)" = "1",
    "Individual targets (partially shared info)" = "2",
    "Baseline model (partially shared info)" = "3",
    "Individual targets (no shared info)" = "4",
    "Baseline model (no shared info)" = "5"
  )
  
  testResultsVec <- list()
  
  # expect an error:
  testResultsVec[1] <-
    suppressWarnings(compileTestModels(testData, testModelTypes[1], priors = testPriors))
  
  # expect success:
  testResultsVec[2] <-
    compileTestModels(testData, testModelTypes[2], priors = testPriors)
  testResultsVec[3] <-
    compileTestModels(testData, testModelTypes[3], priors = testPriors)
  testResultsVec[4] <-
    compileTestModels(testData, testModelTypes[4], priors = testPriors)
  testResultsVec[5] <-
    compileTestModels(testData, testModelTypes[5], priors = testPriors)
  
  names(testResultsVec) <- names(testModelTypes)
  
  for (typeName in c("Update model (all info shared)")) {
    print(paste("Testing modelType:", typeName))
    expect_true(inherits(testResultsVec[[typeName]], "try-error"))
  }
  
  for (typeName in c(
    "Individual targets (partially shared info)",
    "Baseline model (partially shared info)",
    "Individual targets (no shared info)",
    "Baseline model (no shared info)"
  )) {
    print(paste("Testing modelType:", typeName))
    expect_false(inherits(testResultsVec[[typeName]], "try-error"))
    expect_equal(
      names(testResultsVec[[typeName]]),
      c(
        "parameters",
        "userEstimateSamples",
        "wAIC",
        "pValue",
        "BIC"
      )
    )
    expect_equal(testResultsVec[[typeName]]$userEstimateSamples, NULL)
    expect_gt(min(testResultsVec[[typeName]]$parameters), -50)
    expect_lt(max(testResultsVec[[typeName]]$parameters), 101)
    expect_gt(testResultsVec[[typeName]]$wAIC, 440)
    expect_lt(testResultsVec[[typeName]]$wAIC, 550)
    expect_gt(min(testResultsVec[[typeName]]$pValue), 0.99)
    expect_lt(max(testResultsVec[[typeName]]$pValue), 1.01)
    expect_gt(testResultsVec[[typeName]]$BIC, 595)
    expect_lt(testResultsVec[[typeName]]$BIC, 620)
  }
  
  rm(testData, testResultsVec)
})
