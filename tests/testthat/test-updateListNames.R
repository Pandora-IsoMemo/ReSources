test_that("Test matching of names for non-baseline model", {
  testValues <-
    readRDS(testthat::test_path("testdata", "fiveSourcesData_default.rds"))
  
  targetNames <- colnames(testValues[["obsvn"]][[1]])
  obsvnNames <- rownames(testValues[["obsvn"]][[1]])
  
  expect_equal(targetNames, c("Carbon", "Nitrogen"))
  expect_equal(obsvnNames[1:2], c("Individual_1", "Individual_2"))
  
  # here level of targetNames (colnames of obsvn)
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("Carbon", "Nitrogen"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("Carbon", "Nitrogen"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("Carbon", "Nitrogen"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("Carbon", "Nitrogen"))
  
  # here level of obsvnNames (rownames of obsvn)
  expect_null(names(testValues[["source"]][[1]]))
  expect_null(names(testValues[["sourceUncert"]][[1]]))
  expect_null(names(testValues[["sourceOffset"]]))
  expect_null(names(testValues[["sourceOffsetUncert"]]))
  
  # here level of obsvnNames (rownames of obsvn), no targetNames for these entries
  expect_null(names(testValues[["sourceCovariance"]][[1]]))
  expect_null(names(testValues[["concentration"]]))
  expect_null(names(testValues[["concentrationUncert"]]))
  expect_null(names(testValues[["concentrationCovariance"]]))
  
})


test_that("Test matching of names for baseline model", {
  testValues <-
    readRDS(testthat::test_path("testdata", "blackBearData_baselineModel.rds"))
  
  targetNames <- colnames(testValues[["obsvn"]][[1]])
  obsvnNames <- rownames(testValues[["obsvn"]][[1]])
  
  expect_equal(targetNames, c("d13C", "d15N"))
  expect_equal(obsvnNames[1:2], c("Individual_1", "Individual_2"))
  
  # here level of targetNames (colnames of obsvn)
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("d13C", "d15N"))
  
  # here level of obsvnNames (rownames of obsvn)
  expect_equal(names(testValues[["source"]][[1]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["sourceUncert"]][[1]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["sourceOffset"]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["sourceOffsetUncert"]])[1:2], c("Individual_1", "Individual_2"))
  
  # here level of obsvnNames (rownames of obsvn), no targetNames for these entries
  expect_equal(names(testValues[["sourceCovariance"]][[1]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["concentration"]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["concentrationUncert"]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["concentrationCovariance"]])[1:2], c("Individual_1", "Individual_2"))
})


test_that("Test getDepthAndTable", {
  testFile <-
    c(
      "blackBearData_baselineModel.rds",
      "blackBearData_default.rds",
      "brownBearData_default.rds",
      "dataWithPriors_default.rds",
      "fiveSourcesData_default.rds"
    ) %>%
    sample(size = 1)
  testValues <- readRDS(testthat::test_path("testdata", testFile))
  
  # isEntryFun = isDeepestEntry -> here level of targetNames (colnames of obsvn)
  expect_equal(getDepthAndTable(testValues[["source"]], isEntryFun = isDeepestEntry)$nFlatten,
               2)
  expect_equal(getDepthAndTable(testValues[["sourceUncert"]], isEntryFun = isDeepestEntry)$nFlatten,
               2)
  expect_equal(getDepthAndTable(testValues[["sourceOffset"]], isEntryFun = isDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["sourceOffsetUncert"]], isEntryFun = isDeepestEntry)$nFlatten,
               1)
  
  # isEntryFun = isPreDeepestEntry -> here level of obsvnNames (rownames of obsvn)
  expect_equal(getDepthAndTable(testValues[["source"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["sourceUncert"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["sourceOffset"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               0)
  expect_equal(getDepthAndTable(testValues[["sourceOffsetUncert"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               0)
  
  # isEntryFun = isDeepestEntry -> here level of obsvnNames (rownames of obsvn)
  expect_equal(getDepthAndTable(testValues[["sourceCovariance"]], isEntryFun = isDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["concentration"]], isEntryFun = isDeepestEntry)$nFlatten,
               0)
  expect_equal(getDepthAndTable(testValues[["concentrationUncert"]], isEntryFun = isDeepestEntry)$nFlatten,
               0)
  expect_equal(getDepthAndTable(testValues[["concentrationCovariance"]], isEntryFun = isDeepestEntry)$nFlatten,
               0)
})


test_that("Test deleteTableFromList", {
  testValues <-
    readRDS(testthat::test_path("testdata", "blackBearData_default.rds"))
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c(c("d13C", "d15N")))
  
  testFile <-
    c(
      "blackBearData_default.rds",
      "brownBearData_default.rds",
      "dataWithPriors_default.rds",
      "fiveSourcesData_default.rds"
    ) %>%
    sample(size = 1)
  testValues <- readRDS(testthat::test_path("testdata", testFile))
  
  testCols <- names(testValues[["source"]][[1]][[1]])
  colToDelete <- testCols[1]
  leftCols <- testCols[testCols != colToDelete]
  
  expect_equal(names(
    deleteTableFromList(testValues[["source"]],
                        depth = 2,
                        namesList = colToDelete)[[1]][[1]]
  ),
  leftCols)
  expect_equal(names(
    deleteTableFromList(testValues[["sourceUncert"]],
                        depth = 2,
                        namesList = colToDelete)[[1]][[1]]
  ),
  leftCols)
  expect_equal(names(
    deleteTableFromList(testValues[["sourceOffset"]],
                        depth = 1,
                        namesList = colToDelete)[[1]]
  ),
  leftCols)
  expect_equal(names(
    deleteTableFromList(testValues[["sourceOffsetUncert"]],
                        depth = 1,
                        namesList = colToDelete)[[1]]
  ),
  leftCols)
})


test_that("Test updateTargetsInLists if non-baseline model", {
  testValues <-
    readRDS(testthat::test_path("testdata", "blackBearData_default.rds"))
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("d13C", "d15N"))
  
  testValues <-
    updateTargetsInLists(testValues, c("d13C_new", "d15N"), updateFun = updateListNames)
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C_new", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C_new", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C_new", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("d13C_new", "d15N"))
  
  testValues <-
    updateTargetsInLists(testValues, "d13C_new", updateFun = deleteTableFromList)
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), "d15N")
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), "d15N")
  expect_equal(names(testValues[["sourceOffset"]][[1]]), "d15N")
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), "d15N")
})


test_that("Test updateTargetsInLists if baseline model", {
  testValues <-
    readRDS(testthat::test_path("testdata", "blackBearData_baselineModel.rds"))
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("d13C", "d15N"))
  
  expect_equal(names(testValues[["source"]][[1]])[1:2], c("Individual_1", "Individual_2"))
  expect_true(length(testValues[["source"]][[1]]) > 0)
  
  # trying to remove obsvn with functions for targets should change nothing
  testValues <-
    updateTargetsInLists(testValues, "Individual_1", updateFun = deleteTableFromList)
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("d13C", "d15N"))
  
  # now testing deletion of a target
  testValues <-
    updateTargetsInLists(testValues, "d13C", updateFun = deleteTableFromList)
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), "d15N")
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), "d15N")
  expect_equal(names(testValues[["sourceOffset"]][[1]]), "d15N")
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), "d15N")
})


test_that("Test updateObsvnsInLists if non-baseline model", {
  testValues <-
    readRDS(testthat::test_path("testdata", "blackBearData_default.rds"))
  
  expect_null(names(testValues[["source"]][[1]]))
  expect_length(testValues[["source"]][[1]], 1)
  
  # removing obsvn from non-baseline model should change nothing
  testValues <-
    suppressWarnings(updateObsvnsInLists(testValues, "Individual_1", updateFun = deleteTableFromList))
  
  expect_null(names(testValues[["source"]][[1]]))
  expect_true(length(testValues[["source"]][[1]]) > 0)
})


test_that("Test updateObsvnsInLists if baseline model", {
  testValues <-
    readRDS(testthat::test_path("testdata", "blackBearData_baselineModel.rds"))
  
  expect_equal(names(testValues[["source"]][[1]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  expect_equal(names(testValues[["sourceUncert"]][[1]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  expect_equal(names(testValues[["sourceOffset"]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  expect_equal(names(testValues[["sourceOffsetUncert"]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  expect_equal(names(testValues[["sourceCovariance"]][[1]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  expect_equal(names(testValues[["concentration"]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  expect_equal(names(testValues[["concentrationUncert"]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  expect_equal(names(testValues[["concentrationCovariance"]])[1:3],
               c("Individual_1", "Individual_2", "Individual_3"))
  
  testValues <- updateObsvnsInLists(testValues, "Individual_2", updateFun = deleteTableFromList)
  
  expect_equal(names(testValues[["source"]][[1]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
  expect_equal(names(testValues[["sourceUncert"]][[1]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
  expect_equal(names(testValues[["sourceOffset"]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
  expect_equal(names(testValues[["sourceOffsetUncert"]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
  expect_equal(names(testValues[["sourceCovariance"]][[1]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
  expect_equal(names(testValues[["concentration"]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
  expect_equal(names(testValues[["concentrationUncert"]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
  expect_equal(names(testValues[["concentrationCovariance"]])[1:3],
               c("Individual_1", "Individual_3", "Individual_4"))
})


test_that("Test updateListNames", {
  testFile <-
    c(
      "blackBearData_default.rds",
      "brownBearData_default.rds",
      "dataWithPriors_default.rds",
      "fiveSourcesData_default.rds"
    ) %>%
    sample(size = 1)
  testValues <- readRDS(testthat::test_path("testdata", testFile))
  
  testCols <- names(testValues[["source"]][[1]][[1]])
  testCols[1] <- "newColName"
  
  expect_equal(names(
    updateListNames(testValues[["source"]],
                    depth = 2,
                    namesList = testCols)[[1]][[1]]
  ),
  testCols)
  expect_equal(names(
    updateListNames(testValues[["sourceUncert"]],
                    depth = 2,
                    namesList = testCols)[[1]][[1]]
  ),
  testCols)
  expect_equal(names(updateListNames(
    testValues[["sourceOffset"]],
    depth = 1,
    namesList = testCols
  )[[1]]),
  testCols)
  expect_equal(names(updateListNames(
    testValues[["sourceOffsetUncert"]],
    depth = 1,
    namesList = testCols
  )[[1]]),
  testCols)
})
