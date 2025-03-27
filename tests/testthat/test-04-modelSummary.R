context("Model Summary")


test_that("extractResultMatrixOfChain with Exmpl data", {
  data_path <- testthat::test_path("testdata_large", "test-04-modelSummary_dataExmpl.rds")
  skip_if_not(file.exists(data_path), "Skipping large data test on CI or for devtools:check()")
  
  testData <- readRDS(data_path)
  renamedChains <- testData$renamedChains
  fruitsObj <- testData$fruitsObj
  
  statisticsNames <- c("mean", "sd", "lower 95%", "upper 95%")
  statisticsFunctions <-
    c(
      "mean",
      "sd",
      "function(y) quantile(y, 0.025, na.rm = TRUE)",
      "function(y) quantile(y, 0.975, na.rm = TRUE)"
    )
  
  resultMatrixList <- lapply(1:length(renamedChains), function(x) {
    extractResultMatrixOfChain(
      x,
      renamedChains = renamedChains,
      statisticsNames = statisticsNames,
      statisticsFunctions = statisticsFunctions,
      fruitsObj = fruitsObj
    )
  })
  
  resultMatrix <-
    do.call("rbind", lapply(1:length(renamedChains), function(x) {
      extractResultMatrixOfChain(
        x,
        renamedChains = renamedChains,
        statisticsNames = statisticsNames,
        statisticsFunctions = statisticsFunctions,
        fruitsObj = fruitsObj
      )
    }))
  
  expect_true(all(sapply(resultMatrixList,
                         function(x)
                           all(names(x) %in% names(resultMatrixList[[1]])))))
  
  expect_equal(
    head(resultMatrix),
    data.frame(
      stringsAsFactors = FALSE,
      row.names = c(
        "X.Herbivores.",
        "X.Herbivores..1",
        "X.Herbivores..2",
        "X.Herbivores..3",
        "X.Herbivores..4",
        "X.Carnivores."
      ),
      Group = c(
        "Source contributions",
        "Source contributions",
        "Source contributions",
        "Source contributions",
        "Source contributions",
        "Source contributions"
      ),
      Estimate = c(
        "[Herbivores]",
        "[Herbivores]",
        "[Herbivores]",
        "[Herbivores]",
        "[Herbivores]",
        "[Carnivores]"
      ),
      Target = c(
        "Individual_1",
        "Individual_2",
        "Individual_3",
        "Individual_4",
        "Individual_5",
        "Individual_1"
      ),
      Type = c(
        "targets",
        "targets",
        "targets",
        "targets",
        "targets",
        "targets"
      ),
      mean = c(0.099, 0.24, 0.636, 0.068, 0.024, 0.434),
      sd = c(0.114, 0.169, 0.217, 0.079, 0.038, 0.294),
      lower.95. = c(0, 0, 0.139, 0, 0, 0),
      upper.95. = c(0.366, 0.573, 0.957, 0.29, 0.118, 0.945)
    )
  )
  
})


test_that("extractResultMatrixOfChain with User data", {
  data_path <- testthat::test_path("testdata_large", "test-04-modelSummary_dataUser.rds")
  skip_if_not(file.exists(data_path), "Skipping large data test on CI or for devtools:check()")
  
  testData <- readRDS(data_path)
  renamedChains <- testData$renamedChains
  fruitsObj <- testData$fruitsObj
  
  statisticsNames <- c("mean", "sd", "lower 95%", "upper 95%")
  statisticsFunctions <-
    c(
      "mean",
      "sd",
      "function(y) quantile(y, 0.025, na.rm = TRUE)",
      "function(y) quantile(y, 0.975, na.rm = TRUE)"
    )
  
  resultMatrixList <- lapply(1:length(renamedChains), function(x) {
    extractResultMatrixOfChain(
      x,
      renamedChains = renamedChains,
      statisticsNames = statisticsNames,
      statisticsFunctions = statisticsFunctions,
      fruitsObj = fruitsObj
    )
  })
  
  resultMatrix <-
    do.call("rbind", lapply(1:length(renamedChains), function(x) {
      extractResultMatrixOfChain(
        x,
        renamedChains = renamedChains,
        statisticsNames = statisticsNames,
        statisticsFunctions = statisticsFunctions,
        fruitsObj = fruitsObj
      )
    }))
  
  expect_true(all(sapply(resultMatrixList,
                         function(x)
                           all(names(x) %in% names(resultMatrixList[[1]])))))
  
})
