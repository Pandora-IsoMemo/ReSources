test_that("extractContributionLimit", {
  # Arrange
  testEstTypes <- c("Source contributions", "Component contributions", "Source contributions d13C", 
                    "Source contributions d15N", "Target proxies", "Weights", "Component proxies", 
                    "Concentrations", "User estimate userGroup")
  testUserEstimateGroups <- list(list(id = "pzveloiefpdb20240528195917636882", name = "userGroup", 
                                       estimates = c("UserEstimate", "UserEstimate2"), normalize = TRUE))
  
  resTest <- sapply(testEstTypes, extractContributionLimit, userEstimateGroups = testUserEstimateGroups)
  names(resTest) <- testEstTypes
  
  # Act
  expect_equal(resTest,
               c(`Source contributions` = "0-100%", `Component contributions` = "0-100%", 
                 `Source contributions d13C` = "0-100%", `Source contributions d15N` = "0-100%", 
                 `Target proxies` = "None", Weights = "None", `Component proxies` = "None", 
                 Concentrations = "None", `User estimate userGroup` = "0-100%"))
  
  # Arrange
  testEstTypes <- c("Source contributions", "Component contributions", "Source contributions d13C", 
                    "Source contributions d15N", "Target proxies", "Weights", "Component proxies", 
                    "Concentrations")
  testUserEstimateGroups <- list()
  
  resTest <- sapply(testEstTypes, extractContributionLimit, userEstimateGroups = testUserEstimateGroups)
  names(resTest) <- testEstTypes
  
  # Act
  expect_equal(resTest,
               c(`Source contributions` = "0-100%", `Component contributions` = "0-100%", 
                 `Source contributions d13C` = "0-100%", `Source contributions d15N` = "0-100%", 
                 `Target proxies` = "None", Weights = "None", `Component proxies` = "None", 
                 Concentrations = "None"))
})
