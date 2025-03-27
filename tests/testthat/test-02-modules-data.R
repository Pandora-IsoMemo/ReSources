test_that("Test module targetValuesServer", {
  testData <-
    readRDS(testthat::test_path("testdata", "dataWithPriors_default.rds"))
  testData$optimalPrior <- TRUE
  
  testEvents <-
    list(
      processedCache = 8,
      name = list(
        list(
          event = "update",
          variable = "covariateNames",
          old = "covariate",
          new = "Covariate_1"
        )
      ),
      removeName = NULL,
      copyField = "",
      adaptive = FALSE,
      processed = 16
    )
  
  # test targetValuesServer ----
  testServer(targetValuesServer,
             args = list(
               values = do.call(reactiveValues, testData),
               events = do.call(reactiveValues, testEvents),
               termChoices = reactive(c(
                 `Default term` = "default",
                 `Add term 1` = "term1",
                 `Add term 2` = "term2",
                 `Add term 3` = "term3"
               )),
               modelType = reactive("5")
             ),
             {
               # Arrange
               print("test targetValuesServer")
               # Act
               session$setInputs(
                 targetOffset = TRUE,
                 targetValuesShowCovariates = FALSE,
                 targetValuesShowCoordinates = FALSE
               )
               expect_true(setequal(names(reactiveValuesToList(values)), names(testData)))
               expect_identical(reactiveValuesToList(values), testData[names(reactiveValuesToList(values))])
             })
  
  
  # test componentsServer ----
  testServer(componentsServer,
             args = list(
               values = do.call(reactiveValues, testData),
               events = do.call(reactiveValues, testEvents)
             ),
             {
               # Arrange
               print("test componentsServer")
               # Act
               expect_true(setequal(names(reactiveValuesToList(values)), names(testData)))
               expect_identical(reactiveValuesToList(values), testData[names(reactiveValuesToList(values))])
             })
  
  testServer(componentsServer,
             args = list(
               values = do.call(reactiveValues, testData),
               events = do.call(reactiveValues, testEvents)
             ),
             {
               # Arrange
               print("test componentsServer")
               # Act
               expect_true(setequal(names(reactiveValuesToList(values)), names(testData)))
               expect_identical(reactiveValuesToList(values), testData[names(reactiveValuesToList(values))])
             })
  
  
  # test sourcesServer ----
  testServer(sourcesServer,
             args = list(
               values = do.call(reactiveValues, testData),
               events = do.call(reactiveValues, testEvents),
               termChoices = reactive(c(
                 `Default term` = "default",
                 `Add term 1` = "term1",
                 `Add term 2` = "term2",
                 `Add term 3` = "term3"
               ))
             ),
             {
               # Arrange
               print("test sourcesServer")
               # Act
               expect_true(setequal(names(reactiveValuesToList(values)), names(testData)))
               expect_identical(reactiveValuesToList(values), testData[names(reactiveValuesToList(values))])
             })
  
  testServer(sourcesServer,
             args = list(
               values = do.call(reactiveValues, testData),
               events = do.call(reactiveValues, testEvents),
               termChoices = reactive(c(
                 `Default term` = "default",
                 `Add term 1` = "term1",
                 `Add term 2` = "term2",
                 `Add term 3` = "term3"
               ))
             ),
             {
               # Arrange
               print("test sourcesServer")
               # Act
               expect_true(setequal(names(reactiveValuesToList(values)), names(testData)))
               expect_identical(reactiveValuesToList(values), testData[names(reactiveValuesToList(values))])
             })
  
  
  # test concentrationsServer ----
  testServer(concentrationsServer,
             args = list(
               values = do.call(reactiveValues, testData),
               events = do.call(reactiveValues, testEvents)
             ),
             {
               # Arrange
               print("test concentrationsServer")
               # Act
               expect_true(setequal(names(reactiveValuesToList(values)), names(testData)))
               expect_identical(reactiveValuesToList(values), testData[names(reactiveValuesToList(values))])
             })

  testServer(concentrationsServer,
             args = list(
               values = do.call(reactiveValues, testData),
               events = do.call(reactiveValues, testEvents)
             ),
             {
               # Arrange
               print("test concentrationsServer")
               # Act
               expect_true(setequal(names(reactiveValuesToList(values)), names(testData)))
               expect_identical(reactiveValuesToList(values), testData[names(reactiveValuesToList(values))])
               
             })  
})
