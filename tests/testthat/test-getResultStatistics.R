context("getResultStatistics")

# Helper to load test data
test_data_path <- function(name) testthat::test_path("testdata", name)

# Minimal mock fruitsObj for structure tests
mock_fruitsObj <- list(
  valueNames = list(
    sources = c("A", "B"),
    targets = c("T1", "T2"),
    fractions = c("F1", "F2")
  ),
  data = list(
    obsvn = matrix(1:4, nrow = 2, dimnames = list(c("T1", "T2"))),
    covariates = matrix(1:4, nrow = 2, ncol = 2)
    # Add more fields as needed
    
  ),
  modelOptions = list(
    modelType = "1",
    hierarchical = FALSE,
    targetOffset = FALSE
  ),
  userEstimates = list(list(), list())
)

# Minimal parameters matrix
# Include mu[...] columns because translateParameters() always renames this block.
mock_parameters <- matrix(
  runif(12),
  nrow = 2,
  dimnames = list(
    NULL,
    c("alpha_A", "alpha_B", "mu[1]", "mu[2]", "mu[3]", "mu[4]")
  )
)

# Minimal userEstimates
mock_userEstimates <- matrix(
  runif(4),
  nrow = 2,
  dimnames = list(NULL, c("UE1", "UE2"))
)

test_that("agg=TRUE, DT=FALSE, bins=FALSE", {
  res <- getResultStatistics(mock_parameters, mock_userEstimates, mock_fruitsObj, DT = FALSE, agg = TRUE, bins = FALSE)
  expect_true(is.data.frame(res))
  expect_true(all(c("Group", "Estimate") %in% names(res)))
})

test_that("agg=FALSE, DT=FALSE", {
  res <- getResultStatistics(mock_parameters, mock_userEstimates, mock_fruitsObj, DT = FALSE, agg = FALSE)
  expect_true(is.data.frame(res))
  expect_true(all(c("Group", "Estimate") %in% names(res)))
})

test_that("agg=TRUE, DT=TRUE returns DT object", {
  res <- getResultStatistics(mock_parameters, mock_userEstimates, mock_fruitsObj, DT = TRUE, agg = TRUE)
  expect_true(any(class(res) %in% c("datatables", "datatables_htmlwidget")))
})

test_that("bins=TRUE adds bin columns", {
  res <- getResultStatistics(mock_parameters, mock_userEstimates, mock_fruitsObj, DT = FALSE, agg = TRUE, bins = TRUE)
  expect_true(any(grepl("bin_0.5", names(res))))
})

test_that("Custom statistics argument is respected", {
  stats <- c(
    TRUE, #input$SummaryMin,
    TRUE, #input$SummaryMax,
    TRUE, #input$SummaryMedian,
    FALSE, #input$SummaryQuantileCheck,
    NA, #input$SummaryQuantile / 100,
    NA, #input$SummaryQuantile2 / 100,
    FALSE, #input$BayesianPValuesCheck,
    NA, #input$pVal,
    FALSE, #input$SummaryHDICheck,
    NA #input$SummaryHDI / 100
  )
          
  res <- getResultStatistics(mock_parameters, mock_userEstimates, mock_fruitsObj, statistics = stats, DT = FALSE, agg = TRUE)
  expect_true(any(grepl("Minimum|Maximum|Median", names(res))))
})

# Test with empty userEstimates

test_that("Works with empty userEstimates", {
  res <- getResultStatistics(mock_parameters, matrix(nrow=0, ncol=0), mock_fruitsObj, DT = FALSE, agg = TRUE)
  expect_true(is.data.frame(res))
})
