make_minimal_fruits_obj <- function() {
  list(
    data = list(
      obsvn = matrix(0, nrow = 1, ncol = 1, dimnames = list("i1", "t1")),
      obsvnError = matrix(0.1, nrow = 1, ncol = 1, dimnames = list("i1", "t1")),
      covariates = data.frame()
    )
  )
}

extract_trace_map <- function(plot_obj, field) {
  traces <- plotly::plotly_build(plot_obj)[["x"]][["data"]]
  vals <- sapply(traces, function(tr) {
    value <- tr[[field]]
    if (is.null(value)) {
      return(NA_real_)
    }
    as.numeric(value)[1]
  })
  names(vals) <- sapply(traces, function(tr) as.character(tr[["name"]])[1])
  vals
}

extract_trace_error <- function(plot_obj, axis = "y") {
  err_name <- paste0("error_", axis)
  traces <- plotly::plotly_build(plot_obj)[["x"]][["data"]]
  vals <- sapply(traces, function(tr) {
    err <- tr[[err_name]]
    if (is.null(err) || is.null(err[["array"]])) {
      return(NA_real_)
    }
    as.numeric(err[["array"]])[1]
  })
  names(vals) <- sapply(traces, function(tr) as.character(tr[["name"]])[1])
  vals
}

test_that("sourceTargetPlot uses normal-based symmetric intervals for simSources in 1D", {
  set.seed(1)
  confidence <- 0.9

  simSources <- list(
    S1 = matrix(rnorm(200, mean = 0, sd = 1), ncol = 1),
    S2 = matrix(rnorm(200, mean = 1, sd = 2), ncol = 1)
  )
  colnames(simSources[[1]]) <- "t1"
  colnames(simSources[[2]]) <- "t1"

  p <- ReSources:::sourceTargetPlot(
    simSources = simSources,
    simSourcesAll = simSources,
    simGrid = NULL,
    confidence = confidence,
    showConfidence = TRUE,
    targets = "t1",
    fruitsObj = make_minimal_fruits_obj(),
    showIndividuals = FALSE,
    horizontalPlot = "vertical"
  )

  observed_y <- extract_trace_map(p, "y")
  observed_err <- extract_trace_error(p, axis = "y")

  expected_y <- sapply(simSources, function(m) round(mean(m[, 1]), 3))
  expected_err <- sapply(simSources, function(m) {
    sd_from_cov <- sqrt(signif(stats::var(m[, 1]), 3))
    round(qnorm(1 - (1 - confidence) / 2) * sd_from_cov, 3)
  })

  expect_equal(observed_y[names(expected_y)], expected_y)
  expect_equal(observed_err[names(expected_err)], expected_err)
})

test_that("sourceTargetPlot applies the same interval rule for userDefinedSim in 1D", {
  set.seed(2)
  confidence <- 0.95

  simSources <- list(S1 = matrix(rnorm(150, mean = 0.2, sd = 0.8), ncol = 1))
  userDefinedSim <- list(U1 = matrix(rnorm(150, mean = 2.0, sd = 0.3), ncol = 1))

  colnames(simSources[[1]]) <- "t1"
  colnames(userDefinedSim[[1]]) <- "t1"

  p <- ReSources:::sourceTargetPlot(
    simSources = simSources,
    simSourcesAll = simSources,
    simGrid = NULL,
    confidence = confidence,
    showConfidence = TRUE,
    targets = "t1",
    fruitsObj = make_minimal_fruits_obj(),
    showIndividuals = FALSE,
    horizontalPlot = "vertical",
    userDefinedSim = userDefinedSim
  )

  observed_y <- extract_trace_map(p, "y")
  observed_err <- extract_trace_error(p, axis = "y")

  expect_true("U1" %in% names(observed_y))
  expect_true("U1" %in% names(observed_err))

  expected_y_u1 <- round(mean(userDefinedSim[[1]][, 1]), 3)
  expected_err_u1 <- round(
    qnorm(1 - (1 - confidence) / 2) * sqrt(signif(stats::var(userDefinedSim[[1]][, 1]), 3)),
    3
  )

  expect_equal(observed_y[["U1"]], expected_y_u1)
  expect_equal(observed_err[["U1"]], expected_err_u1)
})

test_that("sourceTargetPlot omits 1D error bars when showConfidence is FALSE", {
  set.seed(3)

  simSources <- list(
    S1 = matrix(rnorm(120, mean = 0, sd = 1), ncol = 1),
    S2 = matrix(rnorm(120, mean = 1, sd = 1), ncol = 1)
  )
  colnames(simSources[[1]]) <- "t1"
  colnames(simSources[[2]]) <- "t1"

  p <- ReSources:::sourceTargetPlot(
    simSources = simSources,
    simSourcesAll = simSources,
    simGrid = NULL,
    confidence = 0.9,
    showConfidence = FALSE,
    targets = "t1",
    fruitsObj = make_minimal_fruits_obj(),
    showIndividuals = FALSE,
    horizontalPlot = "vertical"
  )

  observed_err <- extract_trace_error(p, axis = "y")
  expect_true(all(is.na(observed_err)))
})
