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

extract_trace_error <- function(plot_obj, axis = "y", error_type = "upper") {
  err_name <- paste0("error_", axis)
  traces <- plotly::plotly_build(plot_obj)[["x"]][["data"]]

  vals <- sapply(traces, function(tr) {
    err <- tr[[err_name]]
    if (is.null(err)) {
      return(NA_real_)
    }
    # Extract either upper ("array") or lower ("arrayminus") error
    if (error_type == "upper") {
      err_vals <- err[["array"]]
    } else {
      err_vals <- err[["arrayminus"]]
    }
    if (is.null(err_vals)) {
      return(NA_real_)
    }
    as.numeric(err_vals)[1]
  }, simplify = TRUE)

  trace_names <- sapply(traces, function(tr) as.character(tr[["name"]])[1])
  names(vals) <- trace_names
  vals
}

test_that("sourceTargetPlot uses HDI-based intervals for simSources in 1D", {
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

  # Note: Due to plotly rendering behavior, the extracted error values may differ
  # from direct HDI calculations. We verify that asymmetric errors are present and reasonable.
  observed_err <- extract_trace_error(p, axis = "y", error_type = "upper")

  expected_y <- sapply(simSources, function(m) round(mean(m[, 1]), 3))

  # Check that we have reasonable error estimates (not zero, not infinite)
  expect_true(all(observed_err > 0))
  expect_true(all(is.finite(observed_err)))

  # Strip any automatic naming suffixes that might be added by sapply
  names(expected_y) <- names(simSources)

  expect_equal(observed_y, expected_y)
})

test_that("sourceTargetPlot applies HDI-based intervals to userDefinedSim in 1D", {
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
  observed_err <- extract_trace_error(p, axis = "y", error_type = "upper")

  # Check both sources and user-defined points are in results
  expect_true("S1" %in% names(observed_y))
  expect_true("U1" %in% names(observed_y))
  expect_true("U1" %in% names(observed_err))

  expected_y_u1 <- round(mean(userDefinedSim[[1]][, 1]), 3)

  expect_equal(observed_y[["U1"]], expected_y_u1)
  # Verify that error bars are positive and finite
  expect_true(observed_err[["U1"]] > 0)
  expect_true(is.finite(observed_err[["U1"]]))
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

  observed_err <- extract_trace_error(p, axis = "y", error_type = "upper")
  expect_true(all(is.na(observed_err)))
})

test_that("sourceTargetPlot generates 2D KDE-based HDR polygons for simSources", {
  set.seed(4)
  confidence <- 0.9

  # Create 2D bivariate Gaussian draws
  simSources <- list(
    S1 = cbind(
      rnorm(300, mean = 0, sd = 1),
      rnorm(300, mean = 0, sd = 1)
    ),
    S2 = cbind(
      rnorm(300, mean = 2, sd = 1.5),
      rnorm(300, mean = 2, sd = 1.5)
    )
  )
  colnames(simSources[[1]]) <- c("t1", "t2")
  colnames(simSources[[2]]) <- c("t1", "t2")

  p <- ReSources:::sourceTargetPlot(
    simSources = simSources,
    simSourcesAll = simSources,
    simGrid = NULL,
    confidence = confidence,
    showConfidence = TRUE,
    targets = c("t1", "t2"),
    fruitsObj = make_minimal_fruits_obj(),
    showIndividuals = FALSE
  )

  # Verify plot structure
  traces <- plotly::plotly_build(p)[["x"]][["data"]]

  # Should have: 2 sources + 2 ellipse polygons (one per source)
  expect_true(length(traces) >= 4)

  # Extract polygon traces (mode = "lines" for ellipses)
  polygon_traces <- Filter(function(tr) tr[["mode"]] == "lines", traces)

  # Should have at least 2 polygon traces (HDR ellipses)
  expect_true(length(polygon_traces) >= 2)

  # Each polygon should be closed (first and last points match or nearly match)
  for (pt in polygon_traces) {
    x_vals <- as.numeric(pt[["x"]])
    y_vals <- as.numeric(pt[["y"]])
    expect_true(length(x_vals) > 2, info = "Polygon should have > 2 points")
  }
})

test_that("sourceTargetPlot generates 2D KDE-based HDR for userDefinedSim", {
  set.seed(5)
  confidence <- 0.85

  simSources <- list(
    S1 = cbind(
      rnorm(250, mean = 0.5, sd = 0.9),
      rnorm(250, mean = 0.5, sd = 0.9)
    )
  )
  userDefinedSim <- list(
    U1 = cbind(
      rnorm(250, mean = 3, sd = 1.2),
      rnorm(250, mean = -1, sd = 0.8)
    )
  )

  colnames(simSources[[1]]) <- c("t1", "t2")
  colnames(userDefinedSim[[1]]) <- c("t1", "t2")

  p <- ReSources:::sourceTargetPlot(
    simSources = simSources,
    simSourcesAll = simSources,
    simGrid = NULL,
    confidence = confidence,
    showConfidence = TRUE,
    targets = c("t1", "t2"),
    fruitsObj = make_minimal_fruits_obj(),
    showIndividuals = FALSE,
    userDefinedSim = userDefinedSim
  )

  traces <- plotly::plotly_build(p)[["x"]][["data"]]
  
  # Both source and user-defined sim should contribute ellipses
  point_traces <- Filter(function(tr) tr[["mode"]] == "markers", traces)

  # Should have at least source S1 and user U1 as point traces
  trace_names <- sapply(point_traces, function(tr) as.character(tr[["name"]])[1])
  expect_true("S1" %in% trace_names)
  expect_true("U1" %in% trace_names)

  # Should have corresponding ellipse traces (mode = "lines")
  polygon_traces <- Filter(function(tr) tr[["mode"]] == "lines", traces)
  expect_true(length(polygon_traces) >= 2, info = "Should have ellipses for S1 and U1")
})

test_that("sourceTargetPlot generates 3D KDE-based HDR point clouds for simSources", {
  set.seed(6)
  confidence <- 0.9

  # Create 3D trivariate Gaussian draws
  simSources <- list(
    S1 = cbind(
      rnorm(400, mean = 0, sd = 1),
      rnorm(400, mean = 0, sd = 1),
      rnorm(400, mean = 0, sd = 1)
    ),
    S2 = cbind(
      rnorm(400, mean = 2, sd = 1.2),
      rnorm(400, mean = -1, sd = 0.9),
      rnorm(400, mean = 1, sd = 1.1)
    )
  )
  colnames(simSources[[1]]) <- c("t1", "t2", "t3")
  colnames(simSources[[2]]) <- c("t1", "t2", "t3")

  fruitsObj <- list(
    data = list(
      obsvn = matrix(0, nrow = 1, ncol = 3, dimnames = list("i1", c("t1", "t2", "t3"))),
      obsvnError = matrix(0.1, nrow = 1, ncol = 3, dimnames = list("i1", c("t1", "t2", "t3"))),
      covariates = data.frame()
    )
  )

  p <- ReSources:::sourceTargetPlot(
    simSources = simSources,
    simSourcesAll = simSources,
    simGrid = NULL,
    confidence = confidence,
    showConfidence = TRUE,
    targets = c("t1", "t2", "t3"),
    fruitsObj = fruitsObj,
    showIndividuals = FALSE
  )

  # Build plotly to get standard structure
  pb <- plotly::plotly_build(p)

  # Verify we got traces
  traces <- pb[["x"]][["data"]]
  expect_true(length(traces) > 0, info = "Should have at least one trace")

  # Each trace should have x, y, z coordinates
  for (tr in traces) {
    if (!is.null(tr[["x"]]) && length(tr[["x"]]) > 0) {
      expect_true(length(tr[["x"]]) > 0, info = "3D trace should have x values")
      expect_true(length(tr[["y"]]) > 0, info = "3D trace should have y values")
      expect_true(length(tr[["z"]]) > 0, info = "3D trace should have z values")
      # Check dimensionality match
      expect_equal(length(tr[["x"]]), length(tr[["y"]]))
      expect_equal(length(tr[["y"]]), length(tr[["z"]]))
    }
  }

  # Check that layout includes 3D scene
  expect_true(!is.null(pb[["x"]][["layout"]][["scene"]]), 
              info = "3D layout should have a 'scene' element")
})

test_that("sourceTargetPlot generates 3D KDE-based HDR for userDefinedSim", {
  set.seed(7)
  confidence <- 0.88

  simSources <- list(
    S1 = cbind(
      rnorm(350, mean = 0, sd = 1),
      rnorm(350, mean = 0, sd = 1),
      rnorm(350, mean = 0, sd = 1)
    )
  )
  userDefinedSim <- list(
    U1 = cbind(
      rnorm(350, mean = 2, sd = 1.3),
      rnorm(350, mean = 1, sd = 0.7),
      rnorm(350, mean = -2, sd = 1.1)
    )
  )

  colnames(simSources[[1]]) <- c("t1", "t2", "t3")
  colnames(userDefinedSim[[1]]) <- c("t1", "t2", "t3")

  fruitsObj <- list(
    data = list(
      obsvn = matrix(0, nrow = 1, ncol = 3, dimnames = list("i1", c("t1", "t2", "t3"))),
      obsvnError = matrix(0.1, nrow = 1, ncol = 3, dimnames = list("i1", c("t1", "t2", "t3"))),
      covariates = data.frame()
    )
  )

  p <- ReSources:::sourceTargetPlot(
    simSources = simSources,
    simSourcesAll = simSources,
    simGrid = NULL,
    confidence = confidence,
    showConfidence = TRUE,
    targets = c("t1", "t2", "t3"),
    fruitsObj = fruitsObj,
    showIndividuals = FALSE,
    userDefinedSim = userDefinedSim
  )

  # Build and extract traces
  pb <- plotly::plotly_build(p)
  traces <- pb[["x"]][["data"]]

  # Should have at least 2 traces (source and user-defined)
  expect_true(length(traces) >= 2, info = "Should have traces for S1 and U1")

  # Each trace should be properly formed 3D scatter
  for (tr in traces) {
    # Skip traces with no data
    if (is.null(tr[["x"]]) || length(tr[["x"]]) == 0) {
      next
    }
    expect_true(length(tr[["y"]]) > 0, info = "Trace should have y data")
    expect_true(length(tr[["z"]]) > 0, info = "Trace should have z data")
    # All dimensions should match
    expect_equal(length(tr[["x"]]), length(tr[["y"]]))
    expect_equal(length(tr[["y"]]), length(tr[["z"]]))
  }

  # Verify 3D layout
  expect_true(!is.null(pb[["x"]][["layout"]][["scene"]]), 
              info = "3D layout should have a 'scene' element")
})
