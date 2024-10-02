defaultNames <- function(x, prefix = "proxy", sep = "_", l = length(x)) {
  if (is.null(x)) {
    x <- rep("", l)
  } else {
    length(x) <- l
  }

  l[is.na(l)] <- ""

  empty <- which(is.na(x) | trimws(x) == "")

  lapply(empty, function(i) {
    opts <- paste0(prefix, sep, i, c("", letters[1:26]))
    j <- (opts %in% x == FALSE) %>%
      which() %>%
      min()

    x[i] <<- opts[j]
  })
  x
}

defaultMatrixNames <- function(m, prefixRow, prefixCol, sep = "_") {
  if (is.list(m)) {
    m[[1]] <- defaultMatrixNames(m[[1]], prefixRow, prefixCol, sep)
    rownames(m[[2]]) <- rownames(m[[1]])
    colnames(m[[2]]) <- colnames(m[[1]])
    return(m)
  }

  rownames(m) <- defaultNames(rownames(m), prefixRow, sep, nrow(m))
  colnames(m) <- defaultNames(colnames(m), prefixCol, sep, ncol(m))

  m
}

getResetedMatrix <- function(currentData) {
  matrix(nrow = nrow(currentData),
         ncol = ncol(currentData),
         dimnames = list(rownames(currentData), colnames(currentData)))
}

defaultValues <- function() {
  list(
    status = "INITIALIZE",
    statusSim = "INITIALIZE",
    targetNames = "proxy_1",
    fractionNames = "fraction_1",
    sourceNames = "source_1",
    categoricalVars = NULL,
    numericVars = NULL,
    obsvn = list(
      default = emptyMatrix("Individual_1", "proxy_1")
    ),
    obsvnError = list(
      default = emptyMatrix("Individual_1", "proxy_1")
    ),
    targetValuesShowCovariates = TRUE,
    targetValuesCovariates = emptyMatrix("Individual_1", "covariate"),
    targetValuesCovariance = list(),
    obsvnNames = "Individual_1",
    obsvnDistribution = list(default = "normal"),
    fileNotes = NULL,
    weights = emptyMatrix("proxy_1", "proxy_1"),
    weightsUncert = emptyMatrix("proxy_1", "proxy_1"),
    weightOffset = emptyMatrix("proxy_1", "Offset"),
    weightOffsetUncert = emptyMatrix("proxy_1", "Offset"),
    weightDistribution = list(default = "normal"),
    source = list(
      default = list(
        list(
          "proxy_1" = emptyMatrix("source_1", "proxy_1")
        )
      )
    ),
    sourceUncert = list(
      default = list(
        list(
          "proxy_1" = emptyMatrix("source_1", "proxy_1")
        )
      )
    ),
    sourceDistribution = list(default = "normal"),
    sourceDistCovRep = list(default = FALSE),
    sourceCovariance = list(),
    concentration = list(emptyMatrix("source_1", "proxy_1")),
    concentrationUncert = list(emptyMatrix("source_1", "proxy_1")),
    concentrationDistribution = "normal",
    concentrationDistCovRep = FALSE,
    concentrationCovariance = list(emptyMatrix("proxy_1", "proxy_1")),
    modelType = "1",
    minUnc = 0.005,
    modelWeights = FALSE,
    modelConcentrations = TRUE,
    modelWeightsContrained = TRUE,
    modelConcentrationsContrained = TRUE,
    alphaHyper = c(source_1 = 1),
    oxcalCheck = FALSE,
    optimalPrior = TRUE,
    covariateType = "0",
    targetOffset = TRUE,
    burnin = 10000,
    iterations = 10000,
    thinning = 10,
    nchains = 1,
    inflatedBeta = "0",
    targetValuesShowCovariates = FALSE,
    targetValuesCovariance = list(
      Individual_1 = matrix(1, 1, 1, dimnames = list("proxy_1", "proxy_1"))
    ),
    includeSourceOffset = FALSE,
    sourceOffset = list(
      list(
        "proxy_1" = emptyMatrix("source_1", "proxy_1")
      )
    ),
    sourceOffsetUncert = list(
      list(
        "proxy_1" = emptyMatrix("source_1", "proxy_1")
      )
    ),
    targetValuesShowCoordinates = FALSE,
    exportCoordinates = matrix(
      NA, 1, 4,
      dimnames = list(
        paste("Individual", 1:1, sep = "_"),
        c("Longitude", "Latitude", "LowerLimit/Mean/Point", "UpperLimit/SD")
      )
    ),
    userEstimateGroups = list(),
    priors = character(),
    userEstimate = character()
  )
}

getAlphaHyperVec <- function(sourceNames, singleAlphaHyper = 1) {
  alphaHyperVec <- rep(singleAlphaHyper, length(sourceNames))
  names(alphaHyperVec) <- sourceNames
  
  return(alphaHyperVec)
}

allVariables <- function() {
  names(defaultValues())
}

getDefault <- function(variable) {
  defaultValues()[variable]
}

sampleName <- function(variable, suffix = FALSE) {
  n <- try(switch(variable,
    obsvnNames = "Individual",
    targetNames = "proxy",
    offsetNames = "Offset",
    covariateNames = "Covariate",
    sourceNames = "source",
    fractionNames = "fraction",
    targetPlusFractionNames = "proxy-fraction"
  ))
  # if (inherits(n, "try-error")) browser()
  if (suffix) {
    paste0(n, "_1")
  } else {
    n
  }
}
