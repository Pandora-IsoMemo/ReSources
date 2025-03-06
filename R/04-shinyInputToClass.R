#' Build object of class \code{\link{fruits}} from shiny input
#' @param values list: Shiny input
#' @param priors list: input$priors
#' @param userEstimates list: input$userEstimates
shinyInputToClass <- function(values, priors, userEstimates) {
  if (is.null(priors)) priors <- list()
  data <- list(
    obsvn = values[["obsvn"]][["default"]],
    obsvnError = values[["obsvnError"]][["default"]],
    obsvnCov = values[["targetValuesCovariance"]][["default"]],
    obsvnT1 = values[["obsvn"]][["term1"]],
    obsvnErrorT1 = values[["obsvnError"]][["term1"]],
    obsvnCovT1 = values[["targetValuesCovariance"]][["term1"]],
    obsvnT2 = values[["obsvn"]][["term2"]],
    obsvnErrorT2 = values[["obsvnError"]][["term2"]],
    obsvnCovT2 = values[["targetValuesCovariance"]][["term2"]],
    obsvnT3 = values[["obsvn"]][["term3"]],
    obsvnErrorT3 = values[["obsvnError"]][["term3"]],
    obsvnCovT3 = values[["targetValuesCovariance"]][["term3"]],
    weightOffset = values[["weightOffset"]],
    weightOffsetUncert = values[["weightOffsetUncert"]],
    weights = values[["weights"]],
    weightsUncert = values[["weightsUncert"]],
    concentration = values[["concentration"]],
    concentrationUncert = values[["concentrationUncert"]],
    concentrationCov = values[["concentrationCovariance"]],
    source = values[["source"]][["default"]],
    sourceUncert = values[["sourceUncert"]][["default"]],
    sourceCov = values[["sourceCovariance"]][["default"]],
    sourceT1 = values[["source"]][["term1"]],
    sourceUncertT1 = values[["sourceUncert"]][["term1"]],
    sourceCovT1 = values[["sourceCovariance"]][["term1"]],
    sourceT2 = values[["source"]][["term2"]],
    sourceUncertT2 = values[["sourceUncert"]][["term2"]],
    sourceCovT2 = values[["sourceCovariance"]][["term2"]],
    sourceT3 = values[["source"]][["term3"]],
    sourceUncertT3 = values[["sourceUncert"]][["term3"]],
    sourceCovT3 = values[["sourceCovariance"]][["term3"]],
    sourceOffset = values[["sourceOffset"]],
    sourceOffsetUnc = values[["sourceOffsetUncert"]],
    covariates = values[["targetValuesCovariates"]]
  )

  modelOptions <- list(
    modelType = values[["modelType"]],
    modelWeights = values[["modelWeights"]],
    categoricalVars = values[["categoricalVars"]],
    numericVars = values[["numericVars"]],
    modelWeightsContrained = values[["modelWeightsContrained"]],
    modelConcentrations = values[["modelConcentrations"]],
    modelConcentrationsContrained = values[["modelConcentrationsContrained"]],
    minUnc = values[["minUnc"]],
    targetOffset = values[["targetOffset"]],
    includeSourceOffset = values[["includeSourceOffset"]],
    burnin = values[["burnin"]],
    iterations = values[["iterations"]],
    thinning = values[["thinning"]],
    nchains = values[["nchains"]],
    hierarchical = values[["targetValuesShowCovariates"]],
    weightsDist = values[["weightDistribution"]],
    sourceDist = values[["sourceDistribution"]],
    sourceDistCovRep = values[["sourceDistCovRep"]],
    concentrationDist = values[["concentrationDistribution"]],
    concentrationDistCovRep = values[["concentrationDistCovRep"]],
    obsvnDist = values[["obsvnDistribution"]],
    inflatedBeta = values[["inflatedBeta"]],
    alphaHyper = values[["alphaHyper"]],       # this defines "sourceDirichPrior"
    optimalPrior = values[["optimalPrior"]],   # this defines "sourceDirichPrior"
    covariateType = values[["covariateType"]]
  )

  valueNames <- list(
    targets = values[["targetNames"]],
    fractions = values[["fractionNames"]],
    sources = values[["sourceNames"]]
  )

  res <- fruits(
    data = data,
    modelOptions = modelOptions,
    valueNames = valueNames,
    priors = priors,
    userEstimates = list(userEstimates, values[["userEstimateGroups"]])
  )

  res
}

getCurrentFruitsObject <- function(values, input) {
  shinyInputToClass(
    reactiveValuesToList(values),
    as.list(input$priors),
    as.list(input$userEstimate)
  ) %>%
    shinyTryCatch(errorTitle = "Could not create model input object: ", alertStyle = "shinyalert")
}
