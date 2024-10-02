#' Constructor for S3 class \code{fruits}
#' @description All arguments are checked for correct class, length, names etc.
#' Constants and model code are added automatically.
#' @param data list: input data
#' @param modelOptions list: contains elements \code{burnin} and \code{iterations}
#' @param valueNames list: contains names for the \code{targets},
#' \code{fractions}, and \code{sources}
#' @param priors list of characters with prior expressions
#' @param userEstimates list of characters
#' @examples
#' \dontrun{
#' # load fruits data:
#' load(system.file("app/exampleModels/", "fruitsExample.Rdata", package = "ReSources"))
#' # get Fruits-Object
#' fruitsObj <- fruits(data, modelOptions, valueNames, priors, userEstimates)
#' # run model
#' model <- compileRunModel(fruitsObj)
#' modelResults <- getResultStatistics(model$parameters, model$userEstimateSamples, fruitsObj,
#'   DT = FALSE, agg = FALSE
#' )
#'
#' # better: use shiny app:
#' shiny::runApp(paste0(system.file(package = "ReSources"), "/app"))
#' }
#' @export
fruits <- function(data,
                   modelOptions,
                   valueNames,
                   priors = list(),
                   userEstimates = list(list(), list())) {
  # check value names
  # Falls keine Gewichte im Modell spezifiziert (modelWeights - checkbox)
  if (modelOptions$modelConcentrations == FALSE | all(is.na(data$concentration))) {
    data$concentration <- matrix(100,
      ncol = length(valueNames$fractions),
      nrow = length(valueNames$sources)
    )
    rownames(data$concentration) <- valueNames$sources
    colnames(data$concentration) <- valueNames$fractions
    data$concentrationUncert <- matrix(0,
      ncol = length(valueNames$fractions),
      nrow = length(valueNames$sources)
    )
  }

  if (modelOptions$modelWeights == FALSE | all(is.na(data$weights))) {
    # valueNames$fractions <- valueNames$targets
    data$weights <- diag(rep(100, length(valueNames$targets)),
      nrow = length(valueNames$targets),
      ncol = length(valueNames$targets)
    )
    rownames(data$weights) <- valueNames$targets
    colnames(data$weights) <- valueNames$targets
    data$weightsUncert <- matrix(0,
      ncol = length(valueNames$targets),
      nrow = length(valueNames$targets),
      dimnames = list(
        valueNames$targets,
        valueNames$targets
      )
    )
  }
  # separate categorical and numerical covariates
  if (!is.null(data$covariates) &&
    nrow(data$covariates) == nrow(data$obsvn)) {
    numCols <- modelOptions$numericVars
    catCols <- modelOptions$categoricalVars
    # numCols <- suppressWarnings(which(apply(matrix(apply(data$covariates, 2, as.numeric),
    #                                                ncol = ncol(data$covariates)),
    #                                         2, function(x) all(!is.na(x)))))
    if (length(numCols) > 0) {
      data$covariatesNum <- data$covariates[, numCols, drop = FALSE]
      mode(data$covariatesNum) <- "numeric"
      data$covariatesNum <- scale(data$covariatesNum)
      data$covariates <- data$covariates[, catCols, drop = FALSE]
    }
  }
  if (is.null(data$covariates) ||
    ncol(data$covariates) == 0 ||
    modelOptions$covariateType == "0") {
    modelOptions$hierarchical <- FALSE
  }
  # falls nur ein Individuum
  if (length(unique(rownames(data$obsvn))) == 1 & modelOptions$hierarchical == TRUE) {
    stop("Hierarchical model not possible if only one individual in data.")
    # modelOptions$modelType == "1"
    # modelOptions$hierarchical <- FALSE
  }

  if (modelOptions$hierarchical & (!is.null(data$covariates) || ncol(data$covariates) == 0) &
    !identical(gsub(" ", "", unique(rownames(data$covariates))), gsub(" ", "", unique(rownames(data$obsvn)))) &
    modelOptions$covariateType != "0") {
    stop("Number of individuals or individual names in covariates table and targets table are different. Please check your data.")
  }
  
  data <- replaceValues(data, modelOptions, valueNames)
  if (modelOptions$hierarchical == TRUE && exists("catCols") && length(catCols) > 0) {
    hierarchical <- interaction(as.data.frame(data$covariates[, catCols, drop = FALSE]), sep = "-")
    hierarchicalLevels <- levels(hierarchical)
    nHierLevels <- length(hierarchicalLevels)
    hierMatch <- match(hierarchical, hierarchicalLevels)
  } else {
    nHierLevels <- 0
    modelOptions$hierarchical <- FALSE
  }
  # Repeated Measures
  data <- repeatedCovariances(data, modelOptions)
  data <- transformRepeatedMeasures(data, modelOptions)
  valueNames$targets <- unique(colnames(data$obsvn))
  if (modelOptions$modelWeights) {
    valueNames$fractions <- unique(colnames(data$source))
  } else {
    valueNames$fractions <- valueNames$targets
  }
  valueNames$sources <- unique(rownames(data$source))

  data$weightOffset <- c(data$weightOffset)
  data$weightOffsetUncert <- c(data$weightOffsetUncert)
  if (is.null(data$weightOffset)) {
    modelOptions$targetOffset <- FALSE
  }
  # Distributions of targets and sources set to NULL for additional terms if no data
  modelOptions <- replaceDistOptions(data, modelOptions)
  # Checks
  data <- checkIndividualNames(data)
  checkData(data)
  checkValueNames(valueNames)
  checkDataNameConsistency(data, modelOptions)
  checkDataDimensions(data, valueNames, modelOptions)
  checkPriors(priors)
  checkUserEstimates(userEstimates[[1]])
  checkModelOptions(modelOptions)

  constants <- list(
    nTargets = nrow(data[["obsvn"]]),
    nSources = length(valueNames[["sources"]]),
    nFractions = length(valueNames[["fractions"]]),
    nProxies = length(valueNames[["targets"]]),
    nHierLevels = nHierLevels
  )

  if (constants$nSources < 2) {
    stop("Model needs at least two different sources. Please check your data.")
  }

  if (!is.null(data$covariatesNum)) {
    constants$numColsNum <- length(numCols)
  }

  if(modelOptions$optimalPrior == TRUE){
    data$sourceDirichPrior <- rep(0.8 / constants[["nSources"]], constants[["nSources"]])
  } else {
    data$sourceDirichPrior <- modelOptions$alphaHyper
  }
  
  if (modelOptions$inflatedBeta != "0") {
    data$infBetaPrior <- c(0.2, 0.8)
  }
  if (modelOptions$hierarchical == TRUE && exists("catCols") && length(catCols) > 0) {
    data$hierMatch <- hierMatch
  }
  modelCode <- createModelCode(priors, userEstimates[[1]], valueNames,
    constants, gsub(" ", "", (rownames(data$obsvn)), fixed = TRUE),
    modelOptions,
    data$covariates,
    data$covariatesNum,
    termsSources = list(data$sourceT1, data$sourceT2, data$sourceT3),
    termsTargets = list(data$obsvnT1, data$obsvnT2, data$obsvnT3)
  )
  # delete non-needed data
  data <- data[sapply(
    1:(length(names(data)) - 1),
    function(x) {
      grepl(
        names(data)[-which(names(data) == "covariates")][x],
        paste(as.character(modelCode), collapse = " ")
      )
    }
  )]
  res <- list(
    data = data,
    priors = priors,
    modelOptions = modelOptions,
    constants = constants,
    userEstimates = userEstimates,
    valueNames = valueNames,
    modelCode = modelCode
  )
  class(res) <- "fruits"

  res
}


# Check elements of class fruits ----

#' Check individual names
#' @description Check if indibividual names are given and assign them if missing
#' @inheritParams fruits
checkIndividualNames <- function(data) {
  individualNames <- rownames(data$obsvn)
  if (any(individualNames == "") || is.null(individualNames)) {
    individualNames[which(individualNames == "")] <-
      paste0("Individual_", (1:nrow(data$obsvn))[which(individualNames == "")])
    rownames(data$obsvn) <- individualNames
  }

  data
}

#' Check data
#' @description Check data for correct class and names
#' @inheritParams fruits
checkData <- function(data) {
  checkClass(data, argName = "data")

  optionalNames <- c(
    "obsvnCov",
    "obsvnT1",
    "obsvnErrorT1",
    "obsvnCovT1",
    "obsvnT2",
    "obsvnErrorT2",
    "obsvnCovT2",
    "obsvnT3",
    "obsvnErrorT3",
    "obsvnCovT3",
    "sourceCov",
    "sourceT1",
    "sourceUncertT1",
    "sourceCovT1",
    "sourceT2",
    "sourceUncertT2",
    "sourceCovT2",
    "sourceT3",
    "sourceUncertT3",
    "sourceCovT3",
    "sourceOffset",
    "sourceOffsetUnc",
    "weightOffset",
    "weightOffsetUncert",
    "weights",
    "weightsUncert",
    "concentration",
    "concentrationUncert",
    "concentrationCov",
    "covariates",
    "covariatesNum"
  )
  checkNames(data[!(names(data) %in% optionalNames)],
    namesExpected = c(
      "obsvn",
      "obsvnError",
      "source",
      "sourceUncert"
    )
  )
}

#' Check if data names are correct
#' @inheritParams checkClass
#' @inheritParams fruits
checkDataNameConsistency <- function(data, modelOptions) {
  if (modelOptions$modelWeights) {
    if (!identical(colnames(data$obsvn), rownames(data$weights))) {
      stop("Proxy names of target and component data do not match. Please check your data")
    }
    if (!identical(colnames(data$weights), colnames(data$source))) {
      stop("Component names of component and source data do not match. Please check your data")
    }
    if (modelOptions$modelConcentrations) {
      if (!identical(colnames(data$weights), colnames(data$concentration))) {
        stop("Component names of component and concentration data do not match. Please check your data")
      }
    }
  } else {
    if (modelOptions$modelConcentrations) {
      if (!identical(colnames(data$obsvn), colnames(data$concentration))) {
        stop("Proxy names of target and concentration data do not match. Please check your data")
      }
    }
  }

  if (modelOptions$modelConcentrations) {
    if (!identical(rownames(data$source), rownames(data$concentration))) {
      stop("Source names of source and concentration data do not match. Please check your data")
    }
  }
}

#' Check if data dimensions are correct
#' @inheritParams checkClass
#' @inheritParams fruits
checkDataDimensions <- function(data, valueNames, modelOptions, argName = NULL) {
  nProxies <- length(valueNames$targets)
  nFractions <- length(valueNames$fractions)
  nSources <- length(valueNames$sources)
  checkDim(data$weights, c(nProxies, nFractions), "Weight matrix")
  checkDim(
    data$weightsUncert,
    c(nProxies, nFractions),
    "Weight uncertainty matrix"
  )
  if (modelOptions$targetOffset == TRUE) {
    checkLength(data$weightOffset, nProxies, "Weight offset vector")
    checkLength(data$weightOffsetUncert, nProxies, "Weight offset error vector")
  }
  if (modelOptions$modelConcentrations == TRUE) {
    if (modelOptions$modelType %in% c("3", "5")) {
      checkDim(data$concentration, c(nSources, nFractions, nrow(data$obsvn)), "Concentration matrix")
      checkDim(data$concentrationUncert, c(nSources, nFractions, nrow(data$obsvn)), "Concentration uncertainty matrix")
    } else {
      checkDim(data$concentration, c(nSources, nFractions), "Concentration matrix")
      checkDim(data$concentrationUncert, c(nSources, nFractions), "Concentration uncertainty matrix")
    }
  }
  if (modelOptions$modelType %in% c("3", "5")) {
    checkDim(data$source, c(nSources, nFractions, nProxies, nrow(data$obsvn)), "Source array")
    checkDim(data$sourceUncert, c(nSources, nFractions, nProxies, nrow(data$obsvn)), "Source uncertainty array")
  } else {
    checkDim(data$source, c(nSources, nFractions, nProxies), "Source array")
    checkDim(data$sourceUncert, c(nSources, nFractions, nProxies), "Source uncertainty array")
  }
  checkLength(data$obsvn[1, ], nProxies, "Target matrix")
}


#' Check value names
#' @description Check value names and its elements for correct class, length and
#' names
#' @inheritParams fruits
checkValueNames <- function(valueNames) {
  namesExpected <- c("targets", "fractions", "sources")

  checkClass(valueNames, argName = "valueNames")
  checkLength(valueNames, 3, argName = "valueNames")
  checkNames(valueNames,
    namesExpected = namesExpected,
    argName = "valueNames"
  )
  invisible(mapply(
    FUN = checkClass,
    x = valueNames,
    argName = paste0("valueNames$", names(valueNames)),
    MoreArgs = list(classesExpected = "character")
  ))
}


#' Check priors
#' @description Check priors for correct class
#' @inheritParams fruits
checkPriors <- function(priors) {
  checkClass(priors, argName = "priors")
}


#' Check user estimates
#' @description Check user estimates for correct class
#' @inheritParams fruits
checkUserEstimates <- function(userEstimates) {
  checkClass(userEstimates, argName = "userEstimates")
}


#' Check model options
#' @description Check model options and its elements for correct class and names
#' @inheritParams fruits
checkModelOptions <- function(modelOptions) {
  checkClass(modelOptions, argName = "modelOptions")
  checkNames(modelOptions,
    argName = "modelOptions",
    namesExpected = c(
      "modelType", "modelWeights", "modelConcentrations",
      "modelWeightsContrained", "modelConcentrationsContrained",
      "minUnc", "targetOffset", "alphaHyper", "optimalPrior", "covariateType",
      "burnin", "iterations", "thinning", "nchains", "hierarchical",
      "includeSourceOffset", "weightsDist", "sourceDist",
      "concentrationDist", "obsvnDist", "inflatedBeta",
      "categoricalVars", "numericVars", "concentrationDistCovRep", "sourceDistCovRep"
    )
  )
}


# Utility functions for checking ----


#' Check if class is in a set of accepted classes
#' @param x any object
#' @param classesExpected character: vector with accepted classes
#' @param argName character: name of argument displayed in error message
#' @examples
#' ReSources:::checkClass(c(1, 2, 3), c("numeric", "integer"))
#' \dontrun{
#' ReSources:::checkClass("This is not a list", argName = "x")
#' }
checkClass <- function(x,
                       classesExpected = "list",
                       argName = NULL) {
  typeActual <- class(x)

  if (!(typeActual %in% classesExpected)) {
    stop(paste0(
      if (!is.null(argName)) paste0(argName, " has "),
      "wrong type: ", typeActual, " instead of ", classesExpected
    ))
  }
}


#' Check if length is correct
#' @inheritParams checkClass
#' @param lengthExpected numeric or integer of length 1: expected length
checkLength <- function(x, lengthExpected, argName = NULL) {
  lengthActual <- length(x)

  if (lengthActual != lengthExpected) {
    stop(paste0(
      if (!is.null(argName)) paste0(argName, " has "),
      "wrong length: ", lengthActual, " instead of ", lengthExpected
    ))
  }
}


#' Check if names are correct
#' @inheritParams checkClass
#' @param namesExpected character: vector of expected names
checkNames <- function(x, namesExpected, argName = NULL) {
  namesExpected <- sort(namesExpected)
  namesActual <- sort(names(x))

  if (any(namesActual != namesExpected)) {
    stop(paste0(
      if (!is.null(argName)) paste0(argName, " has "),
      "wrong names: '",
      paste(namesActual, collapse = ", "),
      "' instead of '",
      paste(namesExpected, collapse = ", "),
      "'"
    ))
  }
}


#' Check if dimensions are correct
#' @inheritParams checkClass
#' @param dimExpected numeric: vector of expected dimensions
checkDim <- function(x, dimExpected, argName = NULL) {
  dimActual <- dim(x)
  if (length(dimActual) != length(dimExpected) | any(dimActual != dimExpected)) {
    stop(paste0(
      if (!is.null(argName)) paste0(argName, " has "),
      "wrong dimensions: '",
      paste(dimActual, collapse = ", "),
      "' instead of '",
      paste(dimExpected, collapse = ", "),
      "'"
    ))
  }
}

# Help functions for constructing the fruits object ----

weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  (sum(w * x^2) * sum.w - sum(w * x)^2) / (sum.w^2 - sum(w^2))
}


combineRepeatedMeasurements <- function(means, sds, type = "means") {
  if (type == "means") {
    measurementNames <- rownames(means)
  }
  if (type == "sds") {
    measurementNames <- rownames(sds)
  }
  if (length(measurementNames) > length(unique(measurementNames))) {
    if (length(dim(means)) == 2) {
      combinedValues <- lapply(unique(measurementNames), function(x) {
        if (sum(x == measurementNames) > 1) {
          sds <- sds + 1E-6
          meansCombined <- matrix(colSums(means[x == measurementNames, , drop = FALSE] /
            sds[x == measurementNames, , drop = FALSE]^2) /
            colSums(1 / sds[x == measurementNames, , drop = FALSE]^2),
          nrow = 1,
          dimnames = list(x, colnames(means))
          )
          sds <- sqrt(matrix(colSums((sds[x == measurementNames, , drop = FALSE]^2) /
            (sds[x == measurementNames, , drop = FALSE]^2)) /
            colSums(1 / sds[x == measurementNames, , drop = FALSE]^2) +
            sapply(1:length(meansCombined), function(y) {
              weighted.var(
                x = means[x == measurementNames, , drop = FALSE][, y],
                w = sds[x == measurementNames, , drop = FALSE][, y]
              )
            }),
          nrow = 1,
          dimnames = list(x, colnames(means))
          ))
          return(list(means = signif(meansCombined, 5), sds = signif(sds, 5)))
        } else {
          return(list(
            means = round(means[x == measurementNames, ], 5),
            sds = round(sds[x == measurementNames, ], 5)
          ))
        }
      })
      newMeans <- do.call("rbind", lapply(combinedValues, function(x) x$means))
      rownames(newMeans) <- unique(measurementNames)
      colnames(newMeans) <- colnames(means)
      newSds <- do.call("rbind", lapply(combinedValues, function(x) x$sds))
      rownames(newSds) <- unique(measurementNames)
      colnames(newSds) <- colnames(means)
    } else {
      combinedValues <- lapply(unique(measurementNames), function(x) {
        if (sum(x == measurementNames) > 1) {
          sds <- sds + 1E-6
          meansCombined <- abind::abind(lapply(1:dim(means)[3], function(k) {
            matrix(colSums(means[x == measurementNames, , k, drop = FALSE] /
              sds[x == measurementNames, , k, drop = FALSE]^2) /
              colSums(1 / sds[x == measurementNames, , k, drop = FALSE]^2),
            nrow = 1,
            dimnames = list(x, colnames(means))
            )
          }), along = 3)

          sds <- abind::abind(lapply(1:dim(means)[3], function(k) {
            sqrt(matrix(colSums((sds[x == measurementNames, , k, drop = FALSE]^2) /
              (sds[x == measurementNames, , k, drop = FALSE]^2)) /
              colSums(1 / sds[x == measurementNames, , k, drop = FALSE]^2) +
              sapply(1:dim(meansCombined)[2], function(y) {
                weighted.var(
                  x = means[x == measurementNames, , k, drop = FALSE][, y, ],
                  w = sds[x == measurementNames, , k, drop = FALSE][, y, ]
                )
              }),
            nrow = 1,
            dimnames = list(x, colnames(means))
            ))
          }), along = 3)
          return(list(means = signif(meansCombined, 5), sds = signif(sds, 5)))
        } else {
          return(list(
            means = round(means[x == measurementNames, , , drop = F], 5),
            sds = round(sds[x == measurementNames, , , drop = F], 5)
          ))
        }
      })
      newMeans <- abind::abind(lapply(combinedValues, function(x) x$means), along = 1)
      rownames(newMeans) <- unique(measurementNames)
      colnames(newMeans) <- colnames(means)
      newSds <- abind::abind(lapply(combinedValues, function(x) x$sds), along = 1)
      rownames(newSds) <- unique(measurementNames)
      colnames(newSds) <- colnames(means)
    }
    if (type == "means") {
      return(newMeans)
    }
    if (type == "sds") {
      return(newSds)
    }
  } else {
    if (type == "means") {
      return(means)
    }
    if (type == "sds") {
      return(sds)
    }
  }
}

transformRepeatedMeasures <- function(data, modelOptions) {
  data2 <- data
  data2$concentration <- combineRepeatedMeasurements(data$concentration,
    data$concentrationUncert,
    type = "means"
  )
  data2$concentrationUncert <- combineRepeatedMeasurements(data$concentration,
    data$concentrationUncert,
    type = "sds"
  )
  data2$obsvn <- combineRepeatedMeasurements(data$obsvn,
    data$obsvnError,
    type = "means"
  )
  data2$obsvnError <- combineRepeatedMeasurements(data$obsvn,
    data$obsvnError,
    type = "sds"
  )

  if (!is.null(unlist(data2$obsvnT1))) {
    data2$obsvnT1 <- combineRepeatedMeasurements(data$obsvnT1, data$obsvnErrorT1, type = "means")
    data2$obsvnErrorT1 <- combineRepeatedMeasurements(data$obsvnT1, data$obsvnErrorT1, type = "sds")
  }
  if (!is.null(unlist(data2$obsvnT2))) {
    data2$obsvnT2 <- combineRepeatedMeasurements(data$obsvnT2, data$obsvnErrorT2, type = "means")
    data2$obsvnErrorT2 <- combineRepeatedMeasurements(data$obsvnT2, data$obsvnErrorT2, type = "sds")
  }
  if (!is.null(unlist(data2$obsvnT3))) {
    data2$obsvnT3 <- combineRepeatedMeasurements(data$obsvnT3, data$obsvnErrorT3, type = "means")
    data2$obsvnErrorT3 <- combineRepeatedMeasurements(data$obsvnT3, data$obsvnErrorT3, type = "sds")
  }


  data2$source <- combineSources(data$source, data$sourceUncert,
    type = "means",
    perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
  )
  data2$sourceUncert <- combineSources(data$source, data$sourceUncert,
    type = "sds",
    perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
  )

  if (modelOptions$includeSourceOffset) {
    data2$sourceOffset <- combineSources(data$sourceOffset, data$sourceOffsetUnc,
      type = "means",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
    data2$sourceOffsetUnc <- combineSources(data$sourceOffset, data$sourceOffsetUnc,
      type = "sds",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
  }
  if (!is.null(unlist(data2$sourceT1))) {
    data2$sourceT1 <- combineSources(data$sourceT1, data$sourceUncertT1,
      type = "means",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
    data2$sourceUncertT1 <- combineSources(data$sourceT1, data$sourceUncertT1,
      type = "sds",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
  }
  if (!is.null(unlist(data2$sourceT2))) {
    data2$sourceT2 <- combineSources(data$sourceT2, data$sourceUncertT2,
      type = "means",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
    data2$sourceUncertT2 <- combineSources(data$sourceT2, data$sourceUncertT2,
      type = "sds",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
  }
  if (!is.null(unlist(data2$sourceT3))) {
    data2$sourceT3 <- combineSources(data$sourceT3, data$sourceUncertT3,
      type = "means",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
    data2$sourceUncertT3 <- combineSources(data$sourceT3, data$sourceUncertT3,
      type = "sds",
      perm = !modelOptions$modelWeights, modelType = modelOptions$modelType
    )
  }
  return(data2)
}

combineSources <- function(means, unc, type = "means", perm = FALSE, modelType = NULL) {
  combinedSources <- lapply(1:length(means), function(j) {
    dataSourceCombined <- lapply(1:length(means[[j]]), function(i) {
      combineRepeatedMeasurements(means[[j]][[i]], unc[[j]][[i]],
        type = type
      )
    })
    names(dataSourceCombined) <- names(means[[j]])
    ret <- abind::abind(dataSourceCombined, along = 3, use.first.dimnames = TRUE)
    if (perm) {
      ret <- aperm(ret, c(1, 3, 2))
    }
    ret
  })
  if ((length(combinedSources) > 1) | modelType %in% c("3", "5")) {
    combinedSources <- abind::abind(combinedSources, along = 4)
  } else {
    combinedSources <- combinedSources[[1]]
  }

  return(combinedSources)
}

replaceSourceTerms <- function(sourceTerm, sourceUncertTerm, modelOptions, valueNames, constant = FALSE) {
  if (is.null(constant) || length(constant) == 0) constant <- FALSE

  if (!is.null(sourceTerm) &&
    !(all(sapply(
      1:length(sourceTerm),
      function(y) all(sapply(sourceTerm[[y]], function(x) all(is.na(x))))
    ))) &&
    !(all(sapply(
      1:length(sourceTerm),
      function(y) all(sapply(sourceTerm[[y]], function(x) all(is.na(x))))
    )))) {
    allColumnNames <- lapply(
      1:length(sourceTerm),
      function(j) {
        unlist(lapply(
          1:length(sourceTerm[[j]]),
          function(x) colnames(sourceTerm[[j]][[x]])
        ))
      }
    )
    allColumnNames <- unique(unlist(allColumnNames))
    
    # order allColumnNames with respect to valueNames$fractions in order to pass the check:
    # "checkDataNameConsistency()"
    allColumnNames <- allColumnNames[order(match(allColumnNames, valueNames$fractions))]
    
    if (!modelOptions$modelWeights) {
      allColumnNames <- valueNames$targets
    }
    for (i in 1:length(sourceTerm)) {
      sourceTerm[[i]][] <- lapply(
        1:length(sourceTerm[[i]]),
        function(x) {
          if (!modelOptions$modelWeights) {
            x <- 1
          }
          if (any(!(allColumnNames %in% colnames(sourceTerm[[i]][[x]])))) {
            newCol <- allColumnNames[which(!(allColumnNames %in% colnames(sourceTerm[[i]][[x]])))]
            newTerm <- cbind(sourceTerm[[i]][[x]], matrix(0,
              ncol = length(newCol),
              nrow = NROW(sourceTerm[[i]][[x]])
            ))
            colnames(newTerm) <- c(colnames(sourceTerm[[i]][[x]]), newCol)
            sourceTerm[[i]][[x]] <- newTerm
          }
          replace(sourceTerm[[i]][[x]], is.na(sourceTerm[[i]][[x]]), 0)
        }
      )

      sourceUncertTerm[[i]][] <- lapply(
        1:length(sourceUncertTerm[[i]]),
        function(x) {
          if (!modelOptions$modelWeights) {
            x <- 1
          }
          if (any(!(allColumnNames %in% colnames(sourceUncertTerm[[i]][[x]])))) {
            newCol <- allColumnNames[which(!(allColumnNames %in% colnames(sourceUncertTerm[[i]][[x]])))]
            newTerm <- cbind(sourceUncertTerm[[i]][[x]], matrix(0,
              ncol = length(newCol),
              nrow = NROW(sourceUncertTerm[[i]][[x]])
            ))
            colnames(newTerm) <- c(colnames(sourceUncertTerm[[i]][[x]]), newCol)
            sourceUncertTerm[[i]][[x]] <- newTerm
          }
          replace(sourceUncertTerm[[i]][[x]], is.na(sourceUncertTerm[[i]][[x]]), 0)
        }
      )
    }
  } else {
    sourceTerm <- NULL
    sourceUncertTerm <- NULL
  }
  return(list(source = sourceTerm, sourceUnc = sourceUncertTerm))
}

replaceOtherTerms <- function(targetTerm, targetUncertTerm, modelOptions, constant = FALSE) {
  if (is.null(constant) || length(constant) == 0) constant <- FALSE

  if (!is.null(targetTerm) && !(all(sapply(targetTerm, function(x) all(is.na(x)))) &&
    all(sapply(targetUncertTerm, function(x) all(is.na(x)))))) {
    targetTerm <- replace(targetTerm, is.na(targetTerm), 0)
    targetUncertTerm <- replace(
      targetUncertTerm, is.na(targetUncertTerm) | constant == TRUE,
      modelOptions$minUnc
    )
  } else {
    targetTerm <- NULL
    targetUncertTerm <- NULL
  }
  return(list(target = targetTerm, targetUnc = targetUncertTerm))
}

replaceCombineCovTarget <- function(CovList, modelOptions, dist = "normal") {
  if (dist == "multivariate-normal" &&
    !is.null(CovList) &&
    !(all(sapply(CovList, function(x) all(is.na(x)))))) {
    CovList <- lapply(CovList, function(x) replace(x, is.na(x), 0))
    CovList <- lapply(CovList, function(x) {
      diag(x)[diag(x) == 0] <- modelOptions$minUnc
      return(x)
    })
    CovList <- abind::abind(CovList, along = 3)
  } else {
    CovList <- NULL
  }
  return(CovList)
}

replaceCombineCovSource <- function(CovList, modelOptions, valueNames, source = TRUE) {
  if (!is.null(CovList) &&
    !(all(sapply(1:length(CovList), function(z) {
      all(sapply(
        CovList[[z]],
        function(x) {
          all(sapply(x, function(y) {
            all(is.na(y))
          }))
        }
      ))
    })))) {
    for (i in 1:length(CovList)) {
      # CovList[[i]] <- lapply(1:length(CovList[[i]]), function(x){
      # lapply(CovList[[i]][[x]], function(y) {
      diag(CovList[[i]])[diag(CovList[[i]]) == 0] <- modelOptions$minUnc
      if (!modelOptions$modelWeights & source == TRUE & ncol(CovList[[i]]) > 1) {
        m <- diag(length(valueNames$fractions) * length(valueNames$targets))
        m[1:length(valueNames$fractions), 1:length(valueNames$fractions)] <- CovList[[i]]
        m[
          (length(valueNames$fractions) + 1):(2 * length(valueNames$fractions)),
          (length(valueNames$fractions) + 1):(2 * length(valueNames$fractions))
        ] <- CovList[[i]]
        CovList[[i]] <- m
      }
      # CovList[[i]]
      # y
      # })
      # })
      # CovList[[i]] <- lapply(CovList[[i]], function(x) abind::abind(x, along = 2))
      # CovList[[i]] <- abind::abind(CovList[[i]], along = 4)
      # CovList[[i]] <- replace(CovList[[i]], is.na(CovList[[i]]), 0)
    }
  } else {
    CovList <- NULL
  }
  if (length(CovList) > 1) {
    CovList <- abind::abind(CovList, along = 3)
  } else {
    CovList <- CovList[[1]]
  }
  return(CovList)
}

replaceDistOptions <- function(data, modelOptions) {
  # source terms
  if (is.null(data$sourceT1)) {
    modelOptions$sourceDist$term1 <- NULL
  }
  if (is.null(data$sourceT2)) {
    modelOptions$sourceDist$term2 <- NULL
  }
  if (is.null(data$sourceT3)) {
    modelOptions$sourceDist$term3 <- NULL
  }
  # target terms
  if (is.null(data$obsvnT1)) {
    modelOptions$obsvnDist$term1 <- NULL
  }
  if (is.null(data$obsvnT2)) {
    modelOptions$obsvnDist$term2 <- NULL
  }
  if (is.null(data$obsvnT3)) {
    modelOptions$obsvnDist$term3 <- NULL
  }
  return(modelOptions)
}

replaceValues <- function(data, modelOptions, valueNames) {
  # Ersetze Weight-Offset-Werte
  weightOffset <- replaceOtherTerms(data$weightOffset, data$weightOffsetUncert, modelOptions)
  data$weightOffset <- weightOffset[[1]]
  data$weightOffsetUncert <- weightOffset[[2]]

  # Ersetze Weight-Werte
  weights <- replaceOtherTerms(
    data$weights, data$weightsUncert, modelOptions,
    modelOptions$weightsDist == "constant"
  )
  data$weights <- weights[[1]]
  data$weightsUncert <- weights[[2]]

  # Ersetze Concentration-Werte
  concentrations <- lapply(
    1:length(data$concentration),
    function(x) {
      replaceOtherTerms(
        data$concentration[[x]],
        data$concentrationUncert[[x]],
        modelOptions, modelOptions$concentrationDist == "constant"
      )
    }
  )

  data$concentration <- lapply(1:length(concentrations), function(x) {
    concentrations[[x]][[1]]
  })

  data$concentrationUncert <- lapply(1:length(concentrations), function(x) {
    concentrations[[x]][[2]]
  })

  if ((length(data$concentration) > 1 | modelOptions$modelType %in% c("3", "5")) & modelOptions$modelConcentrations) {
    if (!all(sapply(data$concentration, function(x) length(x) == length(data$concentration[[1]])))) {
      stop("Dimensions of concentrations tables differ. Please provide concentrations tables with the same number of columns and the same number of rows!")
    }
    data$concentration <- abind::abind(data$concentration, along = 3)
  } else {
    data$concentration <- data$concentration[[1]]
  }

  if ((length(data$concentrationUncert) > 1 | modelOptions$modelType %in% c("3", "5")) & modelOptions$modelConcentrations) {
    data$concentrationUncert <- abind::abind(data$concentrationUncert, along = 3)
  } else {
    data$concentrationUncert <- data$concentrationUncert[[1]]
  }
  data$concentrationCov <- replaceCombineCovSource(data$concentrationCov,
    modelOptions, valueNames,
    source = FALSE
  )

  # Ersetze Source-Term-Werte
  T0 <- replaceSourceTerms(
    data$source, data$sourceUncert, modelOptions, valueNames,
    modelOptions$sourceDist$default == "constant"
  )
  data$source <- T0[[1]]
  data$sourceUncert <- T0[[2]]
  data$sourceCov <- replaceCombineCovSource(data$sourceCov, modelOptions, valueNames)

  if (is.null(data$source)) {
    stop("No valid source values present. Please check your data")
  }

  T1 <- replaceSourceTerms(
    data$sourceT1, data$sourceUncertT1, modelOptions, valueNames,
    modelOptions$sourceDist$term1 == "constant"
  )
  data$sourceT1 <- T1[[1]]
  data$sourceUncertT1 <- T1[[2]]
  data$sourceCovT1 <- replaceCombineCovSource(data$sourceCovT1, modelOptions, valueNames)

  T2 <- replaceSourceTerms(
    data$sourceT2, data$sourceUncertT2, modelOptions, valueNames,
    modelOptions$sourceDist$term2 == "constant"
  )
  data$sourceT2 <- T2[[1]]
  data$sourceUncertT2 <- T2[[2]]
  data$sourceCovT2 <- replaceCombineCovSource(data$sourceCovT2, modelOptions, valueNames)

  T3 <- replaceSourceTerms(
    data$sourceT3, data$sourceUncertT3, modelOptions, valueNames,
    modelOptions$sourceDist$term3 == "constant"
  )
  data$sourceT3 <- T3[[1]]
  data$sourceUncertT3 <- T3[[2]]
  data$sourceCovT3 <- replaceCombineCovSource(data$sourceCovT3, modelOptions, valueNames)

  # Ersetze Source-Offset-Werte

  sourceOffset <- replaceSourceTerms(data$sourceOffset, data$sourceOffsetUnc, modelOptions, valueNames)
  data$sourceOffset <- sourceOffset[[1]]
  data$sourceOffsetUnc <- sourceOffset[[2]]

  # Ersetze Target-Term-Werte
  T0 <- replaceOtherTerms(
    data$obsvn, data$obsvnError, modelOptions,
    modelOptions$obsvnDist$default == "constant"
  )
  data$obsvn <- T0[[1]]
  data$obsvnError <- T0[[2]]
  data$obsvnCov <- replaceCombineCovTarget(data$obsvnCov,
    modelOptions,
    dist = modelOptions$obsvnDist$default
  )

  if (is.null(data$obsvn)) {
    stop("No valid target values present. Please check your data")
  }


  T1 <- replaceOtherTerms(data$obsvnT1, data$obsvnErrorT1, modelOptions)
  data$obsvnT1 <- T1[[1]]
  data$obsvnErrorT1 <- T1[[2]]
  data$obsvnCovT1 <- replaceCombineCovTarget(data$obsvnCovT1, modelOptions,
    dist = modelOptions$obsvnDist$term1
  )

  T2 <- replaceOtherTerms(data$obsvnT2, data$obsvnErrorT2, modelOptions)
  data$obsvnT2 <- T2[[1]]
  data$obsvnErrorT2 <- T2[[2]]
  data$obsvnCovT2 <- replaceCombineCovTarget(data$obsvnCovT2, modelOptions,
    dist = modelOptions$obsvnDist$term2
  )

  T3 <- replaceOtherTerms(data$obsvnT3, data$obsvnErrorT3, modelOptions)
  data$obsvnT3 <- T3[[1]]
  data$obsvnErrorT3 <- T3[[2]]
  data$obsvnCovT3 <- replaceCombineCovTarget(data$obsvnCovT3, modelOptions,
    dist = modelOptions$obsvnDist$term3
  )


  return(data)
}

repCov <- function(means, unc) {
  covMatrices <- lapply(unique(rownames(means)), function(x) {
    meansT <- means[rownames(means) == x, , drop = F]
    uncT <- unc[rownames(means) == x, , drop = F]
    if (NROW(meansT) == 1) {
      cMatrix <- matrix(0, ncol = NCOL(meansT), nrow = NCOL(meansT))
    } else {
      cMatrix <- cov(meansT) + diag(colSums(uncT^2) / NROW(uncT))
    }
    return(list(cMatrix, NROW(meansT) - 1))
  })
  weights <- lapply(1:length(covMatrices), function(y) covMatrices[[y]][[2]])
  if (all(weights == 0)) {
    return(diag(colMeans(unc^2)))
  }
  res <- Reduce(`+`, Map(
    `*`, lapply(
      1:length(covMatrices),
      function(y) covMatrices[[y]][[1]]
    ),
    weights
  ))
  return(res)
}

repeatedCovariances <- function(data, modelOptions) {
  # concentrations
  if (modelOptions$modelConcentrations & !modelOptions$concentrationDistCovRep) {
    if (length(dim(data$concentration)) > 2) {
      covs <- lapply(1:dim(data$concentration)[3], function(x) {
        means <- data$concentration[, , x]
        unc <- data$concentrationUncert[, , x]
        repCov(means, unc)
      })
      for (i in 1:dim(data$concentration)[3]) {
        data$concentrationCov[, , i] <- covs[[i]]
      }
    } else {
      means <- data$concentration
      unc <- data$concentrationUncert
      data$concentrationCov <- repCov(means, unc)
    }
  }
  # sources
  if (!modelOptions$sourceDistCovRep[[1]]) {
    if (is.list(data$source)) {
      covs <- lapply(1:length(data$source), function(x) {
        means <- do.call("cbind", data$source[[x]])
        unc <- do.call("cbind", data$sourceUncert[[x]])
        repCov(means, unc)
      })
    } else {
      covs <- lapply(1:length(data$source), function(x) {
        means <- do.call("cbind", data$source[[x]])
        unc <- do.call("cbind", data$sourceUncert[[x]])
        repCov(means, unc)
      })
    }
    if (is.list(data$source)) {
      for (i in 1:length(data$source)) {
        data$sourceCov <- covs[[i]]
      }
    } else {
      for (i in 1:length(data$source)) {
        data$sourceCov[, , i] <- covs[[i]]
      }
    }
  }

  if (!is.null(data$sourceT1) && !modelOptions$sourceDistCovRep[[2]]) {
    if (is.list(data$sourceT1)) {
      covs <- lapply(1:length(data$sourceT1), function(x) {
        means <- do.call("cbind", data$sourceT1[[x]])
        unc <- do.call("cbind", data$sourceUncertT1[[x]])
        repCov(means, unc)
      })
    } else {
      covs <- lapply(1:length(data$sourceT1), function(x) {
        means <- do.call("cbind", data$sourceT1[[x]])
        unc <- do.call("cbind", data$sourceUncertT1[[x]])
        repCov(means, unc)
      })
    }
    if (is.list(data$sourceT1)) {
      for (i in 1:length(data$sourceT1)) {
        data$sourceCovT1 <- covs[[i]]
      }
    } else {
      for (i in 1:length(data$sourceT1)) {
        data$sourceCovT1[, , i] <- covs[[i]]
      }
    }
  }

  if (!is.null(data$sourceT2) && !modelOptions$sourceDistCovRep[[3]]) {
    if (is.list(data$sourceT2)) {
      covs <- lapply(1:length(data$sourceT2), function(x) {
        means <- do.call("cbind", data$sourceT2[[x]])
        unc <- do.call("cbind", data$sourceUncertT2[[x]])
        repCov(means, unc)
      })
    } else {
      covs <- lapply(1:length(data$sourceT2), function(x) {
        means <- do.call("cbind", data$sourceT2[[x]])
        unc <- do.call("cbind", data$sourceUncertT2[[x]])
        repCov(means, unc)
      })
    }
    if (is.list(data$sourceT2)) {
      for (i in 1:length(data$sourceT2)) {
        data$sourceCovT2 <- covs[[i]]
      }
    } else {
      for (i in 1:length(data$sourceT2)) {
        data$sourceCovT2[, , i] <- covs[[i]]
      }
    }
  }

  if (!is.null(data$sourceT3) && !modelOptions$sourceDistCovRep[[4]]) {
    if (is.list(data$sourceT3)) {
      covs <- lapply(1:length(data$sourceT3), function(x) {
        means <- do.call("cbind", data$sourceT3[[x]])
        unc <- do.call("cbind", data$sourceUncertT3[[x]])
        repCov(means, unc)
      })
    } else {
      covs <- lapply(1:length(data$sourceT3), function(x) {
        means <- do.call("cbind", data$sourceT3[[x]])
        unc <- do.call("cbind", data$sourceUncertT3[[x]])
        repCov(means, unc)
      })
    }
    if (is.list(data$sourceT3)) {
      for (i in 1:length(data$sourceT3)) {
        data$sourceCovT3 <- covs[[i]]
      }
    } else {
      for (i in 1:length(data$sourceT3)) {
        data$sourceCovT3[, , i] <- covs[[i]]
      }
    }
  }

  return(data)
}
