#' Modeling ReSources
#' @description Run model on ReSources object
#' @param fruitsObj object of class fruits: input data
#' @param progress boolean: show progress in shiny
#' @param onlySim boolean: only simulate from prior
#' @param userDefinedAlphas list of matrices: for simulation only: food source intakes values
#' @param seqSim numeric grid of mixture steps
#' @param simSourceNames names of sources to simulate
#' @param onlyShowNimbleInput boolean: run the model if FALSE, only show input for nimbleModel() if
#' TRUE
#' @export
compileRunModel <- function(fruitsObj, progress = FALSE, onlySim = FALSE,
                            userDefinedAlphas = NULL, seqSim = 0.2, simSourceNames = NULL,
                            onlyShowNimbleInput = FALSE) {
  if (!inherits(fruitsObj, "fruits")) {
    stop('fruitsObj must be class "fruits"')
  }
  if (progress) setProgress(message = "Create model", value = 0.05)
  nimbleOptions(
    showCompilerOutput = FALSE,
    verboseErrors = FALSE,
    checkNimbleFunction = FALSE
  )
  # nimble bug mitigation
  if (fruitsObj$constants$nFractions == 1) {
    fruitsObj$data <-
      bugfixFraction1(fruitsObj$data, fruitsObj$constants)
    fruitsObj$constants$nFractions <- 2
  }
  if (any(sapply(1:length(fruitsObj$modelOptions$obsvnDist), function(x) {
    (any(list(fruitsObj$data$obsvn, fruitsObj$data$obsvnT1, fruitsObj$data$obsvnT2, fruitsObj$data$obsvnT3)[[x]] <= 0) & fruitsObj$modelOptions$obsvnDist[[x]] == "log-normal")
  }))) {
    stop("need positive mean in targets for log normal distribution")
  }
  if ((any(sapply(1:length(fruitsObj$modelOptions$sourceDist), function(x) {
    (any(list(fruitsObj$data$source, fruitsObj$data$sourceT1, fruitsObj$data$sourceT2, fruitsObj$data$sourceT3)[[x]] <= 0) & fruitsObj$modelOptions$sourceDist[[x]] == "log-normal")
  })))) {
    stop("need positive means in sources for log normal distribution")
  }
  if (any(fruitsObj$data$concentration <= 0) & any(unlist(fruitsObj$modelOptions$concentrationDist) == "log-normal")) {
    stop("need positive means in concentrations for log normal distribution")
  }
  if (any(fruitsObj$data$weights <= 0) & any(unlist(fruitsObj$modelOptions$weightsDist) == "log-normal")) {
    stop("need positive means in components for log normal distribution")
  }
  if (onlySim == TRUE) {
    if (length(simSourceNames) < 2) {
      stop("Number of sources to simulate must be larger than 1")
    }

    simGrid <- expand.grid(lapply(1:length(simSourceNames), function(x) seq(0, 1, by = seqSim)))
    simGrid <- as.matrix(unique(simGrid[round(rowSums(simGrid), 2) == 1, ]))
    simGrid <- cbind(simGrid, matrix(0, nrow = nrow(simGrid), ncol = length(fruitsObj$valueNames$sources) - ncol(simGrid)))
    simGrid <- simGrid[do.call(order, split(simGrid, rep(1:ncol(simGrid), each = nrow(simGrid)))), ]
    inc <- fruitsObj$valueNames$sources[match(simSourceNames, fruitsObj$valueNames$sources)]
    nonInc <- fruitsObj$valueNames$sources[-match(simSourceNames, fruitsObj$valueNames$sources)]
    colnames(simGrid) <- c(inc, nonInc)
    simGrid <- simGrid[, match(fruitsObj$valueNames$sources, colnames(simGrid))]

    fruitsObj$constants$nTargets <- nrow(simGrid)
    if (fruitsObj$modelOptions$modelType %in% c("3", "5")) {
      if (fruitsObj$modelOptions$modelConcentrations) {
        fruitsObj$data$concentration <- abind(lapply(1:fruitsObj$constants$nTargets, function(x) fruitsObj$data$concentration[, , 1]), along = 3)
        fruitsObj$data$concentrationUncert <- abind(lapply(1:fruitsObj$constants$nTargets, function(x) fruitsObj$data$concentrationUncert[, , 1]), along = 3)
        if (fruitsObj$modelOptions$concentrationDist == "multivariate-normal") {
          fruitsObj$data$concentrationCov <- abind(lapply(1:fruitsObj$constants$nTargets, function(x) fruitsObj$data$concentrationCov[, , 1]), along = 3)
        }
      }
      fruitsObj$data$source <- abind(lapply(1:fruitsObj$constants$nTargets, function(x) fruitsObj$data$source[, , , 1]), along = 4)
      fruitsObj$data$sourceUncert <- abind(lapply(1:fruitsObj$constants$nTargets, function(x) fruitsObj$data$sourceUncert[, , , 1]), along = 4)

      if (fruitsObj$modelOptions$sourceDist == "multivariate-normal") {
        fruitsObj$data$sourceCov <- abind(lapply(1:fruitsObj$constants$nTargets, function(x) fruitsObj$data$sourceCov[, , 1]), along = 3)
      }
    }
    fruitsObj$data$obsvn <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnError <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnT1 <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnErrorT1 <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnT2 <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnErrorT2 <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnT3 <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnErrorT3 <- matrix(0, ncol = fruitsObj$constants$nSources, nrow = nrow(simGrid))
    fruitsObj$data$obsvnCov <- matrix(0, ncol = nrow(simGrid), nrow = nrow(simGrid))
    fruitsObj$data$obsvnCovT1 <- matrix(0, ncol = nrow(simGrid), nrow = nrow(simGrid))
    fruitsObj$data$obsvnCovT2 <- matrix(0, ncol = nrow(simGrid), nrow = nrow(simGrid))
    fruitsObj$data$obsvnCovT3 <- matrix(0, ncol = nrow(simGrid), nrow = nrow(simGrid))
    fruitsObj$data$hierMatch <- rep(0, nrow(simGrid))
  }
  
  if (onlyShowNimbleInput) {
    return(fruitsObj)
  }
  
  model <- try(
    {
      nimbleModel(
        code = fruitsObj$modelCode,
        name = "FRUITS",
        constants = fruitsObj$constants,
        data = fruitsObj$data[!(names(fruitsObj$data) %in% c("covariates"))]
      )
    },
    silent = TRUE
  )
  if (inherits(model, "try-error")) {
    stop(paste0(
      "nimble::nimbleModel(): Model building failed\n",
      as.character(model)
    ))
  }
  
  if (onlySim == TRUE) {
    if (progress) setProgress(message = "Compile Model", value = 0.3)
    model <- compileNimble(model)
    if (progress) setProgress(message = "Simulate Sources", value = 0.7)
    simSources <- getSimSources(model, fruitsObj, userDefinedAlphas, simGrid = simGrid, simSourceNames = simSourceNames)
    return(list(simSources = simSources, simSourceNames = simSourceNames))
  }

  if (progress) setProgress(message = "Create MCMC algorithm", value = 0.2)
  userEstParameters <- c()
  if (length(fruitsObj$userEstimates[[1]]) > 0) {
    if (fruitsObj$modelOptions$modelType != "1") {
      userEstParameters <- lapply(
        1:length(fruitsObj$userEstimates[[1]]),
        function(x) {
          gsub(" ", "", paste0(
            strsplit(
              fruitsObj$userEstimates[[1]][[x]], "="
            )[[1]][1], "_",
            rownames(fruitsObj$data$obsvn)
          ))
        }
      ) %>%
        unlist() %>%
        na.omit()
      userEstParametersRaw <- lapply(
        1:length(fruitsObj$userEstimates[[1]]),
        function(x) {
          gsub(" ", "",
            strsplit(
              fruitsObj$userEstimates[[1]][[x]], "="
            )[[1]][1]
          )
        }
      ) %>% unlist() %>% na.omit()
      
      userEstParameters <- c(userEstParametersRaw, userEstParameters)[which(c(userEstParametersRaw, userEstParameters) %in% model$getVarNames())]
    } else {
      userEstParameters <- lapply(
        1:length(fruitsObj$userEstimates[[1]]),
        function(x) {
          gsub(" ", "", paste0(strsplit(
            fruitsObj$userEstimates[[1]][[x]], "="
          )[[1]][1], "_All"))
        }
      ) %>%
        unlist() %>%
        na.omit()
    }
  }
  if (length(userEstParameters[[1]]) == 0) {
    userEstParameters <- NULL
  }
  conf <- configureMCMC(model,
    monitors2 = userEstParameters,
    thin = fruitsObj$modelOptions$thinning,
    thin2 = fruitsObj$modelOptions$thinning,
    enableWAIC = TRUE
  )
  conf$monitors <- c(conf$monitors, "alpha_", "I_", "mu", "beta_", "theta_")
  conf$monitors <- conf$monitors[!(conf$monitors %in% "SmuS_")]
  conf$monitors2 <- conf$monitors2[!(conf$monitors2 %in% "SmuS_")]
  conf$monitors <- conf$monitors[!(conf$monitors %in% c("alphaRAW_", "alphaRAW2_", "infBPrior"))]
  if (fruitsObj$modelOptions$hierarchical) {
    conf$monitors <- c(conf$monitors, "alpha_", "I_", "mu", "beta_", "theta_")
  }
  if(fruitsObj$modelOptions$inflatedBeta == "1"){
    conf$monitors <- c(conf$monitors, "a", "alphaRAW_")
  }

  FRUITSMCMC <- buildMCMC(conf)
  if (progress) setProgress(message = "Compile model", value = 0.4)
  CFRUITS <- compileNimble(model)

  if (progress) setProgress(message = "Compile MCMC algorithm", value = 0.6)
  FRUITSMCMC <- compileNimble(FRUITSMCMC, project = model)

  if (progress) setProgress(message = "MCMC sampling", value = 0.75)
  samples <- runMCMC(FRUITSMCMC,
    niter = fruitsObj$modelOptions$iterations +
      fruitsObj$modelOptions$burnin,
    nburnin = fruitsObj$modelOptions$burnin,
    thin = fruitsObj$modelOptions$thinning,
    nchains = fruitsObj$modelOptions$nchains
  )
  wAIC <- calculateWAIC(FRUITSMCMC)$WAIC
  
  if(fruitsObj$modelOptions$inflatedBeta == "1"){
    samples$samples <- samples$samples[, !grepl(("alphaRAW_\\["), colnames(samples$samples))]
    samples$samples <- samples$samples[, !grepl(("a\\["), colnames(samples$samples))]
    
    }
  
  
  if (fruitsObj$modelOptions$nchains > 1 & length(userEstParameters) > 0) {
    parameters <- do.call("rbind", samples$samples)
    userEstimateSamples <- do.call("rbind", samples$samples2)
    userEstimateSamples <- userEstimateSamples[, userEstParameters, drop = FALSE]
  }
  if (fruitsObj$modelOptions$nchains == 1 & length(userEstParameters) > 0) {
    parameters <- samples$samples
    userEstimateSamples <- samples$samples2
    userEstimateSamples <- userEstimateSamples[, userEstParameters, drop = FALSE]
  }
  if (fruitsObj$modelOptions$nchains > 1 & length(userEstParameters) == 0) {
    parameters <- do.call("rbind", samples$samples)
    userEstimateSamples <- c()
  }
  if (fruitsObj$modelOptions$nchains == 1 & length(userEstParameters) == 0) {
    parameters <- samples$samples
    userEstimateSamples <- c()
  }

  if (fruitsObj$modelOptions$modelType != "1") {
    indNames <- rownames(fruitsObj$data$obsvn)
  } else {
    indNames <- NULL
  }
  userEstimateSamples <- normalizeUserEstimates(
    userEstimateSamples,
    fruitsObj$userEstimates[[2]],
    indNames
  )
  if(is.null(fruitsObj$data$obsvnError)){
    obsvnSds <- sqrt(t(sapply(1:dim(fruitsObj$data$obsvnCov)[3], function(x) diag(fruitsObj$data$obsvnCov[,,x]))))
  } else {
    obsvnSds <- fruitsObj$data$obsvnError
  }
  
  pValue <- lapply(1:100, function(z) computePPValues(parameters, fruitsObj$data$obsvn, obsvnSds))
  pValue <- preparePValue(pValue)
  BIC <- getBIC(parameters, fruitsObj$data$obsvn, obsvnSds)
  return(list(
    parameters = parameters, userEstimateSamples = userEstimateSamples, wAIC = wAIC,
    pValue = pValue, BIC = BIC
  ))
}

normalizeUserEstimates <- function(userEstimateSamples, userEstimatesGroups, indNames) {
  if (NROW(userEstimateSamples) > 0 & length(userEstimatesGroups) > 0) {
    userEstimatesGroups <- userEstimatesGroups[sapply(
      1:length(userEstimatesGroups),
      function(x) {
        !is.null(userEstimatesGroups[[x]]$name) &&
          !is.null(userEstimatesGroups[[x]]$estimates)
      }
    )]
    estNames <- colnames(userEstimateSamples)
    estNames <- unlist(lapply(estNames, function(x) strsplit(x, "_")[[1]][1]))
    groupNames <- lapply(userEstimatesGroups, function(x) x$name)
    namesUserEstGroup <- lapply(userEstimatesGroups, function(x) x$estimates)

    names <- lapply(namesUserEstGroup, function(x) estNames[estNames %in% x])
    namesNonGroup <- estNames[!(estNames %in% unlist(names))]
    estNonGroup <- userEstimateSamples[, which(estNames %in% namesNonGroup), drop = FALSE]

    matchedNameCols <- lapply(names, function(x) which(estNames %in% x))
    userEstimateSamples <- lapply(1:length(groupNames), function(x) {
      est <- userEstimateSamples[, matchedNameCols[[x]], drop = FALSE]
      colnames(est) <- paste0(groupNames[[x]], ".", colnames(est))
      if (userEstimatesGroups[[x]]$normalize == TRUE & !is.null(indNames)) {
        names_ind <- rep(1:length(indNames),
                         times = length(namesUserEstGroup[[x]]))
        if(ncol(est) == length(names_ind)){
          splits <- split(1:ncol(est), names_ind)
          estNew <- do.call("cbind", lapply(
            splits,
            function(x) {
              return(est[, x] / rowSums(est[, x]))
            }
          ))
          est <- estNew[, colnames(est)]
        } else {
          return(est / rowSums(est))
        }
      } else {
        if(userEstimatesGroups[[x]]$normalize == TRUE){
          return(est / rowSums(est))
        } else {
          est <- est
        }
      }
      est
    })
    cbind(do.call("cbind", userEstimateSamples), estNonGroup)
  } else {
    return(userEstimateSamples)
  }
}

computePPValues <- function(parameters, dataMeans, dataSds) {
  densitiesLog <- lapply(1:nrow(dataMeans), function(y) {
    yobs <- sapply(1:ncol(dataMeans), function(x) rnorm(nrow(parameters), dataMeans[y, x], dataSds[y, x]))
    yModel <- sapply(1:ncol(dataMeans), function(x) rnorm(nrow(parameters), parameters[1:nrow(parameters), paste0("mu[", y, ", ", x, "]")], dataSds[y, x]))
    dObs <- sapply(1:ncol(dataMeans), function(x) dnorm(yobs[, x], parameters[1:nrow(parameters), paste0("mu[", y, ", ", x, "]")], dataSds[y, x])) %>%
      log() %>%
      rowSums()
    dModel <- sapply(1:ncol(dataMeans), function(x) dnorm(yModel[, x], parameters[1:nrow(parameters), paste0("mu[", y, ", ", x, "]")], dataSds[y, x])) %>%
      log() %>%
      rowSums()
    list(dObs, dModel)
  })

  densTotalObs <- Reduce("+", lapply(densitiesLog, function(x) {
    x[[1]]
  }))
  densTotalModel <- Reduce("+", lapply(densitiesLog, function(x) {
    x[[2]]
  }))

  pAll <- sum(densTotalObs < densTotalModel) / nrow(parameters)
  pIndivid <- lapply(1:nrow(dataMeans), function(x) sum(densitiesLog[[x]][[1]] < densitiesLog[[x]][[2]]) / nrow(parameters))
  names(pIndivid) <- rownames(dataMeans)
  list(pAll = pAll, pIndivid = pIndivid)
}

preparePValue <- function(pValues) {
  mean <- mean(sapply(1:length(pValues), function(x) pValues[[x]][[1]]))
  names(mean) <- "All individuals"
  if (length(pValues[[1]][[2]]) > 1) {
    meansInd <- rowMeans(sapply(1:length(pValues), function(x) unlist(pValues[[x]][[2]])))
    data.frame(pValue = c(mean, meansInd))
  } else {
    data.frame(pValue = c(mean))
  }
}

bugfixFraction1 <- function(data, constant) {
  if (length(dim(data$source)) == 3) {
    data$source <- abind(data$source,
      along = 2,
      array(0, dim = c(
        constant$nSources, 1, constant$nProxies
      ))
    )

    data$sourceUncert <- abind(data$sourceUncert,
      along = 2,
      array(0, dim = c(
        constant$nSources, 1, constant$nProxies
      ))
    )
    if (!is.null(data$sourceOffset)) {
      data$sourceOffset <- abind(data$sourceOffset,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )

      data$sourceOffsetUnc <- abind(data$sourceOffsetUnc,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )
    }
    if (!is.null(data$sourceT1)) {
      data$sourceT1 <- abind(data$sourceT1,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )

      data$sourceUncertT1 <- abind(data$sourceUncertT1,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )
    }
    if (!is.null(data$sourceT2)) {
      data$sourceT2 <- abind(data$sourceT2,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )

      data$sourceUncertT2 <- abind(data$sourceUncertT2,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )
    }
    if (!is.null(data$sourceT3)) {
      data$sourceT3 <- abind(data$sourceT3,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )

      data$sourceUncertT3 <- abind(data$sourceUncertT3,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies
        ))
      )
    }
  } else {
    data$source <- abind(data$source,
      along = 2,
      array(0, dim = c(
        constant$nSources, 1, constant$nProxies, dim(data$source)[4]
      ))
    )

    data$sourceUncert <- abind(data$sourceUncert,
      along = 2,
      array(0, dim = c(
        constant$nSources, 1, constant$nProxies, dim(data$source)[4]
      ))
    )
    if (!is.null(data$sourceOffset)) {
      data$sourceOffset <- abind(data$sourceOffset,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )

      data$sourceOffsetUnc <- abind(data$sourceOffsetUnc,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )
    }

    if (!is.null(data$sourceT1)) {
      data$sourceT1 <- abind(data$sourceT1,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )

      data$sourceUncertT1 <- abind(data$sourceUncertT1,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )
    }
    if (!is.null(data$sourceT2)) {
      data$sourceT2 <- abind(data$sourceT2,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )

      data$sourceUncertT2 <- abind(data$sourceUncertT2,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )
    }
    if (!is.null(data$sourceT3)) {
      data$sourceT3 <- abind(data$sourceT3,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )

      data$sourceUncertT3 <- abind(data$sourceUncertT3,
        along = 2,
        array(0, dim = c(
          constant$nSources, 1, constant$nProxies, dim(data$source)[4]
        ))
      )
    }
  }

  if(length(dim(data$concentration)) == 3){
    data$concentration <-
      abind(data$concentration, matrix(0, nrow = constant$nSources, ncol = constant$nTargets), along = 2)
    data$concentrationUncert <-
      abind(data$concentrationUncert, matrix(1E-6, nrow = constant$nSources, ncol = constant$nTargets), along = 2)
  } else {
    data$concentration <-
      cbind(data$concentration, rep(0, constant$nSources))
    data$concentrationUncert <-
      cbind(data$concentrationUncert, rep(1E-6, constant$nSources))
  }
  
  
  data$weights <- cbind(data$weights, rep(0, constant$nProxies))
  data$weightsUncert <-
    cbind(data$weightsUncert, rep(0, constant$nProxies))


  return(data)
}

getBIC <- function(parameters, obs, obsvar) {
  allsamples <- matrix(colMeans(parameters[, grepl("mu\\[", colnames(parameters)), drop = FALSE]), ncol = NCOL(obs), nrow = NROW(obs))
  logLik <- sum(log(dnorm(as.vector(allsamples), mean = as.vector(obs), sd = as.vector(obsvar))))
  BIC <- -2 * logLik + ncol(parameters) * log(length(as.vector(obs)))
  BIC
}
