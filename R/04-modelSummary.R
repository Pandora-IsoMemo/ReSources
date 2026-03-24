#' Analyse MCMC results
#'
#' @description Get results statistics from ReSources  model object
#'
#' @param parameters parameters object of ReSources model object
#' @param userEstimates userEstimateSamples object of ReSources model object
#' @param fruitsObj object of class fruits: input data
#' @param statistics character vector only if agg = TRUE, compute aggregate statistics,
#' use R functions such as mean or sd
#' @param DT boolean export as data table
#' @param agg boolean aggregate data by statistcs
#' @param bins boolean quantile bins 0-100\%
#'
#' @export
getResultStatistics <- function(parameters, userEstimates, fruitsObj,
                                statistics = NULL, DT = TRUE, agg = TRUE, bins = FALSE) {
  # pay attention to alignment with ui-input-objects
  statisticsNames <- transformUIStatNames(statistics)$statisticsNames
  statisticsFunctions <- transformUIStatNames(statistics)$statisticsReturn
  statisticsNames <- c("mean", "sd", #"lower 95%", "upper 95%",
                       statisticsNames)
  statisticsFunctions <- c(
    #transformStatNames(
    c(
      "mean", "sd" #, "lower 95%", "upper 95%"
    ),
    #),
    statisticsFunctions
  )
  if (bins) {
    statisticsNames <- c(statisticsNames, paste("bin", seq(0, 1, by = 0.01), sep = "_"))
    statisticsFunctions <- c(
      statisticsFunctions,
      sapply(
        seq(0, 1, by = 0.01),
        function(x) {
          paste0("function(y) quantile(y,", x, ", na.rm = TRUE)")
        }
      )
    )
  }

  renamedChains <- translateParameters(parameters, fruitsObj)

  if (NROW(userEstimates) > 0) {
    renamedChains$userEstimates <- userEstimates
  }
  if (agg == FALSE) {
    resultMatrix <- do.call("rbind", lapply(1:length(renamedChains), function(x) {
      if (NCOL(renamedChains[[x]]) == 0) {
        return(NULL)
      } else {
        toDel <- colMeans(renamedChains[[x]]) == 0 & (apply(renamedChains[[x]], 2, sd) == 0)
        resultMatrix <- data.frame(
          Parameter_Type = names(renamedChains[x]),
          Parameter_Name = rep(colnames(renamedChains[[x]]), each = nrow(renamedChains[[x]])),
          estimate = as.vector(renamedChains[[x]])
        )
        colnames(resultMatrix) <- c("Group", "Estimate", "estimate")
        resultMatrix$toDel <- toDel
        if (fruitsObj$modelOptions$modelType != "1" &
          (names(renamedChains))[x] %in% c(
            "Source contributions", "Component contributions",
            "Source contributions by proxy", "Target proxies",
            "userEstimates"
          ) |
          (fruitsObj$modelOptions$modelType %in% c("4", "5")) &
            (names(renamedChains))[x] %in% c("Weights", "Concentrations", "Target-to-source offsets") |
          (fruitsObj$modelOptions$modelType %in% c("3")) &
            (names(renamedChains))[x] %in% c("Concentrations")) {
          repInd <- rep(
            rep(1:length(rownames(fruitsObj$data$obsvn)),
              each = nrow(renamedChains[[x]])
            ),
            length(colnames(renamedChains[[x]])) / length(rownames(fruitsObj$data$obsvn))
          )
          if((names(renamedChains))[x] == "userEstimates"){

            indices <- unlist(lapply(
              resultMatrix$Estimate,
              function(x) paste0(strsplit(x, split = "_")[[1]][-1], collapse = "_")
            ))
            resultMatrix$Estimate <- unlist(lapply(
              resultMatrix$Estimate,
              function(x) strsplit(x, split = "_")[[1]][1]
            ))
            repInd <- match(indices, rownames(fruitsObj$data$obsvn))
          }
            resultMatrix$Target <- rownames(fruitsObj$data$obsvn)[repInd]
            resultMatrix$Target[is.na(resultMatrix$Target)] <- "all"
            
            if (NROW(fruitsObj$data$covariates) > 0 & NCOL(fruitsObj$data$covariates) > 0 &
                fruitsObj$modelOptions$hierarchical == TRUE) {
              resultMatrix <- cbind(resultMatrix, fruitsObj$data$covariates[repInd, , drop = FALSE])
              
              if (NCOL(fruitsObj$data$covariates) > 1) {
                resultMatrix$covariateInteraction <- interaction(split(
                  t(fruitsObj$data$covariates),
                  colnames(fruitsObj$data$covariates)
                ),
                drop = FALSE
                )[repInd]
              }
            }
        } else {
          resultMatrix$Target <- "all"
          if (NROW(fruitsObj$data$covariates) > 0 & NCOL(fruitsObj$data$covariates) > 0 &
            fruitsObj$modelOptions$hierarchical == TRUE) {
            covMatrix <- matrix(
              data = rep("all", ncol(fruitsObj$data$covariates)),
              ncol = ncol(fruitsObj$data$covariates),
              dimnames = list(1, colnames(fruitsObj$data$covariates))
            )
            resultMatrix <- cbind(covMatrix[rep(1, nrow(resultMatrix)), , drop = FALSE], resultMatrix)
            if (NCOL(fruitsObj$data$covariates) > 1) {
              resultMatrix$covariateInteraction <- "all"
            }
          }
        }
        resultMatrix
      }
    }))
    resultMatrix <- resultMatrix[resultMatrix$toDel == FALSE, ]
  } else {
    resultMatrix <- do.call("rbind", lapply(1:length(renamedChains), function(x) {
      extractResultMatrixOfChain(x, 
                                 renamedChains = renamedChains, 
                                 statisticsNames = statisticsNames, 
                                 statisticsFunctions = statisticsFunctions,
                                 fruitsObj = fruitsObj)
    }))

    resultMatrix <- resultMatrix[!(resultMatrix$sd == 0 & resultMatrix$mean == 0), ]
  }
  # split source by proxy
  resultMatrix$`Group` <- as.character(resultMatrix$`Group`)
  newResultSources <- resultMatrix[resultMatrix$`Group` == "Source contributions by proxy", ]
  if (nrow(newResultSources) > 0) {
    newResultSources$`Group` <- as.character(newResultSources$`Group`)
    newResultSources <- do.call(rbind, lapply(fruitsObj$valueNames$targets, function(x) {
      newResultSources[grep(paste0("\\[", x, "\\]"), newResultSources$Estimate), ]$`Group` <- paste("Source contributions", x)
      newResultSources[grep(paste0("\\[", x, "\\]"), newResultSources$Estimate), ]
    }))
    resultMatrix[resultMatrix$`Group` == "Source contributions by proxy", ] <- newResultSources
  }
  # split user estimate by proxy
  newResultUsers <- resultMatrix[resultMatrix$`Group` == "userEstimates", ]
  if (nrow(newResultUsers) > 0) {
    newResultUsers$`Group` <- as.character(newResultUsers$`Group`)
    if (length(fruitsObj$userEstimates[[2]]) > 0) {
      userEstimateNames <- sapply(fruitsObj$userEstimates[[2]], function(x) x$name)
      userEstimateNames <- userEstimateNames[userEstimateNames != ""]
      newResultUsers <- do.call(rbind, lapply(userEstimateNames, function(x) {
        newResultUsers[splitLast(newResultUsers$Estimate) == x, ]$`Group` <- paste("User estimate", x)
        newResultUsers[splitLast(newResultUsers$Estimate) == x, ]
      }))
      resultMatrix <- resultMatrix[resultMatrix$`Group` != "userEstimates", ]
      resultMatrix <- rbind(resultMatrix, newResultUsers)
    }
  }
  resultMatrix$`Group` <- as.factor(resultMatrix$`Group`)

  resultMatrix$Estimate <- gsub("\\]\\[", ".", resultMatrix$Estimate)
  resultMatrix$Estimate <- gsub("\\[", "", resultMatrix$Estimate)
  resultMatrix$Estimate <- gsub("\\]", "", resultMatrix$Estimate)
  resultMatrix$Estimate <- as.factor(resultMatrix$Estimate)

  # names(resultMatrix)[names(resultMatrix) == "Parameter entry"] <- "Group"
  # names(resultMatrix)[names(resultMatrix) == "Parameter"] <- "Estimate"
  # names(resultMatrix)[names(resultMatrix) == "Target"] <- "Target/category"

  if (DT) {
    DT::datatable(
      resultMatrix,
      rownames = FALSE,
      filter = "top",
      style = "bootstrap",
      options = list(
        pageLength = 25
      ),
      selection = list(mode = "single", target = "cell")
    )
  } else {
    resultMatrix
  }
}

splitLast <- function(x, splitter = "\\."){
  lastdelim = tail(gregexpr(splitter,x)[[1]],n=1)
  sapply(x,function(y) {substr(y,1,lastdelim-1)})
}

transformUIStatNames <- function(statistics) {
  statisticsReturn <- c()
  statisticsNames <- c()

  if (!is.null(statistics)) {
    if (statistics[1] == TRUE) {
      statisticsReturn <- c(statisticsReturn, "min")
      statisticsNames <- c(statisticsNames, "Minimum")
    }
    if (statistics[2] == TRUE) {
      statisticsReturn <- c(statisticsReturn, "max")
      statisticsNames <- c(statisticsNames, "Maximum")
    }
    if (statistics[3] == TRUE) {
      statisticsReturn <- c(statisticsReturn, "median")
      statisticsNames <- c(statisticsNames, "Median")
    }
    if (statistics[4] == TRUE) {
      statisticsReturn <- c(statisticsReturn, paste0("function(y) quantile(y, ", statistics[5], ", na.rm = TRUE)"))
      statisticsNames <- c(statisticsNames, paste0("Quantile_", statistics[5]))
      statisticsReturn <- c(statisticsReturn, paste0("function(y) quantile(y, ", statistics[6], ", na.rm = TRUE)"))
      statisticsNames <- c(statisticsNames, paste0("Quantile_", statistics[6]))
    }
    if (statistics[7] == TRUE) {
      statisticsReturn <- c(
        statisticsReturn,
        paste0(
          "function(y) min(2 * mean(y > ", statistics[8],
          " , na.rm = TRUE), 2 * mean(y < ", statistics[8], " , na.rm = TRUE))"
        )
      )
      statisticsNames <- c(statisticsNames, "P-value")
    }
    if (statistics[9] == TRUE) {
      statisticsReturn <- c(statisticsReturn, paste0("function(y) HDInterval::hdi(y, credMass = ", statistics[10], ")[1]"))
      statisticsNames <- c(statisticsNames, paste0("lower_C.I._", statistics[10]))
      statisticsReturn <- c(statisticsReturn, paste0("function(y) HDInterval::hdi(y, credMass = ", statistics[10], ")[2]"))
      statisticsNames <- c(statisticsNames, paste0("upper_C.I._", statistics[10]))
    }
  }
  return(list(statisticsReturn = statisticsReturn, statisticsNames = statisticsNames))
}

# transformStatNames <- function(statistics) {
#   statistics <- sub("lower 95%", "function(y) quantile(y, 0.025, na.rm = TRUE)", statistics)
#   statistics <- sub("upper 95%", "function(y) quantile(y, 0.975, na.rm = TRUE)", statistics)
#   statistics
# }

translateParameters <- function(parameters, fruitsObj, addNameTypes = FALSE) {
  # sources

  if (length(fruitsObj$valueNames$fractions) == 1) {
    fruitsObj$valueNames$fractions[2] <- "fraction_2"
  }

  sources <- parameters[, grep("alpha", colnames(parameters))]
  if (fruitsObj$modelOptions$modelType != "1") {
    sourceNameCombinations <- expand.grid(rownames(fruitsObj$data$obsvn),
      fruitsObj$valueNames$sources,
      stringsAsFactors = FALSE
    )

    colnames(sources) <- paste0("[", sourceNameCombinations[, 2], "]")
  } else {
    colnames(sources) <- paste0("[", fruitsObj$valueNames$sources, "]")
  }

  # target consumer data
  targetConsumer <- as.matrix(parameters[, grep("mu\\[", colnames(parameters))])

  targetNameCombinations <- expand.grid(rownames(fruitsObj$data$obsvn),
    fruitsObj$valueNames$targets,
    stringsAsFactors = FALSE
  )


  colnames(targetConsumer) <- paste0("[", targetNameCombinations[, 2], "]")

  # offset  data
  offset <- as.matrix(parameters[, grep("T_", colnames(parameters))])

  if (fruitsObj$modelOptions$targetOffset == TRUE) {
    if (!(fruitsObj$modelOptions$modelType %in% c("4", "5"))) {
      offsetNameCombinations <- paste0("[", fruitsObj$valueNames$targets, "]")
      colnames(offset) <- offsetNameCombinations
    } else {
      offsetNameCombinations <- expand.grid(rownames(fruitsObj$data$obsvn),
        fruitsObj$valueNames$targets,
        stringsAsFactors = FALSE
      )
      colnames(offset) <- paste0("[", offsetNameCombinations[, 2], "]")
    }
  }

  # weights
  weights <- as.matrix(parameters[, grep("W", colnames(parameters))])

  if (!(fruitsObj$modelOptions$modelType %in% c("4", "5"))) {
    weightsNameCombinations <- expand.grid(fruitsObj$valueNames$targets,
      fruitsObj$valueNames$fractions,
      stringsAsFactors = FALSE
    )
    if (ncol(weights) > 1) {
      colnames(weights) <- applyNames(weightsNameCombinations)
    }
  } else {
    weightsNameCombinations <- expand.grid(rownames(fruitsObj$data$obsvn),
      fruitsObj$valueNames$targets,
      fruitsObj$valueNames$fractions,
      stringsAsFactors = FALSE
    )
    if (ncol(weights) > 1) {
      colnames(weights) <- applyNames(weightsNameCombinations[, -1])
    }
  }

  # Source/food values
  sourceValues <- as.matrix(parameters[, grep("I_", colnames(parameters))])

  if (fruitsObj$modelOptions$modelType %in% c("1", "2")) {
    sourceValuesNameCombinations <- expand.grid(fruitsObj$valueNames$sources,
      fruitsObj$valueNames$fractions,
      fruitsObj$valueNames$targets,
      stringsAsFactors = FALSE
    )
  } else {
    sourceValuesNameCombinations <- expand.grid(fruitsObj$valueNames$sources,
      fruitsObj$valueNames$fractions,
      fruitsObj$valueNames$targets,
      rownames(fruitsObj$data$obsvn),
      stringsAsFactors = FALSE
    )
  }
  if (ncol(sourceValues) > 1) {
    colnames(sourceValues) <- applyNames(sourceValuesNameCombinations[, 1:3])
  }
  # Source offset values

  sourceOffValues <- as.matrix(parameters[, grep("S_", colnames(parameters))])
  if (ncol(sourceOffValues) > 0) {
    if (fruitsObj$modelOptions$modelType %in% c("1", "2")) {
      sourceOffValuesNameCombinations <- expand.grid(fruitsObj$valueNames$sources,
        fruitsObj$valueNames$fractions,
        fruitsObj$valueNames$targets,
        stringsAsFactors = FALSE
      )
    } else {
      sourceOffValuesNameCombinations <- expand.grid(fruitsObj$valueNames$sources,
        fruitsObj$valueNames$fractions,
        fruitsObj$valueNames$targets,
        rownames(fruitsObj$data$obsvn),
        stringsAsFactors = FALSE
      )
    }
    if (ncol(sourceOffValues) > 1) {
      colnames(sourceOffValues) <- applyNames(sourceOffValuesNameCombinations[, 1:3])
    }
  }
  # Concentrations
  concentrations <- as.matrix(parameters[, grep("C", colnames(parameters))])

  if (!(fruitsObj$modelOptions$modelType %in% c("3", "4", "5"))) {
    concentrationsNameCombinations <- expand.grid(fruitsObj$valueNames$sources,
      fruitsObj$valueNames$fractions,
      stringsAsFactors = FALSE
    )
    if (ncol(concentrations) > 1) {
      colnames(concentrations) <- applyNames(concentrationsNameCombinations)
    }
  } else {
    concentrationsNameCombinations <- expand.grid(rownames(fruitsObj$data$obsvn), fruitsObj$valueNames$sources,
      fruitsObj$valueNames$fractions,
      stringsAsFactors = FALSE
    )
    if (ncol(concentrations) > 1) {
      colnames(concentrations) <- applyNames(concentrationsNameCombinations[, -1])
    }
  }
  # Beta
  betas <- as.matrix(parameters[, grep("beta_", colnames(parameters))])
  if (fruitsObj$modelOptions$modelType == "1") {
    betasValuesNameCombinations <- expand.grid(fruitsObj$valueNames$fractions,
      stringsAsFactors = FALSE
    )
    if (ncol(betas) > 1) {
      colnames(betas) <- paste0("[", betasValuesNameCombinations[, 1], "]")
    }
  } else {
    betasValuesNameCombinations <- expand.grid(rownames(fruitsObj$data$obsvn),
      fruitsObj$valueNames$fractions,
      stringsAsFactors = FALSE
    )
    if (ncol(betas) > 1) {
      colnames(betas) <- paste0("[", betasValuesNameCombinations[, 2], "]")
    }
  }

  # Q's
  # covariateConcentrations <- as.matrix(parameters[, grep("q\\[", colnames(parameters))])
  # if(fruitsObj$modelOptions$hierarchical == TRUE){
  #   qCombinations <- unique(fruitsObj$data$covariates)
  #   qCombinations <- cbind(qCombinations[rep(1:NROW(qCombinations),
  #                                            each = length(fruitsObj$valueNames$sources)),], rep(fruitsObj$valueNames$sources,
  #                                                                                                each = NROW(qCombinations)))
  #   colnames(covariateConcentrations) <- applyNames(qCombinations)
  # }

  # Theta
  thetas <- as.matrix(parameters[, grep("theta_", colnames(parameters))])
  if (fruitsObj$modelOptions$modelType == "1") {
    thetasValuesNameCombinations <- expand.grid(fruitsObj$valueNames$targets,
      fruitsObj$valueNames$sources,
      stringsAsFactors = FALSE
    )
    if (ncol(thetas) > 1) {
      colnames(thetas) <- applyNames(thetasValuesNameCombinations[, 1:2])
    }
  } else {
    thetasValuesNameCombinations <- expand.grid(rownames(fruitsObj$data$obsvn),
      fruitsObj$valueNames$targets,
      fruitsObj$valueNames$sources,
      stringsAsFactors = FALSE
    )
    if (ncol(thetas) > 1) {
      colnames(thetas) <- applyNames(thetasValuesNameCombinations[, 2:3])
    }
  }

  translatedParameters <- list(
    "Source contributions" = sources,
    "Component contributions" = betas,
    "Source contributions by proxy" = thetas,
    "Target proxies" = targetConsumer,
    "Target-to-source offsets" = offset,
    Weights = weights, "Component proxies" = sourceValues,
    sourceOffValues = sourceOffValues,
    # covariateConcentrations = covariateConcentrations,
    Concentrations = concentrations
  )

  if (addNameTypes) {
    translatedParameters <- lapply(1:length(translatedParameters), function(x) {
      if (NCOL(translatedParameters[[x]]) > 1) {
        colnames(translatedParameters[[x]]) <- paste0(
          names(translatedParameters)[x],
          "_", colnames(translatedParameters[[x]])
        )
      }
      translatedParameters[[x]]
    })
  }
  return(translatedParameters)
}

applyNames <- function(nameCombinations) {
  unlist(lapply(
    1:nrow(nameCombinations),
    function(x) {
      paste0(
        "[",
        paste(nameCombinations[x, ],
          collapse = "]["
        ), "]"
      )
    }
  ))
}

# deleteNullParameters <- function(x){
#   if(ncol(x) > 0){
#     x <- x[, !((apply(x, 2, max) < 1E-3) & (apply(x, 2, min) > -1E-3)), drop = FALSE]
#   }
#   x
# }

extractResultMatrixOfChain <- function(x, renamedChains, statisticsNames, statisticsFunctions, fruitsObj) {
  resultMatrix <- (do.call("cbind", lapply(statisticsFunctions, function(y) {
    round(apply(renamedChains[[x]], 2, eval(parse(text = y))), 3)
  })))
  colnames(resultMatrix) <- statisticsNames
  if(nrow(resultMatrix)>0){
    Parameter_Names <- rownames(resultMatrix)
    #unlist(lapply(1:nrow(resultMatrix), function(x) strsplit(rownames(resultMatrix)[x], split = "_")[[1]][1]))
  }
  if (fruitsObj$modelOptions$modelType != "1" &
      (names(renamedChains))[x] %in% c(
        "Source contributions", "Component contributions",
        "Source contributions by proxy", "Target proxies",
        "userEstimates"
      ) |
      (fruitsObj$modelOptions$modelType %in% c("4", "5")) &
      (names(renamedChains))[x] %in% c("Weights", "Concentrations", "Target-to-source offsets") |
      (fruitsObj$modelOptions$modelType %in% c("3")) &
      (names(renamedChains))[x] %in% c("Concentrations")) {
    repTimes <- nrow(resultMatrix) / length(rownames(fruitsObj$data$obsvn))
    if (NROW(fruitsObj$data$covariates) > 0 & NCOL(fruitsObj$data$covariates) > 0 &
        fruitsObj$modelOptions$hierarchical == TRUE) {
      # resultMatrix <- cbind(fruitsObj$data$covariates[rep(1:nrow(fruitsObj$data$covariates), repTimes),
      #                                                 , drop = FALSE], resultMatrix)
      
      if((names(renamedChains))[x] == "userEstimates"){
        
        indices <- unlist(lapply(
          rownames(resultMatrix),
          function(x) paste0(strsplit(x, split = "_")[[1]][-1], collapse = "_")
        ))
        userNames <- unlist(lapply(
          rownames(resultMatrix),
          function(x) strsplit(x, split = "_")[[1]][1]
        ))
        repInd <- match(indices, rownames(fruitsObj$data$obsvn))
        colnames(renamedChains[[x]]) <- userNames
      } else {
        repInd <- rep(1:length(rownames(fruitsObj$data$obsvn)),
                      repTimes
        )
        
      }
      
      
      covariateVectors <- lapply(
        1:ncol(fruitsObj$data$covariates),
        function(x) {
          rep(
            fruitsObj$data$covariates[repInd, x]
          )
        }
      )
      names(covariateVectors) <- colnames(fruitsObj$data$covariates)
      
      if (NCOL(fruitsObj$data$covariates) > 1) {
        covariateVectors$interactions <- interaction(split(
          t(fruitsObj$data$covariates),
          colnames(fruitsObj$data$covariates)
        ),
        drop = FALSE
        )[repInd]
      }
      
      resultMatrix2 <- do.call("rbind", lapply(covariateVectors, function(zz) {
        covValues <- unique(zz)
        do.call("rbind", lapply(unique(colnames(renamedChains[[x]])), function(z) {
          do.call("rbind", lapply(covValues, function(zzz) {
            tempDat <- as.vector(renamedChains[[x]][, colnames(renamedChains[[x]]) == z & zz == zzz, drop = FALSE])
            
            tempDat2 <- data.frame(do.call("cbind", lapply(statisticsFunctions, function(y) {
              round(apply(matrix(tempDat, ncol = 1), 2, eval(parse(text = y))), 3)
            })))
            colnames(tempDat2) <- statisticsNames
            
            tempDat2$Target <- zzz
            tempDat2 <- data.frame(Parameter_Name = z, tempDat2)
            tempDat2
          }))
        }))
      }))
    
      resultMatrix2 <- resultMatrix2[!is.na(resultMatrix2[,2]),,drop = FALSE]
    } else {
      if((names(renamedChains))[x] == "userEstimates"){
        
        indices <- unlist(lapply(
          rownames(resultMatrix),
          function(x) paste0(strsplit(x, split = "_")[[1]][-1], collapse = "_")
        ))
        userNames <- unlist(lapply(
          rownames(resultMatrix),
          function(x) strsplit(x, split = "_")[[1]][1]
        ))
        repInd <- match(indices, rownames(fruitsObj$data$obsvn))
        colnames(renamedChains[[x]]) <- userNames
      } else {
        repInd <- rep(1:length(rownames(fruitsObj$data$obsvn)),
                      repTimes
        )
        
      }
    }
    resultMatrix <- data.frame(
      Target = rownames(fruitsObj$data$obsvn)[repInd],
      Type = rep("targets", length(repInd)), resultMatrix
    )
    resultMatrix$Type[is.na(resultMatrix$Target)] <- "all"
    resultMatrix$Target[is.na(resultMatrix$Target)] <- "all"
  } else {
    if (NROW(fruitsObj$data$covariates) > 0 & NCOL(fruitsObj$data$covariates) > 0 &
        fruitsObj$modelOptions$hierarchical == TRUE) {
      # covMatrix <- matrix(data = rep("all", ncol(fruitsObj$data$covariates)),
      #        ncol = ncol(fruitsObj$data$covariates),
      #        dimnames = list(1, colnames(fruitsObj$data$covariates)))
      # resultMatrix <- cbind(covMatrix[rep(1, nrow(resultMatrix)), , drop = FALSE], resultMatrix)
    }
    resultMatrix <- data.frame(resultMatrix,
                               Target = rep("all", nrow(resultMatrix)),
                               Type = rep("all", nrow(resultMatrix))
    )
  }
  if (nrow(resultMatrix) > 0) {
    resultMatrix <- data.frame(
      Parameter_Type = (names(renamedChains))[x],
      Parameter_Name = Parameter_Names, resultMatrix
    )
    colnames(resultMatrix)[1:2] <- c("Group", "Estimate")
    
    if (exists("resultMatrix2") && nrow(resultMatrix2) > 0) {
      resultMatrix2 <- data.frame(
        Parameter_Type = (names(renamedChains))[x], resultMatrix2,
        Type = rep("covariate", nrow(resultMatrix2))
      )
      colnames(resultMatrix2)[1:2] <- c("Group", "Estimate")
      resultMatrix <- rbind(resultMatrix, resultMatrix2)
    }
    
    # names(resultMatrix)[names(resultMatrix) == "Parameter entry"] <- "Group"
    # names(resultMatrix)[names(resultMatrix) == "Parameter"] <- "Estimate"
    # names(resultMatrix)[names(resultMatrix) == "Target"] <- "Target/category"
    
    return(resultMatrix)
  } else {
    NULL
  }
}
