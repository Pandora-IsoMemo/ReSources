getSourceScoreSep <- function(simSources) {
  proxies <- colnames(simSources[[1]])
  data <- do.call("rbind", lapply(1:length(simSources), function(x) {
    dataFrame <- as.data.frame(simSources[[x]])
    dataFrame$source <- names(simSources[x])
    dataFrame
  }))

  output <- summary(manova(as.matrix(cbind(data[, proxies])) ~ source, data = data))
  return(output)
}

getZScoresData <- function(simSources) {
  means <- do.call("rbind", lapply(simSources, colMeans))
  vars <-
    do.call("rbind", lapply(simSources, function(x) {
      apply(x, 2, var)
    }))

  combinations <- combn(rownames(means), 2)

  z_ScoresList <- lapply(1:ncol(simSources[[1]]), function(i) {
    z_score <-
      round(combn(means[, i], 2, diff) / sqrt(combn(vars[, i], 2, sum)), 3)

    analysis <- lapply(1:ncol(combinations), function(x) {
      SourceA <- simSources[[combinations[1, x]]][, 1][1:min(5000, length(simSources[[combinations[1, x]]][, 1]))]
      SourceB <- simSources[[combinations[2, x]]][, 1][1:min(5000, length(simSources[[combinations[2, x]]][, 1]))]
      data.frame(
        Source1_Norm_p.value = shapiro.test(SourceA)$p.value,
        Source2_Norm_p.value = shapiro.test(SourceA)$p.value,
        equalVar_p.value = var.test(SourceA, SourceB)$p.value
      )
    }) %>% bind_rows()

    data.frame(
      combination = sapply(
        1:ncol(combinations),
        function(x) {
          paste0(combinations[, x], collapse = "-")
        }
      ),
      z_score = z_score,
      p_value = round(2 * (1 - pnorm(abs(z_score))), 3),
      Source1_Norm_p.value = round(analysis$Source1_Norm_p.value, 3),
      Source2_Norm_p.value = round(analysis$Source2_Norm_p.value, 3),
      equalVar_p.value = round(analysis$equalVar_p.value, 3)
    )
  })

  z_ScoresList <- lapply(1:length(z_ScoresList), function(x) {
    z_ScoresList[[x]]$proxy <- colnames(simSources[[1]])[x]
    z_ScoresList[[x]]
  })
  z_ScoresList <- do.call("rbind", z_ScoresList)
  z_ScoresList <- z_ScoresList[, c(
    "proxy", "combination", "z_score", "p_value",
    "Source1_Norm_p.value", "Source2_Norm_p.value",
    "equalVar_p.value"
  )]
  colnames(z_ScoresList) <- c(
    "Proxies", "Proxy-Source", "Z Score", "p value",
    "Source1 Normality p value", "Source2 Normality p value",
    "equal variance p value"
  )

  z_ScoresList
}

getZScores <- function(simSources) {
  z_ScoresList <- getZScoresData(simSources)

  DT::datatable(
    z_ScoresList,
    rownames = FALSE,
    filter = "top",
    style = "bootstrap",
    options = list(
      pageLength = 25
    ),
    selection = list(mode = "single", target = "cell")
  )
}

getSourceCorr <- function(simSources, corr = FALSE) {
  output <- list()
  corTargetDataAll <- lapply(1:ncol(simSources[[1]]), function(x) {
    corTargetData <- lapply(1:length(simSources), function(i) simSources[[i]][, x])
    names(corTargetData) <- names(simSources)
    signif(cov(do.call(cbind, corTargetData)), 3)
  })
  if (corr == TRUE) {
    corTargetDataAll <- lapply(1:length(corTargetDataAll), function(i) signif(cov2cor(corTargetDataAll[[i]]), 3))
  }
  names(corTargetDataAll) <- colnames(simSources[[1]])
  output$correlationSources <- corTargetDataAll


  if (ncol(simSources[[1]]) > 1 | corr == FALSE) {
    covMatrices <- lapply(1:length(simSources), function(i) signif(cov(simSources[[i]]), 3))

    if (corr == FALSE) {
      names(covMatrices) <- names(simSources)
      return(covMatrices)
    }

    if (corr == TRUE) {
      covMatrices <- lapply(1:length(covMatrices), function(i) signif(cov2cor(covMatrices[[i]]), 3))
    }
    names(covMatrices) <- names(simSources)
    output$correlationTargets <- covMatrices
  } else {
    output$correlationTargets <- "At least two targets required to compute correlations between targets."
  }


  means <- signif(t(bind_rows(lapply(simSources, colMeans))), 3)
  sds <- signif(t(bind_rows(lapply(simSources, function(x) apply(x, 2, sd)))), 3)
  colnames(means) <- names(simSources)
  colnames(sds) <- names(simSources)
  output$mean <- means
  output$sd <- sds
  return(output)
}

getSourceMahaDistData <- function(simSources) {
  means <- do.call("rbind", lapply(simSources, colMeans))
  combinations <- combn(rownames(means), 2)
  combinations <- lapply(
    1:ncol(combinations),
    function(x) paste0(combinations[, x], collapse = "-")
  )
  combinationNumbers <- combn(length(rownames(means)), 2)
  cov <- getSourceCorr(simSources)

  maha_ScoresList <- lapply(1:length(combinations), function(i) {
    expDiff <- means[combinationNumbers[1, i], ] - means[combinationNumbers[2, i], ]
    sqrt(expDiff %*%
      solve(cov[[combinationNumbers[1, i]]] + cov[[combinationNumbers[2, i]]]) %*%
      expDiff)
  })
  maha_ScoresList <- data.frame(
    combination = unlist(combinations),
    mahalanobis_distance = round(unlist(maha_ScoresList), 3),
    p_value = round(1 - pchisq(unlist(maha_ScoresList)^2, df = ncol(simSources[[1]])), 3)
  )
  colnames(maha_ScoresList) <- c("Proxy-Source", "Mahalanobis distance", "p value")

  maha_ScoresList
}

getSourceMahaDist <- function(simSources) {
  maha_ScoresList <- getSourceMahaDistData(simSources)

  DT::datatable(
    maha_ScoresList,
    rownames = FALSE,
    filter = "top",
    style = "bootstrap",
    options = list(
      pageLength = 25
    ),
    selection = list(mode = "single", target = "cell")
  )
}

getSimSources <- function(model, fruitsObj, simGrid, userDefinedAlphas = NULL, simSourceNames = NULL) {
  simList <- c("I_", "mu", "component.contrib_")

  optional <- c("W_", "C_", "T_", "I0_", "S_", "S1_", "S2_", "S3_", "HT1", "HT2", "HT3")
  optional <- optional[sapply(
    1:length(optional),
    function(i) any(grep(optional[i], fruitsObj$modelCode))
  )]

  simList <- c(optional, simList)
  model$alpha_ <- simGrid
  nSim <- 100
  sim <- lapply(1:nSim, function(x) {
    model$simulate(simList)
    sim <- model$mu
    colnames(sim) <- fruitsObj$valueNames$targets
    return(sim)
  })
  # sim <- do.call("rbind", sim)
  # sim <- Reduce("+", sim) / nSim
  simSourcesAll <- simplify2array(sim)
  simSourcesAll <- lapply(1:nrow(simGrid), function(x) t(simSourcesAll[x, , ]))
  fullSources <- which(sapply(1:nrow(simGrid), function(x) max(simGrid[x, ] == 1)) == 1)
  fullSourcesColNames <- na.omit(match(colnames(simGrid), colnames(simGrid)[sapply(1:length(fullSources), function(x) which.max(simGrid[fullSources, ][x, ]))]))
  simSources <- (simSourcesAll[fullSources][fullSourcesColNames])

  # (sapply(simSources, colMeans) %>% t)[simGrid[,4] == 0 & simGrid[,5] == 0,] %>% plot

  # simSources <- lapply(1:length(fruitsObj$valueNames$sources), function(y){
  #   if(fruitsObj$modelOptions$modelType == "1"){
  #     model$alpha_[y] <- 1
  #     model$alpha_[-y] <- 0
  #   } else {
  #     model$alpha_[, y] <- 1
  #     model$alpha_[, -y] <- 0
  #   }
  #   sim <- lapply(1:100, function(x) {
  #     model$simulate(simList)
  #     return(model$mu)
  #   })
  #   sim <- do.call("rbind", sim)
  #   colnames(sim) <- fruitsObj$valueNames$targets
  #   return(sim)
  # })

  # userDefinedAlphas <- list(userDef1 = data.frame(source = c("Herbivores", "Carnivores", "Plants", "Fish1", "Fish2" ),
  #                                                  mean = c(0.1, 0.2, 0.5, 0.05, 0.15),
  #                                                  sd = c(0.01, 0.02, 0.03, 0.01, 0.03)
  # ))

  if (!is.null(userDefinedAlphas)) {
    userDefinedAlphasNames <- names(userDefinedAlphas)
    userDefinedAlphas <- lapply(1:length(userDefinedAlphas), function(x) {
      userDefinedAlphas[[x]] <- as.data.frame(userDefinedAlphas[[x]])
      userDefinedAlphas[[x]]$source <- rownames(userDefinedAlphas[[x]])
      userDefinedAlphas[[x]]
    })
    names(userDefinedAlphas) <- userDefinedAlphasNames
    userDefinedSim <- lapply(1:length(userDefinedAlphas), function(y) {
      userDefinedAlphas[[y]][is.na(userDefinedAlphas[[y]])] <- 0
      simUserDefined <- lapply(1:100, function(x) {
        # if(fruitsObj$modelOptions$modelType == "1"){
        #   alphas <- rnorm(length(fruitsObj$valueNames$sources),
        #                   mean = userDefinedAlphas[[y]]$mean,
        #                   sd = userDefinedAlphas[[y]]$sd)
        #   model$alpha_[1:length(fruitsObj$valueNames$sources)] <- alphas / sum(alphas)
        # } else {
        alphas <- matrix(rnorm(length(fruitsObj$valueNames$sources)^2,
          mean = rep(userDefinedAlphas[[y]]$mean,
            each = length(fruitsObj$valueNames$sources)
          ),
          sd = rep(userDefinedAlphas[[y]]$sd,
            each = length(fruitsObj$valueNames$sources)
          )
        ),
        ncol = length(fruitsObj$valueNames$sources)
        )

        model$alpha_[1:nrow(model$alpha_), 1:length(fruitsObj$valueNames$sources)] <- matrix(rep(
          sweep(alphas, 1, rowSums(alphas), FUN = "/")[1, ],
          nrow(model$alpha_)
        ), ncol = length(fruitsObj$valueNames$sources), byrow = T)
        # }
        model$simulate(simList)
        return(unique(model$mu))
      })
      simUserDefined <- do.call("rbind", simUserDefined)
      colnames(simUserDefined) <- fruitsObj$valueNames$targets
      return(simUserDefined)
    })
    names(userDefinedSim) <- names(userDefinedAlphas)
  } else {
    userDefinedSim <- NULL
  }
  # names(simSources) <- colnames(simGrid)[sapply(1:length(fullSources), function(x) which.max(simGrid[fullSources, ][x,] ))]
  # simSources <- simSources[match(simSourceNames, names(simSources))]
  names(simSources) <- simSourceNames
  return(list(
    simSources = simSources, userDefinedSim = userDefinedSim,
    simGrid = simGrid, simSourcesAll = simSourcesAll
  ))
}

sourceTargetPlot <- function(simSources = NULL,
                             simSourcesAll = NULL,
                             simGrid = NULL,
                             confidence = 0.9, showConfidence = TRUE,
                             targets = NULL, fractions = NULL,
                             fruitsObj = NULL, showIndividuals = TRUE,
                             sources = NULL, covariates = NULL,
                             covariateValues = NULL,
                             horizontalPlot = "vertical",
                             targetValues = NULL,
                             showTargetNames = TRUE,
                             targetErrors = NULL,
                             concentrationValues = NULL,
                             concentrationErrors = NULL,
                             userDefinedSim = NULL,
                             showLegend = TRUE,
                             legendInside = FALSE,
                             showGrid = TRUE,
                             showPoints = FALSE,
                             hull = "convex hull",
                             alpha = 0.1) {
  if ((is.null(simSources) || all(is.na(simSources))) &
    (is.null(targetValues) || all(is.na(targetValues))) &
    (is.null(concentrationValues) || all(is.na(concentrationValues)))) {
    return(NULL)
  }
  if (!is.null(simSources)) {
    targetValues <- fruitsObj$data$obsvn
    targetErrors <- fruitsObj$data$obsvnError
    covariateValues <- fruitsObj$data$covariates
  }
  if (!is.null(sources)) {
    if (!(length(targets) %in% c(1, 2, 3))) {
      data <- data.frame(
        Message = c("Please select 1, 2 or 3 proxies"),
        Proxy1 <- c(0), Proxy2 <- c(1)
      )
      plot <- plot_ly(data,
        x = ~Proxy1, y = ~Proxy2,
        mode = "text", type = "scatter",
        text = ~Message, textfont = list(color = "red", size = 16)
      )
      plot <- plot %>% layout(showlegend = showLegend)
      if (legendInside) {
        plot <- plot %>% layout(legend = list(x = 0.025, y = 0.975))
      }
      return(plot)
    }
    if ((length(sources) < 3 & length(targets) > 1) | (length(sources) < 2 & length(targets) == 1)) {
      minNSources <- ifelse(length(targets) > 1, 3, 2)
      data <- data.frame(
        Message = sprintf("Please select at least %s sources", minNSources),
        Proxy1 = c(0), 
        Proxy2 = c(1)
      )
      
      plot <- plot_ly(data,
        x = ~Proxy1, y = ~Proxy2,
        mode = "text", type = "scatter",
        text = ~Message, textfont = list(color = "red", size = 16)
      )
      plot <- plot %>% layout(showlegend = showLegend)
      if (legendInside) {
        plot <- plot %>% layout(legend = list(x = 0.025, y = 0.975))
      }

      return(plot)
    }
    # simSources <- simSources[names(simSources) %in% sources]
  }

  if (!is.null(simSources)) {
    if (!is.null(targets) && length(targets) > 0) {
      simSources <- lapply(simSources, function(m) {
        m[, targets, drop = FALSE]
      })
      simSourcesAll <- lapply(simSourcesAll, function(m) {
        m[, targets, drop = FALSE]
      })
    } else {
      return(NULL)
    }
  }

  if (!is.null(userDefinedSim)) {
    userDefinedSim <- lapply(userDefinedSim, function(m) {
      m[, targets, drop = FALSE]
    })
    meansUser <- do.call("rbind", lapply(userDefinedSim, colMeans))
    covsUser <- getSourceCorr(userDefinedSim)
  } else {
    meansUser <- NULL
  }

  if (!is.null(covariates)) {
    individualCovList <- lapply(1:length(covariates), function(k) {
      allInts <- getAllCovariateInteractions(covariateValues, vars = colnames(covariateValues))
      pos <- rep(1:nrow(covariateValues), length(allInts) / nrow(covariateValues))
      pos[which(getAllCovariateInteractions(covariateValues, vars = colnames(covariateValues)) %in% covariates[k])]
    })
  }
  if (!is.null(simSources)) {
    means <- do.call("rbind", lapply(simSources, colMeans))
    covs <- getSourceCorr(simSources)
    if (!is.null(sources)) {
      simGrid <- simGrid[, match(sources, colnames(simGrid)), drop = FALSE]
      simSourcesAll <- simSourcesAll[which(round(rowSums(simGrid), 2) == 1)]
      simGrid <- simGrid[which(rowSums(simGrid) >= 0.99), ]
      simSourcesAll <- simSourcesAll[do.call(order, split(simGrid, rep(1:ncol(simGrid), each = nrow(simGrid))))]
    }
    meansAll <- do.call("rbind", lapply(simSourcesAll, colMeans))
    covsAll <- getSourceCorr(simSourcesAll)

    if (ncol(means) == 3) {
      plot3D <- 1
    }
    if (ncol(means) == 2) {
      plot3D <- 0
    }
    if (ncol(means) == 1) {
      plot3D <- -1
    }
  } else {
    if (!is.null(targetValues)) {
      means <- targetValues
      targetErrors[is.na(targetErrors)] <- 0.005
      covs <- lapply(1:nrow(targetErrors), function(x) {
        diag(targetErrors[x, ])^2
      })
      names(covs) <- rownames(targetErrors)
      if (length(targets) == 3) {
        plot3D <- 1
      }
      if (length(targets) == 2) {
        plot3D <- 0
      }
      if (length(targets) == 1) {
        plot3D <- -1
      }
    } else {
      if (!is.null(concentrationValues)) {
        means <- concentrationValues
        covs <- lapply(1:nrow(concentrationErrors), function(x) {
          diag(concentrationErrors[x, ])^2
        })
        names(covs) <- rownames(concentrationErrors)

        if (length(fractions) == 3) {
          plot3D <- 1
        }
        if (length(fractions) == 2) {
          plot3D <- 0
        }
        if (length(fractions) == 1) {
          plot3D <- -1
        }
      }
    }
  }
  if (!is.null(sources)) {
    means <- means[rownames(means) %in% sources, ,drop=FALSE]
    covs <- covs[names(covs) %in% sources]
  }
  names <- rownames(means)
  
  colorsPlot <- colorRampPalette(brewer.pal(max(3, min(8, nrow(means))), "Set2"))(length(unique(rownames(means))))[rownames(means) %>%
    factor() %>%
    as.numeric()]
  plotData <- data.frame(x = numeric(), y = numeric(), error = numeric(), colorsPlot = character())
  ########### 1D ####
  if (plot3D == -1) {
    if (!is.null(simSources)) {
      plotData <- data.frame(
        y = round(as.vector(means), 3), x = rownames(means),
        error = round(qnorm(1 - (1 - confidence) / 2) * sqrt(unlist(covs)), 3),
        colorsPlot = colorsPlot
      )
      if((showGrid | showPoints) & (!is.null(sources))){
        simData <- data.frame(
          y = round(meansAll[, 1], 3),
          x = sapply(1:nrow(simGrid),
                     function(i) paste(paste0(colnames(simGrid), ":", round(simGrid[i, ],3)), collapse = ", ")),
          error = round(qnorm(1 - (1 - confidence) / 2) * sqrt(unlist(covsAll)), 3),
          colorsPlot = "blue"
        )
        plotData <- rbind(simData, plotData)
      }
    }
    if (showIndividuals == TRUE) {
      if (!is.null(simSources)) {
        cols <- rep("#d3d3d3", NROW(targetErrors))
      } else {
        cols <- colorRampPalette(brewer.pal(max(3, min(8, nrow(means))), "Set2"))(length(unique(rownames(means))))[rownames(means) %>%
          factor() %>%
          as.numeric()]
      }
      individualData <- data.frame(
        y = targetValues[, targets],
        x = rownames(targetValues),
        error = round(qnorm(1 - (1 - confidence) / 2) *
          targetErrors[, targets], 3),
        colorsPlot = cols
      )
      plotData <- rbind(individualData, plotData)
    }
    if (!is.null(userDefinedSim)) {
      cols <- colorRampPalette(brewer.pal(max(3, min(8, nrow(means))), "Set3"))(nrow(meansUser))
      userData <- data.frame(
        y = round(as.vector(meansUser), 3),
        x = rownames(meansUser),
        error = round(qnorm(1 - (1 - confidence) / 2) * sqrt(unlist(covsUser)), 3),
        colorsPlot = cols
      )
      plotData <- rbind(userData, plotData)
    }


    if (!is.null(covariates)) {
      covData <- do.call("rbind", lapply(
        1:length(individualCovList),
        function(x) {
          data.frame(
            y = mean(targetValues[individualCovList[[x]], targets]),
            x = covariates[x],
            error = sqrt(sum((targetErrors[individualCovList[[x]], targets])^2) /
              length(individualCovList[[x]])^2),
            colorsPlot = rep("darkgrey", 1)
          )
        }
      ))
      plotData <- rbind(covData, plotData)
    }
    if (!is.null(concentrationValues)) {
      concData <- data.frame(
        y = concentrationValues[, fractions],
        x = rownames(concentrationValues),
        error = round(qnorm(1 - (1 - confidence) / 2) *
          concentrationErrors[, fractions], 3),
        colorsPlot = colorsPlot
      )
      plotData <- rbind(concData, plotData)
    }
    plotData$x <- factor(plotData$x, levels = unique(plotData$x))
    plotData <- plotData %>% arrange(.data$x)

    if (showConfidence) {
      if (horizontalPlot != "vertical") {
        plot <- plot_ly(
          data = plotData, x = ~y,
          type = "scatter", mode = "markers", showlegend = showLegend, color = I(plotData$colorsPlot),
          name = ~x,
          error_x = ~ list(array = error)
        )
      } else {
        plot <- plot_ly(
          data = plotData, x = ~x, y = ~y,
          type = "scatter", mode = "markers", showlegend = FALSE, color = I(plotData$colorsPlot),
          name = ~x,
          error_y = ~ list(array = error)
        )
      }
    } else {
      if (horizontalPlot != "vertical") {
        plot <- plot_ly(
          data = plotData, x = ~y,
          type = "scatter", mode = "markers", showlegend = showLegend, color = I(plotData$colorsPlot),
          name = ~x
        )
        if (!is.null(concentrationValues)) {
          plot <- plot %>% layout(xaxis = list(range = c(0, 100)))
        }
      } else {
        plot <- plot_ly(
          data = plotData, x = ~x, y = ~y,
          type = "scatter", mode = "markers", showlegend = FALSE, color = I(plotData$colorsPlot),
          name = ~x
        )
        if (!is.null(concentrationValues)) {
          plot <- plot %>% layout(yaxis = list(range = c(0, 100)))
        }
      }
    }

    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )

    if (horizontalPlot != "vertical") {
      plot <- plot %>% layout(xaxis = list(title = colnames(means)), yaxis = ax, showlegend = showLegend)
    } else {
      plot <- plot %>% layout(
        yaxis = list(title = colnames(means)),
        xaxis = list(title = ""), showlegend = showLegend
      )
    }
    if (legendInside) {
      plot <- plot %>% layout(legend = list(x = 0.025, y = 0.975))
    }
    return(plot)
  }
  #### 2D ####
  if (plot3D == 0) {
    if (!is.null(simSources)) {
      plotData <- data.frame(
        y = round(means[, 2], 3), x = round(means[, 1], 3),
        colorsPlot = colorsPlot,
        name = rownames(means)
      )
      plotData$name <- factor(plotData$name, levels = unique(plotData$name))
      plotData <- plotData %>% arrange(.data$name)
      plotDataSegments <- data.frame(y = round(meansAll[, 2], 3), x = round(meansAll[, 1], 3))
    }
    plotDataAll <- plotData

    sourceAnnotations <- list(
      yref = "y",
      x = means[, 1],
      y = means[, 2],
      xanchor = "right",
      yanchor = "middle",
      text = ~ rownames(means),
      font = list(
        family = "Arial",
        size = 16,
        color = "rgba(67,67,67,1)"
      ),
      showarrow = FALSE
    )
    if (showConfidence) {
      if (!is.null(simSources)) {
        ellipses <- lapply(1:nrow(plotData), function(i) {
          car::ellipse(unlist(plotData[i, c("x", "y")]),
            shape = covs[[which(names(covs) == plotData$name[i])]],
            sqrt(qchisq(1 - (1 - confidence), 2)), draw = FALSE
          )
        })
        ellipsesAll <- lapply(1:nrow(plotDataSegments), function(i) {
          car::ellipse(unlist(plotDataSegments[i, c("x", "y")]),
            shape = covsAll[[i]],
            sqrt(qchisq(1 - (1 - confidence), 2)), draw = FALSE
          )
        })
      } else {
        ellipses <- lapply(1:nrow(means), function(i) {
          car::ellipse(unlist(means[i, c(1, 2)]),
            shape = covs[[i]],
            sqrt(qchisq(1 - (1 - confidence), 2)), draw = FALSE
          )
        })
      }
    }

    if (showIndividuals == TRUE) {
      if (!is.null(simSources)) {
        individualData <- data.frame(
          y = targetValues[, targets[2]],
          x = targetValues[, targets[1]],
          name = rownames(targetValues),
          colorsPlot = rep("#d3d3d3", NROW(targetErrors))
        )
      } else {
        individualData <- data.frame(
          y = targetValues[, targets[2]],
          x = targetValues[, targets[1]],
          name = rownames(targetValues),
          colorsPlot = colorsPlot
        )
      }
      plotDataAll <- rbind(plotData, individualData)
      plotDataAll$name <- factor(plotDataAll$name, levels = unique(plotDataAll$name))
      plotDataAll <- plotDataAll %>% arrange(.data$name)

      IndividualAnnotations <- list(
        yref = "y",
        x = c(individualData[, 2]),
        y = c(individualData[, 1]),
        xanchor = "right",
        yanchor = "middle",
        text = as.character(individualData$name),
        font = list(
          family = "Arial",
          size = 16,
          color = "rgba(67,67,67,1)"
        ),
        showarrow = FALSE
      )
    }
    if (!is.null(userDefinedSim)) {
      colsUser <- colorRampPalette(brewer.pal(max(3, min(8, nrow(meansUser))), "Set3"))(nrow(meansUser))
      userData <- data.frame(
        y = round(meansUser[, 2], 3), x = round(meansUser[, 1], 3),
        name = rownames(meansUser),
        colorsPlot = colsUser
      )
      plotDataAll <- rbind(plotDataAll, userData)
      if (showConfidence) {
        ellipsesUser <- lapply(1:nrow(userData), function(i) {
          car::ellipse(unlist(userData[i, c("x", "y")]),
            shape = covsUser[[i]],
            sqrt(qchisq(1 - (1 - confidence), 2)),
            draw = FALSE
          )
        })
      }
      sourceAnnotationsUser <- list(
        yref = "y",
        x = meansUser[, 1],
        y = meansUser[, 2],
        xanchor = "right",
        yanchor = "middle",
        text = ~ rownames(meansUser),
        font = list(
          family = "Arial",
          size = 16,
          color = "rgba(67,67,67,1)"
        ),
        showarrow = FALSE
      )
    }

    if (!is.null(covariates)) {
      if (!is.null(simSources)) {
        covData <- do.call("rbind", lapply(
          1:length(individualCovList),
          function(x) {
            data.frame(
              y = mean(targetValues[individualCovList[[x]], targets[2]]),
              x = mean(targetValues[individualCovList[[x]], targets[1]]),
              name = covariates[x],
              colorsPlot = rep("darkgrey", 1)
            )
          }
        ))
      } else {
        covData <- do.call("rbind", lapply(
          1:length(individualCovList),
          function(x) {
            data.frame(
              y = mean(targetValues[individualCovList[[x]], targets[2]]),
              x = mean(targetValues[individualCovList[[x]], targets[1]]),
              name = covariates[x],
              colorsPlot = rep("darkgrey", 1)
            )
          }
        ))
      }
      plotDataAll <- rbind(plotDataAll, covData)
      plotDataAll$name <- factor(plotDataAll$name, levels = unique(plotDataAll$name))
      plotDataAll <- plotDataAll %>% arrange(.data$name)
      sourceAnnotations <- list(
        yref = "y",
        x = c(means[, 1], covData[, 2]),
        y = c(means[, 2], covData[, 1]),
        xanchor = "right",
        yanchor = "middle",
        text = ~ c(rownames(means), covariates),
        font = list(
          family = "Arial",
          size = 16,
          color = "rgba(67,67,67,1)"
        ),
        showarrow = FALSE
      )
    }

    if (!is.null(concentrationValues)) {
      concData <- data.frame(
        y = concentrationValues[, fractions[2]],
        x = concentrationValues[, fractions[1]],
        name = rownames(concentrationValues),
        colorsPlot = colorsPlot
      )
      plotData <- concData
      plotDataAll <- rbind(plotDataAll, concData)
      if (showConfidence) {
        ellipses <- lapply(1:nrow(plotDataAll), function(i) {
          car::ellipse(unlist(plotDataAll[i, c("x", "y")]),
            shape = covs[[which(names(covs) == plotDataAll$name[i])]],
            sqrt(qchisq(1 - (1 - confidence), 2)), draw = FALSE
          )
        })
      }
    }


    plot <- plot_ly(
      data = plotDataAll, x = ~x, y = ~y,
      type = "scatter", mode = "markers", showlegend = showLegend, name = ~name,
      color = I(plotDataAll$colorsPlot)
    ) %>%
      layout(
        annotations = sourceAnnotations, xaxis = list(title = colnames(means)[1]),
        yaxis = list(title = colnames(means)[2])
      )

    if (showTargetNames & showIndividuals) {
      plot <- plot %>%
        layout(annotations = IndividualAnnotations)
    }

    if (!is.null(userDefinedSim)) {
      plot <- plot %>%
        layout(
          annotations = sourceAnnotationsUser,
          xaxis = list(title = colnames(meansUser)[1]),
          yaxis = list(title = colnames(meansUser)[2])
        )
    }

    if (!is.null(sources)) {
      plotDataSegmentsHull <- plotDataSegments[c(nrow(plotDataSegments), 1:nrow(plotDataSegments), 1), ]

      line <- list(
        type = "line",
        line = list(color = "grey"),
        xref = "x",
        yref = "y"
      )

      # triangle
      lines <- list()

      # hull <- chull(plotDataSegmentsHull[, c("x", "y")])
      #
      # plotDataSegmentsHull <- plotDataSegmentsHull[c(hull[length(hull)], hull),]
      #
      # for (i in 1:(nrow(plotDataSegmentsHull) - 1)) {
      #   line[["x0"]] <- plotDataSegmentsHull$x[i]
      #   line[["x1"]] <- plotDataSegmentsHull$x[i + 1]
      #   line[c("y0")] <- plotDataSegmentsHull$y[i]
      #   line[c("y1")] <- plotDataSegmentsHull$y[i + 1]
      #   lines <- c(lines, list(line))
      # }

      # inner
      linesInner <- list()
      # if(length(sources) == 3){
      dists <- as.matrix(dist(simGrid, method = "maximum"))
      dists[col(dists) > row(dists)] <- 1
      minDist <- min(dists[dists > 0])
      lineInner <- list(
        type = "line",
        line = list(color = "blue"),
        xref = "x",
        yref = "y",
        opacity = 0.25,
        layer = "below"
      )

      for (j in 1:(nrow(plotDataSegments) - 1)) {
        index <- which(abs(dists[, j] - minDist) < 0.01)
        if (length(index) > 0) {
          for (k in index) {
            # if(length(sources) == 3 | (sum(simGrid[j, ] > 0) == 2)  &  (sum(simGrid[j, ] > 0) == 2)){
            lineInner[["x0"]] <- plotDataSegments$x[j]
            lineInner[["x1"]] <- plotDataSegments$x[k]
            lineInner[c("y0")] <- plotDataSegments$y[j]
            lineInner[c("y1")] <- plotDataSegments$y[k]
            linesInner <- c(linesInner, list(lineInner))
            # }
          }
        }
      }
      # }
      if (showGrid) {
        lShapes <- c(linesInner, lines)
      } else {
        lShapes <- lines
      }
    }

    if (showPoints) {
      plot <- add_trace(plot,
        x = plotDataSegments$x, y = plotDataSegments$y,
        type = "scatter", mode = "markers", name = "",
        showlegend = FALSE, color = "rgb(255, 255, 255)",
        text = sapply(1:nrow(simGrid), function(i) paste(paste0(colnames(simGrid), ":", simGrid[i, ]), collapse = ", ")),
        marker = list(
          color = "rgb(255, 255, 255)",
          size = 7,
          line = list(
            color = "rgb(0, 0, 0)",
            width = 2
          )
        )
      )
    }

    if (showConfidence) {
      if (!is.null(sources)) {
        # outer
        allEllipsePoints <- unique(do.call("rbind", ellipsesAll))
        if (hull == "alpha convex hull") {
          hulls <- ashape(x = unique(signif(allEllipsePoints, 6)), alpha = alpha)
          convHull <- hulls$edges
        } else {
          hulls <- chull(x = allEllipsePoints)
          convHull <- allEllipsePoints[c(hulls[length(hulls)], hulls), ]
        }

        # dists <- rowSums((convHull %>% diff)^2)
        # minDist <- min(sort(dists, decreasing = TRUE)[1:length(sources)])
        # outerPoints <- convHull[sort(c(which(dists >= minDist), which(dists >= minDist) + 1)),]

        lineOuter <- list(
          type = "line",
          line = list(color = "grey", dash = "dash"),
          xref = "x",
          yref = "y",
          layer = "below"
        )
        linesOuter <- list()

        if (hull == "alpha convex hull") {
          convHull <- as.data.frame(convHull)
          for (i in 1:(nrow(convHull))) {
            lineOuter[["x0"]] <- convHull$x1[i]
            lineOuter[["x1"]] <- convHull$x2[i]
            lineOuter[c("y0")] <- convHull$y1[i]
            lineOuter[c("y1")] <- convHull$y2[i]
            linesOuter <- c(linesOuter, list(lineOuter))
          }
        } else {
          convHull <- as.data.frame(convHull)
          for (i in 1:(nrow(convHull) - 1)) {
            lineOuter[["x0"]] <- convHull$x[i]
            lineOuter[["x1"]] <- convHull$x[i + 1]
            lineOuter[c("y0")] <- convHull$y[i]
            lineOuter[c("y1")] <- convHull$y[i + 1]
            linesOuter <- c(linesOuter, list(lineOuter))
          }
        }


        # for (i in 1:length(sources)) {
        #   lineOuter[["x0"]] <- outerPoints[2 * i - 1, 1]
        #   lineOuter[["x1"]] <- outerPoints[2 * i, 1]
        #   lineOuter[c("y0")] <- outerPoints[2 * i - 1, 2]
        #   lineOuter[c("y1")] <- outerPoints[2 * i, 2]
        #   linesOuter <- c(linesOuter, list(lineOuter))
        # }
        lShapes <- c(lShapes, linesOuter)
      }
      if (!is.null(sources)) {
        plot <- layout(plot, shapes = lShapes)
      }


      for (i in 1:nrow(means)) {
        plot <- add_trace(plot,
          x = ellipses[[i]][, 1], y = ellipses[[i]][, 2],
          type = "scatter", mode = "lines", name = "",
          showlegend = FALSE, color = I(plotData$colorsPlot[i])
        )
      }

      if (!is.null(userDefinedSim)) {
        for (i in 1:nrow(meansUser)) {
          plot <- add_trace(plot,
            x = ellipsesUser[[i]][, 1], y = ellipsesUser[[i]][, 2],
            type = "scatter", mode = "lines", name = "",
            showlegend = FALSE, color = I(userData$colorsPlot[i])
          )
        }
      }

      if (showIndividuals == TRUE) {
        if (!is.null(simSources)) {
          individualDataPlot <- plotDataAll[!(plotDataAll$name %in% rownames(means)) &
            !(plotDataAll$name %in% covariates) &
            !(plotDataAll$name %in% rownames(meansUser)), ]
        } else {
          individualDataPlot <- plotDataAll[!(plotDataAll$name %in% covariates), ]
        }
        sds <- targetErrors[rownames(targetErrors) %in% individualDataPlot$name, , drop = FALSE]
        if (!is.null(targetErrors) || fruitsObj$modelOptions$obsvnDist$default != "multivariate-normal") {
          ellipsesInd <- lapply(1:nrow(individualDataPlot), function(i) {
            car::ellipse(unlist(individualDataPlot[i, c("x", "y")]),
              shape = diag(sds[i, ] + 1E-5, 2)^2 + 1E-4,
              sqrt(qchisq(1 - (1 - confidence), 2)), draw = FALSE
            )
          })
        }
        else {
          covs <- fruitsObj$data$obsvnCov[rownames(targetErrors) %in% individualDataPlot$name]
          ellipsesInd <- lapply(1:nrow(individualDataPlot), function(i) {
            car::ellipse(unlist(individualDataPlot[i, c("x", "y")]),
              shape = covs[[i]] + diag(1E-5, 2),
              sqrt(qchisq(1 - (1 - confidence), 2)), draw = FALSE
            )
          })
        }
        for (i in 1:nrow(individualDataPlot)) {
          plot <- add_trace(plot,
            x = ellipsesInd[[i]][, 1], y = ellipsesInd[[i]][, 2],
            type = "scatter", mode = "lines", name = "",
            showlegend = FALSE, color = I(individualDataPlot$colorsPlot[i])
          )
        }
      }
      if (!is.null(covariates)) {
        covDataPlot <- plotDataAll[plotDataAll$name %in% covariates, ]

        sds <- do.call("rbind", lapply(1:length(individualCovList), function(x) {
          sqrt(apply(
            targetErrors[individualCovList[[x]], , drop = FALSE],
            2, function(y) (sum(y^2) / length(y)^2)
          ) +
            apply(
              targetValues[individualCovList[[x]], , drop = FALSE],
              2, function(y) {
                if (length(y) > 1) {
                  var(y)
                } else {
                  return(0)
                }
              }
            ))
        }))
        ellipsesCov <- lapply(
          1:nrow(covDataPlot),
          function(i) {
            car::ellipse(unlist(covDataPlot[i, c("x", "y")]),
              shape = diag(sds[i, ] + 1E-5, 2)^2 + 1E-4,
              sqrt(qchisq(1 - (1 - confidence), 2)), draw = FALSE
            )
          }
        )
        for (i in 1:nrow(covDataPlot)) {
          plot <- add_trace(plot,
            x = ellipsesCov[[i]][, 1], y = ellipsesCov[[i]][, 2],
            type = "scatter", mode = "lines", name = "",
            showlegend = FALSE, color = I(covDataPlot$colorsPlot[i])
          )
        }
      }
    }
    plot <- plot %>% layout(showlegend = showLegend)
    if (legendInside) {
      plot <- plot %>% layout(legend = list(x = 0.025, y = 0.975))
    }
    return(plot)
  }
  #### 3D ####
  if (plot3D == 1) {
    plotData <- data.frame(
      y = round(means[, 2], 3),
      x = round(means[, 1], 3),
      z = round(means[, 3], 3),
      colorsPlot = colorsPlot,
      name = rownames(means)
    )
    plotDataAll <- plotData
    plotDataAll$name <- factor(plotDataAll$name, levels = unique(plotDataAll$name))
    plotDataAll <- plotDataAll %>% arrange(.data$name)
    if (!is.null(simSources)) {
      plotDataSegments <- data.frame(y = round(meansAll[, 2], 3), x = round(meansAll[, 1], 3), z = round(meansAll[, 3], 3))
    }

    if (showConfidence) {
      if (!is.null(simSources)) {
        ellipses <- lapply(1:nrow(plotData), function(i) {
          rgl::ellipse3d(
            centre = unlist(plotData[i, c("x", "y", "z")]),
            x = covs[[i]],
            level = confidence
          )
        })
        ellipsesAll <- lapply(1:nrow(plotDataSegments), function(i) {
          rgl::ellipse3d(
            centre = unlist(plotDataSegments[i, c("x", "y", "z")]),
            x = covsAll[[i]],
            level = confidence
          )
        })
      } else {
        ellipses <- lapply(1:nrow(plotData), function(i) {
          rgl::ellipse3d(
            centre = unlist(plotData[i, c("x", "y", "z")]),
            x = covs[[i]],
            level = confidence
          )
        })
      }
    }


    if (showIndividuals == TRUE) {
      individualData <- data.frame(
        z = targetValues[, targets[3]],
        y = targetValues[, targets[2]],
        x = targetValues[, targets[1]],
        name = rownames(targetValues),
        colorsPlot = rep("#d3d3d3", NROW(targetErrors))
      )
      plotDataAll <- rbind(plotData, individualData)
      plotDataAll$name <- factor(plotDataAll$name, levels = unique(plotDataAll$name))
      plotDataAll <- plotDataAll %>% arrange(.data$name)

      IndividualAnnotations <-
        lapply(1:nrow(individualData), function(i) {
          list(
            yref = "y",
            x = individualData$x[i],
            y = individualData$y[i],
            z = individualData$z[i],
            xanchor = "right",
            text = as.character(individualData$name[i]),
            font = list(
              family = "Arial",
              size = 16,
              color = "rgba(67,67,67,1)"
            ),
            showarrow = FALSE
          )
        })
    }

    if (!is.null(userDefinedSim)) {
      colsUser <- colorRampPalette(brewer.pal(max(3, min(8, nrow(meansUser))), "Set3"))(nrow(meansUser))
      userData <- data.frame(
        y = round(meansUser[, 2], 3), x = round(meansUser[, 1], 3),
        z = round(meansUser[, 3], 3),
        name = rownames(meansUser),
        colorsPlot = colsUser
      )
      plotDataAll <- rbind(plotDataAll, userData)

      sourceAnnotationsUser <- lapply(1:nrow(userData), function(i) {
        list(
          yref = "y",
          x = userData$x[i],
          y = userData$y[i],
          z = userData$z[i],
          xanchor = "right",
          text = as.character(userData$name[i]),
          font = list(
            family = "Arial",
            size = 16,
            color = "rgba(67,67,67,1)"
          ),
          showarrow = FALSE
        )
      })

      if (showConfidence) {
        ellipsesUser <- lapply(1:nrow(userData), function(i) {
          rgl::ellipse3d(
            centre = unlist(userData[i, c("x", "y", "z")]),
            x = covsUser[[which(names(covsUser) == userData$name[i])]],
            level = confidence
          )
        })
      }
    }

    if (!is.null(covariates)) {
      covData <- do.call("rbind", lapply(
        1:length(individualCovList),
        function(x) {
          data.frame(
            z = mean(targetValues[individualCovList[[x]], targets[3]]),
            y = mean(targetValues[individualCovList[[x]], targets[2]]),
            x = mean(targetValues[individualCovList[[x]], targets[1]]),
            name = covariates[x],
            colorsPlot = rep("darkgrey", 1)
          )
        }
      ))
      plotDataAll <- rbind(plotDataAll, covData)
      plotDataAll$name <- factor(plotDataAll$name, levels = unique(plotDataAll$name))
      plotDataAll <- plotDataAll %>% arrange(.data$name)
    }

    plot <- plot_ly(
      data = plotDataAll, x = ~x, y = ~y, z = ~z,
      type = "scatter3d", mode = "markers", text = ~name,
      showlegend = showLegend, name = ~name, color = I(plotDataAll$colorsPlot)
    ) %>%
      layout(
        title = "",
        scene = list(
          xaxis = list(title = colnames(means)[1]),
          yaxis = list(title = colnames(means)[2]),
          zaxis = list(title = colnames(means)[3])
        )
      )

    annotations <- list()
    if (showTargetNames & showIndividuals) {
      annotations <- IndividualAnnotations
    }

    if (!is.null(userDefinedSim)) {
      annotations <- c(annotations, sourceAnnotationsUser)
    }

    if (!is.null(sources)) {
      SourceAnnotations <-
        lapply(1:nrow(plotData), function(i) {
          list(
            yref = "y",
            x = plotData$x[i],
            y = plotData$y[i],
            z = plotData$z[i],
            xanchor = "right",
            text = as.character(plotData$name[i]),
            font = list(
              family = "Arial",
              size = 16,
              color = "rgba(67,67,67,1)"
            ),
            showarrow = FALSE
          )
        })
      annotations <- c(annotations, SourceAnnotations)

      # plotDataSegments <- plotData[match(sources[c(3, 1:3, 1)], plotData$name), ]
      plotDataSegmentsHull <- plotDataSegments[c(nrow(plotDataSegments), 1:nrow(plotDataSegments), 1), ]

      # plotDataSegmentsHull <- plotData[match(sources, plotData$name), ]
      # plotDataSegmentsHull <- plotDataSegmentsHull[combn(1:nrow(plotDataSegmentsHull), 2) %>% as.vector(), ]
      lines <- list()
      # line <- list(
      #   type = "line",
      #   line = list(color = "grey"),
      #   xref = "x",
      #   yref = "y"
      # )
      #
      # #triangle
      #
      # for (i in 1:(nrow(plotDataSegmentsHull) - 1)) {
      #   line[["x0"]] <- plotDataSegmentsHull$x[i]
      #   line[["x1"]] <- plotDataSegmentsHull$x[i + 1]
      #   line[c("y0")] <- plotDataSegmentsHull$y[i]
      #   line[c("y1")] <- plotDataSegmentsHull$y[i + 1]
      #   line[c("z0")] <- plotDataSegmentsHull$z[i]
      #   line[c("z1")] <- plotDataSegmentsHull$z[i + 1]
      #
      #   lines <- c(lines, list(line))
      # }

      # for(i in 1:length(lines)){
      # data <- data.frame(x = c(lines[[i]]$x0, lines[[i]]$x1),
      #                    y = c(lines[[i]]$y0, lines[[i]]$y1),
      #                    z = c(lines[[i]]$z0, lines[[i]]$z1),
      #                    name = c("a", "a"),
      #                    coloursPlot = c("grey"))

      # plot <- add_trace(plot,
      #                   data = data,
      #                   x = ~x,
      #                   y = ~y,
      #                   z = ~z,
      #                   mode = 'lines',
      #                   color = ~ coloursPlot,
      #                   line = list(color = 'darkgrey', width = 1), text = NULL,
      #                   showlegend = FALSE)
      # }


      linesInner <- list()
      dists <- as.matrix(dist(simGrid, method = "maximum"))
      dists[col(dists) > row(dists)] <- 1
      minDist <- min(dists[dists > 0])
      lineInner <- list(
        type = "line",
        line = list(color = "blue"),
        xref = "x",
        yref = "y",
        zref = "z",
        opacity = 0.3
      )

      for (j in 1:(nrow(plotDataSegments) - 1)) {
        index <- which(abs(dists[, j] - minDist) < 0.01)
        if (length(index) > 0) {
          for (k in index) {
            # if(length(sources) == 3 | (sum(simGrid[j, ] > 0) == 2)  &  (sum(simGrid[j, ] > 0) == 2)){
            lineInner[["x0"]] <- plotDataSegments$x[j]
            lineInner[["x1"]] <- plotDataSegments$x[k]
            lineInner[c("y0")] <- plotDataSegments$y[j]
            lineInner[c("y1")] <- plotDataSegments$y[k]
            lineInner[c("z0")] <- plotDataSegments$z[j]
            lineInner[c("z1")] <- plotDataSegments$z[k]
            linesInner <- c(linesInner, list(lineInner))
            # }
          }
        }
      }
      if (showGrid) {
        lShapes <- c(linesInner, lines)
      } else {
        lShapes <- lines
      }

      if (showPoints) {
        plot <- add_trace(plot,
          x = plotDataSegments$x, y = plotDataSegments$y,
          z = plotDataSegments$z,
          type = "scatter", mode = "markers", name = "",
          showlegend = FALSE, color = "rgb(255, 255, 255)",
          text = sapply(
            1:nrow(simGrid),
            function(i) paste(paste0(colnames(simGrid), ":", simGrid[i, ]), collapse = ", ")
          ),
          marker = list(
            color = "rgb(255, 255, 255)",
            size = 7,
            line = list(
              color = "rgb(0, 0, 0)",
              width = 2
            )
          )
        )
      }

      # inner
      # lineInner <- list(
      #   type = "line",
      #   line = list(color = "lightgrey"),
      #   xref = "x",
      #   yref = "y",
      #   zref = "z"
      # )
      #
      # linesInner <- list()
      # if(length(sources) == 3){
      # for(j in 2:4){
      #   for(k in seq(0.1, 0.9, by = 0.1)){
      #     lineInner[["x0"]] <- plotDataSegments$x[j] +
      #       k * (plotDataSegments$x[j + 1] - plotDataSegments$x[j])
      #     lineInner[["x1"]] <- plotDataSegments$x[j] +
      #       k * (plotDataSegments$x[j - 1] - plotDataSegments$x[j])
      #     lineInner[c("y0")] <- plotDataSegments$y[j] +
      #       k * (plotDataSegments$y[j + 1] - plotDataSegments$y[j])
      #     lineInner[c("y1")] <- plotDataSegments$y[j] +
      #       k * (plotDataSegments$y[j - 1] - plotDataSegments$y[j])
      #     lineInner[c("z0")] <- plotDataSegments$z[j] +
      #       k * (plotDataSegments$z[j + 1] - plotDataSegments$z[j])
      #     lineInner[c("z1")] <- plotDataSegments$z[j] +
      #       k * (plotDataSegments$z[j - 1] - plotDataSegments$z[j])
      #
      #     linesInner <- c(linesInner, list(lineInner))
      #   }
      # }

      for (i in 1:length(linesInner)) {
        data <- data.frame(
          x = c(linesInner[[i]]$x0, linesInner[[i]]$x1),
          y = c(linesInner[[i]]$y0, linesInner[[i]]$y1),
          z = c(linesInner[[i]]$z0, linesInner[[i]]$z1),
          name = c("a", "a"),
          coloursPlot = c("grey")
        )

        plot <- add_trace(plot,
          data = data,
          x = ~x,
          y = ~y,
          z = ~z,
          mode = "lines",
          color = "blue",
          opacity = 0.2,
          layer = "below",
          line = list(color = "blue", width = 1), text = NULL,
          showlegend = FALSE
        )
      }
    }

    if (showConfidence) {
      for (i in 1:nrow(means)) {
        plot <- add_trace(plot,
          x = ellipses[[i]]$vb[1, ], y = ellipses[[i]]$vb[2, ], z = ellipses[[i]]$vb[3, ],
          type = "mesh3d", name = plotData$name[i],
          showlegend = FALSE,
          color = I(plotData$colorsPlot[i]), opacity = 0.15,
          alphahull = 0, inherit = FALSE
        )
      }

      if (!is.null(userDefinedSim)) {
        for (i in 1:nrow(meansUser)) {
          plot <- add_trace(plot,
            x = ellipsesUser[[i]]$vb[1, ], y = ellipsesUser[[i]]$vb[2, ], z = ellipsesUser[[i]]$vb[3, ],
            type = "mesh3d", name = userData$name[i],
            showlegend = FALSE,
            color = I(userData$colorsPlot[i]), opacity = 0.15,
            alphahull = 0, inherit = FALSE
          )
        }
      }

      if (showIndividuals == TRUE & !is.null(simSources)) {
        individualDataPlot <- plotDataAll[!(plotDataAll$name %in% rownames(means)) &
          !(plotDataAll$name %in% covariates) &
          !(plotDataAll$name %in% rownames(meansUser)), ]
        sds <- targetErrors[rownames(targetErrors) %in% individualDataPlot$name, ]
        if (fruitsObj$modelOptions$obsvnDist$default != "multivariate-normal") {
          ellipsesInd <- lapply(1:nrow(individualDataPlot), function(i) {
            rgl::ellipse3d(
              centre = unlist(individualDataPlot[i, c("x", "y", "z")]),
              x = diag(sds[i, ] + 1E-5, 3)^2,
              level = confidence
            )
          })
        }
        else {
          covs <- fruitsObj$data$obsvnCov[rownames(targetErrors) %in% individualDataPlot$name]
          ellipsesInd <- lapply(1:nrow(individualDataPlot), function(i) {
            rgl::ellipse3d(unlist(individualDataPlot[i, c("x", "y", "z")]),
              shape = covs[[i]] + diag(1E-5, 3),
              level = confidence
            )
          })
        }
        for (i in 1:nrow(individualDataPlot)) {
          plot <- add_trace(plot,
            x = ellipsesInd[[i]]$vb[1, ], y = ellipsesInd[[i]]$vb[2, ], z = ellipsesInd[[i]]$vb[3, ],
            type = "mesh3d", name = individualDataPlot$name[i],
            showlegend = FALSE,
            color = I(individualDataPlot$colorsPlot[i]), opacity = 0.15,
            alphahull = 0, inherit = FALSE
          )
        }
      }
      if (!is.null(covariates)) {
        covDataPlot <- plotDataAll[plotDataAll$name %in% covariates, ]

        sds <- do.call("rbind", lapply(1:length(individualCovList), function(x) {
          sqrt(apply(
            targetErrors[individualCovList[[x]], , drop = FALSE],
            2, function(y) (sum(y^2) / length(y)^2)
          ) +
            apply(
              targetValues[individualCovList[[x]], , drop = FALSE],
              2, function(y) {
                if (length(y) > 1) {
                  var(y)
                } else {
                  return(0)
                }
              }
            ))
        }))

        ellipsesInd <- lapply(1:nrow(covDataPlot), function(i) {
          rgl::ellipse3d(
            centre = unlist(covDataPlot[i, c("x", "y", "z")]),
            x = diag(sds[i, ] + 1E-5, 3)^2,
            level = confidence
          )
        })
        for (i in 1:nrow(covDataPlot)) {
          plot <- add_trace(plot,
            x = ellipsesInd[[i]]$vb[1, ], y = ellipsesInd[[i]]$vb[2, ], z = ellipsesInd[[i]]$vb[3, ],
            type = "mesh3d", name = covDataPlot$name[i],
            showlegend = FALSE,
            color = I(covDataPlot$colorsPlot[i]), opacity = 0.15,
            alphahull = 0, inherit = FALSE
          )
        }
      }
    }
    plot <- plot %>% layout(showlegend = showLegend)
    if (legendInside) {
      plot <- plot %>% layout(legend = list(x = 0.025, y = 0.975))
    }

    plot <- plot %>%
      layout(scene = list(annotations = annotations))
    return(plot)
  }
}

prepareSimSourcesExport <- function(simSources) {
  simSources <- lapply(1:length(simSources), function(x) {
    simSources[[x]] <- as.data.frame(simSources[[x]])
    simSources[[x]]$Source <- names(simSources)[x]
    simSources[[x]]
  })
  simSources <- do.call("rbind", simSources)
  simSources
}

getAllCovariateInteractions <- function(covariates, vars = NULL) {
  if (is.null(covariates) || is.null(vars) || all(covariates == "") || all(is.na(covariates))) {
    return(c())
  }
  covariates <- covariates[, vars, drop = FALSE]

  # one way
  ints <- as.character((interaction(covariates)))
  # Full
  if (length(colnames(covariates)) > 1) {
    ints <- c(ints, as.character((interaction(split(
      t(covariates),
      colnames(covariates)
    ),
    drop = TRUE, sep = "-"
    ))))
  }

  # two way
  if (length(colnames(covariates)) > 2) {
    ints <- c(
      ints,
      as.character((unlist(lapply(
        1:ncol(covariates),
        function(x) {
          (interaction(split(
            t(covariates[, -x]),
            colnames(covariates)[-x]
          ),
          drop = TRUE, sep = "-"
          ))
        }
      ))))
    )
  }
  ints
}

simSourcesOutput <- function(simSources) {
  simSourcesData <- do.call("rbind", lapply(1:length(simSources$simSources), function(x) {
    simSources$simSources[[x]] <- as.data.frame(simSources$simSources[[x]])
    simSources$simSources[[x]]$name <- names(simSources$simSources[x])
    simSources$simSources[[x]]
  }))
  if (!is.null(simSources$userDefinedSim)) {
    userData <- do.call("rbind", lapply(1:length(simSources$userDefinedSim), function(x) {
      simSources$userDefinedSim[[x]] <- as.data.frame(simSources$userDefinedSim[[x]])
      simSources$userDefinedSim[[x]]$name <- names(simSources$userDefinedSim[x])
      simSources$userDefinedSim[[x]]
    }))
    simSourcesData <- rbind(simSourcesData, userData)
  }
  simSourcesData
}
