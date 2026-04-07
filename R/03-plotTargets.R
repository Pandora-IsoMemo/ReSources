plotTargets <- function(fruitsObj, modelResults, individual, estType = "Source contributions",
                        groupType = "Parameter", filterType = "",
                        groupVars = "", 
                        plotType = "BoxPlot", 
                        lineSmoothingMethod = "loess",
                        lineSmoothingSpan = 0.75,
                        returnType = "plot",
                        showLegend = FALSE, colorPalette = "default",
                        contributionLimit = "None",
                        histBins = 50,
                        binSize = NULL,
                        whiskerMultiplier = 0.95, boxQuantile = 0.68,
                        show_mean = FALSE, show_median = FALSE,
                        numCov = FALSE, ...) {
  if (length(groupVars) == 0 && numCov == FALSE) {
    return(NULL)
  }
  
  if (numCov == TRUE) {
    numVars <- matrix(t(apply(fruitsObj$covariatesNum, 1, function(r)r*attr(fruitsObj$covariatesNum,'scaled:scale') + attr(fruitsObj$covariatesNum, 'scaled:center'))), ncol = ncol(fruitsObj$covariatesNum))
    colnames(numVars) <- colnames(fruitsObj$covariatesNum)
    numCols <- cbind(Target = rownames(fruitsObj$covariatesNum), numVars[, groupType, drop = F] %>% as.data.frame)
    modelResults <- left_join(modelResults, numCols, by = "Target")
  }
  
  modelResults <- modelResults[modelResults[, 1] == estType, ]
  filterRows <- which(individual == modelResults[, filterType])
  if (filterType != "all" & length(filterRows) > 0 & filterType != groupType) {
    modelResults <- modelResults[filterRows, ]
  }
  if (length(groupVars) != 0) {
    modelResults <- modelResults[modelResults[, groupType] %in% groupVars, ]
  }
  modelResults$group <- factor(modelResults[, groupType], levels = unique(modelResults[, groupType]))
  modelResults <- modelResults[, c("estimate", "group")]
  if (contributionLimit == "0-100%") {
    modelResults <- modelResults %>% mutate(estimate = .data$estimate * 100)
  }
  
  # default header
  headerLabel <- ""
  if (filterType != "Parameter" & estType == "Source contributions") {
    headerLabel <- getTeaser(fruitsObj, individual, filterType)
  }

  if (returnType == "data") {
    # return data ----
    if (!is.null(binSize)) {
      sequence <- seq(floor(100 * min(modelResults$estimate)) / 100,
        floor(100 * (max(modelResults$estimate) + binSize)) / 100,
        by = binSize
      )

      modelResults$bin <- cut(modelResults$estimate,
        breaks = sequence, include.lowest = TRUE
      )

      modelResults <- modelResults %>%
        group_by(.data$group, .data$bin) %>%
        summarise(nSamples = n()) %>%
        ungroup()
    }
    return(modelResults)
  }

  if (plotType == "BoxPlot") {
    # return BoxPlot ----

    # default labels
    xlabel <- groupType
    if (contributionLimit == "0-100%") {
      ylabel <- "contribution (%)"
    } else {
      ylabel <- "contribution"
    }

    logDebug("Calculating summary statistics for boxplot")
    logDebug(
      "Overall quantile for box: [%s, %s]",
      quantile(modelResults$estimate, probs = (1 - boxQuantile) / 2),
      quantile(modelResults$estimate, probs = 1 - ((1 - boxQuantile) / 2))
    )
    logDebug(
      "Overall quantile for whiskers: [%s, %s]",
      quantile(modelResults$estimate, probs = (1 - whiskerMultiplier) / 2),
      quantile(modelResults$estimate, probs = 1 - ((1 - whiskerMultiplier) / 2))
    )
    hdi_box <- HDInterval::hdi(modelResults$estimate, credMass = boxQuantile)
    logDebug("Overall HDI for box: [%s, %s]", hdi_box[1], hdi_box[2])
    hdi_whisker <- HDInterval::hdi(modelResults$estimate, credMass = whiskerMultiplier)
    logDebug("Overall HDI for whiskers: [%s, %s]", hdi_whisker[1], hdi_whisker[2])

    dataSummary <- modelResults %>%
      group_by(.data$group) %>%
      summarise(
        # sd = sd(.data$estimate),
        median = median(.data$estimate),
        meanEst = mean(.data$estimate),
        q68 = quantile(.data$estimate, (1 - (1 - boxQuantile) / 2)),
        q32 = quantile(.data$estimate, (1 - boxQuantile) / 2),
        q95 = quantile(.data$estimate, 1 - ((1 - whiskerMultiplier) / 2)),
        q05 = quantile(.data$estimate, (1 - whiskerMultiplier) / 2),
        hdi_box = list(HDInterval::hdi(.data$estimate, credMass = boxQuantile)),
        hdi_whisker = list(HDInterval::hdi(.data$estimate, credMass = whiskerMultiplier)),
        box_lower = hdi_box[[1]][1],
        box_upper = hdi_box[[1]][2],
        whisker_lower = hdi_whisker[[1]][1],
        whisker_upper = hdi_whisker[[1]][2],
        .groups = "drop"
      ) %>%
      mutate(
        hdi_box = NULL,
        hdi_whisker = NULL
      ) %>%
      ungroup()
    if (colorPalette == "white") {
      p <- ggplot(dataSummary, aes(x = .data$group)) +
        ylab(ylabel) +
        xlab(xlabel)
    } else {
      p <- ggplot(dataSummary, aes(x = .data$group, fill = .data$group)) +
        ylab(ylabel) +
        xlab(xlabel)
    }

    p <- p + geom_boxplot(
      mapping = aes(
        # quantiles for boxplot:
        # lower = .data$q32,
        # upper = .data$q68,
        # middle = .data$median,
        # ymin = .data$q05,
        # ymax = .data$q95
        # hdi for boxplot:
        lower = .data$box_lower,
        upper = .data$box_upper,
        middle = .data$box_lower,  # hidden via fatten = 0
        ymin = .data$whisker_lower,
        ymax = .data$whisker_upper
      ),
      stat = "identity",
      fatten = 0
    )

    if (show_mean) {
      p <- p + geom_errorbar(
        aes(ymin = .data$meanEst, ymax = .data$meanEst), linetype = "solid", data = dataSummary
      )
    }

    if (show_median) {
      p <- p + geom_errorbar(
        aes(ymin = .data$median, ymax = .data$median), linetype = "dotted", data = dataSummary
      )
    }

    if (contributionLimit == "0-100%") {
      p <- p + ylim(c(0, 100))
    }
    if (contributionLimit == "0-1") {
      p <- p + ylim(c(0, 1))
    }

    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPaletteRamp <- colorRampPalette(colorPalette)
      p <- p + scale_fill_manual(values = colorPaletteRamp(p$data$group %>% unique() %>% length()))
    }
  }
  
  if (plotType == "KernelDensity") {
    # return KernelDensity ----
    
    # default labels
      if (contributionLimit == "0-100%") {
        xlabel <- "contribution (%)"
      } else {
        xlabel <- "contribution"
      }
      ylabel <- "density"
    
    p <- ggplot(modelResults, aes(x = .data$estimate, fill = .data$group)) +
      geom_density(alpha = 0.3) +
      ylab(ylabel) +
      xlab(xlabel)
    if (contributionLimit == "0-100%") {
      p <- p + xlim(c(0, 100))
    }
    if (contributionLimit == "0-1") {
      p <- p + xlim(c(0, 1))
    }
    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_fill_manual(values = colorPalette(p$data$group %>% unique() %>% length()))
    }
  }
  
  if (plotType == "Line") {
    # return Line ----
    
    # default labels
      if (contributionLimit == "0-100%") {
        xlabel <- "contribution (%)"
      } else {
        xlabel <- "contribution"
      }
      ylabel <- "mean"
    
    dataSummary <- modelResults %>%
      group_by(.data$group) %>%
      summarise(
        # sd = sd(.data$estimate),
        median = median(.data$estimate),
        meanEst = mean(.data$estimate),
        q68 = quantile(.data$estimate, boxQuantile),
        q95 = quantile(.data$estimate, 1 - ((1 - whiskerMultiplier) / 2)),
        q32 = quantile(.data$estimate, 1 - boxQuantile),
        q05 = quantile(.data$estimate, (1 - whiskerMultiplier) / 2),
      ) %>%
      ungroup()
    dataSummary$group <- as.numeric(dataSummary$group)
    
    if (nrow(dataSummary) < 4 && lineSmoothingMethod != "lm") { # before we had nrow(dataSummary) < 7 for "lm"
      method = "lm"
      warning("Too few groups to use loess smoothing. Using linear regression instead.")
    } else {
      method = lineSmoothingMethod
    }
    
    p <- ggplot(dataSummary, aes(x = .data$group, y = .data$meanEst)) + 
      geom_point() + 
      suppressWarnings(geom_smooth(method = method, span = lineSmoothingSpan)) +
      ylab(ylabel) + 
      xlab(xlabel)
    
    if (contributionLimit == "0-100%") {
      p <- p + ylim(c(0, 100))
    }
    if (contributionLimit == "0-1") {
      p <- p + ylim(c(0, 1))
    }
  }
  
  if (plotType == "Histogram") {
    # return Histogram ----
    
    # default labels
      if (contributionLimit == "0-100%") {
        xlabel <- "contribution (%)"
      } else {
        xlabel <- "contribution"
      }
      ylabel <- "number of observations"
    
    p <- ggplot(modelResults, aes(x = .data$estimate, fill = .data$group)) +
      geom_histogram(alpha = 0.5, binwidth = NULL, bins = histBins, position = "identity") +
      ylab(ylabel) +
      xlab(xlabel)
    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_fill_manual(values = colorPalette(p$data$group %>% unique() %>% length()))
    }
  }
  if (plotType == "Trace") {
    # return Trace ----
    
    # default labels
      xlabel <- ""
      ylabel <- "value"
    
    modelResults$X <- rep(
      1:(nrow(modelResults) / length(unique(modelResults$group))),
      length(unique(modelResults$group))
    )
    p <- ggplot(modelResults, aes(x = .data$X, y = .data$estimate, colour = .data$group)) +
      geom_line(alpha = 0.65) +
      ylab(ylabel) +
      xlab(xlabel)

    if (contributionLimit == "0-100%") {
      p <- p + ylim(c(0, 100))
    }
    if (contributionLimit == "0-1") {
      p <- p + ylim(c(0, 1))
    }

    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_color_manual(values = colorPalette(p$data$group %>% unique() %>% length()))
    }
  }
  if (plotType == "AutoCorr") {
    # return AutoCorr ----
    
    # default labels
      xlabel <- "lag"
      ylabel <- "autocorrelation"
    
    acfData <- split(modelResults, modelResults$group)
    acfDataNew <- gather(bind_rows(lapply(acfData, function(x) {
      acf(x$estimate, lag.max = 50, plot = FALSE)$acf
    })), key = "estimate", value = "Value")
    acfDataNew$X <- rep(
      1:(nrow(acfDataNew) / length(acfData)),
      length(acfData)
    )

    p <- ggplot(acfDataNew, aes(x = .data$X, y = .data$Value, colour = .data$estimate)) +
      geom_line(alpha = 0.65) +
      ylab(ylabel) +
      xlab(xlabel)
    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_color_manual(values = colorPalette(p$data$estimate %>% unique() %>% length()))
    }
  }
  
  # default title
    p <- p + labs(title = headerLabel) +
      theme(plot.title = element_text(hjust = 0.5))
  
  if (!showLegend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}

getTeaser <- function(data, individual, filterType) {
  if (filterType == "Target") {
    if (individual != "all") {
      means <- data$obsvn[rownames(data$obsvn) == individual, ]
      sds <- data$obsvnError[rownames(data$obsvn) == individual, ]
    } else {
      means <- colMeans(data$obsvn)
      sds <- signif(sqrt(colMeans(data$obsvnError^2)) +
        sqrt(colMeans(scale(data$obsvn, center = TRUE, scale = FALSE)^2)), 3)
    }
  } else {
    if (individual != "all") {
      if (filterType %in% names(data$covariates)) {
        individuals <- which(data$covariates[, filterType] == individual)
      } else {
        individuals <- which(rownames(data$covariates) == individual)
      }
    } else {
      individuals <- 1:NROW(data$covariates)
    }
    means <- colMeans(data$obsvn[individuals, , drop = FALSE])
    sds <- signif(sqrt(colMeans(data$obsvnError[individuals, , drop = FALSE]^2)) +
      sqrt(colMeans(scale(data$obsvn[individuals, , drop = FALSE], center = TRUE, scale = FALSE)^2)), 3)
  }
  teaserText <- paste0(individual, ": ")
  for (i in 1:ncol(data$obsvn)) {
    teaserText <- paste0(teaserText, colnames(data$obsvn)[i], " = ", means[i], " (", sds[i], ") ; ")
  }
  teaserText
}
