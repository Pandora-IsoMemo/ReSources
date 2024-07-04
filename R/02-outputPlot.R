outputPlotUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    mainPanel(
      width = 8,
      plotOutput(outputId = ns("SourcePlot")),
      plotExportButton(ns("exportSourcePlot")),
      exportDataUI(ns("exportData"), "Export Data"),
      HTML("<br><br>"),
      conditionalPanel(
        condition = "input.plotType == 'BoxPlot'",
        ns = ns,
        actionButton(ns("add_btn"), "Add data point"),
        actionButton(ns("rm_btn"), "Remove data point"),
        # colourInput(ns("col_btn"), "Select colour of data point"),
        # sliderInput(ns("size_btn"), "Size data point", min = 0.1, max = 10, value = 1),
        uiOutput(ns("pointInput"))
      )
    ),
    sidebarPanel(
      style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:82%",
      width = 3,
      selectInput(
        inputId = ns("estType"),
        label = "Select group of estimates",
        choices = c("Sources", "User Estimates")
      ),
      selectInput(
        inputId = ns("groupType"),
        label = "Display by:",
        choices = c("Estimate" = "Estimate", "Target" = "Target")
      ),
      selectInput(
        inputId = ns("filterType"),
        label = "Select category:",
        choices = NULL
      ),
      selectInput(
        inputId = ns("individuals"),
        label = "Select filter categories:",
        choices = c("")
      ),
      pickerInput(
        ns("groupVars"),
        "Select target",
        choices = list(),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 12
        )
      ),
      selectInput(
        inputId = ns("plotType"),
        label = "Select plot type:",
        choices = c(
          "BoxPlot", "KernelDensity",
          "Histogram", "Smooth Line" = "Line"
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.plotType == 'Line'",
        lineSmoothingUI(ns("lineSmoothing"))
      ),
      conditionalPanel(
        condition = "input.plotType == 'Histogram'",
        ns = ns,
        tags$hr(),
        sliderInput(
          inputId = ns("histBins"),
          label = "Number of histogram bins",
          min = 5,
          max = 200,
          value = 50
        ),
        tags$hr()
      ),
      checkboxInput(
        inputId = ns("showLegend"),
        label = "Show legend",
        value = FALSE
      ),
      selectInput(
        inputId = ns("colorPalette"),
        label = "Color Palette",
        choices = c(
          "default",
          "white",
          RColorBrewer::brewer.pal.info %>% row.names()
        )
      ),
      selectInput(
        inputId = ns("contributionLimit"),
        label = "Limit contribution axis",
        choices = c("None", "0-1", "0-100%"),
        selected = "0-1"
      ),
      # removing fontFamily because it has side effects with plotTitles module
      # selectInput(
      #   inputId = ns("fontFamily"),
      #   label = "Font",
      #   selected = NULL,
      #   choices = availableFonts()
      # ),
      #helpText("This will only have an effect on the pdf output"),
      sliderInput(
        inputId = ns("boxQuantile"),
        label = "Box upper quantile",
        value = 0.68,
        min = 0.5,
        max = 0.99,
        step = 0.01
      ),
      sliderInput(
        inputId = ns("whiskerMultiplier"),
        label = "Whiskers coverage interval",
        value = 0.95,
        min = 0.5,
        max = 1,
        step = 0.001
      ),
      tags$hr(),
      plotRangesUI(id = ns("outputPlotRanges"), title = "Axis Ranges"),
      actionButton(ns("applyOutputPlotRanges"), "Apply"),
      tags$hr(),
      plotTitlesUI(id = ns("outputPlotTitles"), type = "ggplot"),
      actionButton(ns("applyOutputPlotTitles"), "Apply")
    )
  )
}

lineSmoothingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
      tags$hr(),
      selectInput(
        inputId = ns("method"),
        label = "Smoothing method",
        choices = c("Linear Model" = "lm",
                    "Locally Weighted Scatterplot Smoothing" = "loess"),
        selected = "lm"
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.method == 'loess'",
        sliderInput(
          inputId = ns("loessSpan"),
          label = "Amount of smoothing (span)",
          min = 0.01,
          max = 1,
          value = 0.75,
          step = 0.01
        )
      ),
      tags$hr()
  )
}

outputPlot <- function(input, output, session, model, values) {
  options(deparse.max.lines = 1)
  pointDat <- reactiveVal({
    data.frame(
      index = numeric(0),
      y = numeric(0),
      group = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    )
  })
  
  observeEvent(model(), ignoreNULL = FALSE, {
    pointDat(
      data.frame(
        index = numeric(0),
        y = numeric(0),
        group = character(0),
        pointSize = numeric(0),
        pointAlpha = numeric(0),
        pointColor = character(0)
      )
    )
  })
  plotParams <- reactive({
    binSize <- NULL
    if (!is.null(input$`exportData-bins`) &&
        input$`exportData-bins` == TRUE) {
      binSize <- input$`exportData-binSize`
    }
    if(input$groupType %in% colnames(model()$fruitsObj$data$covariatesNum)){
      numCov <- TRUE
    } else {
      numCov <- FALSE
    }
    list(
      fruitsObj = model()$fruitsObj$data,
      modelResults = values$modelResultSummary,
      estType = input$estType,
      groupType = input$groupType,
      filterType = input$filterType,
      # filterType2 = input$filterType2,
      groupVars = input$groupVars,
      individual = input$individuals,
      plotType = input$plotType,
      lineSmoothingMethod = input[["lineSmoothing-method"]],
      lineSmoothingSpan = input[["lineSmoothing-loessSpan"]],
      showLegend = input$showLegend,
      histBins = input$histBins,
      binSize = binSize,
      colorPalette = input$colorPalette,
      contributionLimit = input$contributionLimit,
      pointDat = na.omit(pointDat()),
      #fontFamily = input$fontFamily,
      boxQuantile = input$boxQuantile,
      whiskerMultiplier = input$whiskerMultiplier,
      numCov = numCov,
      applyRanges = input$applyOutputPlotRanges,
      applyTitles = input$applyOutputPlotTitles
    )
  }) %>% debounce(100)
  
  userRangesOutputPlot <- plotRangesServer("outputPlotRanges",
                                     type = "ggplot",
                                     initRanges = list(xAxis = config()[["plotRange"]],
                                                       yAxis = config()[["plotRange"]]))
  
  plotTitlesOutputPlot <- plotTitlesServer("outputPlotTitles",
                                           type = "ggplot",
                                           availableElements = c("title", "axis"),
                                           initText = list(plotTitle  = config()[["plotTitle"]],
                                                           xAxisTitle = config()[["plotTitle"]],
                                                           yAxisTitle = config()[["plotTitle"]],
                                                           xAxisText  = config()[["plotText"]],
                                                           yAxisText  = config()[["plotText"]]))
  
  plotFunTarget <- reactive({
    logDebug("Entering reactive plotFunTarget")
    validate(validModelOutput(model()))
    function() {
      params <- c(plotParams())
      p <- do.call(
        plotTargets,
        params
      )
      
      # we need to trigger the update after pressing "Apply", that's why we use the if condition
      if (input$applyOutputPlotRanges >= 0) {
        p <- p %>%
          formatRangesOfGGplot(ranges = userRangesOutputPlot)
      }
      
      if (input$applyOutputPlotTitles >= 0) {
        p <- p %>% 
          formatTitlesOfGGplot(text = plotTitlesOutputPlot)
      }
      
      p
    }
  })
  
  plotExportServer("exportSourcePlot",
                   plotFun = plotFunTarget,
                   filename = paste0(gsub("-", "", Sys.Date()), "_output"),
                   initText = plotTitlesOutputPlot,
                   initRanges = userRangesOutputPlot)
  
  dataFunTarget <- reactive({
    validate(validModelOutput(model()))
    function() {
      params <- c(plotParams(),
                  returnType = "data"
      )
      # here only data is returned, no need to format titles or ranges
      do.call(
        plotTargets,
        params
      )
      
    }
  })
  
  output$SourcePlot <- renderCachedPlot(
    {
      validate(validModelOutput(model()))
      # we need to catch errors when printing the plot
      # this only works with ggplots when print() is used 
      plotFunTarget()() %>%
        print() %>%
        shinyTryCatch(errorTitle = "Error in plot", alertStyle = "shinyalert")
    },
    cacheKeyExpr = {
      plotParams()
    }
  )
  
  
  callModule(exportData, "exportData", dataFunTarget)
  
  observe({
    groupTypChoices <- c("Estimate")
    if (!is.null(model()) &&
        model()$fruitsObj$modelOptions$modelType != "1") {
      groupTypChoices <- c(groupTypChoices, "Target")
      if (model()$fruitsObj$modelOptions$hierarchical) {
        groupTypChoices <-
          c(
            groupTypChoices,
            colnames(model()$fruitsObj$data$covariates)
          )
        groupTypChoices <-
          c(
            groupTypChoices,
            colnames(model()$fruitsObj$data$covariatesNum)
          )
        
        if (NCOL(model()$fruitsObj$data$covariates) > 1) {
          groupTypChoices <- c(groupTypChoices, "covariateInteraction")
        }
      }
    }
    
    if (!is.null(model())) {
      estTypChoices <- unique(values$modelResultSummary[, "Group"])
    } else {
      estTypChoices <- NULL
    }
    updateSelectInput(session, "estType", choices = estTypChoices)
    
    updateSelectInput(session, "groupType", choices = groupTypChoices)
  })
  
  observeEvent(input$estType, {
    contribLimit <- extractContributionLimit(input$estType, userEstimateGroups = values$userEstimateGroups)
    updateSelectInput(session, "contributionLimit", selected = contribLimit)
  })
  
  observe({
    filterTypeChoices <- c("Estimate", "Target")
    if (!is.null(model()) &&
        model()$fruitsObj$modelOptions$modelType != "1") {
      if (model()$fruitsObj$modelOptions$hierarchical) {
        filterTypeChoices <- c(
          filterTypeChoices,
          colnames(model()$fruitsObj$data$covariates)
        )
      }
    }
    filterTypeChoices <-
      filterTypeChoices[filterTypeChoices != input$groupType]
    
    updateSelectInput(
      session = session,
      inputId = "filterType",
      choices = filterTypeChoices
    )
  })
  
  observe({
    if (input$filterType == "Target") {
      updateSelectInput(session,
                        "individuals",
                        label = "Target",
                        choices = unique(c(
                          rownames(model()$fruitsObj$data$obsvn), "all"
                        ))
      )
      updatePickerInput(session,
                        "groupVars",
                        label = "Select target"
      )
    }
    if (input$filterType == "Estimate") {
      parameterNames <-
        values$modelResultSummary[values$modelResultSummary[, "Group"] == input$estType, "Estimate"]
      updateSelectInput(
        session,
        "individuals",
        label = "Estimate",
        choices = unique(parameterNames)
      )
      updatePickerInput(session,
                        "groupVars",
                        label = "Select estimates"
      )
    }
    if (!(input$filterType %in% c("Target", "Estimate", ""))) {
      covNames <-
        c(as.character(unique(values$modelResultSummary[, input$filterType])), "all")
      
      updateSelectInput(
        session,
        "individuals",
        label = input$filterType,
        choices = unique(covNames)
      )
      updatePickerInput(session,
                        "groupVars",
                        label = "Select estimates"
      )
    }
    
    if (input$filterType == input$groupType) {
      updateSelectInput(session,
                        "individuals",
                        choices = "all"
      )
    }
  })
  
  groupChoices <- reactive({
    as.character(unique(values$modelResultSummary[values$modelResultSummary[, "Group"] == input$estType, input$groupType]))
  })
  
  
  observe({
    updatePickerInput(
      session = session,
      inputId = "groupVars",
      choices = groupChoices(),
      selected = groupChoices()
    )
  })
  
  addRow <- function(df) {
    rbind(
      df,
      data.frame(
        index = nrow(df) + 1,
        y = NA,
        group = NA,
        pointColor = "black",
        pointSize = 1,
        pointAlpha = 0.5,
        stringsAsFactors = FALSE
      )
    )
  }
  
  rmRow <- function(df) {
    if (nrow(df) > 0) {
      df[-nrow(df), , drop = FALSE]
    } else {
      df
    }
  }
  
  observeEvent(input$add_btn, {
    pointDat(addRow(pointDat()))
  })
  
  observeEvent(input$rm_btn, {
    pointDat(rmRow(pointDat()))
  })
  
  observe({
    df <- pointDat()
    indices <- df$index
    
    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      groupval <- input[[paste("group", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<-
        if (is.null(pointColor)) {
          "#000000"
        } else {
          pointColor
        }
      df[index, "pointSize"] <<-
        if (is.null(pointSize)) {
          1
        } else {
          pointSize
        }
      df[index, "pointAlpha"] <<-
        if (is.null(pointAlpha)) {
          1
        } else {
          pointAlpha
        }
      df[index, "y"] <<- if (is.null(yval)) {
        NA
      } else {
        yval
      }
      df[index, "group"] <<- if (is.null(groupval)) {
        NA
      } else {
        groupval
      }
    })
    
    pointDat(df)
  })
  
  inputGroup <- reactive({
    createPointInputGroup(pointDat(), groupChoices = groupChoices(), ns = session$ns)
  })
  
  output$pointInput <- renderUI(inputGroup())
  output$n <- reactive(nrow(pointDat()))
  outputOptions(output, "n", suspendWhenHidden = FALSE)
}

#' Extract contribution limit
#' 
#' @param estType (character) type of estimates, e.g. "Source contributions", 
#'  "Component contributions", ...
#' @param userEstimateGroups (list) list of user estimate groups
extractContributionLimit <- function(estType, userEstimateGroups) {
  # create general pattern
  pattern <- c("Source contributions", "Component contributions", "Source contributions by proxy")
  
  if (length(userEstimateGroups) > 0) {
    # create pattern for user estimates
    userPattern <- sapply(userEstimateGroups, function(x) x$name) %>%
      sprintf(fmt = "User estimate %s")
    
    # filter only normalized user groups
    userPattern <- userPattern[sapply(userEstimateGroups, function(x) x$normalize)]
    
    # add to general pattern
    pattern <- c(pattern, userPattern)
  }
  
  # create pattern string
  pattern <- paste(pattern, collapse = "|")
  
  # apply pattern
  if (grepl(pattern = pattern, estType)) {
    res <- "0-100%"
  } else {
    res <- "None"
  }
  
  return(res)
}

createPointInputGroup <- function(df, groupChoices, ns) {
  lapply(seq_len(nrow(df)), function(i) {
    createPointInput(
      index = df$index[i],
      y = df$y[i],
      group = df$group[i],
      groupChoices = groupChoices,
      pointColor = df$pointColor[i],
      pointSize = df$pointSize[i],
      pointAlpha = df$pointAlpha[i],
      ns = ns
    )
  })
}

createPointInput <- function(index,
                             y,
                             group,
                             groupChoices,
                             pointColor,
                             pointSize,
                             pointAlpha,
                             ns) {
  tags$div(
    numericInput(
      ns(paste("y", index, sep = "_")),
      paste("y", index, sep = "_"),
      value = y,
      min = 0,
      max = 1,
      step = 0.01
    ),
    selectInput(
      ns(paste("group", index, sep = "_")),
      paste("group", index, sep = "_"),
      selected = group,
      choices = groupChoices
    ),
    colourInput(ns(paste(
      "pointColor", index,
      sep = "_"
    )),
    label = "Choose point color", value = pointColor
    ),
    sliderInput(
      ns(paste("pointSize", index, sep = "_")),
      label = "Point size",
      value = pointSize,
      min = 0.1,
      max = 5,
      step = 0.1
    ),
    sliderInput(
      ns(paste("pointAlpha", index, sep = "_")),
      label = "Point alpha",
      value = pointAlpha,
      min = 0.1,
      max = 1,
      step = 0.1
    )
  )
}

# currently disabled feature
# availableFonts <- function() {
#   intersect(names(postscriptFonts()), names(pdfFonts()))
# }
