modelDiagnosticsPlotUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    mainPanel(
      width = 8,
      plotOutput(outputId = ns("DiagnosticsPlot")),
      plotExportButton(ns("exportDiagnosticsPlot")),
      exportDataUI(ns("exportDataDiag"), "Export Data"),
      exportDataUI(ns("exportDataChainsAll"), "Export all chains")
    ),
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = ns("estTypeDiag"),
        label = "Select group of estimates",
        choices = c("Sources", "User Estimates")
      ),
      selectInput(
        inputId = ns("groupTypeDiag"),
        label = "Display by:",
        choices = c("Estimate" = "Estimate", "Target" = "Target")
      ),
      selectInput(
        inputId = ns("filterTypeDiag"),
        label = "Select category:",
        choices = NULL
      ),
      # conditionalPanel(
      #   condition = "input.estTypeDiag == 'Source contributions by proxy' || input.estTypeDiag == 'userEstimates'",
      #   ns = ns,
      #   selectInput(inputId = ns("filterType2Diag"),
      #               label = "Select parameter group:",
      #               choices = NULL),
      # ),
      selectInput(
        inputId = ns("individualsDiag"),
        label = "Select filter categories:",
        choices = c("")
      ),
      pickerInput(ns("groupVarsDiag"), "Select estimate",
        choices = list(),
        selected = NULL, multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 12
        )
      ),
      selectInput(
        inputId = ns("plotTypeDiag"),
        label = "Select plot type:",
        choices = c("Trace", "AutoCorr")
      ),
      checkboxInput(
        inputId = ns("showLegendDiag"),
        label = "Show Legend",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.plotTypeDiag == 'Histogram'",
        ns = ns,
        sliderInput(
          inputId = ns("histBinsDiag"),
          label = "Nmber of histogram bins", min = 5, max = 200, value = 50
        )
      ),
      selectInput(
        inputId = ns("contributionLimitDiag"),
        label = "Limit contribution axis", choices = c("None", "0-1", "0-100%"),
        selected = "0-1"
      ),
      selectInput(
        inputId = ns("colorPaletteDiag"), label = "Color Palette",
        choices = c("default", RColorBrewer::brewer.pal.info %>% row.names())
      ),
      tags$hr(),
      plotRangesUI(id = ns("plotRangesDiag"), title = "Axis Ranges", titleTag = "strong"),
      actionButton(ns("applyRangesDiag"), "Apply"),
      tags$hr(),
      plotTitlesUI(id = ns("diagPlotTitles"), type = "ggplot"),
      actionButton(ns("applyTitlesDiag"), "Apply")
    )
  )
}


modelDiagnosticsPlot <- function(input, output, session, model, values) {
  plotParams <- reactive({
    list(
      fruitsObj = model()$fruitsObj$data,
      modelResults = values$modelResultSummary,
      estType = input$estTypeDiag,
      groupType = input$groupTypeDiag,
      filterType = input$filterTypeDiag,
      groupVars = input$groupVarsDiag,
      # filterType2 = input$filterType2Diag,
      individual = input$individualsDiag,
      plotType = input$plotTypeDiag,
      showLegend = input$showLegendDiag,
      histBins = input$histBinsDiag,
      contributionLimit = input$contributionLimitDiag,
      colorPalette = input$colorPaletteDiag,
      applyRanges = input$applyRangesDiag,
      applyTitles = input$applyTitlesDiag
    )
  }) %>% debounce(100)

  userRangesDiag <- plotRangesServer("plotRangesDiag",
                                     type = "ggplot",
                                     initRanges = list(xAxis = config()[["plotRange"]],
                                                       yAxis = config()[["plotRange"]]))
  
  ## Plot Function
  plotTitlesDiag <- plotTitlesServer("diagPlotTitles",
                                     type = "ggplot",
                                     availableElements = c("title", "axis"))
  
  plotFunTargetDiagnostics <- reactive({
    logDebug("Entering reactive plotFunTargetDiagnostics")
    validate(validModelOutput(model()))
    function() {
      p <- do.call(
        plotTargets,
        plotParams()
      ) %>%
        shinyTryCatch(errorTitle = "Error during plotting",
                      warningTitle = "Warning during plotting",
                      alertStyle = "shinyalert")
      
      # we need to trigger the update after pressing "Apply", that's why we use the if condition
      if (input$applyRangesDiag > 0) {
        p <- p %>%
          formatRangesOfGGplot(ranges = userRangesDiag)
      }
      
      if (input$applyTitlesDiag > 0) {
        p <- p %>% 
          formatTitlesOfGGplot(text = plotTitlesDiag)
      }
      
      p
    }
  })

  ## Render Plot
  output$DiagnosticsPlot <- renderCachedPlot(
    {
      validate(validModelOutput(model()))
      # we need to catch errors when printing the plot
      # this only works with ggplots when print() is used 
      plotFunTargetDiagnostics()() %>%
        print()
    },
    cacheKeyExpr = {
      plotParams()
    }
  )

  ## Export Plot
  plotExportServer("exportDiagnosticsPlot",
                   plotFun = plotFunTargetDiagnostics,
                   filename = paste0(gsub("-", "", Sys.Date()), "_diagnostics"),
                   initText = plotTitlesDiag,
                   initRanges = userRangesDiag
  )
  
  
  ## Export Data Function
  dataFunTargetDiag <- reactive({
    validate(validModelOutput(model()))
    function() {
      params <- c(
        plotParams(),
        returnType = "data"
      )
      # here only data is returned, no need to format titles or ranges
      do.call(
        plotTargets,
        params
      ) %>%
        shinyTryCatch(errorTitle = "Error during data export",
                      warningTitle = "Warning during data export",
                      alertStyle = "shinyalert")
    }
  })

  ## Export Data
  callModule(exportData, "exportDataDiag", dataFunTargetDiag)

  ## Export all Chains
  expChains <- reactive({
    function() {
      getResultStatistics(
        model()$modelResults$parameters,
        model()$modelResults$userEstimateSamples,
        model()$fruitsObj,
        DT = FALSE,
        agg = FALSE
      )
    }
  })

  callModule(exportData, "exportDataChainsAll", expChains)

  ## Update Input
  observe({
    groupTypChoicesDiag <- c("Estimate")

    if (!is.null(model()) && model()$fruitsObj$modelOptions$modelType != "1") {
      groupTypChoicesDiag <- c(groupTypChoicesDiag, "Target")
    }

    if (!is.null(model())) {
      estTypChoices <- unique(values$modelResultSummary[, "Group"])
    } else {
      estTypChoices <- NULL
    }
    updateSelectInput(session, "estTypeDiag", choices = estTypChoices)

    updateSelectInput(session, "groupTypeDiag", choices = groupTypChoicesDiag)

    observeEvent(input$estTypeDiag, {
      if (input$estTypeDiag %in% c(
        "Source contributions",
        "Component contributions",
        "Source contributions by proxy"
      )) {
        updateSelectInput(session, "contributionLimitDiag", selected = "0-1")
      } else {
        updateSelectInput(session, "contributionLimitDiag", selected = "None")
      }
    })
  })

  observe({
    filterTypeChoicesDiag <- c("Estimate", "Target")
    if (!is.null(model()) && model()$fruitsObj$modelOptions$modelType != "1") {
      if (model()$fruitsObj$modelOptions$hierarchical) {
        filterTypeChoicesDiag <- c(
          filterTypeChoicesDiag,
          colnames(model()$fruitsObj$data$covariates)
        )
      }
    }
    filterTypeChoicesDiag <- filterTypeChoicesDiag[filterTypeChoicesDiag != input$groupTypeDiag]

    updateSelectInput(
      session = session, inputId = "filterTypeDiag",
      choices = filterTypeChoicesDiag
    )
  })

  observe({
    if (input$filterTypeDiag == "Target") {
      updateSelectInput(session,
        "individualsDiag",
        label = "Target",
        choices = unique(c(rownames(model()$fruitsObj$data$obsvn), "all"))
      )
      updatePickerInput(session,
        "groupVarsDiag",
        label = "Select target"
      )
    }

    if (input$filterTypeDiag == "Estimate") {
      parameterNames <- values$modelResultSummary[values$modelResultSummary[, "Group"] ==
        input$estTypeDiag, "Estimate"]

      updateSelectInput(session,
        "individualsDiag",
        label = "Estimate",
        choices = unique(parameterNames)
      )
      updatePickerInput(session,
        "groupVarsDiag",
        label = "Select estimates"
      )
    }

    if (!(input$filterTypeDiag %in% c("Target", "Estimate", ""))) {
      covNames <- c(as.character(unique(values$modelResultSummary[, input$filterType])), "all")
      updateSelectInput(session,
        "individualsDiag",
        label = input$filterTypeDiag,
        choices = unique(covNames)
      )
      updatePickerInput(session,
        "groupVarsDiag",
        label = "Select estimates"
      )
    }
    if (input$filterTypeDiag == input$groupTypeDiag) {
      updateSelectInput(session,
        "individualsDiag",
        choices = "all"
      )
    }
  })
  groupChoices <- reactive({
    as.character(unique(
      values$modelResultSummary[values$modelResultSummary[, "Group"] == input$estTypeDiag, input$groupTypeDiag]
    ))
  })


  observe({
    updatePickerInput(
      session = session, inputId = "groupVarsDiag",
      choices = groupChoices(), selected = groupChoices()
    )
  })
}
