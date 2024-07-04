#' Server Function for the FRUITS Module / Tab
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoMemoData data from IsoMemo App
#' @param isoDataExport data to export to IsoMemo App
#'
#' @export
fruitsTab <- function(input,
                      output,
                      session,
                      isoMemoData = function() {
                        list(data = NULL, event = NULL)
                      },
                      isoDataExport = function() {
                        list(data = NULL, event = NULL)
                      }) {
  ns <- session$ns
  
  values <- do.call(
    "reactiveValues",
    defaultValues()
  )
  
  events <-
    reactiveValues(
      name = list(),
      processed = 0,
      processedCache = 0,
      removeName = NULL,
      copyField = "",
      adaptive = FALSE
    )
  
  ## remove names
  observeEvent(input$removeName, {
    logDebug("Entering observeEvent(input$removeName)")
    events$removeName <- input$removeName
  })
  
  observe({
    logDebug("Entering observe(events)")
    if (events$processed == events$processedCache) {
      events$name <- list()
      logDebug("Resetting events")
    } else {
      invalidateLater(500)
      events$processedCache <- events$processed
    }
  })
  
  ## Reset Input ----
  uploadedNotes <- reactiveVal()
  observeEvent(input$reset, {
    logDebug("Entering observeEvent(input$reset)")
    vars <- defaultValues()
    
    for (name in names(vars)) {
      values[[name]] <- vars[[name]]
    }
    
    values$status <- values$statusSim <- "INITIALIZE"
    values$reset <- runif(1)
    events$name <- list()
    uploadedNotes(character(0))
  })
  
  ## Load Example Model
  # observeEvent(input$exampleModel,
  #              {
  #                logDebug("Entering observeEvent(input$exampleModel)")
  #                
  #                values$status <- values$statusSim <- "INITIALIZE"
  #                
  #                if (input$exampleData == "Five Sources Data") {
  #                  vars <- readRDS("exampleModels/Five_Sources_Data.rds")
  #                }
  #                if (input$exampleData == "Brown Bear Data") {
  #                  vars <- readRDS("exampleModels/bear.rds")
  #                }
  #                if (input$exampleData == "Black Bear Data") {
  #                  vars <- readRDS("exampleModels/blackBear.rds")
  #                }
  #                if (input$exampleData == "Roman Data") {
  #                  vars <- readRDS("exampleModels/Roman.rds")
  #                }
  #                
  #                for (name in names(vars)) {
  #                  values[[name]] <- vars[[name]]
  #                }
  #                
  #                values$reset <- runif(1)
  #              },
  #              priority = 500
  # )
  

  # Download/Upload Model ----
  model <- reactiveVal(NULL)
  modelUploadBaseFileName <- reactiveVal("")
  downloadModelServer("modelDownload",
                      dat = reactiveVal(NULL),
                      inputs = values,
                      model = model,
                      rPackageName = config()[["rPackageName"]],
                      customFileName = modelUploadBaseFileName,
                      defaultFileName = "model",
                      fileExtension = config()[["fileExtension"]],
                      modelNotes = uploadedNotes,
                      triggerUpdate = reactive(TRUE))

  uploadedValues <- importDataServer(
    "modelUpload",
    title = "Import Model",
    importType = "model",
    ckanFileTypes = config()[["ckanModelTypes"]],
    ignoreWarnings = TRUE,
    defaultSource = config()[["defaultSourceModel"]],
    fileExtension = config()[["fileExtension"]],
    options = importOptions(rPackageName = config()[["rPackageName"]])
  )
  
  observeEvent(uploadedValues(), {
    logDebug("Entering observeEvent(uploadedValues())")
    
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["inputs"]]))
    
    uploadedFileName <- names(uploadedValues())[1] %>%
      file_path_sans_ext()
    modelUploadBaseFileName(uploadedFileName)
    
    uploadedNotes(uploadedValues()[[1]][["notes"]])
    valuesDat <- uploadedValues()[[1]][["inputs"]]
    
    emptyTables <- checkForEmptyTables(valuesDat)
    if (length(emptyTables) > 0) {
      warningInputs <- paste(
        "Following tables contain no values: \n",
        paste0(emptyTables, collapse = ", "),
        " ")
      shinyalert(title = "Empty tables",
                 text = warningInputs,
                 type = "warning")
    }
    
    for (name in names(valuesDat)) {
      values[[name]] <- valuesDat[[name]]
    }
    
    if (ncol(values$targetValuesCovariates) > 0) {
      potentialCat <- extractPotentialCat(values$targetValuesCovariates)
      selectedCatVars <- intersect(values$categoricalVars, potentialCat)
      
      updatePickerInput(session, 
                        inputId = "categoricalVars",
                        choices = potentialCat,
                        selected = selectedCatVars)
      
      potentialNumerics <- extractPotentialNumerics(values$targetValuesCovariates)
      selectedNumVars <- intersect(values$numericVars, potentialNumerics)
      
      updatePickerInput(session,
                        inputId = "numericVars",
                        choices = potentialNumerics,
                        selected = selectedNumVars)
    }
    
    if (!is.null(uploadedValues()[[1]][["model"]])) {
      model(uploadedValues()[[1]][["model"]])
    }
  })
  
  ## status ----
  
  output$status <- renderText(values$status)
  output$statusSim <- renderText(values$statusSim)
  outputOptions(output, "status", suspendWhenHidden = FALSE)
  outputOptions(output, "statusSim", suspendWhenHidden = FALSE)
  
  ## hide Model Diagnostics & Output Tab until model has run
  observeEvent(values$status, {
    logDebug("Entering observeEvent(value$status)")
    switch(values$status,
           COMPLETED = {
             showTab("mainTabs", "resultsReport")
             showTab("mainTabs", "modelDiagnostics")
             showTab("mainTabs", "Output")
             showTab("mainTabs", "isomemo")
           },
           {
             hideTab("mainTabs", "resultsReport")
             hideTab("mainTabs", "modelDiagnostics")
             hideTab("mainTabs", "Output")
             hideTab("mainTabs", "isomemo")
           }
    )
  })
  
  observeEvent({
    values$status 
    input$oxcalCheck
    },{
    logDebug("Entering observeEvent(value$status)")
    checkOxcal <- "FALSE"
    
    if (input$oxcalCheck & values$status == "COMPLETED") {
      checkOxcal <- "COMPLETED"
    }
    switch(checkOxcal,
           COMPLETED = {
             showTab("mainTabs", "Oxcal export")
           },
           {
             hideTab("mainTabs", "Oxcal export")
           }
    )
  })
  
  observeEvent(values$statusSim, {
    logDebug("Entering observeEvent(values$statusSim)")
    switch(values$statusSim,
           COMPLETED = {
             showTab("MCharResults", "sourcePlot")
             showTab("MCharResults", "sourceMixPlot")
             showTab("MCharResults", "zScorePlot")
             showTab("MCharResults", "mahaPlot")
             showTab("MCharResults", "scoreSepTest")
             showTab("MCharResults", "corrPlot")
           },
           {
             hideTab("MCharResults", "sourcePlot")
             hideTab("MCharResults", "sourceMixPlot")
             hideTab("MCharResults", "zScorePlot")
             hideTab("MCharResults", "mahaPlot")
             hideTab("MCharResults", "scoreSepTest")
             hideTab("MCharResults", "corrPlot")
           }
    )
  })
  
  observeEvent(values$modelConcentrations, {
    logDebug("Entering observeEvent(values$modelConcentrations)")
    if (values$modelConcentrations == TRUE) {
      showTab("MCharResults", "concentrationsPlot")
    } else {
      hideTab("MCharResults", "concentrationsPlot")
    }
  })
  
  
  ## Set names: targetNames, fractionNames, sourceNames, obsvnNames, offsetNames, targetValuesCovariatesNames ----
  observe(
    priority = 200,
    {
      logDebug("Entering observe() (set values$xxxNames)")
      
      # update list entries that depend on targetNames
      if (!identical(unique(colnames(values$obsvn[["default"]])), values$targetNames)) {
        isolate({
          newTargetNames <- unique(colnames(values$obsvn[["default"]]))
          if (length(values$targetNames) == length(newTargetNames)) {
            # update names of targets
            values <-
              updateTargetsInLists(values, newTargetNames, updateFun = updateListNames)
          }
          
          if (length(values$targetNames) > length(newTargetNames)) {
            # remove target
            removedTarget <-
              values$targetNames[!(values$targetNames %in% newTargetNames)]
            values <-
              updateTargetsInLists(values, removedTarget, updateFun = deleteTableFromList)
          }
          
          values$targetNames <- newTargetNames
        })
      }
      
      # update list entries that depend on obsvnNames (rownames of values$obsvn)
      if (!identical(unique(rownames(values$obsvn[["default"]])), values$obsvnNames)) {
        isolate({
          oldObsvnNames <- values$obsvnNames
          newObsvnNames <- unique(rownames(values$obsvn[["default"]]))
          # always push new obsvn names to values
          values$obsvnNames <- newObsvnNames
          
          # if modelType %in% c(1, 2, 4) there is only one element for all rows -> no update
          # of list elements
          if (values$modelType %in% c(3, 5)) { # that is, if is baseline model
            if (length(oldObsvnNames) == length(newObsvnNames)) {
              # update names of obsvns
              values <-
                updateObsvnsInLists(values, newObsvnNames, updateFun = updateListNames)
            }
            
            if (length(oldObsvnNames) > length(newObsvnNames)) {
              # remove obsvn
              removedObsvn <-
                oldObsvnNames[!(oldObsvnNames %in% newObsvnNames)]
              values <-
                updateObsvnsInLists(values, removedObsvn, updateFun = deleteTableFromList)
            }
          } 
        })
      }
      
      # first, keep old value
      newValue <- values$fractionNames
      
      # check fractionNames
      if (input$modelWeights) { # "Include components" == TRUE
        if (input$modelConcentrations) {
          if (length(values$concentration) > 0) {
            newValue <- unique(colnames(values$concentration[[1]]))
          } # reset if no length??
        } else {
          if (length(values$weights) > 0) {
            newValue <- unique(colnames(values$weights))
          } # reset if no length??
        }
      } else {
        newValue <- values$targetNames
      }
      
      # update fractionNames
      if (!identical(values$fractionNames, newValue)) {
        values$fractionNames <- values$targetNames
      }
      
      # first, keep old value
      newValue <- values$sourceNames
      
      # check sourceNames
      if (input$modelConcentrations) { # "Include concentrations" == TRUE
        if (length(values$concentration) > 0) {
          newValue <- unique(rownames(values$concentration[[1]]))
        } # reset if no length??
      } else if (length(values$source) > 0 && length(values$source[[1]]) > 1) {
        newValue <- unique(rownames(values$source[[1]][[1]][[1]]))
      } # reset if no length??
      
      # update sourceNames
      if (!identical(values$sourceNames, newValue)) {
        values$sourceNames <- newValue
      }
      
      values$offsetNames <- "Offset"
      
      values$targetValuesCovariatesNames <-
        unique(colnames(values$targetValuesCovariates))
  })
  
  ## Data options ----
  # observeEvent(input$adaptiveNames, {
  #   logDebug("Entering observeEvent(input$adaptiveNames)")
  #   events$adaptive <- input$adaptiveNames
  # })
  
  termChoices <- reactive({
    c(
      "Default term" = "default",
      "Add term 1" = "term1",
      "Add term 2" = "term2",
      "Add term 3" = "term3"
    )
  })
  
  targetValuesServer("targetVals",
                     values = values,
                     events = events,
                     termChoices = termChoices,
                     modelType = reactive(input$modelType))
  
  output$targetOffset <- eventReactive(values$targetOffset,
                                       values$targetOffset)
  outputOptions(output, "targetOffset", suspendWhenHidden = FALSE)
  
  output$targetValuesShowCovariates <- eventReactive(values$targetValuesShowCovariates,
                                                     values$targetValuesShowCovariates)
  outputOptions(output, "targetValuesShowCovariates", suspendWhenHidden = FALSE)
  
  output$targetValuesShowCoordinates <- eventReactive(values$targetValuesShowCoordinates,
                                                      values$targetValuesShowCoordinates)
  outputOptions(output, "targetValuesShowCoordinates", suspendWhenHidden = FALSE)
  
  componentsServer("components",
                   values = values,
                   events = events)
  
  sourcesServer("sources",
                values = values,
                events = events,
                termChoices = termChoices)
  
  concentrationsServer("concentration",
                       values = values,
                       events = events
                       )
  
  ## -- from IsoMemo
  observeEvent(isoMemoData()$event, {
    logDebug("Entering observeEvent(isoMemoData()$event)")
    events$isoMemo <- isoMemoData()$data
  })
  
  ## MySql table contents ----
  callModule(dbContentSelect, "popUpTables")
  
  ## Model options ----
  observeEvent(values$modelType, {
    logDebug("Entering observeEvent(values$modelType)")
    
    updateRadioButtons(session, "modelType",
                       selected = values$modelType
    )
  })
  
  observe({
    logDebug("Entering observe() (updatePickerInput(categoricalVars, numericVars))")
    if (values$targetValuesShowCovariates) {
      if (ncol(values$targetValuesCovariates) > 0) {
        potentialCat <- extractPotentialCat(values$targetValuesCovariates)
        selectedCatVars <- intersect(values$categoricalVars, potentialCat)
        
        updatePickerInput(session,
                          inputId = "categoricalVars",
                          choices = potentialCat,
                          selected = selectedCatVars)
        
        potentialNumerics <- extractPotentialNumerics(values$targetValuesCovariates)
        selectedNumVars <- intersect(values$numericVars, potentialNumerics)
        
        updatePickerInput(session,
                          inputId = "numericVars",
                          choices = potentialNumerics,
                          selected = selectedNumVars)
      }
    }
  }) %>%
    bindEvent(values$targetValuesCovariates)
  
  observeEvent(input$categoricalVars, {
    logDebug("Entering observeEvent(input$categoricalVars)")
    if (!identical(input$categoricalVars, values$categoricalVars) &
        ncol(values$targetValuesCovariates) > 0) {
      values$categoricalVars <- input$categoricalVars
      potentialNumerics <- extractPotentialNumerics(values$targetValuesCovariates)
      
      values$numericVars <-
        values$numericVars[!(potentialNumerics %in% values$categoricalVars)]
      
      if (!identical(input$numericVars, values$numericVars)) {
        updatePickerInput(session, "numericVars", selected = values$numericVars)
      }
    }
  })
  
  observeEvent(input$numericVars, {
    logDebug("Entering observeEvent(input$numericVars)")
    if (!identical(input$numericVars, values$numericVars) &
        ncol(values$targetValuesCovariates) > 0) {
      values$numericVars <- input$numericVars
      potentialCat <- extractPotentialCat(values$targetValuesCovariates)
      
      values$categoricalVars <-
        values$categoricalVars[!(potentialCat %in% values$numericVars)]
      
      if (!identical(input$categoricalVars, values$categoricalVars)) {
        updatePickerInput(session, "categoricalVars", selected = values$categoricalVars)
      }
    }
  })
  
  observeEvent(values$targetValuesShowCovariates, {
    logDebug("Entering observeEvent(values$targetValuesShowCovariates)")
    if (values$targetValuesShowCovariates == FALSE) {
      updateCheckboxInput(session, "useSite", value = FALSE)
    }
    
    if (values$targetValuesShowCovariates &&
        !is.null(input$modelType) && input$modelType == "1") {
      selected <- "2"
    } else {
      selected <- input$modelType
    }
    updateRadioButtons(session, "modelType", selected = selected)
  })
  
  observeEvent(values$includeSourceOffset, {
    logDebug("Entering observeEvent(values$includeSourceOffset)")
    updateCheckboxInput(session,
                        "includeSourceOffset",
                        value = values$includeSourceOffset
    )
  })
  
  observeEvent(input$includeSourceOffset, {
    logDebug("Entering observeEvent(input$includeSourceOffset)")
    if (!identical(input$includeSourceOffset, values$includeSourceOffset)) {
      values$includeSourceOffset <- input$includeSourceOffset
    }
  })
  
  observeEvent(values$modelWeights, {
    logDebug("Entering observeEvent(values$modelWeights)")
    if (values$modelWeights == TRUE) {
      showTab(
        inputId = "mainTabs",
        target = "Components",
        session = session
      )
    } else {
      hideTab(
        inputId = "mainTabs",
        target = "Components",
        session = session
      )
    }
    
    updateCheckboxInput(session, "modelWeights",
                        value = values$modelWeights
    )
  })
  
  observeEvent(input$modelWeights, priority = 300, {
    logDebug("Entering observeEvent(input$modelWeights)")
    
    sourceMatrixOld <- sourceMatrixNew <- values$source[[1]][[1]][[1]]
    if (input$modelWeights && !values$modelWeights) {
      values$fractionNames <- paste0("fraction_", 1:ncol(sourceMatrixNew))
      colnames(sourceMatrixNew) <- values$fractionNames
      values$weights <-
        emptyMatrix(values$targetNames, values$fractionNames)
      values$weightsUncert <-
        emptyMatrix(values$targetNames, values$fractionNames)
      events$name <- c(
        events$name,
        createNameEvents(
          sourceMatrixOld,
          sourceMatrixNew,
          "sourceNames",
          "fractionNames"
        )
      )
    }
    
    if (!input$modelWeights && values$modelWeights) {
      # values$fractionNamesCache <- values$fractionNames
      colnames(sourceMatrixNew) <-
        values$targetNames[1:ncol(sourceMatrixNew)]
      events$name <- c(
        events$name,
        createNameEvents(
          sourceMatrixOld,
          sourceMatrixNew,
          "sourceNames",
          "fractionNames"
        ),
        createNameEvents(
          sourceMatrixOld,
          sourceMatrixNew,
          "sourceNames",
          "targetNames"
        )
      )
    }
    
    values$modelWeights <- input$modelWeights
  })
  
  observeEvent(values$modelWeightsContrained, {
    logDebug("Entering observeEvent(values$modelWeightsContrained)")
    updateCheckboxInput(session,
                        "modelWeightsContrained",
                        value = values$modelWeightsContrained
    )
  })
  
  observeEvent(input$modelWeightsContrained, {
    logDebug("Entering observeEvent(input$modelWeightsContrained)")
    values$modelWeightsContrained <- input$modelWeightsContrained
  })
  
  observeEvent(values$modelConcentrationsContrained, {
    logDebug("Entering observeEvent(values$modelConcentrationsContrained)")
    updateCheckboxInput(
      session,
      "modelConcentrationsContrained",
      value = values$modelConcentrationsContrained
    )
  })
  
  observeEvent(input$modelConcentrationsContrained, {
    logDebug("Entering observeEvent(input$modelConcentrationsContrained)")
    values$modelConcentrationsContrained <-
      input$modelConcentrationsContrained
  })
  
  observeEvent(input$optimalPrior, {
    logDebug("Entering observeEvent(input$optimalPrior)")
    values$optimalPrior <-
      input$optimalPrior
  })
  
  observeEvent(values$modelConcentrations, {
    logDebug("Entering observeEvent(values$modelConcentrations)")
    updateCheckboxInput(session,
                        "modelConcentrations",
                        value = values$modelConcentrations
    )
  })
  
  observeEvent(input$modelConcentrations, {
    logDebug("Entering observeEvent(input$modelConcentrations)")
    values$modelConcentrations <- input$modelConcentrations
  })
  
  observeEvent(values$modelConcentrations, {
    logDebug("Entering observeEvent(values$modelConcentrations)")
    if (values$modelConcentrations == TRUE) {
      showTab(
        inputId = "mainTabs",
        target = "Concentrations",
        session = session
      )
    } else {
      hideTab(
        inputId = "mainTabs",
        target = "Concentrations",
        session = session
      )
    }
  })
  
  
  observeEvent(values$burnin, {
    logDebug("Entering observeEvent(values$burnin)")
    updateNumericInput(session, "burnin", value = values$burnin)
  })
  
  observeEvent(input$burnin, {
    logDebug("Entering observeEvent(input$burnin)")
    if (!identical(input$burnin, values$burnin)) {
      values$burnin <- input$burnin
    }
  })
  
  defaultAlphaHyper <- reactiveVal()
  observe({
    logDebug("Entering observeEvent(values$alphaHyper)")
    if (length(values$alphaHyper) == 1 && length(values$sourceNames) > 1) {
      # catch case of depricated alphaHyper (single numeric value for all sources)
      # when e.g. loading an older model
      newValues <- getAlphaHyperVec(sourceNames = values$sourceNames,
                                    singleAlphaHyper = values$alphaHyper)
    } else {
      # update to new values from values$alphaHyper
      # when e.g. loading a recent model
      newValues <- values$alphaHyper
    }
    
    req(!identical(unname(defaultAlphaHyper()), unname(newValues)), 
        any(defaultAlphaHyper() != newValues))
    
    defaultAlphaHyper(newValues)
  }) %>%
    bindEvent(values$alphaHyper)
  
  observe({
    # reset values to "1" only if the number of food sources is changed
    req(length(defaultAlphaHyper()) != length(values$sourceNames)) 
    logDebug("Entering update (defaultAlphaHyper)")
    
    newValues <- getAlphaHyperVec(sourceNames = values$sourceNames)
    defaultAlphaHyper(newValues)
  }) %>%
    bindEvent(values$sourceNames)
  
  alphaHyperReactive <- vectorInputServer("alphaHyper", defaultInputs = defaultAlphaHyper)
  
  observe({
    logDebug("Entering observeEvent(input$alphaHyper)")
    req(!identical(unname(alphaHyperReactive()), unname(values$alphaHyper)))
    
    values$alphaHyper <- alphaHyperReactive()
  }) %>%
    bindEvent(alphaHyperReactive())
  
  observeEvent(values$oxcalCheck, {
    logDebug("Entering observeEvent(values$oxcalCheck)")
    
    updateRadioButtons(session, "oxcalCheck",
                       selected = values$oxcalCheck
    )
  })
  
  observeEvent(input$oxcalCheck, {
    logDebug("Entering observeEvent(input$oxcalCheck)")
    values$oxcalCheck <- input$oxcalCheck
  })
  
  observeEvent(values$covariateType, {
    logDebug("Entering observeEvent(input$covariateType)")
    if (!identical(input$covariateType, values$covariateType)) {
      updateRadioButtons(session, "covariateType", selected = values$covariateType)
    }
  })
  
  
  observeEvent(input$covariateType, {
    logDebug("Entering observeEvent(input$covariateType)")
    if (!identical(input$covariateType, values$covariateType)) {
      values$covariateType <- input$covariateType
    }
  })
  
  
  observeEvent(values$inflatedBeta, {
    logDebug("Entering observeEvent(values$inflatedBeta)")
    updateRadioButtons(session, "inflatedBeta", selected = values$inflatedBeta)
  })
  
  observeEvent(input$inflatedBeta, {
    logDebug("Entering observeEvent(input$inflatedBeta)")
    if (!identical(input$inflatedBeta, values$inflatedBeta)) {
      values$inflatedBeta <- input$inflatedBeta
    }
  })
  
  
  observeEvent(values$iterations, {
    logDebug("Entering observeEvent(values$iterations)")
    updateNumericInput(session, "iterations", value = values$iterations)
  })
  
  observeEvent(input$iterations, {
    logDebug("Entering observeEvent(input$iterations)")
    if (!identical(input$iterations, values$iterations)) {
      values$iterations <- input$iterations
    }
  })
  
  observeEvent(values$thinning, {
    logDebug("Entering observeEvent(values$thinning)")
    updateNumericInput(session, "thinning", value = values$thinning)
  })
  
  observeEvent(input$thinning, {
    logDebug("Entering observeEvent(input$thinning)")
    if (!identical(input$thinning, values$thinning)) {
      values$thinning <- input$thinning
    }
  })
  
  observeEvent(values$nchains, {
    logDebug("Entering observeEvent(values$nchains)")
    updateNumericInput(session, "nchains", value = values$nchains)
  })
  
  observeEvent(input$nchains, {
    logDebug("Entering observeEvent(input$nchains)")
    if (!identical(input$nchains, values$nchains)) {
      values$nchains <- input$nchains
    }
  })

  ## File Notes
  # observeEvent(input$showFileNotes, {
  #   logDebug("Entering observeEvent(input$showFileNotes)")
  #   showModal(fileNotesDialog(id = ns("fileNotes"), value = values$fileNotes))
  # })
  
  # observeEvent(input$fileNotes, {
  #   logDebug("Entering observeEvent(input$fileNotes)")
  #   values$fileNotes <- input$fileNotes
  # })
  
  ## Priors ----
  priorWarning <- reactiveValues(text = NULL)
  output$priorWarning <- renderText({
    priorWarning$text
  })
  
  observeEvent(input$newPrior, {
    logDebug("Entering observeEvent(input$newPrior)")
    priorWarning$text <- NULL
  })
  
  observeEvent(input$minUnc, {
    logDebug("Entering observeEvent(input$minUnc)")
    values$minUnc <- input$minUnc
  })
  
  observeEvent(values$minUnc, {
    logDebug("Entering observeEvent(values$minUnc)")
    updateNumericInput(session, "Unc", value = values$minUnc)
  })
  
  observeEvent(input$addPrior, {
    logDebug("Entering observeEvent(input$addPrior)")
    ## validate
    if (validatePrior(input$newPrior)) {
      if (input$addUnc) {
        updatePriorInput(session, "priors",
                         value = c(
                           input$priors,
                           paste0(input$newPrior, "+{", input$Unc, "}")
                         )
        )
        updateCheckboxInput(session, "addUnc", value = FALSE)
      } else {
        updatePriorInput(session,
                         "priors",
                         value = c(input$priors, input$newPrior)
        )
      }
      updateTextInput(session, "newPrior", value = "")
    } else {
      priorWarning$text <- "Prior validation failed"
    }
  })
  
  observeEvent(input$priors, {
    logDebug("Entering observeEvent(input$priors)")
    values$priors <- input$priors
    })
  
  observe({
    logDebug("Entering observe(update priors)")
    updatePriorInput(session, "priors", value = values$priors)
  })
  
  observe({
    logDebug("Entering observe(update priorsSource)")
    updateSelectInput(session, "priorSource",
                      choices = values$sourceNames
    )
    
    updateSelectInput(session,
                      "priorProxies",
                      choices = apply(
                        expand.grid(values$targetNames, values$sourceNames),
                        1,
                        paste,
                        collapse = "-"
                      )
    )
    
    updateSelectInput(session, "priorOffset",
                      choices = values$targetNames
    )
    
    updateSelectInput(session,
                      "priorConcentration",
                      choices = apply(
                        expand.grid(values$sourceNames, values$fractionNames),
                        1,
                        paste,
                        collapse = "-"
                      )
    )
    
    updateSelectInput(session, "priorSourceFractions",
                      choices = values$fractionNames
    )
    
    updateSelectInput(session,
                      "priorProxyValues",
                      choices = apply(
                        expand.grid(
                          values$sourceNames,
                          values$fractionNames,
                          values$targetNames
                        ),
                        1,
                        paste,
                        collapse = "-"
                      )
    )
    
    updateSelectInput(session,
                      "priorWeightValues",
                      choices = apply(
                        expand.grid(values$targetNames, values$fractionNames),
                        1,
                        paste,
                        collapse = "-"
                      )
    )
    
    updateSelectInput(session,
                      "priorConsumerValues",
                      choices = apply(
                        expand.grid("Consumer", values$targetNames),
                        1,
                        paste,
                        collapse = "-"
                      )
    )
    updateSelectInput(
      session,
      "priorHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$sourceNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "priorHierarchicalValuesBeta",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$fractionNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session,
      "priorHierarchicalValuesTheta",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$targetNames,
        values$sourceNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "priorProxyHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$sourceNames,
        values$fractionNames,
        values$targetNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "priorConsumerHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$targetNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "priorConcentrationHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$sourceNames,
        values$fractionNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "priorWeightHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$targetNames,
        values$fractionNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    
    
  })
  
  # User estimates
  userEstimateWarning <- reactiveValues(text = NULL)
  output$userEstimateWarning <- renderText({
    userEstimateWarning$text
  })
  
  observeEvent(input$newUserEstimate, {
    logDebug("Entering observeEvent(input$newUserEstimate)")
    userEstimateWarning$text <- NULL
  })
  
  observeEvent(input$addUserEstimate, {
    logDebug("Entering observeEvent(input$addUserEstimate)")
    newEstimate <-
      paste0(input$userEstimateName, "=", input$newUserEstimate)
    ok <- TRUE
    if (grepl(" ", input$userEstimateName)) {
      userEstimateWarning$text <-
        "User estimate name contains white space. Please remove or replace with '_'."
      ok <- FALSE
    }
    if (!grepl("[^0-9]", substr(input$userEstimateName, 1, 1))) {
      userEstimateWarning$text <-
        "User estimate name should not start with number"
      ok <- FALSE
    }
    
    if (grepl("_", input$userEstimateName)) {
      userEstimateWarning$text <-
        "User estimate name should not contain underscores: '_'"
      ok <- FALSE
    }
    
    if (ok) {
      if (validateUserEstimate(newEstimate, input$userEstimate)) {
        updatePriorInput(session,
                         "userEstimate",
                         value = c(input$userEstimate, newEstimate)
        )
        updateTextInput(session, "newUserEstimate", value = "")
      } else {
        userEstimateWarning$text <-
          "User estimate validation failed, did you try to assign multiple user estimates to the same name or used special characters in the user estimate name?"
      }
    }
  })
  
  observeEvent(
    input$userEstimate, {
      logDebug("Entering observeEvent(input$userEstimate)")
      values$userEstimate <- input$userEstimate
    }
  )
  observe({
    logDebug("Entering observe(update userEstimates)")
    updatePriorInput(session, "userEstimate", value = values$userEstimate)
    })
  
  observe({
    logDebug("Entering observe(update userEstimateSource)")
    updateSelectInput(session, "userEstimateSource",
                      choices = values$sourceNames
    )
    updateSelectInput(session,
                      "userEstimateProxies",
                      choices = apply(
                        expand.grid(values$targetNames, values$sourceNames),
                        1,
                        paste,
                        collapse = "-"
                      )
    )
    
    updateSelectInput(session,
                      "userEstimateSourceFractions",
                      choices = values$fractionNames
    )
    
    updateSelectInput(
      session,
      "userEstimateProxyValues",
      choices = apply(
        expand.grid(
          values$sourceNames,
          values$fractionNames,
          values$targetNames
        ),
        1,
        paste,
        collapse = "-"
      )
    )
    
    updateSelectInput(session, "userEstimateOffset",
                      choices = values$targetNames
    )
    
    updateSelectInput(
      session,
      "userEstimateConcentration",
      choices = apply(
        expand.grid(values$sourceNames, values$fractionNames),
        1,
        paste,
        collapse = "-"
      )
    )
    
    updateSelectInput(
      session,
      "userEstimateWeightValues",
      choices = apply(
        expand.grid(values$targetNames, values$fractionNames),
        1,
        paste,
        collapse = "-"
      )
    )
    
    updateSelectInput(
      session,
      "userEstimateConsumerValues",
      choices = apply(
        expand.grid("Consumer", values$targetNames),
        1,
        paste,
        collapse = "-"
      )
    )
    
    updateSelectInput(
      session,
      "userEstimateHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$sourceNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session,
      "userEstimateHierarchicalValuesBeta",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$fractionNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session,
      "userEstimateHierarchicalValuesTheta",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$targetNames,
        values$sourceNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    
    updateSelectInput(
      session = session,
      inputId = "userEstimateProxyHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$sourceNames,
        values$fractionNames,
        values$targetNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "userEstimateConsumerHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$targetNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "userEstimateConcentrationHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$sourceNames,
        values$fractionNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    updateSelectInput(
      session = session,
      inputId = "userEstimateWeightHierarchicalValues",
      choices = getAllMainInteractions(
        values$targetValuesCovariates,
        values$targetNames,
        values$fractionNames,
        vars = values$categoricalVars
      ) %>% nullToEmptyList()
    )
    
  })

  userEstimateNames <- reactive({
    gsub("([^=])=.*", "\\1", input$userEstimate)
  })
  
  # User Estimate Groups
  userEstimateGroups <-
    callModule(
      userEstimateGroup,
      "userEstimateGroup",
      userEstimates = userEstimateNames,
      groupsInitial = reactive(values$userEstimateGroups)
    )
  
  observeEvent(userEstimateGroups(), {
    logDebug("Entering observeEvent(userEstimateGroups())")
    values$userEstimateGroups <- userEstimateGroups()
  })
  
  ## Run model ----
  observeEvent(input$run, {
    logDebug("Entering observeEvent(input$run)")
    values$status <- "RUNNING"
    
    model(NULL)
    modelCharacteristics(NULL)
    valuesList <- reactiveValuesToList(values)
    
    fruitsObj <- shinyInputToClass(
        valuesList,
        as.list(input$priors),
        as.list(input$userEstimate)
      ) %>%
      shinyTryCatch(errorTitle = "Could not create model object: ", alertStyle = "shinyalert")
    
    if (is.null(fruitsObj)) {
      values$status <- "ERROR"
      return()
    }
    
    # check user estimates groups
    userEstimatesGroupNames <-
      sapply(values$userEstimateGroups, function(x) {
        x$name
      })
    userEstimatesGroupEst <-
      sapply(values$userEstimateGroups, function(x) {
        x$estimates
      })
    if (any(sapply(userEstimatesGroupEst, function(x) {
      is.null(x)
    }))) {
      alert("User estimate group estimates are empty.")
      values$status <- "ERROR"
      return()
    }
    if (any(userEstimatesGroupNames == "")) {
      alert("Empty user estimate group names.")
      values$status <- "ERROR"
      return()
    }
    
    if (any(grepl(" ", userEstimatesGroupNames))) {
      alert("User estimate group names contain blank characters.")
      values$status <- "ERROR"
      return()
    }
    
    if (any(!grepl("[^0-9]", substr(userEstimatesGroupNames, 1, 1)))) {
      alert("User estimate group names should not begin with numbers")
      values$status <- "ERROR"
      return()
    }
    
    if (any(grepl("_", userEstimatesGroupNames))) {
      alert("User estimate group names should not contain underscore: '_'")
      values$status <- "ERROR"
      return()
    }
    
    # end check user estimates groups
    
    updateSelectInput(session, "exportUserEstimates",
                      choices = as.vector(sapply(input$userEstimate, function(x) {
                        strsplit(x, "=")[[1]][1]
                      }))
    )
    if (length(fruitsObj$userEstimates[[1]]) > 0) {
      updateRadioButtons(
        session,
        "exportType",
        choices = c(
          "Proxy" = "proxy",
          "Source contributions" = "Source contributions",
          "Component contributions" = "Component contributions",
          "Source contributions by proxy" = "Source contributions by proxy",
          "User Estimates" = "userEstimates"
        )
      )
    } else {
      updateRadioButtons(
        session,
        "exportType",
        choices = c(
          "Proxy" = "proxy",
          "Source contributions" = "Source contributions",
          "Component contributions" = "Component contributions",
          "Source contributions by proxy" = "Source contributions by proxy"
        )
      )
    }
    
    withProgress({
      modelResults <- compileRunModel(
        fruitsObj,
        progress = TRUE,
        userDefinedAlphas = values$userDefinedAlphas,
        onlyShowNimbleInput = input$onlyShowNimbleInput
      ) %>%
        shinyTryCatch(errorTitle = "Could not run model", alertStyle = "shinyalert")
    },
    value = 0,
    message = "")
    
    if (is.null(modelResults)) {
      values$status <- "ERROR"
      return()
    }
    
    values$status <- "COMPLETED"
    
    # nimble in here <--  ----
    if (input$onlyShowNimbleInput) {
      # update fruits object after final data preparation
      fruitsObj$data <- modelResults$data
      fruitsObj$constants <- modelResults$constants
      fruitsObj$modelCode <- modelResults$code
      # return only fruits object since there are no real model results
      model(list(fruitsObj = fruitsObj))
      return()
    }
    
    if (!inherits(modelResults, "try-error")) {
      withProgress({
        setProgress(message = "Check convergence", value = 0.85)
        if (any(is.nan(modelResults$parameters) |
                any(is.na(modelResults$parameters)) |
                any(is.infinite(modelResults$parameters)))) {
          alert(
            "Model produced NA or Inf values, please check your data. Introducing or increasing uncertainties might help to mitigate the problem."
          )
          values$status <- "ERROR"
          return()
        } else {
          diagnostic <-
            convergenceDiagnostics(modelResults$parameters, fruitsObj)$geweke[[1]] %>%
            shinyTryCatch(errorTitle = "Could not create Diagnostics", alertStyle = "shinyalert")
          if (any(is.nan(diagnostic[which(grepl("alpha", names(diagnostic)))])) |
              any(is.na(diagnostic[which(grepl("alpha", names(diagnostic)))])) |
              any(is.infinite(diagnostic[which(grepl("alpha", names(diagnostic)))]))) {
            alert(
              "Model produced constant source contribution values, please check your model if this is reasonable,
        otherwise try to rerun the model with more chains or more iterations. If this doesn't help, please check your data.
        Introducing or increasing uncertainties might help to mitigate the problem."
            )
            diagnostic[is.na(diagnostic)] <- 0
            return()
          }
          outText <- produceOutText(fruitsObj, diagnostic) %>%
            shinyTryCatch(errorTitle = "Could not create output", alertStyle = "shinyalert")
        }
      })
      
      withProgress({
        setProgress(message = "Compute summary statistics", value = 0.95)
        model(list(fruitsObj = fruitsObj, modelResults = modelResults))
        values$modelResultSummary <- getResultStatistics(
          model()$modelResults$parameters,
          model()$modelResults$userEstimateSamples,
          model()$fruitsObj,
          DT = FALSE,
          agg = FALSE
        ) %>%
          shinyTryCatch(errorTitle = "Could not compute statistics", alertStyle = "shinyalert")
        values$status <- "COMPLETED"
      })
      
      if (values$status == "COMPLETED") {
        showModal(
          modalDialog(
            title = "Model computation completed ",
            HTML(outText),
            easyClose = FALSE,
            footer = modalButton("Close")
          )
        )
      }
    }
  })
  
  modelCharacteristics <- reactiveVal(NULL)
  
  observeEvent(input$runModelChar, {
    logDebug("Entering observeEvent(input$runModelChar)")
    values$statusSim <- "RUNNING"
    modelCharacteristics(NULL)
    
    valuesList <- reactiveValuesToList(values)
    if (valuesList[["modelType"]] == "1") {
      valuesList[["modelType"]] <- "2"
    }
    
    fruitsObj <- shinyInputToClass(
          valuesList,
          as.list(input$priors),
          as.list(input$userEstimate)
        ) %>%
      shinyTryCatch(errorTitle = "Could not create model object: ", alertStyle = "shinyalert")
    
    if (is.null(fruitsObj)) {
      values$status <- "ERROR"
      return()
    }
    
    withProgress({
      modelResults <- compileRunModel(
        fruitsObj,
        progress = TRUE,
        onlySim = TRUE,
        userDefinedAlphas = values$userDefinedAlphas,
        seqSim = 1 / input$seqSim,
        simSourceNames = input$simSpecSources
      ) %>%
        shinyTryCatch(errorTitle = "Could not run model", alertStyle = "shinyalert")
    },
    value = 0,
    message = "")
    
    if (is.null(modelResults)) {
      values$statusSim <- "ERROR"
      return()
    }
    
    values$statusSim <- "COMPLETED"
    if (any(is.nan(modelResults$simSources$simSources[[1]]) |
            any(is.na(
              modelResults$simSources$simSources[[1]]
            )) |
            any(is.infinite(
              modelResults$simSources$simSources[[1]]
            )))) {
      alert(
        "Simulation produced NA or Inf values, please check your data. Introducing or increasing uncertainties might help to mitigate the problem."
      )
      values$statusSim <- "ERROR"
      return()
    }
    if (values$statusSim == "COMPLETED") {
      modelCharacteristics(list(fruitsObj = fruitsObj, modelResults = modelResults))
    }
  })
  
  observe({
    logDebug("Entering observe(updatePickerInputs)")
    updatePickerInput(
      session,
      inputId = "targetSelect",
      selected = values$targetNames[1],
      choices = values$targetNames
    )
    
    updatePickerInput(
      session,
      inputId = "simSpecSources",
      selected = values$sourceNames[1:min(5, length(values$sourceNames))],
      choices = values$sourceNames
    )
    
    updatePickerInput(
      session,
      inputId = "concentrationsSelect",
      selected = values$fractionNames[1],
      choices = values$fractionNames
    )
    
    updatePickerInput(
      session,
      inputId = "sourceSelect",
      selected = values$targetNames[1],
      choices = values$targetNames
    )
    
    updatePickerInput(
      session,
      inputId = "sourceSelectMix",
      selected = values$targetNames[1],
      choices = values$targetNames
    )
    
    updatePickerInput(
      session,
      inputId = "characteristicsCovariates",
      selected = NULL,
      choices = unique(
        getAllCovariateInteractions(values$targetValuesCovariates,
                                    vars = values$categoricalVars
        )
      )
    )
    
    updatePickerInput(
      session,
      inputId = "characteristicsCovariatesMix",
      selected = NULL,
      choices = unique(
        getAllCovariateInteractions(values$targetValuesCovariates,
                                    vars = values$categoricalVars
        )
      )
    )
    
    updatePickerInput(
      session,
      inputId = "characteristicsCovariatesTarget",
      selected = NULL,
      choices = unique(
        getAllCovariateInteractions(values$targetValuesCovariates,
                                    vars = values$categoricalVars
        )
      )
    )
  })
  
  observe({
    validate(validModelOutput(modelCharacteristics()))
    logDebug("Entering observe(sourceSelectMix2)")
    
    updatePickerInput(
      session,
      inputId = "sourceSelectMix2",
      selected = modelCharacteristics()$modelResults$simSourceNames[1],
      choices = modelCharacteristics()$modelResults$simSourceNames
    )
  })
  
  plotFunCharacteristicsTarget <- reactive({
    function() {
      req(values$obsvn, values$obsvnError, input$targetSelect)
      req(input$targetSelect %in% values$targetNames)
      
      sourceTargetPlot(
        simSources = NULL,
        simGrid = NULL,
        targets = input$targetSelect,
        fruitsObj = NULL,
        showConfidence = input$showConfidence,
        showLegend = input$showLegend,
        legendInside = input$legendInside,
        confidence = input$confidenceLevel,
        showIndividuals = TRUE,
        horizontalPlot = input$horizontalPlot,
        covariates = input$characteristicsCovariatesTarget,
        covariateValues = values$targetValuesCovariates,
        targetValues = values$obsvn[["default"]][, input$targetSelect, drop = FALSE],
        targetErrors = values$obsvnError[["default"]][, input$targetSelect, drop = FALSE]
      )
    }
  })
  
  plotFunCharacteristicsConc <- reactive({
    function() {
      req(values$concentration, values$concentrationUncert, input$concentrationsSelect)
      req(input$concentrationsSelect %in% values$fractionNames)
      
      sourceTargetPlot(
        simSources = NULL,
        simGrid = NULL,
        targets = NULL,
        fractions = input$concentrationsSelect,
        fruitsObj = NULL,
        horizontalPlot = input$horizontalPlot,
        showConfidence = input$showConfidence,
        showLegend = input$showLegend,
        legendInside = input$legendInside,
        confidence = input$confidenceLevel,
        showIndividuals = FALSE,
        covariates = NULL,
        concentrationValues = values$concentration[[1]][, input$concentrationsSelect, drop = FALSE],
        concentrationErrors = values$concentrationUncert[[1]][, input$concentrationsSelect, drop = FALSE]
      )
    }
  })
  
  
  plotFunCharacteristics <- reactive({
    validate(validModelOutput(modelCharacteristics()))
    function() {
      sourceTargetPlot(
        simSources = modelCharacteristics()$modelResults$simSources$simSources,
        simSourcesAll = modelCharacteristics()$modelResults$simSources$simSourcesAll,
        simGrid = modelCharacteristics()$modelResults$simSources$simGrid,
        horizontalPlot = input$horizontalPlot,
        targets = input$sourceSelect,
        fruitsObj = modelCharacteristics()$fruitsObj,
        showConfidence = input$showConfidence,
        showLegend = input$showLegend,
        legendInside = input$legendInside,
        confidence = input$confidenceLevel,
        showIndividuals = input$showIndividuals,
        showTargetNames = input$showTargetNames,
        covariates = input$characteristicsCovariates,
        userDefinedSim = modelCharacteristics()$modelResults$simSources$userDefinedSim
      )
    }
  })
  
  plotFunCharacteristicsMix <- reactive({
    validate(validModelOutput(modelCharacteristics()))
    function() {
      sourceTargetPlot(
        simSources = modelCharacteristics()$modelResults$simSources$simSources,
        simSourcesAll = modelCharacteristics()$modelResults$simSources$simSourcesAll,
        simGrid = modelCharacteristics()$modelResults$simSources$simGrid,
        horizontalPlot = input$horizontalPlot,
        targets = input$sourceSelectMix,
        fruitsObj = modelCharacteristics()$fruitsObj,
        showConfidence = input$showConfidence,
        showLegend = input$showLegend,
        legendInside = input$legendInside,
        confidence = input$confidenceLevel,
        showIndividuals = input$showIndividualsMix,
        showTargetNames = input$showTargetNamesMix,
        sources = input$sourceSelectMix2,
        covariates = input$characteristicsCovariatesMix,
        userDefinedSim = modelCharacteristics()$modelResults$simSources$userDefinedSim,
        showGrid = input$showGrid,
        showPoints = input$showPoints,
        alpha = input$alpha,
        hull = input$hull
      )
    }
  })
  
  
  callModule(verbatimText,
             "modelCode",
             model = model,
             class = "modelCode"
  )
  callModule(
    verbatimText,
    "modelInputData",
    model = model,
    class = "modelInput",
    type = "data"
  )
  callModule(
    verbatimText,
    "modelInputValueNames",
    model = model,
    class = "modelInput",
    type = "valueNames"
  )
  callModule(
    verbatimText,
    "modelInputModelOptions",
    model = model,
    class = "modelInput",
    type = "modelOptions"
  )
  callModule(
    verbatimText,
    "modelInputPriors",
    model = model,
    class = "modelInput",
    type = "priors"
  )
  callModule(
    verbatimText,
    "modelUserEstimates",
    model = model,
    class = "modelInput",
    type = "userEstimates"
  )
  callModule(
    verbatimText,
    "wAIC",
    model = model,
    class = "wAIC",
    type = "wAIC"
  )
  callModule(
    verbatimText,
    "BIC",
    model = model,
    class = "BIC",
    type = "BIC"
  )
  callModule(
    verbatimText,
    "geweke",
    model = model,
    class = "modelDiagnostics",
    type = "geweke"
  )
  callModule(
    verbatimText,
    "raftery",
    model = model,
    class = "modelDiagnostics",
    type = "raftery"
  )
  callModule(
    verbatimText,
    "heidel",
    model = model,
    class = "modelDiagnostics",
    type = "heidel"
  )
  callModule(
    verbatimText,
    "gelman",
    model = model,
    class = "modelDiagnostics",
    type = "gelman"
  )
  
  callModule(OxCalOutput, "oxcal",
             model = model, 
             exportCoordinates = values$exportCoordinates)
  
  expChains <- reactive({
    validate(validModelOutput(model()))
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
  
  output$pValue <- DT::renderDT({
    validate(validModelOutput(model()))
    model()$modelResults$pValue
  })
  
  output$SummaryResults <- DT::renderDT({
    validate(validModelOutput(model()))
    getResultStatistics(
      model()$modelResults$parameters,
      model()$modelResults$userEstimateSamples,
      model()$fruitsObj,
      statistics = c(
        input$SummaryMin,
        input$SummaryMax,
        input$SummaryMedian,
        input$SummaryQuantileCheck,
        input$SummaryQuantile,
        input$SummaryQuantile2,
        input$BayesianPValuesCheck,
        input$pVal
      )
    )
  })
  
  output$zScores <- DT::renderDT({
    validate(validModelOutput(modelCharacteristics()))
    getZScores(modelCharacteristics()$modelResults$simSources$simSources)
  })
  
  callModule(exportData, "exportZScores", data = reactive({
    function() {
      getZScoresData(modelCharacteristics()$modelResults$simSources$simSources)
    }
  }))
  
  output$mahaDist <- DT::renderDT({
    validate(validModelOutput(modelCharacteristics()))
    getSourceMahaDist(modelCharacteristics()$modelResults$simSources$simSources)
  })
  
  callModule(verbatimText,
             "scoreSep",
             model = modelCharacteristics,
             class = "scoreSep"
  )
  
  output$scoreSep <- DT::renderDT({
    validate(validModelOutput(modelCharacteristics()))
    getSourceScoreSep(modelCharacteristics()$modelResults$simSources$simSources)
  })
  
  callModule(exportData, "exportSimSources", data = reactive({
    function() {
      simSourcesOutput(modelCharacteristics()$modelResults$simSources)
    }
  }))
  
  callModule(exportData, "exportMahaDist", data = reactive({
    function() {
      getSourceMahaDistData(modelCharacteristics()$modelResults$simSources$simSources)
    }
  }))
  
  callModule(verbatimText, "corrMat", model = modelCharacteristics, class = "corrMat")
  
  output$targetPlot <- renderPlotly({
    plotFunCharacteristicsTarget()() %>%
      shinyTryCatch(errorTitle = "Error in plot", alertStyle = "shinyalert")
  })
  
  plotExportServer("exportTargetPlot",
                   plotFun = plotFunCharacteristicsTarget,
                   filename = paste0(gsub("-", "", Sys.Date()), "_sourceCharacteristics"),
                   plotly = TRUE
  )
  
  output$concentrationsPlot <- renderPlotly({
    plotFunCharacteristicsConc()() %>%
      shinyTryCatch(errorTitle = "Error in plot", alertStyle = "shinyalert")
  })
  
  plotExportServer("exportConcentrationsPlot",
                   plotFun = plotFunCharacteristicsConc,
                   filename = paste0(gsub("-", "", Sys.Date()), "_sourceCharacteristics"),
                   plotly = TRUE
  )
  
  output$SourceCharacteristicsPlot <- renderPlotly({
    validate(validModelOutput(modelCharacteristics()))
    plotFunCharacteristics()() %>%
      shinyTryCatch(errorTitle = "Error in plot", alertStyle = "shinyalert")
  })
  
  plotExportServer("exportSourceCharacteristicsPlot",
                   plotFun = plotFunCharacteristics,
                   filename = paste0(gsub("-", "", Sys.Date()), "_sourceCharacteristics"),
                   plotly = TRUE
  )
  
  # observeEvent(input$updateMix, {
  output$SourceCharacteristicsPlot2 <- renderPlotly({
    validate(validModelOutput(modelCharacteristics()))
    plotFunCharacteristicsMix()() %>%
      shinyTryCatch(errorTitle = "Error in plot", alertStyle = "shinyalert")
  })
  # })
  
  plotExportServer("exportSourceCharacteristicsPlot2",
                   plotFun = plotFunCharacteristicsMix,
                   filename = paste0(gsub("-", "", Sys.Date()), "_sourceCharacteristics"),
                   plotly = TRUE
  )
  
  #### Model Diagnostics Plot
  callModule(
    modelDiagnosticsPlot,
    "modelDiagnosticsPlot",
    model = model,
    values = values
  )
  
  #### Model Diagnostics Plot
  callModule(outputPlot,
             "outputPlot",
             model = model,
             values = values
  )
  
  output$filtered_row <-
    renderPrint({
      input[["SummaryResults_rows_all"]]
    })
  
  #### Export ----
  expSUMData <- reactive({
    function() {
      getResultStatistics(
        model()$modelResults$parameters,
        model()$modelResults$userEstimateSamples,
        model()$fruitsObj,
        statistics = c(
          input$SummaryMin,
          input$SummaryMax,
          input$SummaryMedian,
          input$SummaryQuantileCheck,
          input$SummaryQuantile,
          input$SummaryQuantile2,
          input$BayesianPValuesCheck,
          input$pVal
        ),
        DT = FALSE
      )[input[["SummaryResults_rows_all"]], ]
    }
  })
  
  callModule(exportData, "exportSummaryData", expSUMData)
  
  # Export to Iso Memo App
  observeEvent(values$targetNames, {
    logDebug("Entering observeEvent(values$targetNames)")
    updateSelectInput(session, "exportProxy", choices = values$targetNames)
    updateSelectInput(session, "exportTheta",
                      choices = applyNames(
                        expand.grid(
                          values$targetNames,
                          values$sourceNames,
                          stringsAsFactors = FALSE
                        )
                      )
    )
  })
  
  observeEvent(values$fractionNames, {
    logDebug("Entering observeEvent(values$fractionNames)")
    updateSelectInput(session, "exportBeta", choices = values$fractionNames)
  })
  
  observeEvent(values$sourceNames, {
    logDebug("Entering observeEvent(values$sourceNames)")
    updateSelectInput(session, "exportSources", choices = values$sourceNames)
  })
  
  observe({
    logDebug("Entering observe(siteExport)")
    if (input$useSite) {
      updateSelectInput(session, "siteExport",
                        choices = c(colnames(
                          model()$fruitsObj$data$covariates
                        ))
      )
    }
  })
  
  
  
  exportData <- reactive({
    data <- as.data.frame(values$exportCoordinates)
    
    if (input$useSite) {
      data$site <- values[["targetValuesCovariates"]][, input$siteExport]
    }
    
    if (input$exportType == "proxy") {
      fruitsObj <- model()$fruitsObj
      modelResults <- model()$modelResults
      if (fruitsObj$modelOptions$modelType != "1") {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "mu\\[",
                                    colnames(modelResults[[1]])
                                  )]][, , drop = FALSE]
        colnames(Data) <-
          rep(
            fruitsObj$valueNames$targets,
            fruitsObj$constants$nTargets
          )
        
        Data <- Data[, colnames(Data) == input$exportProxy]
        
        data$mean <- apply(Data, 2, mean)
        data$sd <- apply(Data, 2, sd)
      } else {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "mu\\[",
                                    colnames(modelResults[[1]])
                                  )]][, drop = FALSE]
        colnames(Data) <- fruitsObj$valueNames$targets
        data$mean <-
          mean(Data[, colnames(Data) == input$exportProxy])
        data$sd <- sd(Data[, colnames(Data) == input$exportProxy])
      }
    }
    if (input$exportType == "Component contributions") {
      fruitsObj <- model()$fruitsObj
      modelResults <- model()$modelResults
      if (fruitsObj$modelOptions$modelType != "1") {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "beta",
                                    colnames(modelResults[[1]])
                                  )]][, , drop = FALSE]
        colnames(Data) <-
          rep(
            fruitsObj$valueNames$fractions,
            fruitsObj$constants$nTargets
          )
        
        Data <- Data[, colnames(Data) == input$exportBeta]
        
        data$mean <- apply(Data, 2, mean)
        data$sd <- apply(Data, 2, sd)
      } else {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "beta",
                                    colnames(modelResults[[1]])
                                  )]][, drop = FALSE]
        colnames(Data) <- fruitsObj$valueNames$fractions
        data$mean <-
          mean(Data[, colnames(Data) == input$exportBeta])
        data$sd <- sd(Data[, colnames(Data) == input$exportBeta])
      }
    }
    if (input$exportType == "Source contributions by proxy") {
      fruitsObj <- model()$fruitsObj
      modelResults <- model()$modelResults
      if (fruitsObj$modelOptions$modelType != "1") {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "theta",
                                    colnames(modelResults[[1]])
                                  )]][, , drop = FALSE]
        colnames(Data) <-
          rep(
            applyNames(
              expand.grid(
                values$targetNames,
                values$sourceNames,
                stringsAsFactors = FALSE
              )
            ),
            fruitsObj$constants$nTargets
          )
        Data <- Data[, colnames(Data) == input$exportTheta]
        
        data$mean <- apply(Data, 2, mean)
        data$sd <- apply(Data, 2, sd)
      } else {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "theta",
                                    colnames(modelResults[[1]])
                                  )]][, drop = FALSE]
        colnames(Data) <- applyNames(
          expand.grid(
            values$targetNames,
            values$sourceNames,
            stringsAsFactors = FALSE
          )
        )
        data$mean <-
          mean(Data[, colnames(Data) == input$exportTheta])
        data$sd <- sd(Data[, colnames(Data) == input$exportTheta])
      }
    }
    
    if (input$exportType == "Source contributions") {
      fruitsObj <- model()$fruitsObj
      modelResults <- model()$modelResults
      if (fruitsObj$modelOptions$modelType != "1") {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "alpha",
                                    colnames(modelResults[[1]])
                                  )]][, , drop = FALSE]
        colnames(Data) <-
          rep(
            fruitsObj$valueNames$sources,
            fruitsObj$constants$nTargets
          )
        
        Data <- Data[, colnames(Data) == input$exportSources]
        
        data$mean <- apply(Data, 2, mean)
        data$sd <- apply(Data, 2, sd)
      } else {
        Data <- modelResults[[1]][, colnames(modelResults[[1]])
                                  [grep(
                                    "alpha",
                                    colnames(modelResults[[1]])
                                  )]][, drop = FALSE]
        colnames(Data) <- fruitsObj$valueNames$sources
        data$mean <-
          mean(Data[, colnames(Data) == input$exportSources])
        data$sd <- sd(Data[, colnames(Data) == input$exportSources])
      }
    }
    if (input$exportType == "userEstimates") {
      fruitsObj <- model()$fruitsObj
      modelResults <- model()$modelResults
      if (fruitsObj$modelOptions$modelType != "1") {
        Data <- modelResults[[2]][, colnames(modelResults[[2]])
                                  [grep(
                                    paste0(input$exportUserEstimates, "_"),
                                    colnames(modelResults[[2]])
                                  )]][, , drop = FALSE]
        
        data$mean <- apply(Data, 2, mean)
        data$sd <- apply(Data, 2, sd)
      } else {
        Data <- modelResults[[2]][, colnames(modelResults[[2]])
                                  [grep(
                                    paste0(input$exportUserEstimates, "_"),
                                    colnames(modelResults[[2]])
                                  )]][, drop = FALSE]
        data$mean <- mean(Data)
        data$sd <- sd(Data)
      }
    }
    data
  })
  
  output$exportPreview <- renderTable(exportData(), bordered = TRUE)
  
  observe({
    if (!isoInstalled()) {
      shinyjs::disable("exportToIsoMemo")
    }
  })
  
  observeEvent(input$exportToIsoMemo, {
    logDebug("Entering observeEvent(input$exportToIsoMemo)")
    isoDataExport(list(
      data = exportData(),
      event = runif(1)
    ))
  })
  
  
  ## food intakes
  callModule(foodIntakes, "foodIntakes", values = values)
}


#' Extract Potential Numerics
#' 
#' Extract potential numerical covariates.
#' 
#' @param targetValuesCovariates table with covariates.
extractPotentialNumerics <- function(targetValuesCovariates) {
  colnames(targetValuesCovariates)[sapply(
      1:ncol(targetValuesCovariates),
      function(x) {
        all(!is.na(
          suppressWarnings(as.numeric(targetValuesCovariates[, x]))
        ))
      }
    )]
}


#' Extract Potential Cat
#' 
#' Extract potential categorical covariates.
#' 
#' @param targetValuesCovariates table with covariates
extractPotentialCat <- function(targetValuesCovariates) {
  colnames(targetValuesCovariates)[sapply(
      1:ncol(targetValuesCovariates),
      function(x) {
        all(!is.na(targetValuesCovariates[, x]))
      }
    )]
}
