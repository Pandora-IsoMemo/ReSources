verbatimTextOutput <- function(id, download = FALSE, ...) {
  ns <- NS(id)

  if (download) {
    tagList(
      shiny::verbatimTextOutput(ns("text"), ...),
      downloadButton(ns("download"), "Download")
    )
  } else {
    shiny::verbatimTextOutput(ns("text"), ...)
  }
}

verbatimText <-
  function(input,
           output,
           session,
           model,
           class,
           type = NULL,
           args = list()) {
    content <- reactive({
      switch(
        class,
        modelInput = capture.output({
          returnType <- function(type, obj) {
            if (type != "userEstimates" |
                length(obj[["userEstimates"]][[1]]) > 0) {
              return(obj[[type]])
            } else {
              return("")
            }
          }
          returnType(type, model()$fruitsObj)
        }),
        modelDiagnostics = capture.output(
          convergenceDiagnostics(model()$modelResults$parameters,
                                 model()$fruitsObj)[[type]]
        ),
        wAIC = model()$modelResults$wAIC,
        BIC = model()$modelResults$BIC,
        # OxCalText = createOxCalTextOutput(model(), args$OxCalType(), args$OxCalA(),
        #                                   args$OxCalB(), args$Bins(), args$Coordinates()),
        # Not sure if we need this "OxCalText" at all here anymore, there is no call of this ...
        OxCalText = createOxCalText(
          model = model(),
          basicCode = args$basicCode(),
          terrestrialCurve = args$terrestrialCurve(),
          aquaticCurve1 = args$aquaticCurve1(),
          aquaticCurve2 = args$aquaticCurve2(),
          OxCalA = args$OxCalA(),
          OxCalB = args$OxCalB(),
          bins = args$bins(),
          coordinates = args$coordinates()
        ),
        corrMat = capture.output(
          getSourceCorr(model()$modelResults$simSources$simSources, corr = TRUE)
        ),
        scoreSep = capture.output(
          getSourceScoreSep(model()$modelResults$simSources$simSources)
        )
      )
    })

  output$text <- renderPrint({
    validate(validInput(model()))

    cat(content(), sep = "\n")
  })
  ## for debugging
  ## observe({
  ##     validate(validInput(model()))

  ##     if (class == "corrMat") browser()
  ##     cat(content(), sep = "\n")
  ## })

  output$download <- downloadHandler(
    filename = function() {
      paste0(paste(c(class, type), collapse = "_"), ".txt")
    },
    content = function(file) {
      writeLines(content(), file)
    }
  )
}
