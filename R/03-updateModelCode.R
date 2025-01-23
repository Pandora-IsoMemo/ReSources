modelCodeUI <- function(id, title = NULL) {
  ns <- NS(id)
  tagList(
    if (!is.null(title)) tags$h4(title) else NULL,
    if (isRunningOnline()) {
      helpText(HTML("Use the local version to edit the code, as editing is ignored in the online version. Please check the <a href='https://pandora-isomemo.github.io/docs/apps.html#resources---food-reconstruction-using-isotopic-transferred-signals' target='_blank'>installation instructions</a>. We recommend the Docker installation."))
    } else {
      helpText("Edit the code if necessary, then press 'Run' to start modeling.")
    },
    aceEditor(
      ns("text"),
      value = NULL,
      mode = "r",
      theme = "dawn",
      fontSize = 16,
      autoScrollEditorIntoView = TRUE,
      minLines = 50,
      maxLines = 100,
      autoComplete = "live"
    ),
    downloadButton(ns("download"), "Download")
  )
}

modelCodeServer <- function(id, model, class, type = NULL) {
  moduleServer(id, function(input, output, session) {
    observe({
      req("fruitsObj" %in% names(model()))
      value <- switch(
        class,
        modelInput = model()$fruitsObj[[type]],
        modelCode = model()$fruitsObj$modelCode
      )
      updateAceEditor(
        session = session,
        "text",
        value = paste(deparse(value), collapse = "\n"),
        autoCompleters = c("snippet", "text", "static", "keyword")
      )
    }) %>%
      bindEvent(model())
    
    output$download <- downloadHandler(
      filename = function() {
        paste0(paste(c(class, type), collapse = "_"), ".txt")
      },
      content = function(file) {
        writeLines(input$text, file)
      }
    )
  })
}

# Update model code in fruits object
#
# @param fruitsObj fruits object
# @param newModelCode new model code
updateModelCode <- function(fruitsObj, newModelCode, newModelInputs = NULL) {
  # prevent evaluation when running online
  if (isRunningOnline()) return(fruitsObj)
  
  if (length(fruitsObj) == 0 ||
      length(newModelCode) == 0 || newModelCode == "" ||
      length(newModelInputs) == 0 || newModelInputs == "") {
    # nothing to update
    return(fruitsObj)
  }
  
  # update model inputs in fruits object
  for (name in names(newModelInputs)) {
    fruitsObj[[name]] <- eval(parse(text = newModelInputs[[name]]))
  }
  
  # update 'modelCode' in fruits object
  # Evaluate the (new) nimbleCode string, then this corresponds to the output of tmplEval() in the createModelCode() function
  evaluated_code <- eval(parse(text = paste("nimbleCode(", newModelCode, ")")))
  
  # Explicitly assign the evaluated code to the object
  fruitsObj$modelCode <- evaluated_code
  
  # Return the updated fruits object
  fruitsObj
}

isRunningOnline <- function() {
  as.logical(Sys.getenv("SHINYPROXY") != "", unset = "FALSE")
}
