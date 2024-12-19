modelCodeUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Model code"),
    helpText("Edit the code if needed, and press 'Run' to execute the model."),
    aceEditor(
      ns("text"),
      value = NULL,
      mode = "r",
      theme = "chrome",
      fontSize = 16,
      autoScrollEditorIntoView = TRUE,
      minLines = 50,
      maxLines = 100,
      autoComplete = "live"
    ),
    downloadButton(ns("download"), "Download")
  )
}

modelCodeServer <- function(id, model) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        req("fruitsObj" %in% names(model()))
        updateAceEditor(
          session = session,
          "text",
          value = paste(deparse(model()$fruitsObj$modelCode), collapse = "\n"),
          autoCompleters = c("snippet", "text", "static", "keyword")
        )
      }) %>%
        bindEvent(model())
      
      output$download <- downloadHandler(
        filename = function() {
          paste0("modelCode.txt")
        },
        content = function(file) {
          writeLines(input$text, file)
        }
      )
    }
  )
}

# Update model code in fruits object
# 
# @param fruitsObj fruits object
# @param newModelCode new model code
updateModelCode <- function(fruitsObj, newModelCode) {
  if (length(fruitsObj) == 0 || length(newModelCode) == 0 || newModelCode == "") {
    # nothing to update
    return(fruitsObj)
  }
  
  # update model code
  # Evaluate the nimbleCode string
  evaluated_code <- eval(parse(text = paste("nimbleCode(", newModelCode, ")")))
  
  # Explicitly assign the evaluated code to the object
  fruitsObj$modelCode <- evaluated_code
  
  fruitsObj 
}

