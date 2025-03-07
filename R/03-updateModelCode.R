modelCodeUI <- function(id, title = NULL) {
  ns <- NS(id)
  tagList(
    if (!is.null(title))
      tags$h4(title)
    else
      NULL,
    if (isRunningOnline()) {
      helpText(
        HTML(
          "Use the local version to edit the code, as editing is disabled in the online version. Please check the <a href='https://pandora-isomemo.github.io/docs/apps.html#resources---food-reconstruction-using-isotopic-transferred-signals' target='_blank'>installation instructions</a>. We recommend the Docker installation."
        )
      )
    } else {
      helpText(
        "Edit the code if necessary, then check 'Run with edited 'Model code' or 'Model inputs', and  press 'Run' to start modeling."
      )
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
      autoComplete = "live",
      readOnly = isRunningOnline()
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

getUpdatedFruitsObj <- function(fruitsObj, input) {
  fruitsObj %>%
    updateModelCode(
      newModelCode = input$`modelCode-text`,
      newModelInputs = list(
        data = input$`modelInputData-text`,
        valueNames = input$`modelInputValueNames-text`,
        modelOptions = input$`modelInputModelOptions-text`,
        priors = input$`modelInputPriors-text`,
        userEstimates = input$`modelInputUserEstimates-text`
      )
    ) %>%
    shinyTryCatch(errorTitle = "Could not update model code", alertStyle = "shinyalert")
}

# Update model code in fruits object
#
# @param fruitsObj fruits object
# @param newModelCode new model code
updateModelCode <- function(fruitsObj, newModelCode, newModelInputs = NULL) {
  # prevent evaluation when running online
  if (isRunningOnline())
    return(fruitsObj)
  
  if (length(fruitsObj) == 0 ||
      length(newModelCode) == 0 || newModelCode == "" ||
      length(newModelInputs) == 0) {
    # nothing to update
    return(fruitsObj)
  }
  
  # update model inputs in fruits object
  for (name in names(newModelInputs)) {
    fruitsObj[[name]] <- eval(parse(text = sanitizeInput(newModelInputs[[name]])))
  }
  
  # update 'modelCode' in fruits object
  # Evaluate the (new) nimbleCode string, then this corresponds to the output of tmplEval() in the createModelCode() function
  evaluated_code <- eval(parse(text = paste(
    "nimbleCode(", sanitizeInput(newModelCode), ")"
  )))
  
  # Explicitly assign the evaluated code to the object
  fruitsObj$modelCode <- evaluated_code
  
  # Return the updated fruits object
  fruitsObj
}

isRunningOnline <- function() {
  as.logical(Sys.getenv("IS_SHINYPROXY") != "", unset = "FALSE")
}

sanitizeInput <- function(input) {
  # Check for empty input explicitly (valid case)
  if (input == "") {
    return(input)
  }
  
  basic_regex <- "^[[:print:][:space:]]+$"
  
  # Step 1: Basic validation to check for printable characters and spaces
  is_valid <- grepl(basic_regex, input)
  if (!is_valid) {
    stop("Invalid input detected: contains non-printable or disallowed characters.")
  }
  
  # Step 2: Check for explicitly forbidden patterns
  forbidden_patterns <- forbidden_patterns <- c(
    "system\\(",
    "system2\\(",
    "shell\\(",
    "eval\\(",
    "parse\\(",
    "source\\(",
    "do\\.call\\(",
    "file\\(",
    "unlink\\(",
    "dir\\(",
    "list\\.files\\(",
    "read\\.table\\(",
    "read\\.csv\\(",
    "write\\.csv\\(",
    "get\\(",
    "assign\\(",
    "environment\\(",
    "library\\(",
    "require\\(",
    "Sys\\.setenv\\(",
    "Sys\\.getenv\\(",
    "options\\(",
    "proc\\.time\\(",
    "pskill\\(",
    "system\\.time\\(",
    "download\\.file\\(",
    "url\\(",
    "getURL\\(",
    "readLines\\(",
    "httr::GET\\(",
    "httr::POST\\("
  )
  
  if (any(sapply(forbidden_patterns, grepl, input))) {
    stop("Invalid input detected: contains forbidden patterns.")
  }
  
  input
}
