checkForEmptyTables <- function(values) {
  isTableEmpty <- function(tbl) {
    all(sapply(tbl, function(x)
      all(is.na(x))))
  }

  ifEnabled <- function(checkboxValue, matrixName) {
    if (!checkboxValue)
      return(NULL)

    matrixName
  }

  tablesInUI <- c(
    Target = "obsvn",
    `target-to-source offset` = "weightOffset",
    Covariates = "targetValuesCovariates",
    `Coordinates & chronology` = "exportCoordinates",
    Components = "weights",
    Sources = "source",
    `source specific offset` = "sourceOffset",
    Concentrations = "concentration"
  )
  checkboxesInUI <- c(
    `Include target offset` = "targetOffset",
    `Enter Covariates` = "targetValuesShowCovariates",
    `Coordinates & chronology` = "targetValuesShowCoordinates",
    `Include source specific offsets` = "includeSourceOffset",
    `Include components` = "modelWeights",
    `Include concentrations` = "modelConcentrations"
  )

  namesTablesToCheck <- c(
    "Target",
    ifEnabled(values$targetOffset, "target-to-source offset"),
    ifEnabled(values$targetValuesShowCovariates, "Covariates"),
    ifEnabled(
      values$targetValuesShowCoordinates,
      "Coordinates & chronology"
    ),
    ifEnabled(values$modelWeights, "Components"),
    "Sources",
    ifEnabled(values$includeSourceOffset, "source specific offset"),
    ifEnabled(values$modelConcentrations, "Concentrations")
  )

  emptyTables <- c()
  for (i in tablesInUI[namesTablesToCheck]) {
    # if the entry is not a table, check if the first sub-entry is an empty table
    isSubTableEmpty <- isTRUE(!is.data.frame(values[[i]]) && (length(values[[i]]) > 1) && isTableEmpty(values[[i]][[1]]))
    
    if (isTableEmpty(values[[i]]) || isSubTableEmpty) {
      emptyTables <- c(emptyTables, i)
    }
  }

  namesEmptyTables <- names(tablesInUI[tablesInUI %in% emptyTables])

  return(namesEmptyTables)
}
