#' Fill Values From Upload
#' 
#' @param uploadedValues list of uploaded values
#' 
#' @return list of values
fillValuesFromUpload <- function(uploadedValues) {
  valuesDat <- uploadedValues[["inputs"]]
  
  # check for empty tables
  emptyTables <- checkForEmptyTables(valuesDat)
  
  if (length(emptyTables) > 0) {
    # try to find data in the model object if available
    #
    # this was not working since the format changes significantly -> leads to errors after upload!!
    # if (!is.null(uploadedValues[["model"]])) {
    #   dataFromModel <- uploadedValues[["model"]][["fruitsObj"]][["data"]]
    #   
    #   for (i in emptyTables) {
    #     if (i %in% names(dataFromModel)) {
    #       valuesDat[[i]] <- dataFromModel[[i]]
    #     }
    #   }
    # }
    
    # check again for empty tables
    emptyTables <- checkForEmptyTables(valuesDat)
    if (length(emptyTables) > 0) warning(
      paste("Empty tables! No data found for input tables: \n",
            paste0(names(emptyTables), collapse = ", "),
            " ")
    )
  }
  
  valuesDat
}
