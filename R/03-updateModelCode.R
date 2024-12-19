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

