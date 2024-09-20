#' Get Fraction Names
#' 
#' @param withComponents (logical) TRUE if "Include components"
#' @param withConcentration (logical) TRUE if "Include concentration"
#' @param weights (list) list of weights tables
#' @param concentration (list) list of concentration tables
#' @param targetNames (character) names of the targets
getFractionNames <- function(withComponents, 
                             withConcentration, 
                             weights, 
                             concentration, 
                             targetNames) {
  # default value
  newValue <- targetNames
  
  if (withComponents) {
    if (withConcentration) {
      if (length(concentration) > 0) {
        newValue <- unique(colnames(concentration[[1]]))
      } # reset if no length??
    } else {
      if (length(weights) > 0) {
        newValue <- unique(colnames(weights))
      } # reset if no length??
    }
  }
  
  newValue
}


#' Get Source Names
#' 
#' @param withConcentration (logical) TRUE if "Include concentrations"
#' @param concentration (list) list of concentration tables
#' @param source (list) list of source tables
getSourceNames <- function(withConcentration, concentration, source) {
  if (withConcentration) {
    if (length(concentration) > 0) {
      newValue <- unique(rownames(concentration[[1]]))
    } # reset if no length??
  } else if (length(source) > 0 && length(source[[1]]) > 0) {
    newValue <- unique(rownames(source[[1]][[1]][[1]]))
  } # reset if no length??
  
  newValue
}


#' Update Targets In Lists
#'
#' Update all list entries named "name" from all lists that contain entries named by targets.
#'
#' @param values (list) list containing all input data (all input tables)
#' @param name (character) name of the target to be removed (only )
#' @param updateFun name of function that updates the lists, either
#'  deleteTableFromList or updateListNames
updateTargetsInLists <- function(values, name, updateFun) {
  for (entry in c("source",
                  "sourceUncert",
                  "sourceOffset",
                  "sourceOffsetUncert")) {
    # use isDeepestEntry()
    depth <-
      getDepthAndTable(values[[entry]], isEntryFun = isDeepestEntry)$nFlatten
    
    values[[entry]] <- updateFun(entryContent = values[[entry]],
                                 depth = depth,
                                 namesList = name)
  }
  
  values
}


#' Remove Obsvn From Lists
#'
#' Removes all list entries named "name" from all lists that contain entries named by observations.
#'
#' @inheritParams updateObsvnsOfEntry
updateObsvnsInLists <- function(values, name, updateFun) {
  for (entry in c("source",
                  "sourceUncert",
                  "sourceOffset",
                  "sourceOffsetUncert",
                  "sourceCovariance",
                  "concentration",
                  "concentrationUncert",
                  "concentrationCovariance")) {
    values[[entry]] <- updateObsvnsOfEntry(values, entry, name, updateFun = updateFun)
  }
  
  values
}


#' Remove Obsvn From Entry
#'
#' Removes all list entries named "name" from the element "entry".
#'
#' @param values (list) list containing all input data (all input tables)
#' @param entry (character) one of c("source", "sourceUncert", "sourceOffset", "sourceOffsetUncert",
#'  "sourceCovariance", "concentration", "concentrationUncert", "concentrationCovariance")
#' @param name (character) name of the target to be removed
#' @param updateFun name of function that updates the lists, either
#'  deleteTableFromList or updateListNames
updateObsvnsOfEntry <- function(values, entry, name, updateFun) {
  if (entry %in% c("source",
                   "sourceUncert",
                   "sourceOffset",
                   "sourceOffsetUncert")) {
    isEntryFun <- isPreDeepestEntry
  } else if (entry %in% c("sourceCovariance",
                          "concentration",
                          "concentrationUncert",
                          "concentrationCovariance")) {
    isEntryFun <- isDeepestEntry
  } else {
    warning("No method to delete obsvn from this entry.")
    return(values[[entry]])
  }
    
  depth <-
    getDepthAndTable(values[[entry]], isEntryFun = isEntryFun)$nFlatten
  
  updateFun(values[[entry]],
                      depth = depth,
                      namesList = name)
}


#' Delete Table From List
#'
#' @param entryContent (list) possibly nested list
#' @param depth depth of list where names should be updated
#' @param namesList names of list elements to be deleted
deleteTableFromList <- function(entryContent, depth, namesList) {
  if (depth == 0) {
    if (!is.null(names(entryContent))) {
      entryContent[!(names(entryContent) %in% namesList)]
    } else {
      # there is only one entry for all elements -> do not remove anything
      entryContent
    }
  } else {
    depth <- depth - 1
    lapply(entryContent, function(elem) {
      deleteTableFromList(elem, depth, namesList)
    })
  }
}


#' Update List Names
#' 
#' @param entryContent (list) possibly nested list
#' @param depth depth of list where names should be updated
#' @param namesList new names for list elements
updateListNames <- function(entryContent, depth, namesList) {
  if (depth == 0) {
    if (!is.null(names(entryContent))) {
      names(entryContent) <- namesList[1:length(names(entryContent))]
      entryContent
    } else {
      # there is only one entry for all elements -> no update of names
      entryContent
    }
  } else {
    depth <- depth - 1
    lapply(entryContent, function(elem) {
      updateListNames(elem, depth, namesList)
    })
  }
}


#' Get Depth And Table
#'
#' Get the depth of the nested list and the content of the most inner table.
#'
#' @param entryContent (list) list to look for names
#' @param isEntryFun (function) function that checks for the correct level in the list hierarchy
#' @param n (numeric) depth of list to look for names
#' of values
getDepthAndTable <-
  function(entryContent,
           isEntryFun = isDeepestEntry,
           n = NULL) {
    nFlatten <- 0
    if (is.null(n)) {
      while (!isEntryFun(entryContent)) {
        # go one level deeper to compare names:
        entryContent <- entryContent[[1]]
        nFlatten <- nFlatten + 1
      }
    } else {
      while (nFlatten < n) {
        # go one level deeper to compare names:
        entryContent <- entryContent[[1]]
        nFlatten <- nFlatten + 1
      }
    }
    
    list(nFlatten = nFlatten,
         entryContent = entryContent)
  }


#' Is Deepest Entry
#' 
#' Checks if names of the list are targetNames (deepest hierarchy in a values object)
#' 
#' @param entryContent (list) element of values, e.g. values$source, values$sourceUncert,
#'  values$sourceOffset, values$sourceOffsetUncert
isDeepestEntry <- function(entryContent) {
  (length(entryContent) == 0) ||
  !is.null(ncol(entryContent[[1]])) && 
    is.null(names(entryContent[[1]][[1]]))
}


#' Is Pre Deepest Entry
#' 
#' Checks if names if the list are obsvnNames (hierarchy above targetNames in a values object)
#' 
#' @param entryContent (list) element of values, e.g. values$source, values$sourceUncert,
#'  values$sourceOffset, values$sourceOffsetUncert
isPreDeepestEntry <- function(entryContent) {
  (length(entryContent[[1]]) == 0) ||
    !is.null(ncol(entryContent[[1]][[1]])) && 
    is.null(names(entryContent[[1]][[1]][[1]]))
}
