#' Read String Wrapper
#' 
#' Read and check string from clipboard
#' 
#' @param content (character) string from clipboard
#' @param mode (character) paste mode, one of "auto", "comma-separated", "tab-separated", "semicolon"
#' @param class (character) class of content, e.g. "numeric", "character"
#' @param withRownames (logical) should the first column be used as rownames?
#' @param withColnames (logical) should the first row be used as colnames?
readStringWrapper <- function(content, mode, class, withRownames = TRUE, withColnames = TRUE) {
  m <- try(readString(content, mode, withColnames = withColnames))
  
  if (inherits(m, "try-error")) {
    alert(paste0("Could not parse text from clipboard. Error: ", as.character(m)))
    return(NULL)
  }
  
  if (ncol(m) > 0 && withRownames) {
    rownames(m) <- m[, 1]
    m <- m[, -1, drop = FALSE]
  }
  
  storage.mode(m) <- class
  
  if (length(m) == 0) {
    alert(paste("Pasted values are empty. Please provide some values."))
    return(NULL)
  }
  
  m
}


#' Read String
#'
#' @inheritParams readStringWrapper
readString <- function(content, mode, withColnames = TRUE) {
  opts <- list(
    "comma-separated" = ",",
    "tab-separated" = "\t",
    "semicolon" = ";"
  )

  if (mode == "auto") {
    l <- lapply(opts, function(cc) {
      length(unlist(regmatches(content, gregexpr(cc, content))))
    })
    sep <- opts[[which.max(l)]]
  } else {
    sep <- opts[[mode]]
  }

  as.matrix(
    read.table(
      textConnection(content),
      row.names = NULL,
      header = withColnames,
      check.names = FALSE,
      sep = sep
    )
  )
}
