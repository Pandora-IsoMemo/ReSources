splitDoubleMatrix <- function(x, rownames = FALSE, colnames = TRUE) {
  stopifnot(is.matrix(x))
  stopifnot(ncol(x) == 0 || !is.null(colnames(x)))
  stopifnot(!rownames || !is.null(rownames(x)))
  stopifnot(!colnames || !is.null(colnames(x)))

  if (ncol(x) %% 2 == 1) {
    x <- cbind(x, NA)
  }

  x <- dropEmptyCols(x, delta = 2)
  if (rownames) x <- dropEmptyRows(x)

  x <- minimalDoubleMatrix(x)

  x1 <- extractMatrixCols(x, remainder = 0)
  x2 <- extractMatrixCols(x, remainder = 1)

  if (colnames) {
    colnames(x1) <- extractColNames(x)
    colnames(x2) <- extractColNames(x)
  }
  
  rownames(x1) <- rownames(x)
  rownames(x2) <- rownames(x)

  list(
    x1,
    x2
  )
}

combineDoubleMatrix <- function(x, y, v1 = "mean", v2 = "uncert") {
  stopifnot(is.matrix(x))
  stopifnot(is.matrix(y))
  stopifnot(all.equal(dim(x), dim(y)))
  stopifnot(ncol(x) == 0 || !is.null(colnames(x)))

  if (ncol(x) == 0) {
    return(x)
  }

  m <- cbind(x, y)
  i <- c(matrix(1:ncol(m), 2, byrow = TRUE))
  m <- m[, i, drop = FALSE]

  rownames(m) <- rownames(x)
  colnames(m) <- combineColnames(colnames(x), c(v1, v2))

  m
}

dropEmptyCols <- function(x, delta = 1) {
  while (ncol(x) > 0 && lastColEmpty(x, delta = delta)) {
    x <- x[, -seq(ncol(x), by = -1, length.out = delta), drop = FALSE]
  }
  x
}

dropEmptyRows <- function(x, delta = 1) {
  while (nrow(x) > 0 && lastRowEmpty(x, delta = delta)) {
    x <- x[-seq(nrow(x), by = -1, length.out = delta), , drop = FALSE] ## nolint
  }
  x
}


lastColEmpty <- function(x, delta = 1) {
  j <- seq(ncol(x), by = -1, length.out = delta)

  all(is.na(x[, j]) | x[, j] == "") &
    all(colnames(x)[j] == "")
}

lastRowEmpty <- function(x, delta = 1) {
  i <- seq(nrow(x), by = -1, length.out = delta)
  ret <- all(is.na(x[i, ]) | x[i, ] == "") &
    all(rownames(x)[i] == "")
  if (is.na(ret)) ret <- FALSE
  ret
}

extractMatrixCols <- function(x, remainder = 0) {
  if (ncol(x) == 0) {
    return(x)
  }
  i <- seq(remainder + 1, ncol(x), by = 2)
  x[, i, drop = FALSE]
}

extractColNames <- function(x) {
  if (ncol(x) == 0) {
    return(character(0))
  }

  split <- strsplit(colnames(x), "||", fixed = TRUE)
  i <- seq(1, ncol(x), by = 2)
  unlist(lapply(split[i], function(x) ifelse(length(x) > 0, x[[1]], "")))
}

combineColnames <- function(a, b) {
  if (is.null(a) || length(a) == 0) {
    return(character(0))
  }
  grid <- expand.grid(b, a, stringsAsFactors = FALSE)
  paste(grid[, 2], grid[, 1], sep = "||")
}

#' Fix Matrix Cols
#' 
#' Fix columns of a matrix. If fixed colums are specified, they are used as column names otherwise
#' oldNames are kept.
#' 
#' @param m matrix
#' @param oldNames (character) old column names, e.g. of "meanData()" or "covarianceData()"
#' @param fixedCols either FALSE or a character vector of column names to be kept, e.g. "Offset",
#'  "longitude", "latitude", ...
#' @param row (character) row variable, e.g. "obsvnNames"
#' @param col (character) column variable, e.g. "targetNames"
fixMatrixCols <- function(m, oldNames, fixedCols = FALSE, row, col) {
  colsFixed <- !is.logical(fixedCols)

  m <- dropEmptyRows(m)
  m <- dropEmptyCols(m)

  if (colsFixed) {
    m <- defaultMatrixNames(m, sampleName(row), sampleName(col))

    m <- m[, seq_len(min(length(fixedCols), ncol(m))), drop = FALSE]
    m <- cbind(m, matrix(NA, nrow(m), length(fixedCols) - ncol(m)))
    colnames(m) <- fixedCols
  } else {
    length(oldNames) <- ncol(m)
    colnames(m) <- oldNames

    m <- defaultMatrixNames(m, sampleName(row), sampleName(col))
  }

  m
}

fixDoubleMatrixCols <- function(m, oldNames, fixedCols = FALSE, row, col) {
  lapply(m, fixMatrixCols, oldNames = oldNames, fixedCols = fixedCols, row, col)
}

minimalMatrix <- function(m) {
  if (nrow(m) == 0) {
    matrix(NA, 1, ncol(m), dimnames = list("", colnames(m)))
  } else if (ncol(m) == 0) {
    matrix(NA, nrow(m), 1, dimnames = list(rownames(m), 1))
  } else {
    m
  }
}

minimalDoubleMatrix <- function(m) {
  if (length(m) == 0) {
    matrix(NA, 1, 2, dimnames = list("", c("||mean", "||SD")))
  } else if (nrow(m) == 0) {
    matrix(NA, 1, ncol(m), dimnames = list("", colnames(m)))
  } else if (ncol(m) == 0) {
    matrix(NA, nrow(m), 1, dimnames = list(rownames(m), c("||mean", "||SD")))
  } else {
    m
  }
}
