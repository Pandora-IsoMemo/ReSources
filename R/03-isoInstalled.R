#' Check Availability for Mpi Iso App package
#'
#' @export
isoInstalled <- function() {
  if (requireNamespace("DSSM", quietly = TRUE)) {
    compareVersion(as.character(packageVersion("DSSM")), isoVersion()) >= 0
  } else {
    FALSE
  }
}

isoVersion <- function() {
  "1.2.5"
}
