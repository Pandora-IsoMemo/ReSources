#' Check Availability for Mpi Iso App package
#'
#' @export
isoInstalled <- function() {
  "DSSM" %in% installed.packages()[, 1] &&
    compareVersion(as.character(packageVersion("DSSM")), isoVersion()) > -1
}

isoVersion <- function() {
  "1.2.5"
}
