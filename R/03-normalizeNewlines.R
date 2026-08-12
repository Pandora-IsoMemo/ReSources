normalize_newlines <- function(x) {
  if (is.character(x)) {
    gsub("\r\n?", "\n", x, perl = TRUE)
  } else if (is.data.frame(x)) {
    x[] <- lapply(x, normalize_newlines)
    x
  } else if (is.list(x)) {
    lapply(x, normalize_newlines)
  } else {
    x
  }
}
