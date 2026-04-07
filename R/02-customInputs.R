percentileSliderInput <- function(inputId, label, value, min = 0, max = 100, step = 0.1, ...) {
  sliderInput(inputId, label, min, max, value, step, post = "%", ...)
}