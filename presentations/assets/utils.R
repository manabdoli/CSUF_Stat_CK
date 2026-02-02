indent_print <- function(x, spaces = 4) {
  indent <- paste(rep(" ", spaces), collapse = "")
  output <- capture.output(print(x))
  # Add indent to EACH line
  indented <- paste0(indent, output)
  cat(indented, sep = "\n")
}
