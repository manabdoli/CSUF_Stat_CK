embedData <- function(DF){
  var_name <- deparse(substitute(DF))
  # Turn DF it into CSV text
  csv <- paste(capture.output(write.csv(DF, row.names = FALSE)),
               collapse = "\n")
  
  # Escape for embedding inside an R string literal
  csv_esc <- gsub("\\\\", "\\\\\\\\", csv)     # escape backslashes
  csv_esc <- gsub('"',  '\\"', csv_esc)        # escape quotes
  
  # Emit a WebR chunk that recreates `DF` in the browser
  paste0("```{webr}\n",
         "#| echo: false\n",
  "#| output: false\n\n",
  var_name, " <- read.csv(text = '", csv_esc, "')\n", sep = "",
  "```\n")
}