embedR <- function(fPath){
  webr_code <- paste(readLines(fPath, warn = FALSE), collapse = "\n")
  paste0("```{webr}\n",
        "#| echo: false\n",
        "#| output: false\n\n",
        webr_code,
        "\n```\n",
        sep = "")
}
