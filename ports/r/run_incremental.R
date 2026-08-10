# run_incremental.R
# Demo incremental real: muestra el mejor árbol tras cada token

source("ParserV7.R")

p <- ParserV7$new(
  grammarPath = file.path("resources", "grammar.json"),
  lexiconPath = file.path("resources", "lexicon.json"),
  beam = 8
)

cat("Modo incremental real (step(token)).\n")
cat("Pegá una oración. Te muestro el mejor árbol tras cada token.\n")
cat("Línea vacía para salir.\n\n")

repeat {
  line <- readline("> ")
  if (!nzchar(trimws(line))) break

  p$resetIncremental()

  parts <- strsplit(line, "\\s+")[[1]]
  for (i in seq_along(parts)) {
    tree <- p$step(parts[[i]])
    cat(sprintf("[%d] +%s => %s\n", i, parts[[i]], if (nzchar(tree)) tree else "(NO-PARSE)"))
  }
  final <- p$bestTree()
  cat("\nFinal => ", if (nzchar(final)) final else "(NO-PARSE)", "\n\n", sep = "")
}
