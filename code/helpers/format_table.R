format_table <- function(artifact_path) {
  if (!file.exists(artifact_path)) {
    stop("Table artifact not found: ", artifact_path, call. = FALSE)
  }
  paste(readLines(artifact_path, warn = FALSE), collapse = "\n")
}
