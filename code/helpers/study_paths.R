study_root <- function() {
  root <- Sys.getenv("REPLICATE_STUDY_ROOT", unset = ".")
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) {
    return(y)
  }
  if (length(x) == 1L && is.na(x)) {
    return(y)
  }
  x
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

output_path <- function(..., root = study_root()) {
  file.path(root, ...)
}

studies_output_path <- function(root = study_root()) {
  file.path(root, "outputs", "prep_analysis_data", "analysis_data.rds")
}

load_analysis_data <- function(root = study_root()) {
  path <- studies_output_path(root = root)
  if (!file.exists(path)) {
    stop("Run prep_analysis_data first (missing ", path, ").", call. = FALSE)
  }
  readRDS(path)
}
