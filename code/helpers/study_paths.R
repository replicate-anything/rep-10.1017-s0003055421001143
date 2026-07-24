study_root <- function() {
  root <- Sys.getenv("REPLICATE_STUDY_ROOT", unset = ".")
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || is.na(x)) y else x

load_analysis_data <- function(root = study_root()) {
  path <- file.path(root, "outputs", "prep_analysis_data", "analysis_data.rds")
  if (!file.exists(path)) {
    stop("Prepared analysis data not found at ", path, call. = FALSE)
  }
  readRDS(path)
}

output_path <- function(..., root = study_root()) {
  file.path(root, ...)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

studies_output_path <- function() {
  file.path(study_root(), "outputs", "prep_analysis_data", "analysis_data.rds")
}

load_analysis_data <- function() {
  path <- studies_output_path()
  if (!file.exists(path)) {
    stop("Run prep_studies first (missing ", path, ").", call. = FALSE)
  }
  readRDS(path)
}