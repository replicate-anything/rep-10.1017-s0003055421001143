source("../helpers/study_paths.R", local = TRUE)

make_access_deposit <- function(root = study_root()) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Install the httr package to fetch the deposit.", call. = FALSE)
  }

  deposit_root <- file.path(root, "outputs", "deposit")
  marker <- file.path(deposit_root, ".manifest_applied")
  ensure_dir(deposit_root)

  if (file.exists(marker)) {
    return(invisible(marker))
  }

  manifest_path <- file.path(root, "manifest", "dataverse_files.csv")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  server <- "dataverse.harvard.edu"
  dataset <- "doi:10.7910/DVN/WJQITU"

  zip_path <- file.path(root, "outputs", "staging", "dataverse_archive.zip")
  ensure_dir(dirname(zip_path))

  url <- paste0(
    "https://", server,
    "/api/access/dataset/:persistentId/?persistentId=", dataset,
    "&format=original"
  )
  httr::GET(url, httr::write_disk(zip_path, overwrite = TRUE), httr::timeout(600))

  utils::unzip(zip_path, exdir = deposit_root)

  for (i in seq_len(nrow(manifest))) {
    rel <- manifest$path[[i]]
    target <- file.path(deposit_root, rel)
    if (!file.exists(target)) {
      stop("Expected deposit file missing after unzip: ", rel, call. = FALSE)
    }
  }

  writeLines(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"), marker)
  invisible(marker)
}
