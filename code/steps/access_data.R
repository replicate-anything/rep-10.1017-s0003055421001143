# Surgical Dataverse fetch → outputs/final_data.csv (file 14008582)
# Replaces full-deposit archive zip for this study.

make_access_data <- function() {
  replicateEverything::fetch_dataverse_file(
    file_id = "14008582",
    path = "outputs/final_data.csv",
    original = TRUE,
    server = "dataverse.harvard.edu"
  )
}
