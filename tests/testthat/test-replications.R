study_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)

test_that("replication.yml lists main-text replications", {
  yaml_path <- file.path(study_root, "replication.yml")
  skip_if_not(file.exists(yaml_path), "replication.yml missing")
  cfg <- yaml::read_yaml(yaml_path)
  ids <- vapply(cfg$steps, function(x) x$id, character(1))
  expect_true(all(c("tab_3a", "fig_1", "fig_7") %in% ids))
})

test_that("dataverse manifest includes analysis csv", {
  manifest_path <- file.path(study_root, "manifest", "dataverse_files.csv")
  skip_if_not(file.exists(manifest_path), "manifest missing")
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
  expect_true(any(manifest$path == "Data/final_data.csv"))
})

test_that("prep step script exists", {
  expect_true(file.exists(file.path(study_root, "code/steps/prep_analysis_data.R")))
})

test_that("figure smoke test runs when prep data present", {
  rds <- file.path(study_root, "outputs/prep_analysis_data/analysis_data.rds")
  skip_if_not(file.exists(rds), "Prepared data not built yet")
  old <- setwd(study_root)
  on.exit(setwd(old), add = TRUE)
  Sys.setenv(REPLICATE_STUDY_ROOT = study_root)
  source("code/figures/fig_1.R")
  expect_s3_class(make_fig_1(), "ggplot")
})
