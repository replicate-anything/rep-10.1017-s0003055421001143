test_that("replication.yml lists main-text replications", {
  yaml_path <- file.path(getwd(), "replication.yml")
  skip_if_not(file.exists(yaml_path), "Run tests from study repo root")
  cfg <- yaml::read_yaml(yaml_path)
  ids <- vapply(cfg$steps, function(x) x$id, character(1))
  expect_true(all(c("tab_3a", "fig_1", "fig_7") %in% ids))
})

test_that("dataverse manifest includes analysis csv", {
  manifest <- read.csv("manifest/dataverse_files.csv", stringsAsFactors = FALSE)
  expect_true(any(manifest$path == "Data/final_data.csv"))
})

test_that("prep step script exists", {
  expect_true(file.exists("code/steps/prep_analysis_data.R"))
})

test_that("figure smoke test runs when prep data present", {
  rds <- "outputs/prep_analysis_data/analysis_data.rds"
  skip_if_not(file.exists(rds), "Prepared data not built yet")
  source("code/figures/fig_1.R")
  expect_s3_class(make_fig_1(), "ggplot")
})
