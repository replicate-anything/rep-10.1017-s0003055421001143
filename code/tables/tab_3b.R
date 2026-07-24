source("../helpers/study_paths.R")

make_tab_3b <- function(data = NULL, root = study_root()) {
  if (is.null(data)) data <- load_analysis_data()

  table1::label(data$D1Agree) <- "Agreement"
  table1::label(data$D2Agree) <- "Implementation"
  table1::label(data$D3Agree) <- "Authority"
  table1::label(data$SatLaw) <- "Satisfaction Domestic Law"
  table1::label(data$sympathy) <- "Sympathy Applicant"
  table1::label(data$National) <- "Nationalist"
  table1::label(data$Autscale) <- "Authoritarianism"

  html_path <- output_path("outputs", "tab_3b.html", root = root)
  ensure_dir(dirname(html_path))

  capture <- function(expr) {
    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp), add = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    sink(tmp)
    on.exit(sink(), add = TRUE)
    expr
    invisible(readLines(tmp, warn = FALSE))
  }

  part2 <- capture(
    print(table1::table1(
      ~ D1Agree + D2Agree + D3Agree + SatLaw + sympathy + National + Autscale | countryLabel * Vignette,
      data = dplyr::filter(data, Vignette == "Quran burning" | Vignette == "Eviction"),
      render = "Mean (SD)"
    ))
  )

  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<title>Table 3b</title></head><body>",
    "<h1>Table 3b — Summary statistics, Quran burning and Eviction vignettes</h1>",
    "<pre>", paste(part2, collapse = "\n"), "</pre>",
    "</body></html>"
  )
  writeLines(html, html_path, useBytes = TRUE)
  html_path
}

format_tab_3b <- function(object) {
  if (is.character(object) && file.exists(object)) {
    return(paste(readLines(object, warn = FALSE), collapse = "\n"))
  }
  object
}

if (sys.nframe() == 0L) make_tab_3b()
