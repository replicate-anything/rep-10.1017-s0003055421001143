source("../helpers/study_paths.R", local = TRUE)
source("../helpers/outcome_labels.R", local = TRUE)
source("../helpers/strat_reg.R", local = TRUE)

make_fig_2b <- function(data = NULL, root = study_root()) {
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfcoef <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, Vignette, country = countryLabel, stratum) |>
    dplyr::filter(Vignette != "Pride Parade") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, Vignette, country) |>
    dplyr::do(broom::tidy(estimatr::lm_lin(agree ~ judgment, covariates = ~stratum, data = .))) |>
    dplyr::ungroup()

  dfcoef$outvar <- factor(dfcoef$outvar, rev(levels(as.factor(dfcoef$outvar))))

  p <- ggplot2::ggplot(
    dplyr::filter(dfcoef, term == "judgmentdisagree"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::ylab("Estimate") +
    ggplot2::facet_wrap(~ Vignette + country, ncol = 3) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete("Outcome variable", labels = rev(labels$primary)) +
    ggplot2::ggtitle("Effect of EC disagreeing with domestic court by country")

  out <- output_path("outputs", "fig_2b.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = p, height = 10, width = 8, units = "in")
  p
}

format_fig_2b <- function(object) object

if (sys.nframe() == 0L) make_fig_2b()
