source("code/helpers/study_paths.R", local = TRUE)
source("code/helpers/outcome_labels.R", local = TRUE)

make_fig_4a <- function(data = NULL, root = study_root()) {
  source("code/helpers/strat_reg.R", local = TRUE)
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfcoef <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, VignetteOutcome, stratum) |>
    dplyr::filter(VignetteOutcome != "Pride parade not banned") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, VignetteOutcome) |>
    dplyr::do(broom::tidy(estimatr::lm_lin(agree ~ caseOutcome, covariates = ~stratum, data = .))) |>
    dplyr::ungroup()

  dfcoef$outvar <- factor(dfcoef$outvar, rev(levels(as.factor(dfcoef$outvar))))

  p <- ggplot2::ggplot(
    dplyr::filter(dfcoef, term == "caseOutcome"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::ylab("Estimate") +
    ggplot2::facet_wrap(~VignetteOutcome, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete("Outcome variable", labels = rev(labels$primary)) +
    ggplot2::ggtitle("Effect of case outcome")

  out <- output_path("outputs", "fig_4a.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = p, height = 4, width = 6, units = "in")
  p
}

format_fig_4a <- function(object) object

if (sys.nframe() == 0L) make_fig_4a()
