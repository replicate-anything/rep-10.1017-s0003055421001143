source("code/helpers/study_paths.R", local = TRUE)
source("code/helpers/outcome_labels.R", local = TRUE)

make_fig_5b <- function(data = NULL, root = study_root()) {
  source("code/helpers/strat_reg.R", local = TRUE)
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfcoef <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, VignetteOutcome, Vignette, sympathy, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, VignetteOutcome) |>
    dplyr::do(strat_reg(agree ~ caseOutcome * sympathy, data = ., stratum = stratum))

  dfcoef$outvar <- factor(dfcoef$outvar, rev(levels(as.factor(dfcoef$outvar))))

  p <- ggplot2::ggplot(
    dplyr::filter(dfcoef, term == "caseOutcome:sympathy"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::facet_wrap(~VignetteOutcome, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Difference in effect of outcome\nbetween unsympathetic and more sympathetic respondents") +
    ggplot2::scale_x_discrete(
      name = "Outcome measure",
      breaks = c("D1Agree", "D2Agree", "D3Agree"),
      labels = labels$primary
    ) +
    ggplot2::ylab("Estimate")

  out <- output_path("outputs", "fig_5b.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = p, height = 6, width = 8, units = "in")
  p
}

format_fig_5b <- function(object) object

if (sys.nframe() == 0L) make_fig_5b()
