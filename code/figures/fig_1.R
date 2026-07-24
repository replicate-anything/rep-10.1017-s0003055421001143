source("code/helpers/study_paths.R", local = TRUE)
source("code/helpers/outcome_labels.R", local = TRUE)
source("code/helpers/strat_reg.R", local = TRUE)

make_fig_1 <- function(data = NULL, root = study_root()) {
  source("code/helpers/strat_reg.R", local = TRUE)
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfsum <- data |>
    dplyr::select(D1Agree:D3Agree, treatmentLabel, Vignette, country, stratum) |>
    dplyr::filter(Vignette != "Pride Parade") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outcome", values_to = "agree") |>
    dplyr::group_by(outcome, treatmentLabel, Vignette) |>
    dplyr::do(strat_reg(agree ~ 1, data = ., stratum = stratum))

  p <- ggplot2::ggplot(dfsum, ggplot2::aes(x = outcome, y = estimate, colour = treatmentLabel)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      size = .3, width = .2, position = ggplot2::position_dodge(.9)
    ) +
    ggplot2::geom_vline(xintercept = 1.5, lty = 2) +
    ggplot2::geom_vline(xintercept = 2.5, lty = 2) +
    ggplot2::scale_y_continuous(limits = c(0.3, 1)) +
    ggplot2::labs(colour = "Treatment") +
    ggplot2::theme_light() +
    ggplot2::scale_x_discrete(labels = labels$primary_short) +
    ggplot2::ylab("Proportion Agree") +
    ggplot2::xlab("Outcome") +
    ggplot2::facet_wrap(~Vignette, ncol = 1) +
    ggplot2::scale_color_manual(values = c("#FF0000", "#FF8000", "#0000FF", "#0080FF", "#000000", "#808080"))

  out <- output_path("outputs", "fig_1.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = p, height = 5, width = 6.5, units = "in")
  p
}

format_fig_1 <- function(object) object

if (sys.nframe() == 0L) make_fig_1()
