source("code/helpers/study_paths.R", local = TRUE)
source("code/helpers/outcome_labels.R", local = TRUE)

make_fig_5a <- function(data = NULL, root = study_root()) {
  source("code/helpers/strat_reg.R", local = TRUE)
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfcoef <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, VignetteOutcome, Vignette, sympathy, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, VignetteOutcome) |>
    dplyr::do(strat_reg(agree ~ caseOutcome * sympathy, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "caseOutcome", "More sympathetic", term))

  dfcoefflip <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, VignetteOutcome, Vignette, sympathy, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, VignetteOutcome) |>
    dplyr::mutate(sympathy2 = sympathy * -1 + 1) |>
    dplyr::do(strat_reg(agree ~ caseOutcome * sympathy2, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "caseOutcome", "Unsympathetic", term))

  dfcoef <- dplyr::bind_rows(dfcoef, dfcoefflip)
  dfcoef$outvar <- factor(dfcoef$outvar, rev(levels(as.factor(dfcoef$outvar))))

  p <- ggplot2::ggplot(
    dplyr::filter(dfcoef, term == "More sympathetic" | term == "Unsympathetic"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high, colour = term)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::labs(colour = "Sympathy towards\nrights claimant") +
    ggplot2::facet_wrap(~VignetteOutcome, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Effect of Outcome,\nby predispositions towards rights claimant") +
    ggplot2::scale_x_discrete(
      name = "Outcome measure",
      breaks = c("D1Agree", "D2Agree", "D3Agree"),
      labels = labels$primary
    ) +
    ggplot2::geom_vline(xintercept = 1.5, lty = 3) +
    ggplot2::geom_vline(xintercept = 2.5, lty = 3) +
    ggplot2::ylab("Estimate") +
    ggplot2::scale_color_manual(values = c("#808080", "#000000"))

  out <- output_path("outputs", "fig_5a.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = p, height = 6, width = 6, units = "in")
  p
}

format_fig_5a <- function(object) object

if (sys.nframe() == 0L) make_fig_5a()
