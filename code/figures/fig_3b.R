source("../helpers/study_paths.R", local = TRUE)
source("../helpers/outcome_labels.R", local = TRUE)
source("../helpers/strat_reg.R", local = TRUE)

make_fig_3b <- function(data = NULL, root = study_root()) {
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfcoef <- data |>
    dplyr::select(D1Agree:D3Agree, country, caseOutcome, judgment, VignetteOutcome, Vignette, SatLaw, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade", judgment != "nocourt", country == "uk" | country == "denmark") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, Vignette) |>
    dplyr::do(strat_reg(agree ~ judgment * SatLaw, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "judgmentdisagree", "Not satisfied", term))

  dfcoefflip <- data |>
    dplyr::select(D1Agree:D3Agree, country, caseOutcome, judgment, VignetteOutcome, Vignette, SatLaw, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade", judgment != "nocourt", country == "uk" | country == "denmark") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, Vignette) |>
    dplyr::mutate(SatLaw2 = SatLaw * -1 + 1) |>
    dplyr::do(strat_reg(agree ~ judgment * SatLaw2, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "judgmentdisagree", "Satisfied", term))

  dfcoef <- dplyr::bind_rows(dfcoef, dfcoefflip)
  dfcoef$outvar <- factor(dfcoef$outvar, rev(levels(as.factor(dfcoef$outvar))))

  p <- ggplot2::ggplot(
    dplyr::filter(dfcoef, term == "Not satisfied" | term == "Satisfied"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high, colour = term)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::labs(colour = "Rule of law\nsatisfaction") +
    ggplot2::facet_wrap(~Vignette, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Effect of EC disagreement,\nby satisfaction with domestic rule of law\n(UK/Denmark Only)") +
    ggplot2::scale_x_discrete(
      name = "Outcome measure",
      breaks = c("D1Agree", "D2Agree", "D3Agree"),
      labels = labels$primary
    ) +
    ggplot2::geom_vline(xintercept = 1.5, lty = 3) +
    ggplot2::geom_vline(xintercept = 2.5, lty = 3) +
    ggplot2::ylab("Estimate") +
    ggplot2::scale_color_manual(values = c("#000000", "#808080"))

  out <- output_path("outputs", "fig_3b.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = p, height = 7, width = 6, units = "in")
  p
}

format_fig_3b <- function(object) object

if (sys.nframe() == 0L) make_fig_3b()
