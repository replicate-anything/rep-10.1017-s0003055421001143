source("code/helpers/study_paths.R", local = TRUE)
source("code/helpers/outcome_labels.R", local = TRUE)
source("code/helpers/strat_reg.R", local = TRUE)

make_fig_3c <- function(data = NULL, root = study_root()) {
  source("code/helpers/strat_reg.R", local = TRUE)
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfcoef <- data |>
    dplyr::select(D1Agree:D3Agree, country, caseOutcome, judgment, VignetteOutcome, Vignette, SatLaw, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade", judgment != "nocourt", country == "uk" | country == "denmark") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, Vignette) |>
    dplyr::do(strat_reg(agree ~ judgment * SatLaw, data = ., stratum = stratum))

  dfcoef$outvar <- factor(dfcoef$outvar, rev(levels(as.factor(dfcoef$outvar))))

  p <- ggplot2::ggplot(
    dplyr::filter(dfcoef, term == "judgmentdisagree:SatLaw"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::labs(colour = "Rule of law\nsatistfaction") +
    ggplot2::facet_wrap(~Vignette, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Difference in effect of EC disagreement between\nrespondentssatisfied with domestic rule of law\nand respondents dissatisfied (UK/Denmark Only)") +
    ggplot2::scale_x_discrete(
      name = "Outcome measure",
      breaks = c("D1Agree", "D2Agree", "D3Agree"),
      labels = labels$primary
    ) +
    ggplot2::ylab("Estimate")

  out <- output_path("outputs", "fig_3c.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = p, height = 4, width = 7, units = "in")
  p
}

format_fig_3c <- function(object) object

if (sys.nframe() == 0L) make_fig_3c()
