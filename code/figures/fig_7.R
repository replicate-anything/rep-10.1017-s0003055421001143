source("../helpers/study_paths.R", local = TRUE)
source("../helpers/outcome_labels.R", local = TRUE)
source("../helpers/strat_reg.R", local = TRUE)

make_fig_7 <- function(data = NULL, root = study_root()) {
  if (is.null(data)) data <- load_analysis_data()
  labels <- outcome_labels()

  dfcoef <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, Vignette, VignetteOutcome, aut, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade", judgment != "nocourt") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, Vignette) |>
    dplyr::do(strat_reg(agree ~ judgment * aut, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "judgmentdisagree", "Not authoritarian", term))

  dfcoefflip <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, Vignette, VignetteOutcome, aut, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade", judgment != "nocourt") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, Vignette) |>
    dplyr::mutate(aut2 = aut * -1 + 1) |>
    dplyr::do(strat_reg(agree ~ judgment * aut2, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "judgmentdisagree", "Authoritarian", term))

  dfcoef <- dplyr::bind_rows(dfcoef, dfcoefflip)
  dfcoef$outvar <- factor(dfcoef$outvar, rev(levels(as.factor(dfcoef$outvar))))

  p <- ggplot2::ggplot(
    dplyr::filter(dfcoef, term == "Not authoritarian" | term == "Authoritarian"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high, colour = term)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::labs(colour = "Authoritarianism") +
    ggplot2::facet_wrap(~Vignette, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(
      name = "Outcome measure",
      breaks = c("D1Agree", "D2Agree", "D3Agree"),
      labels = labels$primary_short
    ) +
    ggplot2::geom_vline(xintercept = 1.5, lty = 3) +
    ggplot2::geom_vline(xintercept = 2.5, lty = 3) +
    ggplot2::ylab("Estimate") +
    ggplot2::ggtitle("EC disagrees") +
    ggplot2::scale_color_manual(values = c("#000000", "#808080"))

  dfcoef2 <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, VignetteOutcome, Vignette, aut, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, VignetteOutcome) |>
    dplyr::do(strat_reg(agree ~ caseOutcome * aut, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "caseOutcome", "Not authoritarian", term))

  dfcoefflip2 <- data |>
    dplyr::select(D1Agree:D3Agree, caseOutcome, judgment, VignetteOutcome, Vignette, aut, LGBTRights, stratum) |>
    dplyr::filter(Vignette != "Pride Parade") |>
    tidyr::pivot_longer(cols = D1Agree:D3Agree, names_to = "outvar", values_to = "agree") |>
    dplyr::group_by(outvar, VignetteOutcome) |>
    dplyr::mutate(aut2 = aut * -1 + 1) |>
    dplyr::do(strat_reg(agree ~ caseOutcome * aut2, data = ., stratum = stratum)) |>
    dplyr::mutate(term = as.character(term)) |>
    dplyr::mutate(term = dplyr::if_else(term == "caseOutcome", "Authoritarian", term))

  dfcoef2 <- dplyr::bind_rows(dfcoef2, dfcoefflip2)
  dfcoef2$outvar <- factor(dfcoef2$outvar, rev(levels(as.factor(dfcoef2$outvar))))

  r <- ggplot2::ggplot(
    dplyr::filter(dfcoef2, term == "Not authoritarian" | term == "Authoritarian"),
    ggplot2::aes(y = estimate, x = outvar, ymin = conf.low, ymax = conf.high, colour = term)
  ) +
    ggplot2::coord_flip() +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_hline(yintercept = 0, lty = 4, color = "grey") +
    ggplot2::labs(colour = "Authoritarianism") +
    ggplot2::facet_wrap(~VignetteOutcome, ncol = 1) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = 1.5, lty = 3) +
    ggplot2::geom_vline(xintercept = 2.5, lty = 3) +
    ggplot2::ylab("Estimate") +
    ggplot2::scale_x_discrete(
      name = "Outcome measure",
      breaks = c("D1Agree", "D2Agree", "D3Agree"),
      labels = labels$primary_short
    ) +
    ggplot2::ggtitle("Applicant win") +
    ggplot2::scale_color_manual(values = c("#000000", "#808080"))

  figure <- ggpubr::ggarrange(p, r, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

  out <- output_path("outputs", "fig_7.png", root = root)
  ensure_dir(dirname(out))
  ggplot2::ggsave(out, plot = figure, height = 8, width = 8, units = "in")
  figure
}

format_fig_7 <- function(object) object

if (sys.nframe() == 0L) make_fig_7()
