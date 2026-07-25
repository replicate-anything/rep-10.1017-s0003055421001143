source("../helpers/study_paths.R", local = TRUE)

make_prep_analysis_data <- function(root = study_root()) {
  csv_path <- file.path(root, "outputs", "final_data.csv")
  if (!file.exists(csv_path)) {
    stop("Run access_data first; missing ", csv_path, call. = FALSE)
  }

  data <- readr::read_csv(csv_path, show_col_types = FALSE)

  data <- data |>
    dplyr::mutate(
      ageBin = dplyr::case_when(
        QQuotas2 >= 18 & QQuotas2 <= 34 ~ "18-34",
        QQuotas2 >= 35 & QQuotas2 <= 50 ~ "35-50",
        QQuotas2 >= 51 ~ "51+"
      )
    )
  data$gender <- data$QQuotas1
  data <- data |>
    dplyr::mutate(
      college = dplyr::case_when(
        QQuotas3 == "ISCED 0:  Early childhood education (‘less than primary’ )" ~ "Non-College",
        QQuotas3 == "ISCED 1: Primary education" ~ "Non-College",
        QQuotas3 == "ISCED 2: Lower secondary education" ~ "Non-College",
        QQuotas3 == "ISCED 3: Upper secondary education" ~ "Non-College",
        QQuotas3 == "ISCED 4: Post-secondary non-tertiary education (NOT a University degree)" ~ "Non-College",
        QQuotas3 == "ISCED 5: Short-cycle tertiary education" ~ "College",
        QQuotas3 == "ISCED 6: Bachelor’s or equivalent level" ~ "College",
        QQuotas3 == "ISCED 7: Master’s or equivalent level" ~ "College",
        QQuotas3 == "ISCED 8: Doctoral or equivalent level" ~ "College"
      )
    )

  data$stratum <- paste(data$country, data$gender, data$college, data$ageBin, sep = "|")
  data$stratum_noage <- paste(data$country, data$gender, data$college, sep = "|")

  data <- data |>
    dplyr::mutate(
      D1Agree = as.integer(!grepl("disagree", D1, ignore.case = TRUE)),
      D2Agree = as.integer(!grepl("disagree", D2, ignore.case = TRUE)),
      D3Agree = as.integer(!grepl("disagree", D3, ignore.case = TRUE)),
      D4Agree = as.integer(!grepl("disagree", D4, ignore.case = TRUE)),
      D5Agree = as.integer(!grepl("disagree", D5, ignore.case = TRUE)),
      D6Agree = as.integer(!grepl("disagree", D6, ignore.case = TRUE))
    )

  data$D1Agree[is.na(data$D1)] <- NA
  data$D2Agree[is.na(data$D2)] <- NA
  data$D3Agree[is.na(data$D3)] <- NA
  data$D4Agree[is.na(data$D4)] <- NA
  data$D5Agree[is.na(data$D5)] <- NA
  data$D6Agree[is.na(data$D6)] <- NA

  data <- data |>
    dplyr::mutate(
      D1AgreeC = dplyr::case_when(
        D1 == "Strongly disagree" ~ 1, D1 == "Disagree" ~ 2, D1 == "Slightly disagree" ~ 3,
        D1 == "Slightly agree" ~ 4, D1 == "Agree" ~ 5, D1 == "Strongly agree" ~ 6
      ),
      D2AgreeC = dplyr::case_when(
        D2 == "Strongly disagree" ~ 1, D2 == "Disagree" ~ 2, D2 == "Slightly disagree" ~ 3,
        D2 == "Slightly agree" ~ 4, D2 == "Agree" ~ 5, D2 == "Strongly agree" ~ 6
      ),
      D3AgreeC = dplyr::case_when(
        D3 == "Strongly disagree" ~ 1, D3 == "Disagree" ~ 2, D3 == "Slightly disagree" ~ 3,
        D3 == "Slightly agree" ~ 4, D3 == "Agree" ~ 5, D3 == "Strongly agree" ~ 6
      ),
      D4AgreeC = dplyr::case_when(
        D4 == "Strongly disagree" ~ 1, D4 == "Disagree" ~ 2, D4 == "Slightly disagree" ~ 3,
        D4 == "Slightly agree" ~ 4, D4 == "Agree" ~ 5, D4 == "Strongly agree" ~ 6
      ),
      D5AgreeC = dplyr::case_when(
        D5 == "Strongly disagree" ~ 1, D5 == "Disagree" ~ 2, D5 == "Slightly disagree" ~ 3,
        D5 == "Slightly agree" ~ 4, D5 == "Agree" ~ 5, D5 == "Strongly agree" ~ 6
      ),
      D6AgreeC = dplyr::case_when(
        D6 == "Strongly disagree" ~ 1, D6 == "Disagree" ~ 2, D6 == "Slightly disagree" ~ 3,
        D6 == "Slightly agree" ~ 4, D6 == "Agree" ~ 5, D6 == "Strongly agree" ~ 6
      )
    )

  data <- data |>
    dplyr::mutate(
      SatLaw = dplyr::case_when(
        QA4 == "Dissatisfied" ~ 0,
        QA4 == "Very dissatisfied" ~ 0,
        QA4 == "Very Satisfied" ~ 1,
        QA4 == "Satisfied" ~ 1,
        QA4 == "Neither dissatisfied nor satisfied" ~ 0
      ),
      trust = dplyr::case_when(
        QA1 == "You can't be too careful in dealing with people (1)" ~ 1,
        QA1 == "(2)" ~ 2, QA1 == "(3)" ~ 3, QA1 == "(4)" ~ 4,
        QA1 == "(5)" ~ 5, QA1 == "(6)" ~ 6,
        QA1 == "Most people can be trusted (7)" ~ 7
      ),
      ideology = dplyr::case_when(
        QC1 == "Extreme Left (0)" ~ 0, QC1 == "(1)" ~ 1, QC1 == "(2)" ~ 2,
        QC1 == "(3)" ~ 3, QC1 == "(4)" ~ 4, QC1 == "(5)" ~ 5, QC1 == "(6)" ~ 6,
        QC1 == "(7)" ~ 7, QC1 == "(8)" ~ 8, QC1 == "(9)" ~ 9,
        QC1 == "Extreme right (10)" ~ 10
      ),
      National = dplyr::if_else(QC4 == "British only", 1, 0),
      Refugee = dplyr::if_else(
        QB9 == "To deny legal status to someone who did not misrepresent their situation (e.g. faces a death threat back home)",
        0, 1
      ),
      EvictFair = dplyr::if_else(
        QB10 == "To evict people who were treated unfairly by a landlord or mortgage company.",
        0, 1
      ),
      EvictFair2 = dplyr::case_when(
        QB5 == "Strongly agree" ~ 1, QB5 == "Agree" ~ 1, QB5 == "Slightly agree" ~ 1,
        QB5 == "Slightly disagree" ~ 0, QB5 == "Disagree" ~ 0, QB5 == "Strongly disagree" ~ 0
      ),
      LGBTNotSameRights = dplyr::case_when(
        QB1 == "Strongly agree" ~ 0, QB1 == "Agree" ~ 0, QB1 == "Slightly agree" ~ 0,
        QB1 == "Slightly disagree" ~ 1, QB1 == "Disagree" ~ 1, QB1 == "Strongly disagree" ~ 1
      ),
      LGBTRights = dplyr::case_when(
        QB1 == "Strongly agree" ~ 6, QB1 == "Agree" ~ 5, QB1 == "Slightly agree" ~ 4,
        QB1 == "Slightly disagree" ~ 3, QB1 == "Disagree" ~ 2, QB1 == "Strongly disagree" ~ 1
      ),
      IslamSymbol = dplyr::case_when(
        QB4 == "Strongly agree" ~ 0, QB4 == "Agree" ~ 0, QB4 == "Slightly agree" ~ 0,
        QB4 == "Slightly disagree" ~ 1, QB4 == "Disagree" ~ 1, QB4 == "Strongly disagree" ~ 1
      ),
      Immigrants = dplyr::case_when(
        QB3 == "Strongly agree" ~ 1, QB3 == "Agree" ~ 1, QB3 == "Slightly agree" ~ 1,
        QB3 == "Slightly disagree" ~ 0, QB3 == "Disagree" ~ 0, QB3 == "Strongly disagree" ~ 0
      ),
      Aut1 = dplyr::case_when(
        QC2_1 == "Strongly agree" ~ 0, QC2_1 == "Agree" ~ 1, QC2_1 == "Slightly agree" ~ 2,
        QC2_1 == "Slightly disagree" ~ 3, QC2_1 == "Disagree" ~ 4, QC2_1 == "Strongly disagree" ~ 5
      ),
      Aut2 = dplyr::case_when(
        QC2_2 == "Strongly agree" ~ 5, QC2_2 == "Agree" ~ 4, QC2_2 == "Slightly agree" ~ 3,
        QC2_2 == "Slightly disagree" ~ 2, QC2_2 == "Disagree" ~ 2, QC2_2 == "Strongly disagree" ~ 2
      ),
      Aut3 = dplyr::case_when(
        QC2_3 == "Strongly agree" ~ 5, QC2_3 == "Agree" ~ 4, QC2_3 == "Slightly agree" ~ 3,
        QC2_3 == "Slightly disagree" ~ 2, QC2_3 == "Disagree" ~ 1, QC2_3 == "Strongly disagree" ~ 0
      ),
      Aut4 = dplyr::case_when(
        QC2_4 == "Strongly agree" ~ 0, QC2_4 == "Agree" ~ 1, QC2_4 == "Slightly agree" ~ 2,
        QC2_4 == "Slightly disagree" ~ 3, QC2_4 == "Disagree" ~ 4, QC2_4 == "Strongly disagree" ~ 5
      ),
      Autscale = (Aut1 + Aut2 + Aut3 + Aut4) / 20,
      Nat1 = dplyr::case_when(
        QC2_5 == "Strongly agree" ~ 1, QC2_5 == "Agree" ~ .8, QC2_5 == "Slightly agree" ~ .6,
        QC2_5 == "Slightly disagree" ~ .4, QC2_5 == "Disagree" ~ .2, QC2_5 == "Strongly disagree" ~ 0
      ),
      Nat2 = dplyr::case_when(
        QC2_6 == "Strongly agree" ~ 1, QC2_6 == "Agree" ~ .8, QC2_6 == "Slightly agree" ~ .6,
        QC2_6 == "Slightly disagree" ~ .4, QC2_6 == "Disagree" ~ .2, QC2_6 == "Strongly disagree" ~ 0
      ),
      Nat3 = dplyr::case_when(
        QC3 == "Very proud" ~ 1, QC3 == "Somewhat proud" ~ .75,
        QC3 == "Not very proud" ~ .6, QC3 == "Not proud at all" ~ .4
      ),
      Natscale = National + Nat1 + Nat2 + Nat3
    )

  data <- data |>
    dplyr::mutate(
      aut = dplyr::if_else(Autscale > .5, 1, 0),
      nat = dplyr::if_else(Natscale > 1.4, 1, 0)
    )

  data <- data |>
    dplyr::mutate(
      Vignette = dplyr::case_when(
        vignette == "immigration" ~ "Deportation",
        vignette == "other" & (country == "uk" | country == "denmark") ~ "Quran burning",
        vignette == "other" & (country == "spain" | country == "france") ~ "Eviction",
        vignette == "other" & country == "poland" ~ "Pride Parade"
      ),
      caseOutcome = dplyr::case_when(
        outcome == "can" & judgment == "agree" ~ 0,
        outcome == "can" & judgment == "disagree" ~ 1,
        outcome == "cannot" & judgment == "disagree" ~ 0,
        outcome == "cannot" & judgment == "agree" ~ 1,
        outcome == "can" & judgment == "nocourt" ~ 0,
        outcome == "cannot" & judgment == "nocourt" ~ 1
      )
    )
  data$caseOutcome <- dplyr::if_else(data$Vignette == "Pride Parade", data$caseOutcome * -1 + 1, data$caseOutcome)

  data <- data |>
    dplyr::mutate(
      VignetteOutcome = dplyr::case_when(
        Vignette == "Quran burning" ~ "Quran burning, no fine",
        Vignette == "Eviction" ~ "Eviction canceled",
        Vignette == "Pride Parade" ~ "Pride parade not banned",
        Vignette == "Deportation" ~ "Deportation prohibited"
      ),
      treatmentBlock = dplyr::case_when(
        outcome == "can" & judgment == "agree" ~ "Entitled, Upheld",
        outcome == "can" & judgment == "disagree" ~ "Entitled, Overturned",
        outcome == "cannot" & judgment == "disagree" ~ "Not Entitled, Overturned",
        outcome == "cannot" & judgment == "agree" ~ "Not Entitled, Upheld",
        outcome == "can" & judgment == "nocourt" ~ "Entitled, No Court",
        outcome == "cannot" & judgment == "nocourt" ~ "Not Entitled, No Court"
      ),
      treatmentLabel = dplyr::case_when(
        treatmentBlock == "Entitled, Upheld" ~ "Applicant loses, EC defers (2)",
        treatmentBlock == "Entitled, Overturned" ~ "Applicant wins, EC overturns (3)",
        treatmentBlock == "Not Entitled, Overturned" ~ "Applicant loses, EC overturns (4)",
        treatmentBlock == "Not Entitled, Upheld" ~ "Applicant wins, EC defers (1)",
        treatmentBlock == "Entitled, No Court" ~ "Applicant loses, No EC (6)",
        treatmentBlock == "Not Entitled, No Court" ~ "Applicant wins, No EC (5)"
      ),
      sympathy = dplyr::case_when(
        Vignette == "Deportation" ~ Refugee,
        Vignette == "Quran burning" ~ IslamSymbol,
        Vignette == "Eviction" ~ EvictFair,
        Vignette == "Pride Parade" ~ LGBTNotSameRights
      )
    )

  data$countryLabel <- data$country
  data$countryLabel[data$country == "denmark"] <- "Denmark"
  data$countryLabel[data$country == "uk"] <- "UK"
  data$countryLabel[data$country == "poland"] <- "Poland"
  data$countryLabel[data$country == "france"] <- "France"
  data$countryLabel[data$country == "spain"] <- "Spain"

  out_dir <- file.path(root, "outputs", "prep_analysis_data")
  ensure_dir(out_dir)
  out_path <- file.path(out_dir, "analysis_data.rds")
  saveRDS(data, out_path)
  data
}

if (sys.nframe() == 0L) {
  prepare_analysis_data()
}
