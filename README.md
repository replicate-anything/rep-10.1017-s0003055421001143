# Sovereignty, Substance, and Public Support for European Courts' Human Rights Rulings

replicateEverything study repo for Voeten et al. (2021), *American Political Science Review*.

| | |
|---|---|
| Paper DOI | https://doi.org/10.1017/S0003055421001143 |
| Dataverse DOI | https://doi.org/10.7910/DVN/WJQITU |
| Author driver | `replication_final.Rmd` (monolithic R Markdown) |
| Engine | R |

## Source deposit

Harvard Dataverse archive ([doi:10.7910/DVN/WJQITU](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WJQITU)) contains:

| Path | Role |
|------|------|
| `Data/final_data.csv` | Analysis data (~22 MB; listed as `final_data.tab`) |
| `replication_final.Rmd` | Annotated code for all main-text and appendix outputs |
| `Appendix/`, `QuestionnaireFinal.docx`, `IRBApproval.pdf` | Skipped for MVP replication |

There is no separate `ReadMe.txt`; pipeline order is documented in the Rmd introduction and section headers.

## Step DAG (main text MVP)

```
access_deposit → prep_analysis_data → tab_3, fig_1 … fig_7
```

| Step | Author reference | Output |
|------|------------------|--------|
| `access_deposit` | Dataverse fetch | `outputs/deposit/Data/final_data.csv` |
| `prep_analysis_data` | Rmd chunks: read data, strata, cleaning, vignette | `outputs/prep_analysis_data/analysis_data.rds` |
| `tab_3` | Table 3 — summary statistics | `outputs/tab_3.html` |
| `fig_1` | Figure 1 — treatment summaries | `outputs/fig_1.png` |
| `fig_2` | Figure 2 — H1 deference | `outputs/fig_2.png` |
| `fig_3` | Figure 3 — H1 × rule-of-law satisfaction | `outputs/fig_3.png` |
| `fig_4` | Figure 4 — H2 case outcome | `outputs/fig_4.png` |
| `fig_5` | Figure 5 — H2 × sympathy | `outputs/fig_5.png` |
| `fig_6` | Figure 6 — heterogeneity by nationalism | `outputs/fig_6.png` |
| `fig_7` | Figure 7 — heterogeneity by authoritarianism | `outputs/fig_7.png` |

Appendix tables (E1–E8) and figures (C1–C2, D1–D20) remain in the author Rmd for a follow-up pass.

## Layout

```
code/
  steps/       access_deposit.R, prep_analysis_data.R
  figures/     fig_1.R … fig_7.R
  tables/      tab_3.R
  helpers/     study_paths.R, strat_reg.R, outcome_labels.R, format_table.R
data/raw/      README only (data fetched from Dataverse)
manifest/      dataverse_files.csv
outputs/       display artifacts + intermediate step products
tests/testthat/
```

## Run locally

Requires [replicateEverything](https://github.com/replicate-anything/replicateEverything) and R 4.x with internet on first run (Dataverse download).

```r
Sys.setenv(REPLICATE_STUDY_ROOT = "/path/to/rep-10.1017-s0003055421001143")

# Manual smoke test
source("code/steps/access_deposit.R")
source("code/steps/prep_analysis_data.R")
source("code/figures/fig_1.R"); make_fig_1()
```

With replicateEverything loaded:

```r
run_replication("10.1017/S0003055421001143", "fig_1", given = "nothing", install_deps = TRUE)
build_study_outputs("rep-10.1017-s0003055421001143", install_deps = TRUE)
```

## R dependencies

`tidyverse`, `haven`, `estimatr`, `table1`, `ggpubr`, `knitr`, `readr`, `broom`, `dataverse`, `httr`

Appendix-only packages (`interflex`, `ggcorrplot`) are not required for the main-text MVP.

## Maintainer
