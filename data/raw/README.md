# Raw data

Analysis data for Voeten et al. (2021), *American Political Science Review*.

| Item | Value |
|------|-------|
| Harvard Dataverse DOI | https://doi.org/10.7910/DVN/WJQITU |
| Listed filename | `final_data.tab` |
| Native format | `Data/final_data.csv` (~22 MB) |
| Paper DOI | https://doi.org/10.1017/S0003055421001143 |

**Pattern C (deposit cache):** the `access_deposit` step downloads the original
archive and extracts `Data/final_data.csv` to `outputs/deposit/`. No binary is
committed to git.

Author driver: `replication_final.Rmd` (monolithic R Markdown with all tables and
figures). See `README.md` at repo root for the replicateEverything step DAG.
