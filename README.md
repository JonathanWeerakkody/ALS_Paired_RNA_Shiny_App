# ALS Paired RNA Manuscript Companion

Live companion app: [Open the public Shiny app](https://jonathanweerakkody.shinyapps.io/als-paired-rna/)

Source code and released source data: [GitHub repository](https://github.com/JonathanWeerakkody/ALS_Paired_RNA_Shiny_App)

This Shiny application guides readers through the manuscript's evidence chain
using 15 purpose built, web native graphs generated from the released data.
The graphs explain the analysis but do not imitate or redistribute the dense,
author edited publication figures.

## Guided story

The sequence integrates all five main figures with all ten Extended Data figures:

1. Measurement foundation and EV phenotype
2. RNA composition, motif and junction quality controls
3. Outcome guided dual RNA discovery and clinical sensitivity
4. Directional miRNA mRNA pair support
5. Five miRNA transfer to total plasma and processing sensitivity
6. Horizon dependent prediction benchmarking
7. Five mRNA cortical bridge, external context and lineage gating
8. Cross arm pathway convergence

Each step has one scientific question, a manuscript-derived guide, a bounded
statement of what the evidence supports, one reading cue and one explanatory
graph. Readers can download the plotted CSV, generated graph and source workbook.

## Run

```r
install.packages(c("shiny", "ggplot2", "readxl"))
shiny::runApp()
```

Run from this directory. The application has no network calls and no absolute
paths. The 15 released source workbooks are included in `source_data/`, so the
app is self contained for GitHub or shinyapps.io deployment. Personal raw FASTQ
files are not part of this public app package.

## Downloads and reproducibility

The public interface has four tabs: `Guided story`, `Downloads`,
`Audit and agents` and `About`.
The `Audit and agents` tab provides a complete machine-readable story and claim
trail plus stable schema, manifest, catalogue and R verification entry points.
Code is intentionally kept out of the main reading path. The Downloads tab
provides a complete ZIP containing all source workbooks, processed tables,
metadata and R code. Direct machine-readable entry points include:

- `config/evidence_manifest.json`
- `config/evidence_trail.csv`
- `config/story_steps.csv`
- `config/paper_settings.yml`
- `data-processed/panels/*.csv`
- `R/verify.R`
- the JSON output from `Rscript verify.R --json`


## Context of use

Research use only. The app does not provide patient-level estimates, refit the
published models, alter the locked feature set, or store uploaded data. The
hosted audit starts from bundled processed evidence. Full preprocessing and
MOFA+ reconstruction remain part of the downloadable repository pipeline and
use public GEO inputs after release.

## Verification

```bash
Rscript verify.R
```

The command exits with a nonzero status if a claim check fails.

## Public deployment

Create a GitHub repository containing this folder, including `app.R`, `R/`,
`analysis/`, `config/`, `data-processed/`, `demo-data/`, `source_data/` and the
README files. The `.gitignore` excludes local inspection artifacts and R session
files. Do not add GEO credentials, private FASTQ files or access tokens.

For a free hosted app, use shinyapps.io and deploy this same folder with
`rsconnect::deployApp()`. The GitHub repository is the public code and source
archive; shinyapps.io is the runnable web link. The live URL should be added to
the manuscript Code Availability statement after deployment.

## Legacy app

The previous exploratory interface is preserved as
`app_legacy_figure_browser.R` for reference and is not launched by default.
