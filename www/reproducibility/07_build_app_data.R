#!/usr/bin/env Rscript
# Build the app's frozen data layer from the paper's own tables.
# Base R only, so the app has no dependency beyond shiny and ggplot2.
here    <- normalizePath(".")
root    <- normalizePath(file.path(here, "..", ".."))
tables  <- file.path(root, "submission_draft/REWRITE_2026-08/supplementary_final/tables")
results <- file.path(root, "submission_draft/REANALYSIS_2026-08/results")
figs    <- file.path(root, "figures")
out     <- file.path(here, "data-processed")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

grab <- function(from, pattern, to) {
  f <- list.files(from, pattern = pattern, full.names = TRUE)
  if (!length(f)) { message("  ! missing: ", pattern); return(invisible(NULL)) }
  d <- read.csv(f[1], check.names = FALSE, stringsAsFactors = FALSE)
  write.csv(d, file.path(out, to), row.names = FALSE)
  message(sprintf("  %-38s %5d x %2d", to, nrow(d), ncol(d)))
}
message("building app data layer")
grab(tables,  "S5_MOFA_factor_summary",       "factors.csv")
grab(tables,  "S6_factor1_loadings",          "loadings.csv")
grab(tables,  "S7_pair_prior_evidence",       "pairs.csv")
grab(results, "04_G1_pair_destruction_null",  "pair_null.csv")
grab(results, "04_G1_pair_destruction[.]csv", "pair_null_summary.csv")
grab(tables,  "S3_candidate_miRNA_ranking",   "candidates.csv")
grab(tables,  "S10_cross_compartment",        "concordance.csv")
grab(tables,  "S11_incremental_AUROC",        "auroc.csv")
grab(results, "05_Q4_clinical_tiers",         "clinical_tiers.csv")
grab(tables,  "S12_tissue_bridge_summary",    "tissue_bridge.csv")
grab(tables,  "S13_state_programs",           "programs.csv")
grab(tables,  "S14_cargo_program",            "cargo_programs.csv")
grab(results, "04_G4_ALS_only_significant",   "als_only.csv")
km <- file.path(figs, "F3/Main/Part E/plot_data.csv")
if (file.exists(km)) grab(dirname(km), "^plot_data[.]csv$", "km_plasma.csv")
file.copy("config/paper_settings.yml", file.path(out, "paper_settings.yml"), overwrite = TRUE)
message("\ndone -> ", out)
