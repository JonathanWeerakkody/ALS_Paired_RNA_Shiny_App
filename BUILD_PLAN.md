# ALS Paired-RNA Explorer — build plan

Interactive companion to the paper. Frozen-paper mode only: it loads precomputed outputs and never refits a
model, never accepts uploads on the public server, and never returns an individual survival estimate.

Current state: repository scaffolded, `config/paper_settings.yml` frozen, Tab 1 module written.

---

## What the app is for

One question, answered by walking the paper's own chain:

> How did the analysis move from paired GLAST-positive sEV RNA to a plasma-transferable, cortically grounded ALS
> progression program?

Everything else is out of scope and belongs to a later methods paper.

---

## Phase 1 — publication-ready minimum

Five tabs, in build order. Each depends only on files that already exist.

### 1.1 Discovery ladder ✅ written

`app/R/mod_discovery.R`. Nine clickable stages from cohort to cortical mapping; each reports the rule applied,
what survived, and where the result sits in the paper.

**Data:** `config/paper_settings.yml` plus the per-stage tables.
**Remaining:** wire the stage tables, add the Paper/Sensitivity toggle.

### 1.2 Factor explorer

Twelve factors, variance and survival association, top loadings per view, candidate flags.

A permanent banner on Factor 1:

> Factor 1 is outcome-guided because survival was included as a model view. Its survival association is a
> prioritization property, not independent evidence.

Add the unsupervised comparison as a switch, since that model already exists and is now Supplementary Note 8:
**Outcome-guided (paper)** against **Unsupervised (two views)**. Selecting the second shows that no factor
survives correction, which is the honest counterpart to the banner.

**Data:** `Supplementary_Table_S5_MOFA_factor_summary.csv`, `S6_factor1_loadings.csv`,
`results/05_Q1_outcome_free_mofa.csv`.

### 1.3 Pair explorer — the centrepiece

Interactive 19 × 16 matrix or bipartite network. Select a miRNA or an mRNA and see every partner, seed class,
database support, evidence tier, directional state, and the pair-state distribution across survival tertiles.

Filters: primary-supported only (119) against any evidence (173); evidence class; directional state.

Include the **pair-destruction control** as a precomputed null, not a live permutation: observed r = 0.738
against a 10,000-permutation null whose maximum was 0.602. Displaying that histogram with the observed value
marked is the single clearest way to convey why matched pairing matters.

**Data:** `S7_pair_prior_evidence.csv`, `results/04_G1_pair_destruction_null.csv`.

### 1.4 Transfer to plasma

Cross-compartment plot for all 19, the locked five highlighted, and the raw-rho against Cox-direction
distinction made explicit, since that is a known source of confusion.

Horizon selector at 12, 24, 36 and 54 months against the four-tier model ladder: clinical, +NfL, +RNA, +both.
This is where the paper's strongest clinical result lives, so it should be the default view on this tab.

A fixed context box:

> Context of use: candidate cohort-level prognostic stratification. Not diagnostic, not treatment-response, not
> individual clinical decision support.

**No input box for patient values.** That single omission is what keeps the app a research explorer.

**Data:** `S10_cross_compartment_concordance.csv`, `S11_incremental_AUROC.csv`, `results/05_Q4_clinical_tiers.csv`.

### 1.5 Brain biology

Select one of the 16 candidates or the five-gene core; see bulk and single-nucleus evidence and expression
across cell classes. Then select lineage, program and diagnosis subset and see the donor-level correlation with
rho, q and donor n, **including whether it holds within ALS alone** — which is the question a reader will ask.

**Data:** `S12_tissue_bridge_summary.csv`, `S13_state_programs.csv`, `S14_cargo_program_associations.csv`,
`results/04_G4_ALS_only_significant.csv`.

### 1.6 Reproducibility drawer

On every tab: source-data table, analysis script, config version, git commit, environment, and download buttons
for the current plot and its data. Plus a single **Download paper settings** that emits the frozen YAML.

---

## Phase 2 — only if straightforward

Bookmarkable views, plot export at publication resolution, and the sensitivity toggles already reported in the
paper (loading top-N, supported against all pairs, disease subset).

## Deferred to a later paper

Uploads, live MOFA fitting, arbitrary omic layers, generalized cell-type frameworks.

---

## Precompute versus live

**Precomputed and shipped as `.parquet` or `.rds`:** MOFA model and factor outputs, candidate sets, the 304-pair
matrix and evidence calls, all permutation nulls, plasma Cox and cross-validation results, horizon AUCs, tissue
bridge scores, donor-level associations, pathway enrichment.

**Live:** filtering, sorting, selection, subsetting, drawing a network subset, rendering existing statistics.

The app must not refit anything at page load.

---

## Data preparation step

The app reads from `data-processed/`, which is built once by a script that copies from
`../REWRITE_2026-08/supplementary_final/` and `../REANALYSIS_2026-08/results/`. That keeps a single source of
truth: the tables in the paper and the tables in the app are the same objects.

```
analysis/07_build_app_data.R      # supplementary_final + results -> data-processed/
```

Nothing in `data-processed/` should be hand-edited.

---

## Third-party data

The plasma, bulk cortical and single-nucleus resources are cited by accession and are **not** redistributed in
the repository. The app ships only derived summary statistics computed from them, which is what the tables
already contain.

---

## Release

Pin R packages with `renv`, pin the Python environment used for the frozen MOFA model, tag a release, archive to
Zenodo, and put the DOI in Code Availability. The manuscript already carries the Methods paragraph and the
Results sentence describing the companion.

---

## Honest scope note

Tab 1 is written; tabs 2 to 5 are specified but not implemented. The data they need all exists in
`supplementary_final/` and `REANALYSIS_2026-08/results/`, so the remaining work is Shiny plumbing rather than
analysis. A realistic estimate is two to three days for a working Phase 1 by someone comfortable with Shiny
modules, and the Pair explorer is the tab worth building first if only one gets built.
