# Deployment, and the agent-verification idea

## What changed, and why

The app was a picture gallery. It is now three things: a reader's tour of the figures, a place to run the
analysis on your own cohort, and a machine-checkable record of every number in the paper.

The third is the part I think is genuinely new, and it came out of looking at how this problem is being solved
elsewhere. **CODECHECK** has volunteers independently execute a paper's computations during peer review. The
`reborn` project argues for machine-readable findings created *before* publication rather than scraped after.
Neither is common in neuroscience yet. The app can do both at no extra cost, because the numbers are already in
the shipped tables.

---

## The verification layer

`R/verify.R` lists every numeric claim the paper makes that can be checked from the shipped data. Each entry
carries the claim in words, the expected value, a tolerance, the file it comes from, and a function that
recomputes it. Nothing is read from the manuscript, so a disagreement means the manuscript and the data have
diverged.

**Currently 22 claims, all passing.** Factor 1 variance, the 304-pair cross-product, the 119 supported pairs,
the r = 0.738 coherence and its 10,000-permutation null, the locked five, the horizon crossover, the five-gene
core, the 25 programs, the ALS-only associations, the non-circularity check, the cohort size.

Three ways to run it:

```bash
Rscript verify.R              # human-readable table
Rscript verify.R --json       # machine-readable manifest
```

```
<live-app-url>/?verify=json  # live URL NEED TO CHECK      # same manifest, over HTTP
```

Exit status is non-zero when a claim fails, so it drops straight into CI:

```yaml
- run: Rscript verify.R
```

### It already caught two real problems

Writing the checks found a mislabelled column I had introduced — `Primary five-gene core` actually held a
`Top 7`/`Context`/`Control` category, so the core was not machine-checkable. Fixed in both the app and
Supplementary Table S12. It also caught the publication tables' formatted scientific notation
(`2.4 × 10⁻⁵⁸` with Unicode superscripts) breaking naive numeric parsing, which would have silently broken any
downstream reader.

That is the argument for the layer. It is a test suite for a paper.

### What an agent gets

A single request returns every claim with `expected`, `observed`, `tolerance`, `source` and `status`. An agent
reviewing the paper can confirm the numbers without parsing prose or trusting a PDF, and can point at the exact
file behind any figure. Adding a claim is four lines, so the manifest grows as the paper does.

---

## Running your own data

Two modes, both cohort-level, neither returning a per-participant prediction.

**Score a cohort with the fixed five-miRNA feature set.** A miRNA matrix, optionally with survival. The five panel miRNAs are
z-scored, averaged, split at your median; you get log-rank separation and a C-index. The panel is fixed, so this
asks whether the published panel carries signal in your data.

**Run the exploratory paired workflow.** Paired miRNA and mRNA matrices with survival are analysed with a lightweight outcome-guided singular-value decomposition followed by candidate, direction-state and participant-pairing summaries. This is an exploratory analogue of the study logic, not a reimplementation of the paper's MOFA+ model.

A demo cohort ships for both, so the output is visible before anyone uploads anything.

Identifiers are matched loosely, so `miR-31-5p` and `hsa-miR-31-5p` both work. Uploaded files are read into
memory and discarded when the session ends; nothing is written to disk or transmitted.

---

## Deploying

### shinyapps.io, simplest

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
rsconnect::deployApp(appName = "als-paired-rna")
```

The free tier allows 5 apps and 25 active hours a month, which suits a paper companion. The bundle is
approximately 25 MB, inside the 1 GB limit. The three direct dependencies are `shiny`, `ggplot2` and `readxl`.

### Posit Connect or a self-hosted Shiny Server

Copy the folder to `/srv/shiny-server/als-paired-rna`. Install `shiny`, `ggplot2` and `readxl` on the server.

### Docker, for archival reproducibility

```dockerfile
FROM rocker/shiny:4.4.1
RUN R -e "install.packages(c('shiny','ggplot2','readxl'), repos='https://cloud.r-project.org')"
COPY . /srv/shiny-server/als-paired-rna
```

Pinning the base image is what makes the app still run in five years. Worth doing for the Zenodo archive even if
the live deployment is elsewhere.

### Before you deploy

- Rerun `analysis/08_build_panel_assets.R` after any figure edit, or the app shows uncorrected panels
- Rerun `Rscript verify.R` and confirm 22/22
- Add the repository URL and Zenodo DOI to the footer
- Put the live URL in the manuscript's Code Availability

---

## What I would add next

**A claim for every number in the abstract.** Twenty-two is a good start but the abstract alone has about eight
that are not yet covered, mostly because they need the raw matrices rather than the summary tables.

**A permalink per claim.** `?claim=C13` opening the app at the panel behind that number would let a reviewer
move from a sentence in the paper to the data in one click.

**Tolerance provenance.** Each tolerance is currently my judgement. Deriving them from the resampling already in
the paper would make the check itself auditable.

I would not add user accounts, saved sessions, or a database. The app's value is that it is small enough to
audit in an afternoon.
