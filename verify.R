#!/usr/bin/env Rscript
# Standalone claim check. Exits non-zero if any claim fails, so it drops into CI.
#   Rscript verify.R            human-readable table
#   Rscript verify.R --json     machine-readable manifest
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("R/verify.R")
v <- verify_all(); s <- verify_summary(v)
if ("--json" %in% commandArgs(TRUE)) { cat(verify_json(v), "\n") } else {
  cat(sprintf("\nALS Paired-RNA — claim verification, settings %s\n\n", s$settings_version))
  cat(sprintf("%-5s %-7s %-58s %10s %10s\n", "ID", "STATUS", "CLAIM", "EXPECTED", "OBSERVED"))
  cat(strrep("-", 94), "\n")
  for (i in seq_len(nrow(v)))
    cat(sprintf("%-5s %-7s %-58s %10s %10s\n", v$id[i], v$status[i],
        substr(v$claim[i], 1, 56), v$expected[i],
        ifelse(is.na(v$observed[i]), "NA", format(round(v$observed[i], 4)))))
  cat(sprintf("\n%d claims: %d pass, %d fail, %d skipped\n", s$total, s$pass, s$fail, s$skip))
}
quit(status = if (s$fail > 0) 1 else 0)
