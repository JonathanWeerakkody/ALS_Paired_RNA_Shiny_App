# Stage-level executable audit for the ALS paired-RNA evidence trail.
# The app recomputes numerical claims and file-integrity checks; it does not
# regenerate the author-edited figures.

evidence_items <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

stage_code_text <- function(stage_row) {
  ids <- evidence_items(stage_row$claim_ids)
  csvs <- evidence_items(stage_row$panel_csvs)
  id_expr <- if (length(ids)) {
    paste0("c(", paste(sprintf('"%s"', ids), collapse = ", "), ")")
  } else "character()"
  csv_expr <- if (length(csvs)) {
    paste0("c(", paste(sprintf('"%s"', csvs), collapse = ", "), ")")
  } else "character()"
  paste(
    "source(\"R/verify.R\")",
    sprintf("claim_ids <- %s", id_expr),
    "claim_results <- verify_all()",
    "claim_results <- claim_results[claim_results$id %in% claim_ids, ]",
    sprintf("panel_files <- file.path(\"data-processed/panels\", %s)", csv_expr),
    sprintf("source_workbook <- file.path(\"www/source-data\", \"%s\")", stage_row$source_workbook),
    "audit_code <- c(\"app.R\", \"R/evidence_engine.R\", \"R/verify.R\")",
    "stopifnot(all(file.exists(c(panel_files, source_workbook, audit_code))))",
    "tools::md5sum(c(panel_files, source_workbook, audit_code))",
    sep = "\n"
  )
}

run_evidence_stage <- function(stage_row) {
  started <- Sys.time()
  ids <- evidence_items(stage_row$claim_ids)
  csvs <- evidence_items(stage_row$panel_csvs)

  assets <- c(
    file.path("www/source-data", stage_row$source_workbook),
    file.path("data-processed/panels", csvs),
    "app.R", "R/evidence_engine.R", "R/verify.R"
  )
  asset_rows <- data.frame(
    id = paste0("FILE", seq_along(assets)),
    check = paste("Readable artifact:", basename(assets)),
    expected = "present",
    observed = ifelse(file.exists(assets), "present", "missing"),
    status = ifelse(file.exists(assets), "PASS", "FAIL"),
    stringsAsFactors = FALSE
  )

  claim_rows <- data.frame()
  if (length(ids)) {
    v <- verify_all()
    v <- v[v$id %in% ids, , drop = FALSE]
    claim_rows <- data.frame(
      id = v$id,
      check = v$claim,
      expected = as.character(v$expected),
      observed = as.character(v$observed),
      status = v$status,
      stringsAsFactors = FALSE
    )
  }

  results <- rbind(claim_rows, asset_rows)
  checksums <- as.list(unname(tools::md5sum(assets[file.exists(assets)])))
  names(checksums) <- basename(assets[file.exists(assets)])

  list(
    stage_id = stage_row$stage_id,
    stage = stage_row$stage,
    started_utc = format(started, tz = "UTC", usetz = TRUE),
    elapsed_seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 3),
    status = if (all(results$status == "PASS")) "PASS" else "FAIL",
    results = results,
    checksums = checksums,
    code = stage_code_text(stage_row)
  )
}
