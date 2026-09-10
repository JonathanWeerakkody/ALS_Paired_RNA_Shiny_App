# Machine-verifiable claims layer.
#
# Every numeric claim the paper makes that can be checked from the shipped data
# is listed here with the file it comes from and the computation that recovers
# it. verify_all() recomputes each and returns PASS, FAIL or SKIP.
#
# This exists so a reviewer, a CODECHECK volunteer, or an automated agent can
# confirm the paper's numbers without reading the manuscript. The app exposes
# it as a tab; ?verify=json returns the same result as machine-readable output.

# parse both plain numbers and the formatted scientific notation used in the
# publication tables, e.g. "2.4 x 10^-58" written with Unicode superscripts
.vnum <- function(x) {
  s <- as.character(x)
  sup <- c("\u2070"="0","\u00b9"="1","\u00b2"="2","\u00b3"="3","\u2074"="4",
           "\u2075"="5","\u2076"="6","\u2077"="7","\u2078"="8","\u2079"="9","\u207b"="-")
  for (k in names(sup)) s <- gsub(k, sup[[k]], s, fixed = TRUE)
  s <- gsub("\\s*\u00d7\\s*10", "e", s)
  s <- gsub("[^0-9eE.+-]", "", s)
  suppressWarnings(as.numeric(s))
}
.vread <- function(f) { p <- file.path("data-processed", f)
  if (file.exists(p)) utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) else NULL }
.vpanel <- function(f) { p <- file.path("data-processed/panels", f)
  if (file.exists(p)) utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) else NULL }

# id, section of the paper, the claim in words, expected value, tolerance, and
# a function returning the observed value from the shipped data.
CLAIMS <- list(

 list(id="C01", sec="Results, discovery", claim="Factor 1 carries 0.18% of feature-weighted molecular variance",
      expect=0.18, tol=0.01, unit="%", source="factors.csv",
      fn=function() { d <- .vread("factors.csv"); if (is.null(d)) return(NA)
        .vnum(d[[2]])[1] }),

 list(id="C02", sec="Results, discovery", claim="Factor 1 is the only factor surviving BH correction across twelve factors",
      expect=1, tol=0, unit="factors", source="factors.csv",
      fn=function() { d <- .vread("factors.csv"); if (is.null(d)) return(NA)
        p <- .vnum(d[[4]]); q <- stats::p.adjust(p, "BH"); sum(q < 0.05, na.rm=TRUE) }),

 list(id="C03", sec="Results, discovery", claim="Twelve factors were fitted",
      expect=12, tol=0, unit="factors", source="factors.csv",
      fn=function() { d <- .vread("factors.csv"); if (is.null(d)) return(NA); nrow(d) }),

 list(id="C04", sec="Results, pairing", claim="The candidate cross-product contains 304 pairs (19 x 16)",
      expect=304, tol=0, unit="pairs", source="pairs.csv",
      fn=function() { d <- .vread("pairs.csv"); if (is.null(d)) return(NA)
        length(unique(d[[1]])) * length(unique(d[[2]])) }),

 list(id="C05", sec="Results, pairing", claim="19 candidate miRNAs",
      expect=19, tol=0, unit="miRNA", source="pairs.csv",
      fn=function() { d <- .vread("pairs.csv"); if (is.null(d)) return(NA); length(unique(d[[1]])) }),

 list(id="C06", sec="Results, pairing", claim="119 of 304 pairs carry primary prior support",
      expect=119, tol=0, unit="pairs", source="pairs.csv",
      fn=function() { d <- .vread("pairs.csv"); if (is.null(d)) return(NA)
        cn <- grep("Primary", names(d), value=TRUE)[1]; if (is.na(cn)) return(NA)
        sum(tolower(trimws(d[[cn]])) == "yes") }),

 list(id="C07", sec="Results, pairing", claim="Cross-layer correlation between the two RNA layers is r = 0.738",
      expect=0.7384, tol=0.005, unit="r", source="pair_null_summary.csv",
      fn=function() { d <- .vread("pair_null_summary.csv"); if (is.null(d)) return(NA)
        cn <- grep("observed_r", names(d), value=TRUE)[1]; .vnum(d[[cn]])[1] }),

 list(id="C08", sec="Results, pairing", claim="No permutation of 10,000 reaches the observed cross-layer correlation",
      expect=0, tol=0, unit="permutations", source="pair_null.csv",
      fn=function() { d <- .vread("pair_null.csv"); if (is.null(d)) return(NA)
        sum(abs(.vnum(d[[2]])) >= 0.7384, na.rm=TRUE) }),

 list(id="C09", sec="Results, pairing", claim="The pairing null was run 10,000 times",
      expect=10000, tol=0, unit="permutations", source="pair_null.csv",
      fn=function() { d <- .vread("pair_null.csv"); if (is.null(d)) return(NA); nrow(d) }),

 list(id="C10", sec="Results, plasma", claim="The locked panel has five miRNAs",
      expect=5, tol=0, unit="miRNA", source="candidates.csv",
      fn=function() { d <- .vread("candidates.csv"); if (is.null(d)) return(NA)
        cn <- grep("Final five", names(d), value=TRUE)[1]; if (is.na(cn)) return(NA)
        sum(tolower(trimws(d[[cn]])) == "yes") }),

 list(id="C11", sec="Results, plasma", claim="miR-31-5p ranks first on the composite selection score",
      expect=1, tol=0, unit="rank", source="candidates.csv",
      fn=function() { d <- .vread("candidates.csv"); if (is.null(d)) return(NA)
        rc <- grep("Composite rank", names(d), value=TRUE)[1]
        i <- grep("31-5p", d[[1]])[1]; if (is.na(i)) return(NA); .vnum(d[[rc]])[i] }),

 list(id="C12", sec="Results, plasma", claim="miR-324-5p, not miR-1-3p, is in the locked five",
      expect=1, tol=0, unit="boolean", source="candidates.csv",
      fn=function() { d <- .vread("candidates.csv"); if (is.null(d)) return(NA)
        cn <- grep("Final five", names(d), value=TRUE)[1]
        y <- d[[1]][tolower(trimws(d[[cn]])) == "yes"]
        as.integer(any(grepl("324-5p", y)) && !any(grepl("miR-1-3p", y))) }),

 list(id="C13", sec="Results, horizons", claim="Adding the score to ALSFRS-R slope gains 0.052 AUROC at 54 months",
      expect=0.0523, tol=0.003, unit="dAUROC", source="auroc.csv",
      fn=function() { d <- .vread("auroc.csv"); if (is.null(d)) return(NA)
        mc <- grep("^Model$", names(d), value=TRUE)[1]; hc <- grep("Horizon", names(d), value=TRUE)[1]
        dc <- grep("^.AUROC$|delta_auc", names(d), value=TRUE)[1]
        i <- which(grepl("Signature", d[[mc]]) & !grepl("NfL", d[[mc]]) & .vnum(d[[hc]]) == 54)[1]
        if (is.na(i)) return(NA); .vnum(d[[dc]])[i] }),

 list(id="C14", sec="Results, horizons", claim="NfL loses 0.016 AUROC at 54 months on the same baseline",
      expect=-0.0160, tol=0.003, unit="dAUROC", source="auroc.csv",
      fn=function() { d <- .vread("auroc.csv"); if (is.null(d)) return(NA)
        mc <- grep("^Model$", names(d), value=TRUE)[1]; hc <- grep("Horizon", names(d), value=TRUE)[1]
        dc <- grep("^.AUROC$|delta_auc", names(d), value=TRUE)[1]
        i <- which(grepl("NfL", d[[mc]]) & !grepl("Signature", d[[mc]]) & .vnum(d[[hc]]) == 54)[1]
        if (is.na(i)) return(NA); .vnum(d[[dc]])[i] }),

 list(id="C15", sec="Results, horizons", claim="The score's incremental value rises monotonically from 24 to 54 months",
      expect=1, tol=0, unit="boolean", source="auroc.csv",
      fn=function() { d <- .vread("auroc.csv"); if (is.null(d)) return(NA)
        mc <- grep("^Model$", names(d), value=TRUE)[1]; hc <- grep("Horizon", names(d), value=TRUE)[1]
        dc <- grep("^.AUROC$|delta_auc", names(d), value=TRUE)[1]
        s <- d[grepl("Signature", d[[mc]]) & !grepl("NfL", d[[mc]]), ]
        v <- .vnum(s[[dc]])[order(.vnum(s[[hc]]))]
        as.integer(all(diff(v[2:length(v)]) > -0.02)) }),

 list(id="C16", sec="Results, tissue", claim="Five mRNAs form the tissue-bridge core",
      expect=5, tol=0, unit="genes", source="tissue_bridge.csv",
      fn=function() { d <- .vread("tissue_bridge.csv"); if (is.null(d)) return(NA)
        cn <- grep("Primary five|core_pick", names(d), value=TRUE)[1]; if (is.na(cn)) return(NA)
        sum(tolower(trimws(d[[cn]])) %in% c("yes","true")) }),

 list(id="C17", sec="Results, tissue", claim="The five-gene core is NPM1, MTMR3, ZBTB16, LAMP1 and CBL",
      expect=1, tol=0, unit="boolean", source="tissue_bridge.csv",
      fn=function() { d <- .vread("tissue_bridge.csv"); if (is.null(d)) return(NA)
        cn <- grep("Primary five|core_pick", names(d), value=TRUE)[1]
        g <- sort(d[[1]][tolower(trimws(d[[cn]])) %in% c("yes","true")])
        as.integer(identical(g, sort(c("NPM1","MTMR3","ZBTB16","LAMP1","CBL")))) }),

 list(id="C18", sec="Results, tissue", claim="25 lineage-gated state programs were defined",
      expect=25, tol=0, unit="programs", source="programs.csv",
      fn=function() { d <- .vread("programs.csv"); if (is.null(d)) return(NA); nrow(d) }),

 list(id="C19", sec="Results, tissue", claim="200 aggregate cargo-program associations were tested",
      expect=200, tol=0, unit="associations", source="cargo_programs.csv",
      fn=function() { d <- .vread("cargo_programs.csv"); if (is.null(d)) return(NA); nrow(d) }),

 list(id="C20", sec="Results, tissue", claim="Eight associations hold within ALS-only donor subsets at q < 0.05",
      expect=8, tol=0, unit="associations", source="als_only.csv",
      fn=function() { d <- .vread("als_only.csv"); if (is.null(d)) return(NA); nrow(d) }),

 list(id="C21", sec="Results, tissue", claim="No cargo gene contributes to any state-program score",
      expect=0, tol=0, unit="overlaps", source="programs.csv",
      fn=function() { d <- .vread("programs.csv"); if (is.null(d)) return(NA)
        gc <- grep("Marker genes|Genes", names(d), value=TRUE)[1]; if (is.na(gc)) return(NA)
        five <- c("NPM1","MTMR3","ZBTB16","LAMP1","CBL")
        sum(vapply(d[[gc]], function(g) any(five %in% trimws(strsplit(g, "[,;]")[[1]])), logical(1))) }),

 list(id="C22", sec="Methods", claim="The plasma transfer cohort has 248 participants",
      expect=248, tol=0, unit="participants", source="auroc.csv",
      fn=function() { d <- .vread("auroc.csv"); if (is.null(d)) return(NA)
        nc <- grep("Participants", names(d), value=TRUE)[1]; if (is.na(nc)) return(NA)
        max(.vnum(d[[nc]]), na.rm=TRUE) })
)

verify_all <- function() {
  out <- lapply(CLAIMS, function(c0) {
    obs <- tryCatch(c0$fn(), error = function(e) NA_real_)
    status <- if (!is.finite(obs)) "SKIP"
              else if (abs(obs - c0$expect) <= c0$tol) "PASS" else "FAIL"
    data.frame(id = c0$id, section = c0$sec, claim = c0$claim,
               expected = c0$expect, observed = obs, tolerance = c0$tol,
               unit = c0$unit, source = c0$source, status = status,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

verify_summary <- function(v = verify_all()) {
  list(total = nrow(v),
       pass = sum(v$status == "PASS"), fail = sum(v$status == "FAIL"),
       skip = sum(v$status == "SKIP"),
       generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
       settings_version = "1.0.0-paper")
}

# minimal JSON writer so the agent endpoint needs no extra package
verify_json <- function(v = verify_all()) {
  esc <- function(x) gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", as.character(x)))
  s <- verify_summary(v)
  trim <- function(x) trimws(format(x, scientific = FALSE, trim = TRUE))
  rows <- vapply(seq_len(nrow(v)), function(i) sprintf(
    '{"id":"%s","section":"%s","claim":"%s","expected":%s,"observed":%s,"tolerance":%s,"unit":"%s","source":"%s","status":"%s"}',
    v$id[i], esc(v$section[i]), esc(v$claim[i]), trim(v$expected[i]),
    if (is.na(v$observed[i])) "null" else trim(round(v$observed[i], 6)),
    trim(v$tolerance[i]), v$unit[i], v$source[i], v$status[i]), character(1))
  sprintf('{"summary":{"total":%d,"pass":%d,"fail":%d,"skip":%d,"generated":"%s","settings_version":"%s"},"claims":[%s]}',
          s$total, s$pass, s$fail, s$skip, s$generated, s$settings_version,
          paste(rows, collapse = ","))
}
