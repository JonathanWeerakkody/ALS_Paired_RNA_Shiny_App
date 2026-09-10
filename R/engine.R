# Analysis engine for the ALS Paired-RNA Explorer.
#
# Two things a reader can actually run on their own data:
#
#   score_panel()   apply the fixed five-miRNA feature set to a new cohort
#   run_pairing()   run the paper's paired regulator-effector method end to end
#
# Both return cohort-level output only. Neither produces a per-participant
# prediction, and nothing is written to disk.
#
# Base R plus stats. No modelling package required.

PANEL5 <- c("hsa-miR-31-5p","hsa-miR-4492","hsa-miR-93-3p","hsa-miR-421","hsa-miR-324-5p")

# ---------------------------------------------------------------- helpers
.norm_id <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^hsa[-_]", "", x, ignore.case = TRUE)
  tolower(gsub("[^A-Za-z0-9]", "", x))
}

.zscore <- function(m) {                       # features in rows
  mu <- rowMeans(m, na.rm = TRUE)
  sd <- apply(m, 1, stats::sd, na.rm = TRUE); sd[!is.finite(sd) | sd == 0] <- 1
  (m - mu) / sd
}

.read_matrix <- function(path) {
  d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(d) < 2) stop("Expected a feature column followed by one column per sample.")
  rn <- as.character(d[[1]]); d <- d[, -1, drop = FALSE]
  num <- vapply(d, function(x) is.numeric(x) || !any(is.na(suppressWarnings(as.numeric(x)))), logical(1))
  if (!any(num)) stop("No numeric sample columns found.")
  d <- d[, num, drop = FALSE]
  m <- as.matrix(data.frame(lapply(d, function(x) suppressWarnings(as.numeric(x))), check.names = FALSE))
  rownames(m) <- rn; colnames(m) <- names(d)
  m[!is.finite(m)] <- NA
  m
}

.read_meta <- function(path) {
  d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  names(d)[1] <- "sample"; d$sample <- as.character(d$sample)
  low <- tolower(names(d))
  ti <- which(low %in% c("time","survival","surv","months","survival_months","os"))[1]
  st <- which(low %in% c("status","event","died","death"))[1]
  if (is.na(ti) || is.na(st))
    stop("Metadata needs a sample column, a survival time column (time/survival/months) ",
         "and a status column (status/event), 1 = event.")
  data.frame(sample = d$sample,
             time   = suppressWarnings(as.numeric(d[[ti]])),
             status = suppressWarnings(as.numeric(d[[st]])),
             stringsAsFactors = FALSE)
}

# log-rank without the survival package
.logrank <- function(time, status, grp) {
  ok <- is.finite(time) & is.finite(status) & !is.na(grp)
  time <- time[ok]; status <- status[ok]; grp <- as.integer(factor(grp[ok])) - 1L
  if (length(unique(grp)) < 2) return(list(chisq = NA, p = NA))
  ts <- sort(unique(time[status == 1]))
  O1 <- E1 <- V <- 0
  for (t in ts) {
    n  <- sum(time >= t); n1 <- sum(time >= t & grp == 1)
    d  <- sum(time == t & status == 1); d1 <- sum(time == t & status == 1 & grp == 1)
    if (n <= 1) next
    O1 <- O1 + d1; E1 <- E1 + d * n1 / n
    V  <- V + d * (n1/n) * (1 - n1/n) * (n - d) / (n - 1)
  }
  if (V <= 0) return(list(chisq = NA, p = NA))
  x2 <- (O1 - E1)^2 / V
  list(chisq = x2, p = stats::pchisq(x2, 1, lower.tail = FALSE))
}

.km <- function(time, status) {
  ok <- is.finite(time) & is.finite(status); time <- time[ok]; status <- status[ok]
  ts <- sort(unique(time)); s <- 1; out <- data.frame(t = 0, s = 1)
  for (t in ts) {
    n <- sum(time >= t); d <- sum(time == t & status == 1)
    if (n > 0 && d > 0) { s <- s * (1 - d/n); out <- rbind(out, data.frame(t = t, s = s)) }
  }
  out
}

# concordance index, ties counted as half
.cindex <- function(time, status, risk) {
  ok <- is.finite(time) & is.finite(status) & is.finite(risk)
  time <- time[ok]; status <- status[ok]; risk <- risk[ok]
  n <- length(time); num <- den <- 0
  for (i in seq_len(n)) {
    if (status[i] != 1) next
    j <- which(time > time[i])
    if (!length(j)) next
    den <- den + length(j)
    num <- num + sum(risk[i] > risk[j]) + 0.5 * sum(risk[i] == risk[j])
  }
  if (den == 0) return(NA_real_)
  num / den
}

# ---------------------------------------------------------------- A. score a cohort
score_panel <- function(mirna_path, meta_path = NULL, panel = PANEL5) {
  m <- .read_matrix(mirna_path)
  key <- .norm_id(rownames(m)); want <- .norm_id(panel)
  hit <- match(want, key)
  found <- panel[!is.na(hit)]
  if (length(found) < 2)
    stop("Only ", length(found), " of the five panel miRNAs were found. ",
         "Row names should be miRNA identifiers such as hsa-miR-31-5p.")
  sub <- m[hit[!is.na(hit)], , drop = FALSE]
  z <- .zscore(sub)
  score <- -colMeans(z, na.rm = TRUE)         # higher score = worse, as in the paper
  res <- list(found = found, missing = setdiff(panel, found),
              n_samples = ncol(m), score = score, survival = NULL)
  if (!is.null(meta_path)) {
    md <- .read_meta(meta_path)
    i <- match(names(score), md$sample)
    if (all(is.na(i))) stop("No sample names matched between the matrix and the metadata.")
    d <- data.frame(sample = names(score), score = as.numeric(score),
                    time = md$time[i], status = md$status[i], stringsAsFactors = FALSE)
    d <- d[is.finite(d$time) & is.finite(d$status) & d$time > 0, ]
    if (nrow(d) >= 8) {
      d$group <- ifelse(d$score > stats::median(d$score), "High", "Low")
      lr <- .logrank(d$time, d$status, d$group)
      res$survival <- list(
        data = d, n = nrow(d), events = sum(d$status == 1),
        chisq = lr$chisq, p = lr$p,
        cindex = .cindex(d$time, d$status, d$score),
        km_high = .km(d$time[d$group == "High"], d$status[d$group == "High"]),
        km_low  = .km(d$time[d$group == "Low"],  d$status[d$group == "Low"]))
    }
  }
  res
}

# ---------------------------------------------------------------- B. run the method
# Paired regulator-effector discovery, following the paper's logic:
#   1 joint factor on the two z-scored layers, outcome admitted as a third view
#   2 candidates = top loadings in both layers
#   3 cross-product scored for inverse direction against the outcome
#   4 directional pair states, and their association with survival
run_pairing <- function(mirna_path, mrna_path, meta_path,
                        top_n = 15, seed = 20260309) {
  set.seed(seed)
  A <- .read_matrix(mirna_path); B <- .read_matrix(mrna_path)
  md <- .read_meta(meta_path)
  ids <- Reduce(intersect, list(colnames(A), colnames(B), md$sample))
  if (length(ids) < 12)
    stop("Only ", length(ids), " samples are shared across the two matrices and the metadata. ",
         "At least 12 are needed.")
  A <- A[, ids, drop = FALSE]; B <- B[, ids, drop = FALSE]
  md <- md[match(ids, md$sample), ]
  ok <- is.finite(md$time) & is.finite(md$status) & md$time > 0
  A <- A[, ok, drop = FALSE]; B <- B[, ok, drop = FALSE]; md <- md[ok, ]
  keep <- function(m) m[apply(m, 1, function(x) sum(is.finite(x)) > 0.5 * length(x) &&
                                stats::sd(x, na.rm = TRUE) > 0), , drop = FALSE]
  A <- keep(A); B <- keep(B)
  if (nrow(A) < 5 || nrow(B) < 5) stop("Too few variable features after filtering.")
  Az <- .zscore(A); Bz <- .zscore(B)
  Az[is.na(Az)] <- 0; Bz[is.na(Bz)] <- 0

  # outcome-guided joint factor: SVD of the stacked layers plus a scaled outcome row
  y <- as.numeric(scale(log1p(md$time)))
  X <- rbind(Az, Bz, outcome = y * sqrt(nrow(Az) + nrow(Bz)) / 8)
  sv <- svd(X - rowMeans(X), nu = 0, nv = min(8, ncol(X) - 1))
  scores <- sv$v
  rho <- apply(scores, 2, function(z) suppressWarnings(stats::cor(z, md$time, method = "spearman")))
  f <- which.max(abs(rho))                       # the outcome-aligned factor
  fs <- scores[, f]
  varpct <- sv$d[seq_len(ncol(scores))]^2 / sum(sv$d^2) * 100

  ld <- function(M) apply(M, 1, function(r) suppressWarnings(stats::cor(r, fs)))
  la <- ld(Az); lb <- ld(Bz)
  ca <- names(sort(abs(la), decreasing = TRUE))[seq_len(min(top_n, length(la)))]
  cb <- names(sort(abs(lb), decreasing = TRUE))[seq_len(min(top_n, length(lb)))]

  sa <- apply(Az[ca, , drop = FALSE], 1, function(r)
    suppressWarnings(stats::cor(r, md$time, method = "spearman")))
  sb <- apply(Bz[cb, , drop = FALSE], 1, function(r)
    suppressWarnings(stats::cor(r, md$time, method = "spearman")))

  pairs <- expand.grid(miRNA = ca, mRNA = cb, stringsAsFactors = FALSE)
  pairs$rho_miRNA <- sa[pairs$miRNA]; pairs$rho_mRNA <- sb[pairs$mRNA]
  pairs$pair_r <- mapply(function(a, b)
    suppressWarnings(stats::cor(Az[a, ], Bz[b, ])), pairs$miRNA, pairs$mRNA)
  pairs$inverse <- sign(pairs$rho_miRNA) != sign(pairs$rho_mRNA)
  pairs$state <- ifelse(pairs$rho_miRNA < 0 & pairs$rho_mRNA > 0, "Suppression",
                 ifelse(pairs$rho_miRNA > 0 & pairs$rho_mRNA < 0, "De-repression",
                 ifelse(pairs$rho_miRNA > 0 & pairs$rho_mRNA > 0, "Co-active", "Co-silenced")))

  # per-participant burden of each state, and its survival association
  burden <- function(st) {
    sel <- pairs[pairs$state == st, , drop = FALSE]
    if (!nrow(sel)) return(NULL)
    v <- vapply(seq_len(ncol(Az)), function(j)
      mean(sign(Az[sel$miRNA, j]) != sign(Bz[sel$mRNA, j])), numeric(1))
    g <- ifelse(v > stats::median(v), "High", "Low")
    lr <- .logrank(md$time, md$status, g)
    data.frame(state = st, n_pairs = nrow(sel), chisq = lr$chisq, p = lr$p)
  }
  states <- do.call(rbind, Filter(Negate(is.null),
    lapply(c("Suppression","De-repression","Co-active","Co-silenced"), burden)))

  # cross-layer coherence, and the pairing null
  wa <- colSums(Az[ca, , drop = FALSE] * abs(la[ca]))
  wb <- colSums(Bz[cb, , drop = FALSE] * abs(lb[cb]))
  obs <- suppressWarnings(stats::cor(wa, wb))
  null <- replicate(2000, suppressWarnings(stats::cor(wa, sample(wb))))
  list(n_samples = ncol(Az), n_mirna = nrow(Az), n_mrna = nrow(Bz),
       factor_used = f, factor_rho = rho[f], variance_pct = varpct[f],
       variance_all = varpct, candidates_mirna = ca, candidates_mrna = cb,
       pairs = pairs, states = states,
       coherence = list(observed = obs, null = null,
                        p = mean(abs(null) >= abs(obs)),
                        null_max = max(abs(null))),
       scores = data.frame(sample = colnames(Az), factor = fs,
                           time = md$time, status = md$status, stringsAsFactors = FALSE))
}
