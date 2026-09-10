# Responsive, source-driven companion views for every main and Extended Data panel.
# These plots preserve the panel's scientific chart family where the released
# source data permit it. They never embed or rasterize the publication artwork.

FIGURE_CATALOG <- read.csv("config/figure_catalog.csv", check.names = FALSE,
                           stringsAsFactors = FALSE)
FIGURE_SOURCE_ROOT <- normalizePath("source_data", mustWork = TRUE)
.panel_cache <- new.env(parent = emptyenv())

as_panel_number <- function(x) suppressWarnings(as.numeric(unlist(x)))

coerce_panel_frame <- function(d) {
  for (nm in names(d)) {
    x <- d[[nm]]
    if (is.list(x)) {
      x <- vapply(x, function(v) if (!length(v) || all(is.na(v))) NA_character_ else as.character(v[1]), "")
      d[[nm]] <- x
    }
    if (is.character(d[[nm]])) {
      raw <- trimws(d[[nm]])
      present <- !is.na(raw) & nzchar(raw)
      parsed <- suppressWarnings(as.numeric(raw))
      if (sum(is.finite(parsed)) >= max(3, ceiling(0.45 * sum(present)))) d[[nm]] <- parsed
    }
  }
  d
}

figure_choices <- function(group) {
  d <- FIGURE_CATALOG[FIGURE_CATALOG$group == group, , drop = FALSE]
  setNames(d$figure_id, d$label)
}

figure_row <- function(figure_id) {
  FIGURE_CATALOG[match(figure_id, FIGURE_CATALOG$figure_id), , drop = FALSE]
}

figure_panels <- function(figure_id) {
  r <- figure_row(figure_id)
  sheets <- readxl::excel_sheets(file.path(FIGURE_SOURCE_ROOT, r$workbook))
  sheets[!grepl("^Index$|stats", sheets, ignore.case = TRUE)]
}

read_figure_panel <- function(figure_id, panel) {
  cache_key <- paste(figure_id, panel, sep = "::")
  if (exists(cache_key, envir = .panel_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .panel_cache, inherits = FALSE))
  }
  r <- figure_row(figure_id)
  sheet <- panel
  if (figure_id == "M2" && panel == "M") sheet <- "M_stats"
  if (figure_id == "M3" && panel == "K") {
    out <- read.csv("data-processed/clinical_tiers.csv", check.names = FALSE,
                    stringsAsFactors = FALSE)
  } else {
    out <- suppressMessages(readxl::read_excel(
      file.path(FIGURE_SOURCE_ROOT, r$workbook), sheet = sheet,
      guess_max = 100000, .name_repair = "unique"))
    out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  }
  out <- coerce_panel_frame(out)
  assign(cache_key, out, envir = .panel_cache)
  out
}

figure_panel_title <- function(id, panel) {
  titles <- list(
    M1 = c(B = "Particle-size distributions by nanoparticle tracking",
           E = "Astrocyte-associated protein enrichment",
           F = "Platelet and lipoprotein depletion controls",
           G = "RNA-biotype composition across discovery profiles",
           H = "Detected RNA features in ALS and controls",
           I = "Sequence-architecture motif atlas"),
    M2 = c(A = "MOFA+ factor landscape", B = "Cross-layer Factor 1 concordance",
           C = "miRNA abundance by survival", D = "mRNA abundance by survival",
           E = "Evidence-set overlap", F = "High-loading miRNA features",
           G = "High-loading mRNA features", H = "Highest-confidence target-supported links",
           I = "Complete candidate-pair evidence", J = "miRNA score and survival",
           K = "mRNA score and survival", L = "Combined score and survival",
           M = "Apparent Cox estimates in the discovery cohort"),
    M3 = c(A = "Cross-compartment miRNA coverage", B = "Survival effects across compartments",
           C = "Bootstrap ranking of the 19 candidates", D = "Five-miRNA score in GLAST-positive vesicles",
           E = "Five-miRNA score in total plasma", F = "NfL in total plasma",
           G = "miR-181 benchmark", H = "Five-miRNA score combined with NfL",
           I = "Repeated-split test-participant hazard ratios", J = "Time-dependent discrimination",
           K = "Incremental AUROC over clinical baselines"),
    M4 = c(B = "Cross-resource evidence matrix", C = "Two-axis five-gene selection",
           D = "Four-component selection sensitivity", E = "Cortical cell map and cargo expression",
           F = "Cargo expression and SLC1A3 across cell classes",
           G = "Lineage-gated state-program associations",
           H = "Representative donor-level state relationships"),
    M5 = c(A = "Cargo-supported pathway territories", B = "Cortical pathway network",
           C = "Feature-by-module evidence matrix", D = "Integrated module landscape"),
    E1 = c(B = "Tetraspanin phenotype frequencies", C = "Particle-level size data availability",
           D = "Cross-platform particle-size estimates"),
    E2 = c(A = "RNA-composition heatmap", B = "RNA-composition association atlas",
           C = "IsomiR read depth by age", D = "Detected isomiR parent miRNAs by age",
           E = "Detected isomiR species by age", F = "Mature-miRNA read depth by age",
           G = "Detected mature-miRNA species by age", H = "IsomiRs per parent miRNA by age",
           I = "mRNA fraction by sex in ALS", J = "lncRNA fraction by sex in ALS",
           K = "Gene-exon depth and survival", L = "Transcript depth and survival"),
    E3 = c(A = "Motifs evaluated by sequence length", B = "Motif prevalence by sequence length",
           C = "Feature flow into the motif atlas", D = "miRNA EXOmotif atlas",
           E = "Candidate mRNA 3′-UTR and RBP motifs"),
    E4 = c(A = "Recurrent non-canonical junction landscape"),
    E5 = c(A = "Factor scores and ALSFRS-R slope", B = "Factor scores and ALSFRS-R",
           C = "miRNA disease effects", D = "mRNA disease effects",
           E = "miRNA Factor 1 loading rank", F = "mRNA Factor 1 loading rank",
           G = "Candidate association matrix"),
    E6 = c(A = "Prior-supported miRNA–mRNA network", B = "Supported mRNAs per miRNA",
           C = "Supported miRNAs per mRNA", D = "Directional states across survival tertiles",
           E = "Suppression burden and survival", F = "Complete candidate-pair matrix",
           G = "Inverse-direction pair states and survival", H = "Same-direction control states and survival"),
    E7 = c(A = "Plasma abundance of EV-nominated miRNAs", B = "Plasma ALS–control abundance",
           C = "Plasma survival abundance", D = "Evidence-set overlap",
           E = "EV score and survival", F = "Plasma score and survival",
           G = "Cross-compartment measurement depth", H = "Candidate and background variability",
           I = "Ranked abundance and diversity", J = "Survival by processing batch",
           K = "Candidate effects before and after ComBat", L = "ComBat score sensitivity",
           M = "Longitudinal plasma stability"),
    E8 = c(A = "AUROC across classifiers and horizons", B = "Signature-focused performance",
           C = "Signature-focused ROC summary", D = "Signature-focused precision–recall summary",
           E = "Signature-focused calibration summary", F = "NfL-focused performance",
           G = "NfL-focused ROC summary", H = "NfL-focused precision–recall summary",
           I = "NfL-focused calibration summary"),
    E9 = c(A = "Bulk-cortex differential-expression audit", B = "Single-nucleus donor-pseudobulk audit",
           C = "Bulk-cortex classification", D = "Single-nucleus classification",
           E = "Cross-context classification summary"),
    E10 = c(A = "Lineage localization of cortical state programs",
            B = "Significant cargo-gene and state-program associations")
  )
  z <- titles[[id]][panel]
  if (length(z) && !is.na(z)) unname(z) else paste("Panel", panel)
}

.replica_cols <- c("#2B6F73", "#B33B52", "#4676A9", "#DB8A35",
                   "#6A8E3A", "#8064A2", "#8A8175", "#D0B36D")

wrap_labels <- function(x, width = 24) {
  vapply(as.character(x), function(z) paste(strwrap(z, width), collapse = "\n"), "")
}

theme_replica <- function(width = 1000, rotate_x = FALSE) {
  mobile <- is.finite(width) && width < 620
  base <- if (mobile) 11.5 else if (width < 900) 13 else 14.5
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E7E9E6", linewidth = 0.35),
      panel.border = ggplot2::element_rect(colour = "#C7CBC7", fill = NA, linewidth = 0.45),
      plot.title = ggplot2::element_text(face = "bold", size = base + 2,
                                         margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(colour = "#545B5A", size = base - 0.5,
                                            margin = ggplot2::margin(b = 10)),
      axis.title = ggplot2::element_text(face = "bold", size = base),
      axis.text = ggplot2::element_text(size = base - 1, colour = "#2D3332"),
      axis.text.x = ggplot2::element_text(angle = if (rotate_x) 35 else 0,
                                          hjust = if (rotate_x) 1 else 0.5),
      legend.position = "bottom", legend.box = "vertical",
      legend.text = ggplot2::element_text(size = base - 1),
      strip.text = ggplot2::element_text(face = "bold", size = base - 0.5),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(18, 28, 18, 22))
}

finish_plot <- function(p, id, panel, width, subtitle = NULL, rotate_x = FALSE) {
  mobile <- is.finite(width) && width < 620
  title_width <- if (mobile) 23 else 68
  subtitle_width <- if (mobile) 34 else 92
  p + ggplot2::labs(
    title = paste(strwrap(figure_panel_title(id, panel), title_width), collapse = "\n"),
    subtitle = if (is.null(subtitle)) NULL else paste(strwrap(subtitle, subtitle_width), collapse = "\n")) +
    theme_replica(width, rotate_x)
}

numeric_columns <- function(d) names(d)[vapply(d, is.numeric, logical(1))]
character_columns <- function(d) names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]

volcano_plot <- function(d, id, panel, width) {
  fc <- intersect(c("logFC", "log2fc_or_equivalent"), names(d))[1]
  pv <- intersect(c("P.Value", "PValue", "pval", "p_value", "fdr"), names(d))[1]
  lab <- intersect(c("label", "feature_id", "gene_name", "gene_symbol", "feature"), names(d))[1]
  d$x <- as.numeric(d[[fc]])
  d$y <- -log10(pmax(as.numeric(d[[pv]]), .Machine$double.xmin))
  d$highlight <- abs(d$x) >= 1 & as.numeric(d[[pv]]) < 0.05
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = highlight)) +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = 2, colour = "#777777") +
    ggplot2::geom_vline(xintercept = c(-1, 1), linetype = 2, colour = "#777777") +
    ggplot2::geom_point(alpha = 0.65, size = if (width < 620) 1.7 else 2.2) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "#AEB5B2", `TRUE` = "#B33B52"), guide = "none") +
    ggplot2::labs(x = expression(log[2]~fold~change), y = expression(-log[10]~P))
  if (!is.na(lab)) {
    top <- head(d[order(-d$y), , drop = FALSE], if (width < 620) 5 else 10)
    p <- p + ggrepel::geom_text_repel(data = top, ggplot2::aes(label = .data[[lab]]),
                                      size = if (width < 620) 3 else 3.5,
                                      max.overlaps = Inf, show.legend = FALSE)
  }
  finish_plot(p, id, panel, width, "Dashed guides mark |log₂ fold change| = 1 and P = 0.05")
}

precomputed_km_plot <- function(d, id, panel, width) {
  d$time <- as.numeric(d$time); d$surv <- as.numeric(d$surv)
  group_col <- if ("group" %in% names(d)) "group" else "strata"
  a <- aggregate(d$surv, list(group = d[[group_col]], time = d$time),
                 function(x) c(mean = mean(x, na.rm = TRUE),
                               lo = unname(quantile(x, 0.10, na.rm = TRUE)),
                               hi = unname(quantile(x, 0.90, na.rm = TRUE))))
  summary_matrix <- if (is.matrix(a$x)) a$x else do.call(rbind, a$x)
  a <- data.frame(group = a$group, time = a$time, summary_matrix, check.names = FALSE)
  p <- ggplot2::ggplot(a, ggplot2::aes(time, mean, colour = group, fill = group)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.13, colour = NA) +
    ggplot2::geom_step(linewidth = 1.05) +
    ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(unique(a$group)))) +
    ggplot2::scale_fill_manual(values = rep(.replica_cols, length.out = length(unique(a$group)))) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(x = "Survival time (months)", y = "Survival probability", colour = NULL, fill = NULL)
  finish_plot(p, id, panel, width, "Mean curve with 10th–90th percentile variation across repeated splits")
}

raw_km_plot <- function(d, id, panel, width) {
  keep <- is.finite(as.numeric(d$survival_months)) & !is.na(d$group)
  dd <- d[keep, , drop = FALSE]
  dd$.curve_group <- if ("state" %in% names(dd)) paste(dd$state, dd$group, sep = " — ") else dd$group
  fit <- survival::survfit(survival::Surv(as.numeric(dd$survival_months), rep(1, nrow(dd))) ~ .curve_group, data = dd)
  s <- summary(fit)
  kd <- data.frame(time = s$time, surv = s$surv,
                   group = sub("^.curve_group=", "", s$strata))
  p <- ggplot2::ggplot(kd, ggplot2::aes(time, surv, colour = group)) +
    ggplot2::geom_step(linewidth = 1.1) +
    ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(unique(kd$group)))) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(x = "Survival time (months)", y = "Survival probability", colour = NULL)
  finish_plot(p, id, panel, width, "Kaplan–Meier companion view from the released participant-level panel data")
}

heatmap_plot <- function(d, row_col, col_col, value_col, id, panel, width,
                         subtitle = NULL, max_rows = 45, max_cols = 36) {
  z <- d[!is.na(d[[row_col]]) & !is.na(d[[col_col]]) & is.finite(as.numeric(d[[value_col]])), , drop = FALSE]
  row_score <- aggregate(abs(as.numeric(z[[value_col]])), list(row = z[[row_col]]), max)
  rows <- head(row_score$row[order(-row_score$x)], max_rows)
  cols <- head(unique(z[[col_col]]), max_cols)
  z <- z[z[[row_col]] %in% rows & z[[col_col]] %in% cols, , drop = FALSE]
  z$.value <- as.numeric(z[[value_col]])
  z$.row <- factor(z[[row_col]], levels = rev(rows))
  z$.col <- factor(z[[col_col]], levels = cols)
  p <- ggplot2::ggplot(z, ggplot2::aes(.col, .row, fill = .value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#4676A9", mid = "#F5F4EF", high = "#B33B52",
                                  midpoint = 0, name = "Effect") +
    ggplot2::scale_x_discrete(labels = function(x) wrap_labels(x, if (width < 620) 9 else 16)) +
    ggplot2::scale_y_discrete(labels = function(x) wrap_labels(x, if (width < 620) 14 else 22)) +
    ggplot2::labs(x = NULL, y = NULL)
  out <- finish_plot(p, id, panel, width, subtitle, rotate_x = TRUE)
  if (is.finite(width) && width < 620) {
    out <- out + ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8.2, lineheight = 0.88),
      axis.text.x = ggplot2::element_text(size = 8, angle = 65, hjust = 1))
  }
  out
}

network_plot <- function(d, left_col, right_col, id, panel, width, weight_col = NULL) {
  z <- d[!is.na(d[[left_col]]) & !is.na(d[[right_col]]), , drop = FALSE]
  if (!is.null(weight_col) && weight_col %in% names(z)) {
    z <- z[order(-as.numeric(z[[weight_col]])), , drop = FALSE]
  }
  z <- head(z, if (width < 620) 28 else 55)
  left <- unique(z[[left_col]]); right <- unique(z[[right_col]])
  nodes_l <- data.frame(name = left, x = 0, y = seq_along(left))
  nodes_r <- data.frame(name = right, x = 1, y = seq(1, max(length(left), length(right)), length.out = length(right)))
  z$y1 <- nodes_l$y[match(z[[left_col]], nodes_l$name)]
  z$y2 <- nodes_r$y[match(z[[right_col]], nodes_r$name)]
  p <- ggplot2::ggplot(z) +
    ggplot2::geom_curve(ggplot2::aes(x = 0.04, y = y1, xend = 0.96, yend = y2),
                        curvature = 0.16, alpha = 0.28, colour = "#487F83") +
    ggplot2::geom_point(data = nodes_l, ggplot2::aes(x, y), shape = 23, size = 3.4,
                        fill = "#DB8A35", colour = "#6F542C") +
    ggplot2::geom_point(data = nodes_r, ggplot2::aes(x, y), shape = 22, size = 3.4,
                        fill = "#4676A9", colour = "#365A7D") +
    ggplot2::geom_text(data = nodes_l, ggplot2::aes(x, y, label = wrap_labels(name, 18)),
                       hjust = 1.08, size = if (width < 620) 3 else 3.4) +
    ggplot2::geom_text(data = nodes_r, ggplot2::aes(x, y, label = wrap_labels(name, 18)),
                       hjust = -0.08, size = if (width < 620) 3 else 3.4) +
    ggplot2::coord_cartesian(xlim = c(-0.45, 1.45), clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void()
  finish_plot(p, id, panel, width, "Highest-supported released links are shown for legibility")
}

replica_plot <- function(id, panel, d, width = 1000) {
  width <- ifelse(is.null(width) || !is.finite(width), 1000, width)
  nms <- names(d)

  if (nrow(d) == 0) {
    return(finish_plot(ggplot2::ggplot() + ggplot2::annotate("text", 0, 0,
      label = "No panel-level numerical source data are available for this view.", size = 5) +
      ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1), id, panel, width))
  }
  if (all(c("source_data_status", "submission_note") %in% nms)) {
    return(finish_plot(ggplot2::ggplot() + ggplot2::annotate("text", 0, 0,
      label = paste(strwrap(d$submission_note[1], if (width < 620) 42 else 75), collapse = "\n"),
      size = if (width < 620) 4 else 5) + ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1),
      id, panel, width, "The source workbook explicitly records this limitation."))
  }

  if (id == "M1" && panel == "B") {
    p <- ggplot2::ggplot(d, ggplot2::aes(size_nm, concentration_particles_ml,
                                         group = sample_id)) +
      ggplot2::geom_line(colour = "#9DA6A3", alpha = 0.32, linewidth = 0.35) +
      ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line",
                            colour = "#B33B52", linewidth = 1.15) +
      ggplot2::geom_vline(xintercept = 138.8, linetype = 2, colour = "#B33B52") +
      ggplot2::labs(x = "Particle diameter (nm)", y = "Particle concentration (particles ml⁻¹)")
    return(finish_plot(p, id, panel, width, "Grey traces are technical preparations; red is the mean; dashed line is 138.8 nm"))
  }
  if (id == "M1" && panel %in% c("E", "F")) {
    eff <- if ("log2_effect" %in% nms) "log2_effect" else "log2fc_glast_vs_total"
    marker <- "marker"; z <- d[!is.na(d[[eff]]) & !is.na(d[[marker]]), , drop = FALSE]
    z <- z[!duplicated(paste(z[[marker]], z[[eff]])), , drop = FALSE]
    z$.marker <- reorder(z[[marker]], as.numeric(z[[eff]]))
    p <- ggplot2::ggplot(z, ggplot2::aes(.marker, as.numeric(.data[[eff]]))) +
      ggplot2::geom_hline(yintercept = 0, colour = "#777777") +
      ggplot2::geom_col(fill = if (panel == "E") "#2B6F73" else "#DB8A35", width = 0.62) +
      ggplot2::geom_point(size = 2.2, colour = "#303736") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "log₂ effect")
    return(finish_plot(p, id, panel, width, "Positive values indicate enrichment; negative values indicate depletion"))
  }
  if (id == "M1" && panel == "G") {
    ids <- c("sample", "group"); cols <- setdiff(nms, ids)
    z <- stack(d[cols]); z$sample <- rep(d$sample, times = length(cols)); z$group <- rep(d$group, times = length(cols))
    names(z)[1:2] <- c("percent", "biotype")
    z$sample <- factor(z$sample, levels = d$sample[order(d$group)])
    p <- ggplot2::ggplot(z, ggplot2::aes(sample, percent, fill = biotype)) +
      ggplot2::geom_col(width = 1) + ggplot2::facet_grid(~group, scales = "free_x", space = "free_x") +
      ggplot2::scale_fill_manual(values = rep(.replica_cols, length.out = length(cols))) +
      ggplot2::labs(x = "Discovery participants", y = "RNA biotype (%)", fill = "RNA biotype") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
    return(finish_plot(p, id, panel, width, "Sample-level stacked composition; facets separate ALS and control profiles"))
  }
  if (id == "M1" && panel == "H") {
    metric_cols <- setdiff(nms, c("sample", "group", "transcript_imputed"))
    z <- stack(d[metric_cols]); z$group <- rep(d$group, times = length(metric_cols)); names(z)[1:2] <- c("value", "metric")
    z$metric <- wrap_labels(z$metric, if (width < 620) 12 else 20)
    p <- ggplot2::ggplot(z, ggplot2::aes(group, value, fill = group)) +
      ggplot2::geom_boxplot(width = 0.62, outlier.shape = NA, alpha = 0.78) +
      ggplot2::geom_jitter(width = 0.12, size = 1.25, alpha = 0.48) +
      ggplot2::facet_wrap(~metric, scales = "free_y", ncol = if (width < 620) 1 else 2) +
      ggplot2::scale_fill_manual(values = c(ALS = "#B33B52", Control = "#4676A9")) +
      ggplot2::labs(x = NULL, y = "Detected features", fill = NULL)
    return(finish_plot(p, id, panel, width, "Boxes show the interquartile range; points are participants"))
  }
  if (id == "M1" && panel == "I") {
    z <- head(d[order(-as.numeric(d$shared_prevalence)), , drop = FALSE], if (width < 620) 22 else 40)
    z$motif <- reorder(z$motif, z$shared_prevalence)
    p <- ggplot2::ggplot(z, ggplot2::aes(shared_prevalence, motif, colour = source)) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = shared_prevalence, yend = motif), colour = "#D4D8D5") +
      ggplot2::geom_point(size = 3) + ggplot2::scale_colour_manual(values = .replica_cols) +
      ggplot2::labs(x = "Shared prevalence (%)", y = NULL, colour = "Motif source")
    return(finish_plot(p, id, panel, width, "Most prevalent retained motif entries are displayed; complete values remain downloadable"))
  }

  if (all(c("time", "surv") %in% nms)) return(precomputed_km_plot(d, id, panel, width))
  if (all(c("survival_months", "group") %in% nms) && panel %in% c("G", "H")) return(raw_km_plot(d, id, panel, width))
  if (("logFC" %in% nms || "log2fc_or_equivalent" %in% nms) &&
      any(c("P.Value", "PValue", "pval", "p_value", "fdr") %in% nms) &&
      nrow(d) > 80 && !(id %in% c("E9"))) return(volcano_plot(d, id, panel, width))

  if (id == "M2" && panel == "A") {
    p <- ggplot2::ggplot(d, ggplot2::aes(factor, variance_explained_pct, fill = view)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = .replica_cols) +
      ggplot2::labs(x = NULL, y = "Variance explained (%)", fill = "MOFA view")
    return(finish_plot(p, id, panel, width, "Factor 1 is outcome guided and explains little molecular variance" , TRUE))
  }
  if (id == "M2" && panel == "B") {
    rr <- cor(as.numeric(d$mRNA_factor1), as.numeric(d$miRNA_factor1), use = "complete.obs")
    p <- ggplot2::ggplot(d, ggplot2::aes(mRNA_factor1, miRNA_factor1, colour = survival_time_months)) +
      ggplot2::geom_point(size = 3, alpha = 0.82) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "#B33B52", fill = "#E7C8CE") +
      ggplot2::scale_colour_gradient(low = "#DB8A35", high = "#2B6F73") +
      ggplot2::labs(x = "mRNA Factor 1 score", y = "miRNA Factor 1 score", colour = "Survival\n(months)")
    return(finish_plot(p, id, panel, width, sprintf("Matched participant scores: Pearson r = %.3f", rr)))
  }
  if (id == "M2" && panel == "E") {
    z <- aggregate(feature ~ omic + region, d, length)
    p <- ggplot2::ggplot(z, ggplot2::aes(region, feature, fill = omic)) +
      ggplot2::geom_col(position = "dodge") + ggplot2::scale_fill_manual(values = .replica_cols) +
      ggplot2::labs(x = NULL, y = "Features", fill = NULL)
    return(finish_plot(p, id, panel, width, "Counts reproduce the released overlap regions", TRUE))
  }
  if (id == "M2" && panel %in% c("F", "G")) {
    z <- d[order(-abs(as.numeric(d$value))), , drop = FALSE]; z <- head(z, if (width < 620) 18 else 30)
    z$label <- ifelse(is.na(z$gene_name) | z$gene_name == "", z$feature, z$gene_name)
    z$label <- reorder(z$label, abs(z$value))
    p <- ggplot2::ggplot(z, ggplot2::aes(label, abs(value))) + ggplot2::geom_col(fill = "#2B6F73") +
      ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Absolute Factor 1 loading")
    return(finish_plot(p, id, panel, width, "Highest-loading display subset"))
  }
  if (id == "M2" && panel == "H") return(network_plot(d, "miRNA_feature", "mRNA_feature", id, panel, width, "confidence_score"))
  if ((id == "M2" && panel == "I") || (id == "E6" && panel == "F")) {
    return(heatmap_plot(d, if ("miRNA_feature" %in% nms) "miRNA_feature" else "miRNA",
                        if ("mRNA_feature" %in% nms) "mRNA_feature" else "mRNA",
                        "pair_rho", id, panel, width, "Fill is the pairwise expression association", 24, 20))
  }
  if (id == "M2" && panel == "M" && all(c("term", "estimate") %in% nms)) {
    z <- d[is.finite(as.numeric(d$estimate)), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(reorder(term, estimate), estimate)) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::geom_point(size = 3, colour = "#2B6F73") + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Hazard ratio")
    return(finish_plot(p, id, panel, width, "Separate standardized univariable models"))
  }

  if (id == "M3" && panel == "A") {
    z <- d[!is.na(d$label) & is.finite(as.numeric(d$n)), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(reorder(label, n), n, fill = zone)) +
      ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = .replica_cols) +
      ggplot2::labs(x = NULL, y = "miRNAs", fill = NULL)
    return(finish_plot(p, id, panel, width, "Coverage and differential-abundance enrichment across compartments"))
  }
  if (id == "M3" && panel == "B") {
    p <- ggplot2::ggplot(d, ggplot2::aes(ev_survival_rho, plasma_rho, colour = concordant,
                                         size = -log10(pmax(plasma_p, .Machine$double.xmin)))) +
      ggplot2::geom_hline(yintercept = 0, colour = "#999999") + ggplot2::geom_vline(xintercept = 0, colour = "#999999") +
      ggplot2::geom_point(alpha = 0.82) + ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 3.2) +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "#B33B52", `FALSE` = "#AEB5B2")) +
      ggplot2::labs(x = "EV survival correlation", y = "Plasma survival correlation",
                    colour = "Concordant", size = expression(-log[10]~P))
    return(finish_plot(p, id, panel, width, "Directionally concordant effects fall in the matching quadrants"))
  }
  if (id == "M3" && panel == "C") {
    z <- aggregate(rank_score ~ feature, d, max, na.rm = TRUE); z <- z[order(z$rank_score), ]
    z$selected <- rank(-z$rank_score, ties.method = "first") <= 5
    z$feature <- factor(z$feature, levels = z$feature)
    p <- ggplot2::ggplot(z, ggplot2::aes(feature, rank_score, fill = selected)) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c(`TRUE` = "#B33B52", `FALSE` = "#B7BDB8"), guide = "none") +
      ggplot2::labs(x = NULL, y = "Cross-compartment rank score")
    return(finish_plot(p, id, panel, width, "The five highest concordance-weighted candidates are highlighted"))
  }
  if (id == "M3" && panel == "I") {
    z <- aggregate(hr ~ model, d, function(x) c(median = median(x, na.rm = TRUE), lo = quantile(x, .1, na.rm = TRUE), hi = quantile(x, .9, na.rm = TRUE)))
    hr_matrix <- if (is.matrix(z$hr)) z$hr else do.call(rbind, z$hr)
    colnames(hr_matrix) <- c("median", "lo", "hi")
    z <- data.frame(model = z$model, hr_matrix, check.names = FALSE); z$model <- reorder(z$model, z$median)
    p <- ggplot2::ggplot(z, ggplot2::aes(model, median)) + ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = .15, colour = "#2B6F73") +
      ggplot2::geom_point(size = 3, colour = "#B33B52") + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Hazard ratio")
    return(finish_plot(p, id, panel, width, "Median and 10th–90th percentile across repeated test splits"))
  }
  if (id == "M3" && panel == "J") {
    p <- ggplot2::ggplot(d, ggplot2::aes(horizon_months, auroc, colour = model, group = model)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_low, ymax = ci_high, fill = model), alpha = .08, colour = NA) +
      ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2.6) +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(unique(d$model)))) +
      ggplot2::scale_fill_manual(values = rep(.replica_cols, length.out = length(unique(d$model)))) +
      ggplot2::labs(x = "Prediction horizon (months)", y = "Time-dependent AUROC", colour = NULL, fill = NULL)
    return(finish_plot(p, id, panel, width, "Lines show model discrimination; ribbons show released confidence intervals"))
  }
  if (id == "M3" && panel == "K") {
    models <- setdiff(nms, "horizon"); z <- stack(d[models]); z$horizon <- rep(d$horizon, times = length(models)); names(z) <- c("AUROC", "model", "horizon")
    p <- ggplot2::ggplot(z, ggplot2::aes(as.numeric(horizon), AUROC, colour = model)) +
      ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2.7) +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(models))) +
      ggplot2::labs(x = "Prediction horizon (months)", y = "Cross-validated AUROC", colour = NULL)
    return(finish_plot(p, id, panel, width, "Clinical, NfL and RNA model tiers across the four prespecified horizons"))
  }

  if (id == "M4" && panel == "B") return(heatmap_plot(d, "gene_label", "metric", "value", id, panel, width, "Normalized evidence across bulk and single-nucleus resources", 20, 14))
  if (id == "M4" && panel == "C") {
    z <- d[d$component == "bridge_scatter", , drop = FALSE]
    z$core <- z$gene_symbol %in% c("NPM1", "MTMR3", "ZBTB16", "LAMP1", "CBL")
    p <- ggplot2::ggplot(z, ggplot2::aes(x_value, value, size = point_size, colour = core)) +
      ggplot2::geom_vline(xintercept = median(z$x_value, na.rm = TRUE), linetype = 2) +
      ggplot2::geom_hline(yintercept = median(z$value, na.rm = TRUE), linetype = 2) +
      ggplot2::geom_point(alpha = .85) + ggrepel::geom_text_repel(ggplot2::aes(label = gene_symbol), size = 3.2) +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "#B33B52", `FALSE` = "#AEB5B2"), guide = "none") +
      ggplot2::labs(x = "Tissue-bridge concordance", y = "EV survival support", size = NULL)
    return(finish_plot(p, id, panel, width, "Dashed lines are candidate medians; the upper-right conjunction defines the five-gene bridge"))
  }
  if (id == "M4" && panel == "D") {
    z <- d[order(d$core4_rank), , drop = FALSE]; z <- head(z, if (width < 620) 12 else 16); z$gene_symbol <- reorder(z$gene_symbol, z$composite)
    p <- ggplot2::ggplot(z, ggplot2::aes(gene_symbol, composite, fill = is_top6_core4)) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c(`TRUE` = "#B33B52", `FALSE` = "#B7BDB8"), guide = "none") +
      ggplot2::labs(x = NULL, y = "Four-component sensitivity score")
    return(finish_plot(p, id, panel, width, "Sensitivity ranking integrates statistical, biological and genetics-context components"))
  }
  if (id == "M4" && panel == "E") {
    z <- d
    if (nrow(z) > 18000) z <- z[seq(1, nrow(z), length.out = 18000), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(UMAP1, UMAP2, colour = cell_group)) +
      ggplot2::geom_point(size = .35, alpha = .45) +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(unique(z$cell_group)))) +
      ggplot2::labs(x = "UMAP 1", y = "UMAP 2", colour = "Cell class")
    return(finish_plot(p, id, panel, width, "Downsampled display of analyzed nuclei; downloads retain the released panel table"))
  }
  if (id == "M4" && panel == "F") {
    p <- ggplot2::ggplot(d, ggplot2::aes(glast_norm, cargo_norm, colour = cell_type, size = n_cells)) +
      ggplot2::geom_point(alpha = .78) + ggplot2::facet_wrap(~gene, ncol = if (width < 620) 1 else 3) +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(unique(d$cell_type)))) +
      ggplot2::labs(x = "Normalized SLC1A3 expression", y = "Normalized cargo expression", colour = "Cell class", size = "Cells")
    return(finish_plot(p, id, panel, width, "Class-level means among expressing cells"))
  }
  if ((id == "M4" && panel == "G") || (id == "E10" && panel == "B")) {
    row <- if ("state_label" %in% nms) "state_label" else "program"
    col <- if ("group_subset" %in% nms) "group_subset" else "cargo_label"
    return(heatmap_plot(d, row, col, "rho", id, panel, width, "Signed donor-pseudobulk Spearman associations", 30, 12))
  }
  if (id == "M4" && panel == "H") {
    top <- unique(d$program[order(as.numeric(d$fdr_q))]); top <- head(top, if (width < 620) 3 else 6)
    z <- d[d$program %in% top, , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(cargo_agg, state_score, colour = group)) +
      ggplot2::geom_point(alpha = .62, size = 1.8) + ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::facet_wrap(~state_label, scales = "free", ncol = if (width < 620) 1 else 3) +
      ggplot2::scale_colour_manual(values = .replica_cols) +
      ggplot2::labs(x = "Aggregate five-gene cargo score", y = "State-program score", colour = NULL)
    return(finish_plot(p, id, panel, width, "Representative donor-level relationships with the strongest released FDR values"))
  }

  if (id == "M5" && panel %in% c("A", "B")) {
    z <- d[is.finite(as.numeric(d$fdr)), , drop = FALSE]
    z$evidence <- -log10(pmax(as.numeric(z$fdr), .Machine$double.xmin))
    z <- head(z[order(-z$evidence), ], if (width < 620) 24 else 45)
    z$pathway_short <- reorder(z$pathway_short, z$evidence)
    p <- ggplot2::ggplot(z, ggplot2::aes(pathway_short, evidence, colour = lineage, size = k)) +
      ggplot2::geom_point(alpha = .78) + ggplot2::coord_flip() +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(unique(z$lineage)))) +
      ggplot2::labs(x = NULL, y = expression(-log[10]~FDR), colour = "Lineage", size = "Genes")
    return(finish_plot(p, id, panel, width, "Highest-confidence displayed pathway terms; the full network table is downloadable"))
  }
  if (id == "M5" && panel %in% c("C", "D")) {
    z <- d[is.na(d$from) | d$from == "", , drop = FALSE]
    z <- z[!duplicated(z$module), , drop = FALSE]
    z$module <- reorder(z$module, z$neg_log10_fdr)
    p <- ggplot2::ggplot(z, ggplot2::aes(module, neg_log10_fdr, fill = direction)) +
      ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = .replica_cols) +
      ggplot2::labs(x = NULL, y = expression(Best~-log[10]~FDR), fill = "ALS direction")
    return(finish_plot(p, id, panel, width, "Shared cargo and cortical modules ranked by strongest adjusted evidence"))
  }

  if (id == "E1" && panel == "B") {
    d$marker_combination <- reorder(d$marker_combination, d$percent_displayed)
    p <- ggplot2::ggplot(d, ggplot2::aes(marker_combination, percent_displayed)) +
      ggplot2::geom_col(fill = "#2B6F73") + ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(label = paste0(percent_displayed, "%")), hjust = -0.12) +
      ggplot2::expand_limits(y = max(d$percent_displayed) * 1.17) +
      ggplot2::labs(x = NULL, y = "Localized particles (%)")
    return(finish_plot(p, id, panel, width, "Three-colour dSTORM phenotypes among 339 localized particles"))
  }
  if (id == "E1" && panel == "D") {
    p <- ggplot2::ggplot(d, ggplot2::aes(estimated_size_nm, phenotype, colour = platform, shape = positivity_class)) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = estimated_size_nm - estimated_sd_nm,
                                          xmax = estimated_size_nm + estimated_sd_nm),
                             orientation = "y", width = .2) +
      ggplot2::geom_point(size = 3.2) + ggplot2::scale_colour_manual(values = .replica_cols) +
      ggplot2::labs(x = "Estimated particle size (nm)", y = NULL, colour = "Platform", shape = "Phenotype")
    return(finish_plot(p, id, panel, width, "Platform-specific estimates are interpreted within their measurement principles"))
  }

  if (id == "E2" && panel == "A") {
    num <- numeric_columns(d); num <- num[!grepl("_num$|^sex_male$", num)]; num <- head(num, 32)
    z <- stack(d[num]); z$sample_id <- rep(d$sample_id, times = length(num)); z$disease_status <- rep(d$disease_status, times = length(num)); names(z)[1:2] <- c("value", "metric")
    z$value <- ave(as.numeric(z$value), z$metric, FUN = function(x) as.numeric(scale(x)))
    return(heatmap_plot(z, "metric", "sample_id", "value", id, panel, width, "Each RNA-composition measure is standardized across participants", 32, 60))
  }
  if (id == "E2" && panel == "B") return(heatmap_plot(d, "metric_label", "association", "signed_neglog10_p", id, panel, width, "Signed association strength; full adjusted results are downloadable", 32, 16))
  if (id == "E2" && panel %in% LETTERS[3:8]) {
    measure <- setdiff(nms, c("sample_id", "disease_status", "sex", "age_collection_midpoint", "survival_time_months_num", "association", "scope", "test_type", "metric", "metric_label", "effect", "effect_label", "p_value", "n_used", "signed_neglog10_p", "fdr"))[1]
    keep <- is.finite(as.numeric(d$age_collection_midpoint)) & is.finite(as.numeric(d[[measure]]))
    z <- d[keep, , drop = FALSE]
    z$.value <- as.numeric(z[[measure]])
    z$age_group <- cut(as.numeric(z$age_collection_midpoint), breaks = c(22,38,54,69,85), include.lowest = TRUE)
    p <- ggplot2::ggplot(z, ggplot2::aes(age_group, .value, fill = age_group)) +
      ggplot2::geom_violin(alpha = .45, trim = FALSE) + ggplot2::geom_boxplot(width = .22, outlier.shape = NA) +
      ggplot2::geom_jitter(width = .08, size = 1, alpha = .45) +
      ggplot2::scale_fill_manual(values = rep(.replica_cols, length.out = 4), guide = "none") +
      ggplot2::labs(x = "Age at collection (years)", y = wrap_labels(measure, 24))
    return(finish_plot(p, id, panel, width, "Participant distributions across the four prespecified age groups"))
  }
  if (id == "E2" && panel %in% c("I", "J")) {
    measure <- if (panel == "I") "gene_pct_mRNA" else "gene_pct_lncRNA"
    z <- d[d$disease_status == "ALS" & is.finite(as.numeric(d[[measure]])), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(sex, as.numeric(.data[[measure]]), fill = sex)) +
      ggplot2::geom_boxplot(width = .55, outlier.shape = NA) + ggplot2::geom_jitter(width = .1, alpha = .55) +
      ggplot2::scale_fill_manual(values = .replica_cols, guide = "none") + ggplot2::labs(x = NULL, y = "Read fraction (%)")
    return(finish_plot(p, id, panel, width, "ALS discovery participants only"))
  }
  if (id == "E2" && panel %in% c("K", "L")) {
    measure <- if (panel == "K") "gene_total_exon_reads_log10" else "transcript_total_reads_log10"
    z <- d[d$disease_status == "ALS" & is.finite(as.numeric(d[[measure]])) &
             is.finite(as.numeric(d$survival_time_months_num)), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(as.numeric(.data[[measure]]), survival_time_months_num)) +
      ggplot2::geom_point(size = 2.5, colour = "#2B6F73", alpha = .75) +
      ggplot2::geom_smooth(method = "lm", colour = "#B33B52", fill = "#E7C8CE") +
      ggplot2::labs(x = "Log₁₀ read depth", y = "Observed survival (months)")
    return(finish_plot(p, id, panel, width, "Line is an ordinary-least-squares visual summary; inference used Spearman correlation"))
  }

  if (id == "E3" && panel == "A") {
    p <- ggplot2::ggplot(d, ggplot2::aes(k, n_distinct_motifs, colour = source)) + ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_y_log10() + ggplot2::scale_colour_manual(values = .replica_cols) +
      ggplot2::labs(x = "Motif length (nt)", y = "Distinct motifs (log scale)", colour = NULL)
    return(finish_plot(p, id, panel, width, "Prespecified miRNA and mRNA motif-analysis universes"))
  }
  if (id == "E3" && panel == "B") {
    p <- ggplot2::ggplot(d, ggplot2::aes(k, mean, colour = source)) + ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_colour_manual(values = .replica_cols) + ggplot2::labs(x = "Motif length (nt)", y = "Mean maximum prevalence (%)", colour = NULL)
    return(finish_plot(p, id, panel, width, "Shaded windows in the manuscript define the prespecified motif lengths"))
  }
  if (id == "E3" && panel == "C") {
    cols <- c("mofa_input_features", "in_motif_universe", "prevalent_union", "motifs_displayed"); cols <- intersect(cols, nms)
    d[cols] <- lapply(d[cols], as_panel_number)
    z <- stack(d[cols]); z$omic <- rep(d$omic, times = length(cols)); names(z)[1:2] <- c("features", "stage")
    p <- ggplot2::ggplot(z, ggplot2::aes(stage, features, fill = omic)) + ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_y_log10() + ggplot2::scale_fill_manual(values = .replica_cols) + ggplot2::labs(x = NULL, y = "Features (log scale)", fill = NULL)
    return(finish_plot(p, id, panel, width, "Feature flow from abundance universe to displayed motif atlas", TRUE))
  }
  if (id == "E3" && panel %in% c("D", "E")) {
    row <- if (panel == "D") "motif" else "rbp"; value <- "lfc"
    class_col <- if (panel == "D") "classes" else "display_class"
    z <- d[order(-abs(as.numeric(d[[value]]))), ]; z <- head(z, if (width < 620) 24 else 45)
    z$.row <- reorder(z[[row]], as.numeric(z[[value]]))
    p <- ggplot2::ggplot(z, ggplot2::aes(.row, as.numeric(.data[[value]]), colour = .data[[class_col]])) +
      ggplot2::geom_segment(ggplot2::aes(y = 0, yend = as.numeric(.data[[value]]), xend = .row), colour = "#D0D5D2") +
      ggplot2::geom_point(size = 2.5) + ggplot2::coord_flip() + ggplot2::scale_colour_manual(values = .replica_cols) +
      ggplot2::labs(x = NULL, y = "log₂ ALS/control prevalence", colour = "Motif class")
    return(finish_plot(p, id, panel, width, "Displayed motif effects; all computable BH FDR values equal 1"))
  }

  if (id == "E4" && panel == "A") {
    z <- d[d$molecules > 0, , drop = FALSE]
    return(heatmap_plot(z, "junction_id", "participant_id", "molecules", id, panel, width,
                        "Only detected junction–participant cells are coloured", 55, 60))
  }
  if (id == "E5" && panel %in% c("A", "B")) {
    colour <- if (panel == "A") "alsfrs_slope_per_month" else "alsfrs_closest"
    z <- d[is.finite(as.numeric(d$mRNA_factor1)) & is.finite(as.numeric(d$miRNA_factor1)), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(mRNA_factor1, miRNA_factor1, colour = .data[[colour]])) +
      ggplot2::geom_point(size = 2.8) + ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "#555555") +
      ggplot2::scale_colour_gradient(low = "#4676A9", high = "#B33B52") +
      ggplot2::labs(x = "mRNA Factor 1 score", y = "miRNA Factor 1 score", colour = wrap_labels(colour, 14))
    return(finish_plot(p, id, panel, width, "Clinical measure was not used to fit the survival view"))
  }
  if (id == "E5" && panel %in% c("E", "F")) {
    z <- d[order(d$rank_abs_loading), ]; z$candidate <- z$rank_abs_loading <= 50
    p <- ggplot2::ggplot(z, ggplot2::aes(rank_abs_loading, abs_loading, colour = candidate)) +
      ggplot2::geom_vline(xintercept = 50, linetype = 2) + ggplot2::geom_point(alpha = .75) +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "#B33B52", `FALSE` = "#AEB5B2"), guide = "none") +
      ggplot2::labs(x = "Absolute loading rank", y = "Absolute Factor 1 loading")
    return(finish_plot(p, id, panel, width, "Dashed line marks the top-50 loading prefilter"))
  }
  if (id == "E5" && panel == "G") return(heatmap_plot(d, "feature", "metadata", "signed_neglog10_p", id, panel, width, "Signed nominal association evidence", 38, 12))

  if (id == "E6" && panel == "A") return(network_plot(d, "miRNA", "mRNA", id, panel, width, "db_confidence"))
  if (id == "E6" && panel %in% c("B", "C")) {
    entity <- if (panel == "B") "miRNA" else "mRNA"; count <- if (panel == "B") "n_supported_mRNAs" else "n_supported_miRNAs"
    z <- d[order(as.numeric(d[[count]])), ]; z[[entity]] <- factor(z[[entity]], levels = z[[entity]])
    p <- ggplot2::ggplot(z, ggplot2::aes(.data[[entity]], .data[[count]])) + ggplot2::geom_col(fill = "#2B6F73") + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Supported partners")
    return(finish_plot(p, id, panel, width, "Complete partner counts for the fixed candidate set"))
  }
  if (id == "E6" && panel == "D") {
    cols <- c("pct_supp", "pct_unexp", "pct_coact", "pct_codn"); z <- stack(d[cols]); z$survival_tertile <- rep(d$survival_tertile, times = length(cols)); names(z)[1:2] <- c("percent", "state")
    z <- aggregate(percent ~ state + survival_tertile, z, mean, na.rm = TRUE)
    p <- ggplot2::ggplot(z, ggplot2::aes(survival_tertile, percent, fill = state)) + ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = .replica_cols) + ggplot2::labs(x = "Survival tertile", y = "Mean supported-pair state (%)", fill = "Pair state")
    return(finish_plot(p, id, panel, width, "Suppression is most frequent in the shortest-survival tertile"))
  }
  if (id == "E6" && panel == "E") {
    z <- d[is.finite(as.numeric(d$pct_supp)) & is.finite(as.numeric(d$survival_months)), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(pct_supp, survival_months)) + ggplot2::geom_point(colour = "#2B6F73", size = 2.6) +
      ggplot2::geom_smooth(method = "lm", colour = "#B33B52", fill = "#E7C8CE") + ggplot2::labs(x = "Suppression-state burden (%)", y = "Observed survival (months)")
    return(finish_plot(p, id, panel, width, "Participant-level suppression burden is inversely related to survival"))
  }

  if (id == "E7" && panel == "A") {
    z <- d[, c("feature", "sample_id", "value")]; z$value <- ave(as.numeric(z$value), z$feature, FUN = function(x) as.numeric(scale(x)))
    return(heatmap_plot(z, "feature", "sample_id", "value", id, panel, width, "Row-scaled abundance of the 19 EV-nominated miRNAs", 19, 70))
  }
  if (id == "E7" && panel == "D") {
    z <- d[!is.na(d$label) & is.finite(as.numeric(d$n)), ];
    p <- ggplot2::ggplot(z, ggplot2::aes(reorder(label, n), n, fill = segment)) + ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = .replica_cols) + ggplot2::labs(x = NULL, y = "Features", fill = NULL)
    return(finish_plot(p, id, panel, width, "Released overlap segments across loading, disease and survival sets"))
  }
  if (id == "E7" && panel %in% c("E", "F")) {
    z <- d[is.finite(as.numeric(d$miRNA_score)) & is.finite(as.numeric(d$survival_months)), , drop = FALSE]
    p <- ggplot2::ggplot(z, ggplot2::aes(miRNA_score, survival_months)) + ggplot2::geom_point(colour = "#2B6F73", size = 2.3, alpha = .68) +
      ggplot2::geom_smooth(method = "lm", colour = "#B33B52", fill = "#E7C8CE") + ggplot2::labs(x = "Five-miRNA score", y = "Observed survival (months)")
    return(finish_plot(p, id, panel, width, if (panel == "E") "GLAST-positive EV discovery cohort" else "Total-plasma cohort"))
  }
  if (id == "E7" && panel %in% c("G", "H")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(ev_sd, plasma_sd, colour = group)) + ggplot2::geom_abline(linetype = 2) +
      ggplot2::geom_point(alpha = .6, size = 1.8) + ggplot2::scale_colour_manual(values = .replica_cols) +
      ggplot2::labs(x = "EV variability (s.d.)", y = "Plasma variability (s.d.)", colour = NULL)
    return(finish_plot(p, id, panel, width, "Candidate and background measurement variability across compartments"))
  }
  if (id == "E7" && panel %in% c("I", "M")) {
    z <- d[!is.na(d$family) & is.finite(as.numeric(d$mean_t4_t1_change)), ]; z <- z[!duplicated(z$feature), ]
    p <- ggplot2::ggplot(z, ggplot2::aes(family, mean_t4_t1_change, colour = family)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::geom_jitter(width = .14, alpha = .45, size = 1.4) +
      ggplot2::stat_summary(fun = median, geom = "point", shape = 23, size = 3.5, fill = "white") +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(unique(z$family)))) +
      ggplot2::labs(x = NULL, y = "Standardized t4-to-t1 change", colour = NULL)
    return(finish_plot(p, id, panel, width, "Longitudinal stability among participants with serial plasma measurements", TRUE))
  }
  if (id == "E7" && panel == "K") {
    z <- d; z$label <- reorder(z$label, z$rho_tmm)
    p <- ggplot2::ggplot(z, ggplot2::aes(label, rho_tmm, xend = label, yend = rho_combat, colour = sign_flip)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::geom_segment(linewidth = .8) +
      ggplot2::geom_point(ggplot2::aes(y = rho_tmm), shape = 16, size = 2.4) +
      ggplot2::geom_point(ggplot2::aes(y = rho_combat), shape = 17, size = 2.4) + ggplot2::coord_flip() +
      ggplot2::scale_colour_manual(values = c(True = "#B33B52", False = "#2B6F73")) +
      ggplot2::labs(x = NULL, y = "Survival correlation", colour = "Direction changed")
    return(finish_plot(p, id, panel, width, "Circles are normalized expression; triangles are ComBat-corrected effects"))
  }
  if (id == "E7" && panel == "L") {
    z <- d[d$section == "holdout_scores", ]; cols <- c("score_tmm", "score_combat"); q <- stack(z[cols]); q$sample_id <- rep(z$sample_id, times = 2); names(q)[1:2] <- c("score", "method")
    p <- ggplot2::ggplot(q, ggplot2::aes(method, score, group = sample_id)) + ggplot2::geom_line(alpha = .22, colour = "#78817E") +
      ggplot2::geom_point(ggplot2::aes(colour = method), alpha = .62) + ggplot2::scale_colour_manual(values = .replica_cols, guide = "none") +
      ggplot2::labs(x = NULL, y = "Five-miRNA score")
    return(finish_plot(p, id, panel, width, "Participant scores before and after ComBat sensitivity correction"))
  }

  if (id == "E8") {
    metric <- if (panel %in% c("D", "H")) "ppv_mean" else if (panel %in% c("E", "I")) "bal_mean" else "auc_mean"
    z <- aggregate(d[[metric]], list(model = d$fs, horizon = d$horizon), mean, na.rm = TRUE); names(z)[3] <- "value"
    top_models <- head(names(sort(tapply(z$value, z$model, mean, na.rm = TRUE), decreasing = TRUE)), if (width < 620) 5 else 8)
    z <- z[z$model %in% top_models, ]
    p <- ggplot2::ggplot(z, ggplot2::aes(horizon, value, colour = model)) + ggplot2::geom_line(linewidth = .95) + ggplot2::geom_point(size = 2.3) +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(top_models))) +
      ggplot2::labs(x = "Prediction horizon (months)", y = switch(metric, auc_mean = "Mean AUROC", ppv_mean = "Mean precision", bal_mean = "Mean balanced accuracy"), colour = NULL)
    note <- if (panel %in% c("E", "I")) "The released sheet contains summary performance rather than participant-level calibration coordinates" else "Mean performance across the released classifier results"
    return(finish_plot(p, id, panel, width, note))
  }

  if (id == "E9" && panel %in% c("A", "B")) {
    row <- "gene_symbol"; col <- if (panel == "A") "contrast_label" else "column_id"; val <- if (panel == "A") "log2fc_or_equivalent" else "logFC_display"
    return(heatmap_plot(d, row, col, val, id, panel, width, "Fill is differential-expression effect; untested cells are omitted", 20, 35))
  }
  if (id == "E9" && panel %in% c("C", "D")) {
    auc_cols <- grep("cv_mean|loocv", nms, value = TRUE); z <- stack(d[auc_cols]); z$context <- rep(paste(d$tissue, d$subgroup, sep = ": "), times = length(auc_cols)); names(z)[1:2] <- c("AUROC", "classifier")
    z <- z[is.finite(as.numeric(z$AUROC)), ]; z$context <- reorder(z$context, as.numeric(z$AUROC))
    p <- ggplot2::ggplot(z, ggplot2::aes(context, as.numeric(AUROC), colour = classifier)) + ggplot2::geom_point(position = ggplot2::position_jitter(width = .12), alpha = .75) +
      ggplot2::coord_flip() + ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = length(auc_cols))) +
      ggplot2::labs(x = NULL, y = "AUROC", colour = "Classifier")
    return(finish_plot(p, id, panel, width, "Seven-classifier context summary"))
  }
  if (id == "E9" && panel == "E") {
    d$context <- paste(d$dataset, d$comparison, d$subgroup, sep = ": "); d$context <- reorder(d$context, d$auc_mean_across_tissue)
    p <- ggplot2::ggplot(d, ggplot2::aes(context, auc_mean_across_tissue, colour = dataset)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = auc_min, ymax = auc_max), width = .15) + ggplot2::geom_point(size = 3) +
      ggplot2::coord_flip() + ggplot2::scale_colour_manual(values = .replica_cols) + ggplot2::labs(x = NULL, y = "Median AUROC across classifiers", colour = NULL)
    return(finish_plot(p, id, panel, width, "Error bars show the released across-context range"))
  }

  if (id == "E10" && panel == "A") {
    z <- aggregate(score_z ~ display_label + cell_type, d, mean, na.rm = TRUE)
    return(heatmap_plot(z, "display_label", "cell_type", "score_z", id, panel, width, "Mean program score standardized across broad cell classes", 30, 12))
  }

  # General, deterministic fallback for panels whose released table is a compact
  # summary rather than the exact coordinates of the manuscript artwork.
  nums <- numeric_columns(d); chars <- character_columns(d)
  if (length(nums) >= 2) {
    x <- nums[1]; y <- nums[2]; colour <- if (length(chars)) chars[1] else NULL
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(ggplot2::aes(colour = if (!is.null(colour)) .data[[colour]] else NULL), alpha = .65, size = 2) +
      ggplot2::scale_colour_manual(values = rep(.replica_cols, length.out = if (is.null(colour)) 1 else length(unique(d[[colour]])))) +
      ggplot2::labs(x = wrap_labels(x, 24), y = wrap_labels(y, 24), colour = if (is.null(colour)) NULL else wrap_labels(colour, 16))
    return(finish_plot(p, id, panel, width, "Source-data companion view; the complete selected panel table is downloadable"))
  }
  finish_plot(ggplot2::ggplot() + ggplot2::annotate("text", 0, 0,
    label = "This panel contains categorical or annotation-only source data.", size = 5) +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1), id, panel, width)
}
