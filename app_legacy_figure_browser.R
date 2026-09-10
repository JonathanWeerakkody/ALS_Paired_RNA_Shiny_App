# ALS Paired-RNA Explorer
# Interactive companion to the GLAST-positive sEV miRNA-mRNA analysis.
#
# Shows the published panels themselves, not reconstructions. Every image is the
# figure's own plot.png and every table its own exported source data, so what a
# reader sees here is what is in the paper.
#
#   shiny::runApp()      from this directory
#
# Requires: shiny, ggplot2.

library(shiny)
library(ggplot2)
source("R/engine.R")
source("R/verify.R")

rd <- function(f) { p <- file.path("data-processed", f)
  if (file.exists(p)) read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) else NULL }
IDX      <- rd("panel_index.csv")
PAIR_NUL <- rd("pair_null.csv")

TEAL <- "#1B6C6F"; CRIM <- "#A63446"
num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", as.character(x))))
theme_app <- function(b = 13) theme_minimal(base_size = b) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey92"),
        plot.title = element_text(face = "bold", size = b + 1),
        plot.subtitle = element_text(colour = "grey40", size = b - 2),
        plot.margin = margin(10, 16, 10, 10))

CAP <- c(
 "Figure 1|B"="Nanoparticle tracking across ten matched preparations; GLAST+ particles are 0.18% of the total pool",
 "Figure 1|E"="Astrocyte markers enriched in GLAST+ sEV relative to total EV",
 "Figure 1|F"="Platelet and lipoprotein markers depleted against the GLAST-negative fraction",
 "Figure 1|G"="RNA biotype composition across all 60 discovery profiles",
 "Figure 1|H"="Cargo richness by group; only transcript isoforms differ, and that contrast is confounded",
 "Figure 1|I"="Sequence-motif atlas; motif presence, not measured protein binding",
 "Figure 2|A"="Variance explained per factor. Factor 1 carries 0.18% and is outcome-guided",
 "Figure 2|B"="The two RNA layers agree across participants (r = 0.738)",
 "Figure 2|C"="miRNA differential abundance, long versus short survival",
 "Figure 2|D"="mRNA differential abundance, long versus short survival",
 "Figure 2|E"="Overlap of the three selection criteria; they agree poorly",
 "Figure 2|F"="Factor 1 miRNA loadings with clinical annotation",
 "Figure 2|G"="Factor 1 mRNA loadings with clinical annotation",
 "Figure 2|H"="The 42 highest-confidence miRNA-mRNA links",
 "Figure 2|I"="Evidence composition across the 304-pair cross-product",
 "Figure 2|J"="Repeated-split survival separation, miRNA score",
 "Figure 2|K"="Repeated-split survival separation, mRNA score",
 "Figure 2|L"="Repeated-split survival separation, combined score",
 "Figure 2|M"="Multivariable Cox sensitivity in the discovery cohort",
 "Figure 3|A"="Detection overlap between compartments; all 19 candidates are shared",
 "Figure 3|B"="Raw survival-time rank correlations in each compartment",
 "Figure 3|C"="Composite ranking that selects the five; the concordance term excludes miR-1-3p",
 "Figure 3|D"="Survival separation of the score in the discovery cohort",
 "Figure 3|E"="Survival separation in total plasma",
 "Figure 3|F"="Survival separation by NfL",
 "Figure 3|G"="Survival separation by miR-181",
 "Figure 3|H"="Survival separation by score plus NfL",
 "Figure 3|I"="Held-out hazard ratios across 200 stratified splits",
 "Figure 3|J"="Time-dependent discrimination by horizon",
 "Figure 3|K"="Incremental AUROC: the score rises with horizon while NfL falls",
 "Figure 4|B"="Cross-resource evidence for the 16 mRNA candidates",
 "Figure 4|C"="Two-axis selection; the upper-right quadrant is the prespecified rule",
 "Figure 4|D"="Four-axis score, reported as a sensitivity analysis",
 "Figure 4|E"="Expression of the five genes across cortical lineages",
 "Figure 4|F"="Class-mean relationship with SLC1A3; exploratory, not same-cell co-expression",
 "Figure 4|G"="Aggregate cargo score against 25 lineage-gated programs",
 "Figure 4|H"="Representative donor-level relationships",
 "Figure 5|A"="Target-supported network linking the two RNA arms",
 "Figure 5|B"="Pathway territories recovered from the lineage-gated programs",
 "Figure 5|C"="Feature-by-module convergence",
 "Figure 5|D"="Lineage contribution to each module; normalised enrichment, not cell fractions")
capt <- function(f, p) {
  k <- paste0(f, "|", p)
  if (k %in% names(CAP)) return(CAP[[k]])
  r <- IDX[IDX$figure == f & IDX$panel == p, ]
  if (nrow(r)) r$why[1] else ""
}

TOUR <- if (!is.null(IDX)) lapply(seq_len(nrow(IDX)), function(i)
  list(f = IDX$figure[i], p = IDX$panel[i], why = IDX$why[i], sec = IDX$section[i])) else list()

CSS <- HTML("
 .well{background:#fbfbfa;border:1px solid #ececec}
 .ctx{background:#fff8f0;border-left:4px solid #C87D2B;padding:10px 14px;margin:6px 0 14px;font-size:13px;color:#5b4636}
 .note{background:#f4f7f4;border-left:4px solid #9CAF88;padding:10px 14px;margin:6px 0 14px;font-size:13px;color:#3c4a3c}
 .why{background:#eef5f5;border-left:4px solid #1B6C6F;padding:11px 15px;margin:4px 0 14px;font-size:13.5px;color:#254a4b}
 .stat{display:inline-block;margin:0 26px 10px 0}
 .stat .v{font-size:22px;font-weight:600;color:#1B6C6F;display:block;line-height:1.15}
 .stat .l{font-size:11px;color:#777;text-transform:uppercase;letter-spacing:.5px}
 .panelimg{max-width:100%;height:auto;border:1px solid #e6e6e6;border-radius:3px;background:#fff}
 .panelwrap{max-height:660px;overflow:auto;border:1px solid #e6e6e6;border-radius:3px;background:#fff;padding:4px}
 .panelwrap img{display:block;border:0}
 .wide img{max-width:none}
 .fullsize{font-size:12px;color:#1B6C6F;margin:6px 2px 0;display:block}
 .warnbox{background:#fdf3f3;border-left:4px solid #A63446;padding:9px 13px;margin:6px 0 12px;
          font-size:12.5px;color:#6b3a3a}
 .pcap{color:#666;font-size:12.5px;margin:8px 2px 14px}
 table{font-size:12.5px}
 h4{margin-top:4px;color:#2b2b2b}
 .shiny-output-error-validation{color:#A63446;font-size:13px}
")
sb <- function(v, l) div(class="stat", span(class="v", v), span(class="l", l))

# Render a panel so its text stays legible: very wide panels scroll horizontally at
# native size instead of being shrunk to nothing; tall panels are capped and scroll
# vertically. Everything also gets a full-size link.
panel_view <- function(row) {
  src <- file.path("panels", row$image)
  ar  <- row$width / row$height
  wide <- ar > 2.2
  tagList(
    if (wide) div(class = "warnbox",
      "This panel is much wider than it is tall. It is shown at native resolution and scrolls ",
      "horizontally; use the full-size link for a closer look."),
    div(class = paste("panelwrap", if (wide) "wide" else ""),
        tags$img(src = src, class = if (wide) "" else "panelimg",
                 width = if (wide) row$width else NULL)),
    tags$a(class = "fullsize", href = src, target = "_blank",
           sprintf("Open at full size (%d x %d px) ->", row$width, row$height)))
}

ui <- fluidPage(
  tags$head(tags$style(CSS), tags$script(HTML(
    "Shiny.addCustomMessageHandler('verifyJson', function(x){",
    "  document.open('application/json');document.write(x);document.close();});"))),
  titlePanel("ALS Paired-RNA Explorer"),
  p(style="color:#666;margin-top:-8px;font-size:14px",
    "Interactive companion to the GLAST-positive extracellular-vesicle miRNA–mRNA analysis. ",
    "Every panel shown is the published figure itself."),
  div(class="ctx", strong("Research use only. "),
      "Cohort-level analyses from the study. Not a diagnostic tool, treatment-response assay, ",
      "or individual survival calculator."),
  tabsetPanel(id="tab",
    tabPanel("Guided tour",     br(), uiOutput("t_tour")),
    tabPanel("All panels",      br(), uiOutput("t_all")),
    tabPanel("Key evidence",    br(), uiOutput("t_key")),
    tabPanel("Analyse your data", br(), uiOutput("t_run")),
    tabPanel("Verify the paper", br(), uiOutput("t_ver")),
    tabPanel("Reproducibility", br(), uiOutput("t_rep"))),
  hr(),
  div(style="color:#999;font-size:11.5px;padding-bottom:18px",
      "Frozen paper analysis · settings v1.0.0-paper · no model is refitted at runtime"))

server <- function(input, output, session) {

  step <- reactiveVal(1L)
  observeEvent(input$nxt, step(min(length(TOUR), step() + 1L)))
  observeEvent(input$prv, step(max(1L, step() - 1L)))

  output$t_tour <- renderUI({
    s <- TOUR[[step()]]
    fluidRow(
      column(3, div(class="well",
        h4("The argument"),
        p(style="color:#777;font-size:12.5px", sprintf("Step %d of %d", step(), length(TOUR))),
        actionButton("prv", "◀ Back"), " ", actionButton("nxt", "Next ▶"),
        hr(),
        tags$ol(style="padding-left:18px;font-size:12.5px;color:#555",
          lapply(seq_along(TOUR), function(i)
            tags$li(style = if (i == step()) "font-weight:600;color:#1B6C6F" else "",
                    paste0(TOUR[[i]]$f, TOUR[[i]]$p),
                    span(style="color:#999;font-size:11px", paste0("  ", TOUR[[i]]$sec))))))),
      column(9, h4(paste0(s$f, s$p)), div(class="why", s$why),
        uiOutput("tour_img"), div(class="pcap", capt(s$f, s$p))))
  })

  output$tour_img <- renderUI({
    s <- TOUR[[step()]]
    r <- IDX[IDX$figure == s$f & IDX$panel == s$p, ]
    if (!nrow(r)) return(div(class="note", "panel image not found"))
    panel_view(r[1, ])
  })

  output$t_all <- renderUI({
    validate(need(!is.null(IDX), "panel_index.csv not found"))
    fluidRow(
      column(3, div(class="well",
        h4("Browse"),
        selectInput("sec", "Section", choices = unique(IDX$section)),
        uiOutput("panel_sel"),
        div(class="note", "A curated selection: the panels that carry the argument. ",
            "All 107 panels and their complete source data ship with the paper."))),
      column(9, uiOutput("all_img"), div(class="pcap", textOutput("all_cap")),
        h4("Source data"), tableOutput("all_tbl"), uiOutput("all_dl")))
  })

  output$panel_sel <- renderUI({
    d <- IDX[IDX$section == input$sec, ]
    ch <- setNames(paste0(d$figure, "|", d$panel), paste0(d$figure, d$panel))
    selectInput("pan", "Panel", choices = ch, selected = ch[1])
  })

  cur <- reactive({
    req(input$pan)
    fp <- strsplit(input$pan, "\\|")[[1]]
    r <- IDX[IDX$figure == fp[1] & IDX$panel == fp[2], ]
    validate(need(nrow(r) > 0, "panel not found")); r[1, ]
  })

  output$all_img <- renderUI(panel_view(cur()))
  output$all_cap <- renderText(cur()$why)
  output$all_tbl <- renderTable({
    f <- cur()$data
    validate(need(nzchar(f), "No source table under 3 MB is bundled for this panel."))
    d <- read.csv(file.path("data-processed/panels", f), check.names = FALSE)
    validate(need(nrow(d) > 0, "empty table"))
    head(d[, seq_len(min(9, ncol(d))), drop = FALSE], 12)
  }, striped = TRUE, spacing = "xs", width = "100%", digits = 4)
  output$all_dl <- renderUI(if (nzchar(cur()$data))
    downloadButton("dl_panel", "Download this panel's source data"))
  output$dl_panel <- downloadHandler(
    filename = function() cur()$data,
    content  = function(f) file.copy(file.path("data-processed/panels", cur()$data), f))

  output$t_key <- renderUI(tagList(
    h4("Three results that carry the argument"),
    p(style="color:#666;font-size:13px",
      "Two are published panels. The third is the pairing control reported in the Supplementary ",
      "Information, included because it is the direct test of the paper's premise."),
    fluidRow(column(6, uiOutput("k1")), column(6, uiOutput("k2"))),
    hr(), h4("Does matched participant pairing carry information?"),
    plotOutput("k_null", height = "300px"),
    div(class="note", "Permuting participant identity between the miRNA and mRNA matrices destroys the ",
        "cross-layer correlation. No permutation of ten thousand reached the observed value.")))

  output$k1 <- renderUI({
    r <- IDX[IDX$figure == "Figure 3" & IDX$panel == "K", ]
    tagList(h5("Figure 3K — incremental value by horizon"),
            if (nrow(r)) panel_view(r[1, ]),
            div(class="pcap", capt("Figure 3", "K")))})
  output$k2 <- renderUI({
    r <- IDX[IDX$figure == "Figure 4" & IDX$panel == "C", ]
    tagList(h5("Figure 4C — two-axis gene selection"),
            if (nrow(r)) panel_view(r[1, ]),
            div(class="pcap", capt("Figure 4", "C")))})

  output$k_null <- renderPlot({
    validate(need(!is.null(PAIR_NUL), "pair_null.csv not found"))
    v <- num(PAIR_NUL[[2]]); obs <- 0.7384
    ggplot(data.frame(v = v), aes(v)) +
      geom_histogram(bins = 60, fill = "grey82", colour = "white", linewidth = .2) +
      geom_vline(xintercept = obs, colour = CRIM, linewidth = 1.1) +
      annotate("text", x = obs, y = Inf, label = paste0("observed  r = ", obs),
               hjust = 1.06, vjust = 1.9, colour = CRIM, size = 4.1, fontface = "bold") +
      labs(title = "Cross-layer correlation when participant pairing is destroyed",
           subtitle = paste0("10,000 permutations · null maximum ", round(max(abs(v)), 3),
                             " · empirical P < 0.0001"),
           x = "correlation between miRNA and mRNA factor scores", y = "permutations") +
      theme_app()})

  # ---------------------------------------------------- analyse your data
  output$t_run <- renderUI(tagList(
    div(class="ctx", strong("Your data stays in this session. "),
        "Files are read into memory, analysed, and discarded when you close the tab. ",
        "Nothing is written to disk or transmitted. Even so, upload de-identified data only."),
    radioButtons("mode", NULL, inline = TRUE,
      c("Score a cohort with the fixed five-miRNA feature set" = "score",
        "Run an exploratory paired regulator-effector workflow" = "pair")),
    uiOutput("run_inputs"), hr(), uiOutput("run_out")))

  output$run_inputs <- renderUI({
    if (identical(input$mode, "score")) {
      fluidRow(
        column(4, div(class="well",
          h4("Inputs"),
          fileInput("f_mi", "miRNA matrix (CSV)", accept = ".csv"),
          fileInput("f_md", "Metadata, optional (CSV)", accept = ".csv"),
          actionButton("demo1", "Load demo cohort"), " ",
          actionButton("go1", "Run", class = "btn-primary"))),
        column(8, div(class="note",
          strong("Format. "), "miRNA matrix: first column the miRNA identifier ",
          "(hsa-miR-31-5p or miR-31-5p), one further column per sample, values on a log scale. ",
          "Metadata: sample, time, status, with status 1 for the event. ",
          "Without metadata you get the score only; with it you also get survival separation.",
          br(), br(),
          strong("What it does. "), "Z-scores each of the five panel miRNAs across your samples, ",
          "averages them, and splits at the median. The panel is fixed; nothing is refitted.")))
    } else {
      fluidRow(
        column(4, div(class="well",
          h4("Inputs"),
          fileInput("p_mi", "miRNA matrix (CSV)", accept = ".csv"),
          fileInput("p_mr", "mRNA matrix (CSV)", accept = ".csv"),
          fileInput("p_md", "Metadata (CSV)", accept = ".csv"),
          sliderInput("p_top", "Candidates per layer", 5, 30, 12, step = 1),
          actionButton("demo2", "Load demo cohort"), " ",
          actionButton("go2", "Run", class = "btn-primary"))),
        column(8, div(class="note",
          strong("What it does. "), "Runs the paper's method on your data: an outcome-guided joint ",
          "factor across the two layers, candidate selection by loading in both, the full ",
          "cross-product scored for inverse direction against outcome, the four directional pair ",
          "states, and the participant-pairing null.",
          br(), br(),
          strong("Requirements. "), "Both matrices must share sample column names, and at least ",
          "12 samples must have survival. Twelve or more features per layer.")))
    }
  })

  DEMO <- reactiveValues(on1 = FALSE, on2 = FALSE)
  observeEvent(input$demo1, DEMO$on1 <- TRUE)
  observeEvent(input$demo2, DEMO$on2 <- TRUE)

  res1 <- eventReactive(input$go1, {
    mi <- if (DEMO$on1 && is.null(input$f_mi)) "demo-data/demo_mirna_matrix.csv" else input$f_mi$datapath
    md <- if (DEMO$on1 && is.null(input$f_md)) "demo-data/demo_metadata.csv"
          else if (!is.null(input$f_md)) input$f_md$datapath else NULL
    validate(need(!is.null(mi), "Upload a miRNA matrix, or load the demo cohort."))
    tryCatch(score_panel(mi, md), error = function(e) structure(conditionMessage(e), class = "err"))
  })

  res2 <- eventReactive(input$go2, {
    d <- DEMO$on2 && is.null(input$p_mi)
    mi <- if (d) "demo-data/demo_paired_mirna.csv"    else input$p_mi$datapath
    mr <- if (d) "demo-data/demo_paired_mrna.csv"     else input$p_mr$datapath
    md <- if (d) "demo-data/demo_paired_metadata.csv" else input$p_md$datapath
    validate(need(!is.null(mi) && !is.null(mr) && !is.null(md),
                  "Upload all three files, or load the demo cohort."))
    tryCatch(run_pairing(mi, mr, md, top_n = input$p_top),
             error = function(e) structure(conditionMessage(e), class = "err"))
  })

  output$run_out <- renderUI({
    if (identical(input$mode, "score")) tagList(uiOutput("o1_head"), plotOutput("o1_km", height="330px"))
    else tagList(uiOutput("o2_head"), fluidRow(
      column(6, plotOutput("o2_null", height="300px")),
      column(6, plotOutput("o2_state", height="300px"))),
      h4("Candidate pairs"), tableOutput("o2_pairs"))
  })

  output$o1_head <- renderUI({
    r <- res1(); if (inherits(r, "err")) return(div(class="warnbox", strong("Could not run. "), r))
    s <- r$survival
    tagList(
      div(class="note", sprintf("%d of 5 panel miRNAs found across %d samples%s.",
          length(r$found), r$n_samples,
          if (length(r$missing)) paste0("; missing ", paste(r$missing, collapse=", ")) else "")),
      if (!is.null(s)) div(
        sb(s$n, "participants"), sb(s$events, "events"),
        sb(sprintf("%.2f", s$chisq), "log-rank chi2"),
        sb(format.pval(s$p, digits=2), "P"), sb(sprintf("%.3f", s$cindex), "C-index"))
      else div(class="ctx", "No metadata supplied, so only the score was computed."))
  })

  output$o1_km <- renderPlot({
    r <- res1(); validate(need(!inherits(r,"err"), " ")); s <- r$survival
    validate(need(!is.null(s), "Supply metadata to see survival separation."))
    d <- rbind(cbind(s$km_high, g="High score"), cbind(s$km_low, g="Low score"))
    ggplot(d, aes(t, s, colour=g)) + geom_step(linewidth=1) +
      scale_colour_manual(values=c("High score"=CRIM,"Low score"=TEAL), name=NULL) +
      coord_cartesian(ylim=c(0,1)) +
      labs(title="Survival by five-miRNA score, split at the cohort median",
           subtitle=sprintf("n = %d, %d events · log-rank chi2 = %.2f, P = %s",
             s$n, s$events, s$chisq, format.pval(s$p, digits=2)),
           x="Time", y="Survival probability") + theme_app()
  })

  output$o2_head <- renderUI({
    r <- res2(); if (inherits(r,"err")) return(div(class="warnbox", strong("Could not run. "), r))
    tagList(div(class="note",
      sprintf("%d samples · %d miRNA and %d mRNA features after filtering.",
              r$n_samples, r$n_mirna, r$n_mrna)),
      div(sb(sprintf("%.2f%%", r$variance_pct), "variance on the outcome axis"),
          sb(sprintf("%+.2f", r$factor_rho), "factor-survival rho"),
          sb(nrow(r$pairs), "candidate pairs"),
          sb(sprintf("%.0f%%", 100*mean(r$pairs$inverse)), "inverse direction")))
  })

  output$o2_null <- renderPlot({
    r <- res2(); validate(need(!inherits(r,"err")," ")); co <- r$coherence
    ggplot(data.frame(v=co$null), aes(v)) +
      geom_histogram(bins=50, fill="grey82", colour="white", linewidth=.2) +
      geom_vline(xintercept=co$observed, colour=CRIM, linewidth=1.1) +
      labs(title="Cross-layer coherence against the pairing null",
           subtitle=sprintf("observed r = %.3f · 2,000 permutations · P = %.4f",
                            co$observed, co$p),
           x="correlation between layer scores", y="permutations") + theme_app()
  })

  output$o2_state <- renderPlot({
    r <- res2(); validate(need(!inherits(r,"err")," "))
    d <- r$states; validate(need(!is.null(d) && nrow(d)>0, "No states to show."))
    d$lab <- paste0(d$state, "\n(", d$n_pairs, " pairs)")
    d$sig <- ifelse(is.finite(d$p) & d$p < 0.05, "P < 0.05", "ns")
    ggplot(d, aes(reorder(lab, -chisq), chisq, fill=sig)) + geom_col(width=.65) +
      scale_fill_manual(values=c("P < 0.05"=TEAL, "ns"="grey78"), name=NULL) +
      labs(title="Survival separation by directional pair state",
           subtitle="log-rank chi-square, participants split at the median burden",
           x=NULL, y="chi-square") + theme_app()
  })

  output$o2_pairs <- renderTable({
    r <- res2(); validate(need(!inherits(r,"err")," "))
    d <- r$pairs[order(-abs(r$pairs$pair_r)), ]
    d <- head(d[, c("miRNA","mRNA","state","rho_miRNA","rho_mRNA","pair_r")], 15)
    names(d) <- c("miRNA","mRNA","Directional state","miRNA-survival rho","mRNA-survival rho","Pair correlation")
    d
  }, striped=TRUE, spacing="xs", width="100%", digits=3)

  # ---------------------------------------------------- verify the paper
  VER <- reactive(verify_all())

  output$t_ver <- renderUI({
    v <- VER(); s <- verify_summary(v)
    tagList(
      h4("Every checkable number in the paper, recomputed from the shipped data"),
      p(style="color:#666;font-size:13px",
        "Each claim below is recomputed live from the same files the paper ships. ",
        "Nothing is read from the manuscript text, so a disagreement means either the ",
        "manuscript or the data is wrong."),
      div(class = if (s$fail == 0) "note" else "warnbox",
          sb(s$total, "claims"), sb(s$pass, "pass"), sb(s$fail, "fail"), sb(s$skip, "skipped"),
          br(),
          if (s$fail == 0) "Every claim reproduces from the shipped data."
          else "One or more claims do not reproduce. See the table."),
      div(class="ctx", strong("For automated checking. "),
          "Append ", tags$code("?verify=json"), " to the app URL for the same result as JSON, ",
          "or run ", tags$code("Rscript -e 'source(\"R/verify.R\"); cat(verify_json())'"),
          " in the repository. Exit status is non-zero if any claim fails, so it drops into CI."),
      downloadButton("dl_ver_json", "Download JSON"), " ",
      downloadButton("dl_ver_csv", "Download CSV"),
      br(), br(), tableOutput("ver_tbl"))
  })

  output$ver_tbl <- renderTable({
    v <- VER()
    data.frame(ID = v$id, Section = v$section, Claim = v$claim,
               Expected = v$expected, Observed = round(v$observed, 4),
               Source = v$source, Status = v$status, check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%", digits = 4)

  output$dl_ver_json <- downloadHandler(
    filename = function() "als_paired_rna_claims.json",
    content  = function(f) writeLines(verify_json(VER()), f))
  output$dl_ver_csv <- downloadHandler(
    filename = function() "als_paired_rna_claims.csv",
    content  = function(f) write.csv(VER(), f, row.names = FALSE))

  # machine endpoint: ?verify=json returns the manifest and nothing else
  observe({
    q <- parseQueryString(session$clientData$url_search)
    if (identical(q$verify, "json")) {
      session$sendCustomMessage("verifyJson", verify_json())
    }
  })

  output$t_rep <- renderUI(fluidRow(column(9,
    h4("Frozen analysis"),
    p("Every panel shown is the published image and every table its own exported source data. ",
      "Paper-reproduction mode refits no published model; optional cohort uploads are session-local, and the application returns no individual clinical estimate."),
    div(class="note",
        sb(nrow(IDX), "curated panels"), sb(sum(nzchar(IDX$data)), "with source data"),
        sb("v1.0.0-paper", "settings"), sb("0", "models refitted")),
    h4("Panel inventory"), tableOutput("rep_tbl"),
    h4("Downloads"),
    downloadButton("dl_settings", "Paper settings (YAML)"), " ",
    downloadButton("dl_index", "Panel index (CSV)"))))

  output$rep_tbl <- renderTable({
    d <- IDX
    data.frame(Figure = unique(d$figure),
      Panels = as.integer(table(factor(d$figure, levels = unique(d$figure)))),
      `With source data` = as.integer(tapply(nzchar(d$data), factor(d$figure, levels = unique(d$figure)), sum)),
      check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%")

  output$dl_settings <- downloadHandler(
    filename = function() "paper_settings.yml",
    content  = function(f) file.copy("data-processed/paper_settings.yml", f))
  output$dl_index <- downloadHandler(
    filename = function() "panel_index.csv",
    content  = function(f) write.csv(IDX, f, row.names = FALSE))
}

shinyApp(ui, server)
