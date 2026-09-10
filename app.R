# ALS Paired RNA manuscript companion
# Fifteen purpose-built data views. These explain the paper; they do not imitate
# or redistribute the author-edited publication figures.

library(shiny)
library(ggplot2)
library(readxl)

TRAIL <- read.csv("config/story_steps.csv", check.names = FALSE,
                  stringsAsFactors = FALSE)
FIGURE_CATALOG <- read.csv("config/figure_catalog.csv", check.names = FALSE,
                           stringsAsFactors = FALSE)
FIGURE_SOURCE_ROOT <- normalizePath("source_data", mustWork = TRUE)

addResourcePath("all-source-data", FIGURE_SOURCE_ROOT)
addResourcePath("machine", normalizePath("config"))
addResourcePath("r-code", normalizePath("R"))

read_data <- function(name) read.csv(file.path("data-processed", name),
                                     check.names = FALSE,
                                     stringsAsFactors = FALSE)
RNA_COMPOSITION <- read.csv("data-processed/panels/Figure 1_G.csv",
                            check.names = FALSE, stringsAsFactors = FALSE)
FACTOR_SCORES <- read.csv("data-processed/panels/Figure 2_B.csv",
                          check.names = FALSE, stringsAsFactors = FALSE)
CANDIDATES <- read_data("candidates.csv")
TISSUE_BRIDGE <- read_data("tissue_bridge.csv")

stage_labels <- setNames(TRAIL$stage_id,
                         paste0(TRAIL$stage_id, ". ", TRAIL$figure_label,
                                ": ", TRAIL$stage))

CHAPTERS <- data.frame(
  start = c(1, 6, 9, 12, 15),
  end = c(5, 8, 11, 14, 15),
  label = c("1  Build the measurement", "2  Discover the paired program",
            "3  Test plasma transfer", "4  Connect to cortex",
            "5  Integrate the biology"),
  stringsAsFactors = FALSE
)

READ_CUES <- c(
  "Compare the dominant RNA fractions between ALS and control profiles.",
  "Read the bars as the fraction of localized particles in each tetraspanin phenotype.",
  "Red and blue encode opposite association directions; stronger colour means stronger nominal evidence.",
  "Follow each RNA layer from measured features to the prevalent universe and the small displayed motif subset.",
  "Each coloured cell is a detected molecule; blank cells were not detected.",
  "The two participant-level factor scores should move together if the paired RNA layers share a joint signal.",
  "Rows are candidate RNAs and columns are clinical metadata; colour shows signed association evidence.",
  "Compare the mean pair-state composition across short, medium and long survival tertiles.",
  "The five highlighted miRNAs retain concordant effect structure across vesicles and total plasma.",
  "Each line joins a candidate's survival correlation before and after ComBat correction.",
  "Each line averages across classifier families and shows how discrimination changes with prediction horizon.",
  "The highlighted genes balance vesicle survival support with independent cortical-tissue support.",
  "Points and ranges summarize classification performance across external cortical contexts.",
  "The heatmap localizes lineage-gated state programs across broad cortical cell classes.",
  "Bars rank the 11 shared modules by adjusted evidence; labels give the number of pathways in each module and colour gives ALS gain or loss."
)

MANUSCRIPT_GUIDE <- c(
  "The study begins with 60 discovery participants: 45 with ALS and 15 controls. One small RNA library and one transcriptome library were generated from the same GLAST positive sEV RNA eluate for every participant, giving 120 matched libraries. This first view establishes what was measured before any survival guided selection.",
  "The captured material was a rare, surface defined vesicle fraction, not a uniform vesicle subtype. Three colour dSTORM classified 339 particles and found single, double and triple positive tetraspanin combinations. That heterogeneity is why the manuscript uses the operational term GLAST positive sEV fraction rather than claiming a pure cell of origin population.",
  "Global RNA variation was examined before candidate selection because age, sex and sequencing depth contributed prominent variation in composition and complexity. This atlas makes those possible covariates visible and explains why the downstream survival scores were subjected to depth sensitivity analyses.",
  "Sequence context was analyzed separately from the outcome guided MOFA+ input. The motif universe contained 162 eligible miRNAs and 6,097 protein coding mRNAs with representative 3 prime UTRs, whereas MOFA+ used 187 miRNAs and the 374 most variable mRNAs. Keeping these universes separate prevents the motif atlas from being mistaken for a feature selection step.",
  "The junction analysis was a scoped audit of what low input sEV RNA could support. It detected 110 recurrent unannotated junctions with canonical splice site motifs, but no qualifying published KCNQ2, STMN2 or UNC13A associated TDP 43 cryptic junctions at the obtained depth. The negative result is part of the evidence boundary.",
  "Outcome guided MOFA+ was fitted in the 45 ALS participants using miRNA, mRNA and survival as three views. Factor 1 explained only 0.18% of feature weighted molecular variance, so it should be read as a supervised prioritization axis rather than the dominant source of global variance. Its loading weighted miRNA and mRNA scores nevertheless moved together across matched participants.",
  "Candidate nomination integrated Factor 1 loading, ALS control differential abundance and survival association. That process produced 19 miRNAs and 16 mRNAs. The clinical association map shows how those candidates relate to survival, ALSFRS-R measures and other metadata; it does not imply that every displayed association is independently significant after multiplicity correction.",
  "The 19 miRNAs and 16 mRNAs create 304 possible cross layer pairs; 119 had a canonical seed match, an experimental target record or both. The manuscript asks whether relative miRNA mRNA states, rather than isolated abundance values, organize survival. Suppression burden was greatest in the shortest survival tertile and inversely correlated with observed survival.",
  "All 19 discovery miRNAs were measurable in the independent total plasma cohort. Transfer was then restricted by bootstrap Cox effect magnitude and preservation of survival effect direction across compartments. This selected miR-31-5p, miR-4492, miR-93-3p, miR-421 and miR-324-5p; miR-1-3p was excluded because it lacked bootstrap direction concordance.",
  "The plasma cohort is retrospective and processing batch covaried with survival. ComBat changed the survival correlation sign for 9 of 19 candidates and weakened repeated split score separation. Showing normalized and ComBat corrected results together makes this sensitivity explicit instead of treating one preprocessing choice as definitive.",
  "The five miRNA score was compared with ALSFRS-R slope, NfL and clinical baselines across 12, 24, 36 and 54 month horizons. Its incremental contribution was horizon dependent: NfL contributed more at 12 months, whereas the RNA score produced the larger incremental gain at later horizons in the reported analyses. The classifier comparison is therefore a robustness view, not a claim that one algorithm is clinically validated.",
  "The mRNA arm was evaluated in independent cortical resources: 586 bulk profiles from 308 donors and 527,261 nuclei from 69 donors. Five genes, NPM1, MTMR3, ZBTB16, LAMP1 and CBL, exceeded the candidate median on both the original sEV survival axis and the external tissue support axis.",
  "The selected five gene core was then examined across bulk and single nucleus disease contexts. Classification ranges describe how consistently the panel separates those contexts, but the same tissue resources also informed core refinement. Pooled comparisons anchor interpretation; small regional or subtype comparisons remain descriptive.",
  "Cell state programs were interpreted only within prespecified parent lineages, so shared interferon, inflammatory, reactive and stress responses were not treated as lineage specific merely because they appeared in one cell class. The five cargo genes were excluded from the state program gene sets, reducing direct mathematical overlap.",
  "Finally, the two RNA arms were placed into a source attributed target and pathway context. The mRNA program and predicted miRNA target footprints overlapped 11 shared modules: ten showed an ALS gain direction and one captured reduced myelination and axon glia support. These are convergent biological contexts, not proof of direct molecular causation."
)

CLAIM_SUPPORT <- c(
  "Supports feasibility of paired miRNA and mRNA profiling from the same GLAST-positive sEV preparation.",
  "Supports characterization of a heterogeneous GLAST-enriched sEV fraction; it does not establish vesicle brain origin or lineage purity.",
  "Supports explicit treatment of technical and demographic structure when interpreting the downstream survival-linked signal.",
  "Supports RNA-handling sequence context in the recovered cargo; motif-family comparisons did not survive multiple-testing correction.",
  "Supports a recurrent junction inventory at the available depth; it does not support detection of the queried canonical TDP-43 cryptic junctions.",
  "Supports a matched, survival-associated cross-layer program and nomination of a discovery candidate space; it is outcome guided.",
  "Supports the prespecified 19-miRNA and 16-mRNA discovery set while showing that candidate-clinical relationships are heterogeneous.",
  "Supports a directional, target-informed interpretation of the paired program; database or seed support does not establish direct target engagement.",
  "Supports transfer across compartments of a locked five miRNA panel from GLAST positive sEVs to total plasma.",
  "Supports a material processing-batch sensitivity limitation in the plasma survival analysis.",
  "Supports horizon-dependent prognostic information in retrospective cross-validation; it does not establish prospective clinical utility or a treatment threshold.",
  "Supports a five mRNA core with joint circulating survival and independent cortical context evidence.",
  "Supports representation of the five-gene core across external cortical disease contexts, with small subgroups interpreted cautiously.",
  "Supports association of the cargo score with lineage-gated glial and vascular states, not person-level blood-to-brain correspondence.",
  "Supports pathway-level convergence across the selected RNA arms and cortical programs; it does not demonstrate direct mechanism."
)

.panel_cache <- new.env(parent = emptyenv())
read_source_panel <- function(workbook, sheet) {
  key <- paste(workbook, sheet, sep = "::")
  if (exists(key, .panel_cache, inherits = FALSE)) return(get(key, .panel_cache))
  d <- suppressMessages(read_excel(file.path(FIGURE_SOURCE_ROOT, workbook),
                                   sheet = sheet, guess_max = 100000))
  d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
  for (nm in names(d)) {
    if (is.list(d[[nm]])) d[[nm]] <- vapply(d[[nm]], function(x)
      if (!length(x) || all(is.na(x))) NA_character_ else as.character(x[1]), "")
    if (is.character(d[[nm]])) {
      raw <- trimws(d[[nm]]); present <- !is.na(raw) & nzchar(raw)
      parsed <- suppressWarnings(as.numeric(raw))
      if (sum(is.finite(parsed)) >= max(3, ceiling(.45 * sum(present)))) d[[nm]] <- parsed
    }
  }
  assign(key, d, .panel_cache); d
}

short_name <- function(x) sub("^hsa-", "", as.character(x))
wrap_text <- function(x, width) {
  vapply(as.character(x), function(z) paste(strwrap(z, width), collapse = "\n"), "")
}

theme_story <- function(width = 1000) {
  mobile <- is.finite(width) && width < 620
  base <- if (mobile) 11.5 else if (width < 900) 13 else 14.5
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#e7e9e6", linewidth = .35),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "#303634"),
      legend.position = "bottom",
      legend.box = "vertical",
      plot.margin = margin(18, 28, 18, 22)
    )
}

CSS <- HTML("
body{background:#f7f7f5;color:#252525;font-family:Arial,Helvetica,sans-serif}
.container-fluid{max-width:1360px;margin:auto;padding:0 22px 28px}
.app-title{margin:20px 0 4px;font-weight:700;color:#183C3F}
.intro{max-width:1050px;color:#4f5755;font-size:16px;margin:0 0 16px;line-height:1.5}
.context{background:#eef5f5;border-left:4px solid #1B6C6F;padding:11px 14px;margin:8px 0 18px;color:#304d4f}
.story-control{background:white;border:1px solid #dfe3df;padding:14px 16px;margin-bottom:16px}
.chapter-map{display:grid;grid-template-columns:repeat(5,1fr);gap:8px;margin:4px 0 16px}
.chapter-link{display:block;background:#eef1ef;border:1px solid #d8dedb;border-radius:7px;padding:10px 9px;color:#35504e!important;text-decoration:none!important;font-size:13px;line-height:1.25;min-height:56px}
.chapter-link:hover{background:#e3ecea}
.chapter-link.active{background:#183C3F;color:white!important;border-color:#183C3F;font-weight:700}
.story-control .form-group{margin-bottom:0}
.nav-buttons{display:flex;justify-content:space-between;gap:8px;margin:8px 0 16px}
.step-line{display:flex;align-items:center;gap:12px;margin:4px 0 13px;color:#52615f;font-size:13px}
.step-track{height:6px;background:#e2e8e6;flex:1;max-width:420px;overflow:hidden;border-radius:4px}
.step-fill{height:100%;background:#2B6F73;border-radius:4px}
.stage-question{color:#666;font-size:14px;margin:0 0 6px}
.conclusion{background:#eef5f5;border-left:4px solid #1B6C6F;padding:12px 15px;margin:10px 0;font-size:16px;line-height:1.5}
.figure-badge{display:inline-block;background:#183C3F;color:white;border-radius:16px;padding:5px 10px;font-size:12px;font-weight:700;letter-spacing:.02em;margin-bottom:8px}
.figure-badge.extended{background:#8064A2}
.guide-copy{font-size:15px;line-height:1.58;color:#3f4947;max-width:1100px;margin:7px 0 12px}
.claim-support{background:#f2f0f7;border-left:4px solid #8064A2;padding:11px 14px;margin:10px 0 12px;line-height:1.5}
.text-label{font-size:12px;text-transform:uppercase;letter-spacing:.06em;font-weight:700;color:#52615f;display:block;margin-bottom:3px}
.graph-note{background:#fff8f0;border-left:4px solid #C87D2B;padding:10px 14px;margin:10px 0 12px;font-size:14px;line-height:1.45}
.takeaway-title{margin:18px 0 8px;color:#183C3F}
.continue-card{display:flex;align-items:center;justify-content:space-between;gap:16px;background:#183C3F;color:white;border-radius:8px;padding:14px 16px;margin:18px 0 8px}
.continue-card .next-copy{line-height:1.35}
.continue-card .btn{background:white;color:#183C3F;border:0;font-weight:700;white-space:normal}
.button-row{display:flex;gap:8px;flex-wrap:wrap;margin:8px 0 15px}
.story-plot{background:white;border:1px solid #dfe2dd;padding:8px 12px;margin-bottom:8px;overflow:visible}
.figure-help{color:#6c7472;font-size:13px;margin:7px 2px 20px}
.download-lead,.card{background:white;border:1px solid #dfe3df;padding:18px;margin:0 0 18px}
.download-lead h3,.card h4{margin-top:0}
.card h4{color:#1B6C6F}
.download-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(250px,1fr));gap:12px;margin:12px 0 24px}
.download-grid .card{margin:0}
@media (max-width:767px){
  .container-fluid{padding-left:12px;padding-right:12px}
  .app-title{font-size:25px}
  .intro{font-size:15px}
  .story-control{padding:12px}
  .chapter-map{display:flex;overflow-x:auto;gap:6px;padding-bottom:4px}
  .chapter-link{min-width:185px;min-height:0;padding:8px 10px}
  .nav-buttons{justify-content:flex-start;margin-top:2px}
  .button-row .btn,.button-row .shiny-download-link{width:100%;white-space:normal}
  .continue-card{align-items:stretch;flex-direction:column}
  .story-plot{padding:4px}
  #story_plot{height:680px!important}
  .navbar-nav>li>a{padding-left:10px;padding-right:10px}
}
@media (max-width:420px){
  .step-line{align-items:flex-start;flex-direction:column;gap:5px}
  .step-track{width:100%;max-width:none}
}
")

ui <- fluidPage(
  tags$head(
    tags$style(CSS),
    tags$script(HTML("Shiny.addCustomMessageHandler('scrollTop', function(message){var x=document.getElementById('stage_header'); if(x){x.scrollIntoView({behavior:'smooth',block:'start'});}});"))
  ),
  h2(class = "app-title", "ALS Paired RNA Data Story"),
  p(class = "intro",
    "Fifteen guided checkpoints follow the manuscript from measurement and quality control through paired RNA discovery, total plasma transfer, cortical context and pathway convergence."),
  div(class = "context",
      "Main and Extended Data figures are integrated in reading order. Each step explains what the manuscript asked, shows a responsive graph generated from released data, and states what the evidence supports and what it does not support."),
  tabsetPanel(
    id = "main_tab",
    tabPanel("Guided story", br(),
      uiOutput("chapter_nav"),
      div(class = "story-control",
        fluidRow(
          column(8, selectInput("stage_id", "Story step",
                                choices = stage_labels, selected = 1)),
          column(4, div(class = "nav-buttons",
                        actionButton("previous_stage", "Previous"),
                        actionButton("next_stage", "Next"))))),
      uiOutput("stage_header"),
      uiOutput("graph_note"),
      uiOutput("stage_actions"),
      div(class = "story-plot", plotOutput("story_plot", height = "600px")),
      p(class = "figure-help",
        "This graph is generated from the downloadable data shown for this step. It summarizes one complete main or Extended Data figure; it does not replace the publication figure."),
      uiOutput("stage_takeaway"),
      uiOutput("continue_card")),
    tabPanel("Downloads", br(), uiOutput("all_downloads")),
    tabPanel("Audit and agents", br(),
      div(class = "download-lead",
        h3("A stable evidence trail for people and software"),
        p("This section exposes the same story as simple CSV, JSON and R entry points. A future reviewer or software agent can map each statement to a figure workbook and worksheet, rebuild the web graph, and run the numerical checks without scraping the interface."),
        downloadButton("download_agent_story", "Download complete story and claim trail",
                       class = "btn-primary")),
      div(class = "download-grid",
        div(class = "card", h4("Story schema"),
            p("The ordered question, finding, figure and source-workbook mapping for all 15 checkpoints."),
            tags$a(class = "btn btn-default", href = "machine/story_steps.csv", download = NA,
                   "Download story schema")),
        div(class = "card", h4("Evidence manifest"),
            p("Machine-readable scope, figure counts, source inventory and runtime contract."),
            tags$a(class = "btn btn-default", href = "machine/evidence_manifest.json", download = NA,
                   "Download manifest")),
        div(class = "card", h4("Figure catalogue"),
            p("Stable mapping of every main and Extended Data figure to its released workbook."),
            tags$a(class = "btn btn-default", href = "machine/figure_catalog.csv", download = NA,
                   "Download figure catalogue")),
        div(class = "card", h4("Numerical checks"),
            p("R assertions for the manuscript-level values represented in the companion."),
            tags$a(class = "btn btn-default", href = "r-code/verify.R", download = NA,
                   "Download verification code"))),
      div(class = "card", h4("Current checkpoint audit record"),
          p("This compact record updates with the selected story step and identifies the exact source table behind the graph."),
          tableOutput("current_audit"))),
    tabPanel("About", br(),
      div(class = "card",
        h4("Why these graphs are different"),
        p("The manuscript figures are dense, publication-edited composites. This app uses one responsive data view per checkpoint and follows the manuscript's argument in sequence, so readers can understand the evidence without reproducing the printed page layout in a browser."),
        h4("Reproducibility"),
        p("Every graph can be downloaded with its plotted data and source workbook. The complete bundle includes all 15 source workbooks, processed evidence tables, metadata and R code."),
        h4("Scope"),
        p("The app does not refit MOFA+, select a new signature or generate participant-level predictions."))))
)

server <- function(input, output, session) {
  current <- reactive({
    i <- match(as.integer(input$stage_id), TRAIL$stage_id)
    validate(need(!is.na(i), "Story step not found"))
    TRAIL[i, , drop = FALSE]
  })

  observeEvent(input$previous_stage, {
    updateSelectInput(session, "stage_id",
                      selected = max(1L, as.integer(input$stage_id) - 1L))
  })
  observeEvent(input$next_stage, {
    updateSelectInput(session, "stage_id",
                      selected = min(nrow(TRAIL), as.integer(input$stage_id) + 1L))
  })
  observeEvent(input$continue_stage, {
    updateSelectInput(session, "stage_id",
                      selected = min(nrow(TRAIL), as.integer(input$stage_id) + 1L))
    session$sendCustomMessage("scrollTop", list())
  })

  lapply(seq_len(nrow(CHAPTERS)), function(i) {
    observeEvent(input[[paste0("chapter_", i)]], {
      updateSelectInput(session, "stage_id", selected = CHAPTERS$start[i])
    })
  })

  output$chapter_nav <- renderUI({
    id <- as.integer(input$stage_id)
    active <- which(id >= CHAPTERS$start & id <= CHAPTERS$end)
    div(class = "chapter-map", lapply(seq_len(nrow(CHAPTERS)), function(i) {
      actionLink(paste0("chapter_", i), CHAPTERS$label[i],
                 class = paste("chapter-link", if (i == active) "active" else ""))
    }))
  })

  plotted_data <- reactive({
    id <- as.integer(input$stage_id)
    if (id == 1) {
      cols <- setdiff(names(RNA_COMPOSITION), c("sample", "group"))
      z <- stack(RNA_COMPOSITION[cols])
      z$group <- rep(RNA_COMPOSITION$group, times = length(cols))
      names(z)[1:2] <- c("percent", "biotype")
      out <- aggregate(percent ~ biotype + group, z, median, na.rm = TRUE)
      out$biotype <- as.character(out$biotype)
      rank <- aggregate(percent ~ biotype, out, mean)
      keep <- head(rank$biotype[order(-rank$percent)], 7)
      out$display <- ifelse(out$biotype %in% keep, out$biotype, "Other low-abundance RNA")
      return(aggregate(percent ~ display + group, out, sum))
    }
    r <- current()
    if (id == 2) {
      d <- read_source_panel(r$workbook, r$sheet)
      d$percent_displayed <- as.numeric(d$percent_displayed)
      return(d[is.finite(d$percent_displayed), ])
    }
    if (id == 3) {
      d <- read_source_panel(r$workbook, r$sheet)
      d$signed_neglog10_p <- as.numeric(d$signed_neglog10_p)
      d <- d[is.finite(d$signed_neglog10_p) & !is.na(d$metric_label) & !is.na(d$association), ]
      d <- d[order(-abs(d$signed_neglog10_p)), ]
      return(head(d[!duplicated(paste(d$metric_label, d$association)), ], 24))
    }
    if (id == 4) {
      d <- read_source_panel(r$workbook, r$sheet)
      d <- d[is.finite(as.numeric(d$motifs_displayed)), ]
      cols <- c("mofa_input_features", "prevalent_union", "motifs_displayed")
      z <- stack(lapply(d[cols], as.numeric)); z$omic <- rep(d$omic, times = length(cols))
      names(z)[1:2] <- c("features", "stage")
      z$stage <- factor(z$stage, levels = cols,
                        labels = c("MOFA input", "Prevalent universe", "Displayed motifs"))
      return(z)
    }
    if (id == 5) {
      d <- read_source_panel(r$workbook, r$sheet)
      d$molecules <- as.numeric(d$molecules)
      d <- d[is.finite(d$molecules) & d$molecules > 0, ]
      totals <- aggregate(molecules ~ junction_id, d, sum)
      keep_rows <- head(totals$junction_id[order(-totals$molecules)], 24)
      keep_cols <- head(names(sort(table(d$participant_id[d$junction_id %in% keep_rows]), decreasing = TRUE)), 36)
      d <- d[d$junction_id %in% keep_rows & d$participant_id %in% keep_cols, ]
      d$junction_label <- substr(d$junction_id,
                                 pmax(1, nchar(d$junction_id) - 19), nchar(d$junction_id))
      return(aggregate(molecules ~ junction_label + participant_id, d, sum))
    }
    if (id == 6) return(FACTOR_SCORES)
    if (id == 7) {
      d <- read_source_panel(r$workbook, r$sheet)
      d$signed_neglog10_p <- as.numeric(d$signed_neglog10_p)
      d <- d[is.finite(d$signed_neglog10_p) & !is.na(d$feature) & !is.na(d$metadata), ]
      score <- aggregate(abs(d$signed_neglog10_p), list(feature = d$feature), max)
      keep <- head(score$feature[order(-score$x)], 18)
      return(d[d$feature %in% keep, ])
    }
    if (id == 8) {
      d <- read_source_panel(r$workbook, r$sheet)
      cols <- c("pct_supp", "pct_unexp", "pct_coact", "pct_codn")
      z <- stack(lapply(d[cols], as.numeric))
      z$survival_tertile <- rep(d$survival_tertile, times = length(cols))
      names(z)[1:2] <- c("percent", "state")
      z$state <- factor(z$state, levels = cols,
                        labels = c("Suppression", "Unexpected", "Co-activation", "Co-down"))
      z$survival_tertile <- factor(z$survival_tertile,
                                   levels = c("Short", "Medium", "Long"))
      return(aggregate(percent ~ state + survival_tertile, z, mean, na.rm = TRUE))
    }
    if (id == 9) return(CANDIDATES)
    if (id == 10) return(read_source_panel(r$workbook, r$sheet))
    if (id == 11) {
      d <- read_source_panel(r$workbook, r$sheet)
      d$horizon <- as.numeric(d$horizon); d$auc_mean <- as.numeric(d$auc_mean)
      d <- d[is.finite(d$horizon) & is.finite(d$auc_mean), ]
      return(aggregate(auc_mean ~ fs + horizon, d, mean, na.rm = TRUE))
    }
    if (id == 12) return(TISSUE_BRIDGE)
    if (id == 13) return(read_source_panel(r$workbook, r$sheet))
    if (id == 14) {
      d <- read_source_panel(r$workbook, r$sheet)
      d$score_z <- as.numeric(d$score_z)
      z <- aggregate(score_z ~ display_label + cell_type, d, mean, na.rm = TRUE)
      score <- aggregate(abs(z$score_z), list(program = z$display_label), max)
      keep <- head(score$program[order(-score$x)], 20)
      return(z[z$display_label %in% keep, ])
    }
    d <- read_source_panel(r$workbook, r$sheet)
    d$n_terms <- as.numeric(d$n_terms)
    d$neg_log10_fdr <- as.numeric(d$neg_log10_fdr)
    d <- d[is.finite(d$n_terms) & is.finite(d$neg_log10_fdr) & !is.na(d$module),
           c("module", "n_terms", "neg_log10_fdr", "direction")]
    d <- d[!duplicated(d$module), ]
    names(d)[names(d) == "n_terms"] <- "pathways"
    d
  })

  story_plot_object <- reactive({
    id <- as.integer(input$stage_id)
    d <- plotted_data()
    width <- session$clientData$output_story_plot_width
    width <- if (is.null(width) || !is.finite(width)) 1000 else width
    teal <- "#2B6F73"; red <- "#B33B52"; blue <- "#4676A9"; orange <- "#DB8A35"

    if (id == 1) {
      d$display <- reorder(wrap_text(d$display, if (width < 620) 15 else 24), d$percent)
      return(ggplot(d, aes(display, percent, fill = group)) +
        geom_col(position = "dodge", width = .72) + coord_flip() +
        scale_fill_manual(values = c(ALS = red, Control = blue)) +
        labs(x = NULL, y = "Median RNA composition (%)", fill = NULL) +
        theme_story(width))
    }
    if (id == 2) {
      d$marker_combination <- reorder(wrap_text(d$marker_combination, 18), d$percent_displayed)
      return(ggplot(d, aes(marker_combination, percent_displayed)) +
        geom_col(fill = teal, width = .68) + coord_flip() +
        geom_text(aes(label = paste0(round(percent_displayed), "%")), hjust = -.12,
                  size = if (width < 620) 3.5 else 4.2) +
        expand_limits(y = max(d$percent_displayed) * 1.22) +
        labs(x = NULL, y = "Localized particles (%)") +
        theme_story(width))
    }
    if (id == 3) {
      d$metric <- factor(wrap_text(d$metric_label, if (width < 620) 16 else 25))
      return(ggplot(d, aes(association, metric, fill = signed_neglog10_p)) +
        geom_tile(colour = "white", linewidth = .5) +
        scale_fill_gradient2(low = blue, mid = "#f7f5ef", high = red, midpoint = 0,
                             name = "Signed\nevidence") +
        labs(x = NULL, y = NULL) + theme_story(width) +
        theme(axis.text.x = element_text(angle = if (width < 620) 90 else 35,
                                         hjust = 1, vjust = if (width < 620) .5 else 1,
                                         size = if (width < 620) 7.5 else rel(1))))
    }
    if (id == 4) {
      return(ggplot(d, aes(stage, features, fill = omic)) +
        geom_col(position = "dodge", width = .66) +
        geom_text(aes(label = format(features, big.mark = ",")),
                  position = position_dodge(width = .66), vjust = -.25,
                  size = if (width < 620) 3.2 else 4) +
        scale_y_log10() + scale_fill_manual(values = c(miRNA = orange, mRNA = teal)) +
        labs(x = NULL, y = "Features or motifs (log scale)", fill = NULL) + theme_story(width))
    }
    if (id == 5) {
      d$junction_label <- factor(d$junction_label, levels = rev(unique(d$junction_label)))
      return(ggplot(d, aes(participant_id, junction_label, fill = molecules)) +
        geom_tile() + scale_fill_gradient(low = "#edf3f2", high = red, name = "Molecules") +
        labs(x = "Discovery participants", y = "Recurrent junction") + theme_story(width) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.text.y = element_text(size = if (width < 620) 7.5 else 9.5)))
    }
    if (id == 6) {
      rho <- cor(d$mRNA_factor1, d$miRNA_factor1, use = "complete.obs")
      return(ggplot(d, aes(mRNA_factor1, miRNA_factor1, colour = survival_time_months)) +
        geom_hline(yintercept = 0, colour = "#c5cac7") + geom_vline(xintercept = 0, colour = "#c5cac7") +
        geom_smooth(method = "lm", se = TRUE, colour = red, fill = "#ead1d5") +
        geom_point(size = if (width < 620) 2.3 else 3, alpha = .82) +
        scale_colour_gradient(low = orange, high = teal) +
        labs(x = "mRNA Factor 1 score", y = "miRNA Factor 1 score", colour = "Survival\n(months)",
             caption = sprintf("Matched participants: Pearson r = %.3f", rho)) +
        theme_story(width) + theme(plot.caption = element_text(face = "bold")))
    }
    if (id == 7) {
      d$feature_label <- factor(short_name(d$feature))
      return(ggplot(d, aes(metadata, feature_label, fill = signed_neglog10_p)) +
        geom_tile(colour = "white", linewidth = .4) +
        scale_fill_gradient2(low = blue, mid = "#f7f5ef", high = red, midpoint = 0,
                             name = "Signed\nevidence") +
        labs(x = NULL, y = NULL) + theme_story(width) +
        theme(axis.text.x = element_text(angle = if (width < 620) 90 else 35,
                                         hjust = 1, vjust = if (width < 620) .5 else 1,
                                         size = if (width < 620) 7.5 else rel(1)),
              axis.text.y = element_text(size = if (width < 620) 7.5 else 9.5)))
    }
    if (id == 8) {
      return(ggplot(d, aes(survival_tertile, percent, fill = state)) +
        geom_col(width = .72) + scale_fill_manual(values = c(red, "#c6cbc8", orange, blue)) +
        labs(x = "Survival tertile", y = "Mean pair-state burden (%)", fill = NULL) + theme_story(width))
    }
    if (id == 9) {
      d$locked <- d$`Final five-miRNA panel` == "Yes"
      d$label <- ifelse(d$locked, short_name(d$miRNA), "")
      return(ggplot(d, aes(`Scaled sEV effect`, `Scaled plasma effect`, colour = locked)) +
        geom_abline(linetype = 2, colour = "#9ba3a0") +
        geom_point(size = ifelse(d$locked, 4, 2.6), alpha = .85) +
        geom_text(aes(label = label), nudge_y = .045, check_overlap = TRUE,
                  size = if (width < 620) 3.1 else 3.7, show.legend = FALSE) +
        scale_colour_manual(values = c(`FALSE` = "#b8bdbb", `TRUE` = red),
                            labels = c("Other 14", "Final five")) +
        expand_limits(x = 1.18, y = 1.15) +
        labs(x = "Scaled sEV effect",
             y = "Scaled total-plasma effect", colour = NULL) + theme_story(width))
    }
    if (id == 10) {
      d$label <- factor(d$label, levels = d$label[order(d$rho_tmm)])
      d$sign_flip <- d$sign_flip == "True"
      return(ggplot(d, aes(label, rho_tmm, xend = label, yend = rho_combat,
                           colour = sign_flip)) +
        geom_hline(yintercept = 0, linetype = 2, colour = "#8f9693") +
        geom_segment(linewidth = .85) + geom_point(aes(y = rho_tmm), shape = 16, size = 2.6) +
        geom_point(aes(y = rho_combat), shape = 17, size = 2.6) + coord_flip() +
        scale_colour_manual(values = c(`FALSE` = teal, `TRUE` = red),
                            labels = c("Direction retained", "Direction changed")) +
        labs(x = NULL, y = "Survival correlation", colour = NULL) + theme_story(width) +
        guides(colour = guide_legend(ncol = if (width < 620) 1 else 2)))
    }
    if (id == 11) {
      d$fs <- wrap_text(d$fs, if (width < 620) 14 else 24)
      return(ggplot(d, aes(horizon, auc_mean, colour = fs)) +
        geom_hline(yintercept = .5, linetype = 2, colour = "#9ea5a2") +
        geom_line(linewidth = 1) + geom_point(size = 2.4) +
        scale_colour_manual(values = rep(c(teal, red, blue, orange, "#8064A2", "#6A8E3A"),
                                         length.out = length(unique(d$fs)))) +
        labs(x = "Prediction horizon (months)", y = "Mean AUROC", colour = NULL) +
        theme_story(width) + guides(colour = guide_legend(ncol = if (width < 620) 1 else 2)))
    }
    if (id == 12) {
      d$core <- d$`Primary five-gene core` == "Yes"
      d$label <- ifelse(d$core, d$Gene, "")
      return(ggplot(d, aes(`Tissue-bridge score`, `sEV survival-support score`, colour = core)) +
        geom_point(size = ifelse(d$core, 4.2, 2.6), alpha = .85) +
        geom_text(aes(label = label), nudge_y = .035, check_overlap = TRUE,
                  size = if (width < 620) 3.2 else 3.8, show.legend = FALSE) +
        scale_colour_manual(values = c(`FALSE` = "#b8bdbb", `TRUE` = red),
                            labels = c("Other 11", "Five-gene core")) +
        expand_limits(y = max(d$`sEV survival-support score`) * 1.14) +
        labs(x = "External tissue support", y = "sEV survival support", colour = NULL) +
        theme_story(width))
    }
    if (id == 13) {
      d$auc_mean_across_tissue <- as.numeric(d$auc_mean_across_tissue)
      d$auc_min <- as.numeric(d$auc_min); d$auc_max <- as.numeric(d$auc_max)
      d$context <- paste(d$dataset, d$comparison, d$subgroup, sep = " · ")
      d$context <- reorder(wrap_text(d$context, if (width < 620) 23 else 38), d$auc_mean_across_tissue)
      return(ggplot(d, aes(context, auc_mean_across_tissue, colour = dataset)) +
        geom_errorbar(aes(ymin = auc_min, ymax = auc_max), width = .16) +
        geom_point(size = 3) + coord_flip() +
        scale_colour_manual(values = c(red, teal),
                            labels = c("Bulk RNA", "Single-nucleus RNA")) +
        labs(x = NULL, y = "Median AUROC", colour = NULL) + theme_story(width) +
        guides(colour = guide_legend(ncol = if (width < 620) 1 else 2)))
    }
    if (id == 14) {
      d$program <- factor(wrap_text(d$display_label, if (width < 620) 15 else 24))
      return(ggplot(d, aes(cell_type, program, fill = score_z)) + geom_tile() +
        scale_fill_gradient2(low = blue, mid = "#f7f5ef", high = red, midpoint = 0,
                             name = "Mean score") +
        labs(x = "Cell class", y = "State program") + theme_story(width) +
        theme(axis.text.x = element_text(angle = if (width < 620) 90 else 40,
                                         hjust = 1, vjust = if (width < 620) .5 else 1,
                                         size = if (width < 620) 7.5 else rel(1)),
              axis.text.y = element_text(size = if (width < 620) 7.3 else 9.5)))
    }
    d$evidence <- d$neg_log10_fdr
    d$label <- reorder(wrap_text(d$module, if (width < 620) 18 else 30), d$evidence)
    ggplot(d, aes(label, evidence, fill = direction)) + geom_col(width = .68) +
      coord_flip() + geom_text(aes(label = paste0("n=", pathways)), hjust = -.12,
                               size = if (width < 620) 3.4 else 4) +
      expand_limits(y = max(d$evidence) * 1.30) +
      scale_fill_manual(values = c("ALS gain" = red, "ALS loss" = blue)) +
      labs(x = NULL, y = expression(Best~-log[10]~FDR), fill = NULL) + theme_story(width)
  })

  output$stage_header <- renderUI({
    r <- current(); id <- as.integer(r$stage_id)
    tagList(
      div(class = "step-line", span(sprintf("Step %d of %d", id, nrow(TRAIL))),
          div(class = "step-track", div(class = "step-fill",
              style = sprintf("width: %.1f%%", 100 * id / nrow(TRAIL))))),
      span(class = paste("figure-badge", if (r$group == "Extended Data") "extended" else "main"),
           r$figure_label),
      h3(paste0(id, ". ", r$stage)),
      p(class = "stage-question", r$question),
      div(class = "guide-copy", span(class = "text-label", "Manuscript guide"),
          MANUSCRIPT_GUIDE[id]))
  })

  output$stage_takeaway <- renderUI({
    r <- current(); id <- as.integer(r$stage_id)
    tagList(
      h3(class = "takeaway-title", "Interpret this checkpoint"),
      div(class = "conclusion", span(class = "text-label", "Finding in this view"),
          r$conclusion),
      div(class = "claim-support", span(class = "text-label", "What the evidence supports"),
          CLAIM_SUPPORT[id])
    )
  })

  output$continue_card <- renderUI({
    id <- as.integer(input$stage_id)
    if (id >= nrow(TRAIL)) {
      return(div(class = "continue-card",
                 div(class = "next-copy", strong("Story complete"), br(),
                     "The complete source and audit trail is available in Downloads and Audit and agents.")))
    }
    nxt <- TRAIL[id + 1L, ]
    div(class = "continue-card",
        div(class = "next-copy", span(class = "text-label", style = "color:#c8dcda",
                                      "Next evidence checkpoint"),
            strong(paste0(nxt$figure_label, ": ", nxt$stage))),
        actionButton("continue_stage", "Continue the story →"))
  })

  output$graph_note <- renderUI({
    id <- as.integer(input$stage_id)
    div(class = "graph-note", span(class = "text-label", "How to read the graph"),
        READ_CUES[id])
  })

  output$stage_actions <- renderUI({
    r <- current()
    workbook <- paste0("all-source-data/", URLencode(r$workbook, reserved = TRUE))
    div(class = "button-row",
      downloadButton("download_story_data", "Download this graph's data"),
      downloadButton("download_story_plot", "Download this graph"),
      tags$a(class = "btn btn-default", href = workbook, download = NA,
             "Download source workbook"))
  })

  output$story_plot <- renderPlot(print(story_plot_object()), res = 120)
  output$download_story_data <- downloadHandler(
    filename = function() paste0("ALS_data_story_step_", input$stage_id, ".csv"),
    content = function(file) write.csv(plotted_data(), file, row.names = FALSE, na = ""))
  output$download_story_plot <- downloadHandler(
    filename = function() paste0("ALS_data_story_step_", input$stage_id, ".png"),
    content = function(file) ggsave(file, story_plot_object(), width = 12,
                                    height = 7.5, dpi = 240, bg = "white"))

  output$download_agent_story <- downloadHandler(
    filename = "ALS_complete_story_and_claim_trail.csv",
    content = function(file) {
      out <- TRAIL
      out$manuscript_guide <- MANUSCRIPT_GUIDE
      out$how_to_read_graph <- READ_CUES
      out$claim_support_boundary <- CLAIM_SUPPORT
      write.csv(out, file, row.names = FALSE, na = "")
    })

  output$download_complete_bundle <- downloadHandler(
    filename = "ALS_paired_RNA_reproducibility_bundle.zip",
    content = function(file) {
      bundle_root <- file.path(tempdir(), paste0("als_bundle_", Sys.getpid(), "_", as.integer(Sys.time())))
      dir.create(bundle_root, recursive = TRUE, showWarnings = FALSE)
      for (folder in c("source_data", "processed_data", "R_code", "metadata"))
        dir.create(file.path(bundle_root, folder), recursive = TRUE)
      file.copy(file.path(FIGURE_SOURCE_ROOT, FIGURE_CATALOG$workbook),
                file.path(bundle_root, "source_data"), overwrite = TRUE)
      processed <- list.files("data-processed", recursive = TRUE, full.names = TRUE)
      processed <- processed[!dir.exists(processed)]
      for (src in processed) {
        rel <- sub("^data-processed/", "", src)
        dst <- file.path(bundle_root, "processed_data", rel)
        dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
        file.copy(src, dst, overwrite = TRUE)
      }
      code_files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE),
                      list.files("www/reproducibility", pattern = "\\.R$", full.names = TRUE))
      file.copy(code_files, file.path(bundle_root, "R_code"), overwrite = TRUE)
      file.copy(c("config/figure_catalog.csv", "config/evidence_trail.csv",
                  "config/story_steps.csv", "config/evidence_manifest.json", "README.md"),
                file.path(bundle_root, "metadata"), overwrite = TRUE)
      old_dir <- setwd(bundle_root); on.exit(setwd(old_dir), add = TRUE)
      utils::zip(zipfile = file, files = list.files(".", recursive = TRUE), flags = "-r9X")
    })

  output$all_downloads <- renderUI({
    workbook_cards <- function(group) {
      d <- FIGURE_CATALOG[FIGURE_CATALOG$group == group, , drop = FALSE]
      div(class = "download-grid", lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]; workbook <- paste0("all-source-data/", URLencode(r$workbook, reserved = TRUE))
        div(class = "card", h4(r$label), tags$a(class = "btn btn-default",
            href = workbook, download = NA, "Download source workbook"))
      }))
    }
    tagList(
      div(class = "download-lead", h3("Download the complete reproducibility bundle"),
          p("Includes all 15 source-data workbooks, processed evidence tables, metadata and R code."),
          downloadButton("download_complete_bundle", "Download complete bundle", class = "btn-primary")),
      h3("Main figures"), workbook_cards("Main figure"),
      h3("Extended Data figures"), workbook_cards("Extended Data"),
      div(class = "card", h4("For independent review"),
          p("Technical checks are available here, but are deliberately kept out of the story."),
          tags$a(class = "btn btn-default", href = "machine/evidence_manifest.json", download = NA,
                 "Evidence manifest"), " ",
          tags$a(class = "btn btn-default", href = "r-code/verify.R", download = NA,
                 "Numerical checks")))
  })

  output$current_audit <- renderTable({
    r <- current()
    data.frame(
      Field = c("Story step", "Figure", "Scientific question", "Manuscript guide",
                "Finding", "Claim support boundary", "Source workbook", "Worksheet",
                "Plotted rows"),
      Value = c(r$stage_id, r$figure_label, r$question,
                MANUSCRIPT_GUIDE[as.integer(r$stage_id)], r$conclusion,
                CLAIM_SUPPORT[as.integer(r$stage_id)], r$workbook, r$sheet,
                nrow(plotted_data())),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s", sanitize.text.function = identity)
}

shinyApp(ui, server)
