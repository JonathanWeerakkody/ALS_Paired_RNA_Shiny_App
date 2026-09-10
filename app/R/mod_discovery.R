# Tab 1 — Discovery ladder.
# A reader should understand the whole analysis in about thirty seconds.
# Each rung is clickable and reports what entered, what rule applied, what
# survived, and where the corresponding result appears in the paper.

discovery_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5,
        h4("Analysis path"),
        p(class = "muted",
          "Click a stage to see the rule that was applied and how many features survived it."),
        uiOutput(ns("ladder"))
      ),
      column(7,
        h4(textOutput(ns("stage_title"))),
        uiOutput(ns("stage_detail")),
        hr(),
        tableOutput(ns("stage_table"))
      )
    )
  )
}

discovery_server <- function(id, paper) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    stages <- list(
      list(key = "cohort",  label = "45 ALS + 15 controls",
           rule = "Paired small-RNA and mRNA sequencing of GLAST/SLC1A3-captured plasma sEV from the same preparation.",
           n = "60 participants", panel = "Fig. 1a-h, Table 1"),
      list(key = "mofa",    label = "MOFA+ inputs",
           rule = "187 miRNAs and the 374 most variable mRNAs, with survival entered as a third view. Twelve factors.",
           n = "561 features, 3 views", panel = "Fig. 2a, Supplementary Table S6"),
      list(key = "factor",  label = "Outcome-guided Factor 1",
           rule = paste("Carries 0.18% of feature-weighted variance across the two molecular views.",
                        "Because survival was a model view, the factor-survival association is a",
                        "prioritization property, not independent evidence."),
           n = "0.18% of variance", panel = "Fig. 2a,b, Supplementary Tables S6, S7"),
      list(key = "cands",   label = "19 miRNAs + 16 mRNAs",
           rule = paste("Four conditions simultaneously: absolute Factor 1 loading in the top 50; nominal",
                        "ALS-versus-control effect; long-versus-short survival effect; directional consistency",
                        "across RNA layers."),
           n = "35 features", panel = "Fig. 2e-g, Supplementary Table S3"),
      list(key = "pairs",   label = "304 possible pairs",
           rule = "The complete 19 x 16 cross-product, retained in full including unsupported cells.",
           n = "304 pairs", panel = "Extended Data Fig. 6f, Supplementary Table S8"),
      list(key = "support", label = "119 prior-supported pairs",
           rule = "Canonical seed match, a scored target-support record, or both. Prior evidence, not validation here.",
           n = "119 of 304", panel = "Fig. 2h,i, Supplementary Table S8"),
      list(key = "plasma",  label = "Five-miRNA plasma representation",
           rule = paste("Composite of the scaled bootstrap effect in each compartment and the cross-compartment",
                        "direction concordance. Plasma Cox effects contribute, so this is transfer and",
                        "refinement rather than untouched validation."),
           n = "5 of 19", panel = "Fig. 3c-k, Supplementary Tables S4, S12"),
      list(key = "tissue",  label = "Five-mRNA cortical bridge",
           rule = "Prespecified conjunction of the sEV-survival and tissue-bridge axes, both above their medians.",
           n = "5 of 16", panel = "Fig. 4c, Supplementary Table S14"),
      list(key = "cortex",  label = "Human cortical state mapping",
           rule = "Donor-level pseudobulk association with 25 lineage-gated state programs. Donor, not nucleus, is the inferential unit.",
           n = "69 donors, 527,261 nuclei", panel = "Fig. 4e-h, Supplementary Tables S15, S16")
    )

    sel <- reactiveVal("cohort")
    observe({
      lapply(stages, function(s)
        observeEvent(input[[paste0("go_", s$key)]], sel(s$key), ignoreInit = TRUE))
    })

    output$ladder <- renderUI({
      tagList(lapply(seq_along(stages), function(i) {
        s <- stages[[i]]
        div(class = if (identical(sel(), s$key)) "rung rung-active" else "rung",
            actionLink(ns(paste0("go_", s$key)), s$label),
            span(class = "rung-n", s$n),
            if (i < length(stages)) div(class = "rung-arrow", HTML("&darr;")))
      }))
    })

    cur <- reactive(Filter(function(s) s$key == sel(), stages)[[1]])
    output$stage_title  <- renderText(cur()$label)
    output$stage_detail <- renderUI(tagList(
      p(strong("Rule applied. "), cur()$rule),
      p(strong("Surviving. "), cur()$n),
      p(class = "muted", strong("In the paper: "), cur()$panel)
    ))
    output$stage_table <- renderTable({
      d <- paper$stage_tables[[sel()]]
      if (is.null(d)) return(NULL)
      head(d, 12)
    }, striped = TRUE, spacing = "xs")
  })
}
