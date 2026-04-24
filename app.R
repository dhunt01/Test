## ------------------------------------------------------------------
## TLF Generator POC
##
## Shiny application that allows a user to:
##   1. Use a built-in fake ADVS dataset, or upload their own ADaM file
##   2. Choose to build a Table, Listing, or Figure
##   3. Customize the output and download the result
## ------------------------------------------------------------------

library(shiny)
library(dplyr)
library(readr)
library(haven)
library(DT)
library(ggplot2)

source("R/generate_advs.R")
source("R/tlf_functions.R")

advs_demo <- generate_advs()

## -------- UI ----------------------------------------------------------

ui <- fluidPage(
  titlePanel("ADaM TLF Generator (Proof of Concept)"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Data source"),
      radioButtons(
        "data_source", NULL,
        choices = c("Use demo ADVS data" = "demo",
                    "Upload ADaM dataset" = "upload"),
        selected = "demo"
      ),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Upload file (.csv, .sas7bdat, .xpt, .rds)",
                  accept = c(".csv", ".sas7bdat", ".xpt", ".rds"))
      ),

      hr(),
      h4("2. Output type"),
      radioButtons(
        "output_type", NULL,
        choices = c("Table" = "table",
                    "Listing" = "listing",
                    "Figure" = "figure"),
        inline = TRUE
      ),

      hr(),
      h4("3. Customize"),
      uiOutput("param_ui"),
      uiOutput("visits_ui"),
      uiOutput("group_ui"),

      ## Table-specific
      conditionalPanel(
        condition = "input.output_type == 'table'",
        checkboxGroupInput(
          "stats", "Statistics",
          choices  = c("n", "mean", "sd", "median", "min", "max"),
          selected = c("n", "mean", "sd", "median", "min", "max")
        )
      ),

      ## Listing-specific
      conditionalPanel(
        condition = "input.output_type == 'listing'",
        uiOutput("listing_cols_ui"),
        uiOutput("listing_trt_ui")
      ),

      ## Figure-specific
      conditionalPanel(
        condition = "input.output_type == 'figure'",
        selectInput(
          "plot_type", "Figure type",
          choices = c("Mean profile"          = "mean_profile",
                      "Box plot"              = "boxplot",
                      "Spaghetti plot"        = "spaghetti",
                      "Change from baseline"  = "change_from_baseline")
        )
      ),

      hr(),
      h4("4. Download"),
      conditionalPanel(
        condition = "input.output_type != 'figure'",
        downloadButton("dl_csv", "Download CSV")
      ),
      conditionalPanel(
        condition = "input.output_type == 'figure'",
        downloadButton("dl_png", "Download PNG")
      )
    ),

    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Output",
          br(),
          conditionalPanel(
            condition = "input.output_type == 'figure'",
            plotOutput("figure", height = "520px")
          ),
          conditionalPanel(
            condition = "input.output_type != 'figure'",
            DT::DTOutput("tbl")
          )
        ),
        tabPanel(
          "Source data preview",
          br(),
          DT::DTOutput("raw_preview")
        )
      )
    )
  )
)

## -------- Server ------------------------------------------------------

server <- function(input, output, session) {

  data_in <- reactive({
    if (input$data_source == "demo") {
      return(advs_demo)
    }
    req(input$file)
    path <- input$file$datapath
    ext  <- tools::file_ext(path)
    switch(
      tolower(ext),
      csv       = readr::read_csv(path, show_col_types = FALSE),
      sas7bdat  = haven::read_sas(path),
      xpt       = haven::read_xpt(path),
      rds       = readRDS(path),
      validate(need(FALSE, "Unsupported file type."))
    )
  })

  ## ---- Dynamic UI pieces ----

  output$param_ui <- renderUI({
    df <- data_in()
    validate(need("PARAMCD" %in% names(df),
                  "Dataset is missing PARAMCD - is this an ADaM BDS dataset?"))
    choices <- unique(df$PARAMCD)
    selectInput("param_cd", "Parameter (PARAMCD)", choices = choices,
                selected = choices[1])
  })

  output$visits_ui <- renderUI({
    df <- data_in()
    if (!"AVISIT" %in% names(df)) return(NULL)
    visits <- unique(df$AVISIT[order(df$AVISITN)])
    checkboxGroupInput("visits", "Visits", choices = visits, selected = visits)
  })

  output$group_ui <- renderUI({
    df <- data_in()
    candidates <- intersect(c("TRT01P", "TRT01A", "SEX", "RACE"), names(df))
    if (length(candidates) == 0) return(NULL)
    selectInput("group_var", "Grouping variable", choices = candidates,
                selected = candidates[1])
  })

  output$listing_cols_ui <- renderUI({
    df <- data_in()
    default <- intersect(
      c("USUBJID", "TRT01P", "AVISIT", "ADT", "AVAL", "AVALU", "BASE", "CHG"),
      names(df)
    )
    checkboxGroupInput("listing_cols", "Columns",
                       choices = names(df), selected = default)
  })

  output$listing_trt_ui <- renderUI({
    df <- data_in()
    if (!"TRT01P" %in% names(df)) return(NULL)
    trts <- unique(df$TRT01P)
    checkboxGroupInput("listing_trts", "Treatments",
                       choices = trts, selected = trts)
  })

  ## ---- Output building ----

  tbl_data <- reactive({
    req(input$param_cd)
    df <- data_in()

    if (input$output_type == "table") {
      req(input$group_var)
      build_summary_table(
        df,
        param_cd   = input$param_cd,
        group_var  = input$group_var,
        visits     = input$visits,
        statistics = input$stats
      )
    } else if (input$output_type == "listing") {
      req(input$listing_cols)
      build_listing(
        df,
        param_cd   = input$param_cd,
        columns    = input$listing_cols,
        visits     = input$visits,
        treatments = input$listing_trts
      )
    }
  })

  fig_plot <- reactive({
    req(input$param_cd, input$plot_type, input$group_var)
    build_figure(
      data_in(),
      param_cd  = input$param_cd,
      plot_type = input$plot_type,
      group_var = input$group_var,
      visits    = input$visits
    )
  })

  output$tbl <- DT::renderDT({
    req(input$output_type != "figure")
    DT::datatable(
      tbl_data(),
      rownames = FALSE,
      options  = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$figure <- renderPlot({
    fig_plot()
  })

  output$raw_preview <- DT::renderDT({
    DT::datatable(
      data_in(),
      rownames = FALSE,
      options  = list(pageLength = 10, scrollX = TRUE)
    )
  })

  ## ---- Downloads ----

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0(input$output_type, "_", input$param_cd, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      readr::write_csv(tbl_data(), file)
    }
  )

  output$dl_png <- downloadHandler(
    filename = function() {
      paste0("figure_", input$param_cd, "_", input$plot_type, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = fig_plot(),
                      width = 10, height = 6, dpi = 200)
    }
  )
}

shinyApp(ui, server)
