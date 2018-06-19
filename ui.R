library(shiny)
library(rmarkdownreportmanager)
library(DT)

source(file.path(".","config","config.R"))

tab_panel_arguments <- list(
  "title" = "R Markdown Report Manager",
  tabPanel(
    'Generate report',
    DT::dataTableOutput("report_table"),
    actionButton(inputId = "button_report_create", label = "Create new report"),
    uiOutput("UIcomponent_editReport"),
    uiOutput("UIcomponent_deleteReport"),
    fluidRow(
      column(
        6,
        "R message log",
        verbatimTextOutput("UIcomponent_reportMessage", placeholder = T)
      ),
      column(
        6,
        "R output log",
        verbatimTextOutput("UIcomponent_reportOutput", placeholder = T)
      )
    ),
    downloadButton("download_placeholder_report", "")
  ),
  tabPanel(
    'Datasets',
    DT::dataTableOutput("datasets_table"),
    actionButton(inputId = "button_dataset_create", label = "Create new dataset"),
    uiOutput("UIcomponent_editDataset"),
    uiOutput("UIcomponent_deleteDataset"),
    downloadButton("download_placeholder_dataset", "")
  ),
  tabPanel(
    'Report packages',
    DT::dataTableOutput("reportpackages_table"),
    actionButton(inputId = "button_reportpackage_create", label = "Create new report package"),
    uiOutput("UIcomponent_editReportpackage"),
    uiOutput("UIcomponent_deleteReportpackage"),
    downloadButton("download_placeholder_reportpackage", "")
  ),
  tabPanel(
    'Configuration',
    textAreaInput(
      inputId = "configuration",
      label = "Current contents of the config file:",
      value = readConfigfile(collapse = "\n",
                             directory = CONFIG_PATH)
    ),
    textOutput("config_eval"),
    actionButton(inputId = "action_update_config",
                 label = "Update config file")
  ),
  tabPanel('Help & about',
    withMathJax(includeMarkdown("README.md"))
  )
)
temp_rights <- loadRights(directory = CONFIG_PATH)
if (!granted("update_config", temp_rights)) {
  tab_panel_arguments[5] <- NULL
}
shinyUI(tagList(
  tags$head(
    tags$script(
      HTML(
        'Shiny.addCustomMessageHandler("start_download",
        function startDownload(obj) {
        window.location.href = $("#download_placeholder_"+obj.value).attr("href");
        });')
      ),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
  do.call(navbarPage, tab_panel_arguments)
  ))
