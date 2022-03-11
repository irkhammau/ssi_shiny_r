library(shiny)
library(ECharts2Shiny)
library(DT)

ui <- fluidPage(
  br(),
  sidebarPanel(
    selectInput("dataset", "Select the dataset", choices = c("Surveillance SSI","Antibiotics","ASA Class","Surgical Wounds Class","Urgency of Operation")),
    uiOutput("yearmonth"),
    br(),
    helpText("Select the download format"),
    radioButtons("type", "Format type:",
                 choices =c("Excel (CSV)","Text (TSV)","Text (Space Separated)","Doc")),
    br(),
    helpText("Click on the download button to download the dataset observations"),
    downloadButton('downloadData','Download')
  ),
  mainPanel(
    br(),
    loadEChartsLibrary(),
    DT::dataTableOutput("mytable"),
    br(),
    tableOutput("table"),
    tags$div(id="linechart", style="width:100%;height:400px;"),
    deliverChart(div_id = "linechart"),
  )
  
)

