library(growthcurver)
library(shinydashboard)
library(shinyWidgets)
library(shiny) 
library(readxl) 
library(tools)
library(stringr) 
library(ggplot2)
library(rhandsontable) 
library(reshape2) 
library(dplyr)
library(lemon) 
library(scales) 
library(esquisse) 
library(gridExtra)
library(ggpubr)
library(parallel)
library(Cairo)
library(sortable)
library(shinythemes)
library(RColorBrewer)
source('utility.R', local = TRUE)

version <- "1.0"

##### UI #####
ui = dashboardPage(
  dashboardHeader(title = "TECAN Extractor"),
  dashboardSidebar(
    fileInput("files", "Choose TECAN Excel File",
              multiple = TRUE,
              accept = c(".xlsx", ".txt")),
    
    div(style="height:calc(100vh - 250px);",
        width=12,
        uiOutput("multifiles_options"),
        uiOutput("options"),
        uiOutput("downloads"),
    ),
    tags$hr(style='margin-bottom:5px'),
    column(
      width=12,
      align="center", 
      p('\u00A9 Julien D\u00E9n\u00E9r\u00E9az'),
      p(paste0('2021 - Version ', version))
    )
    
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    tags$style(HTML('.shiny-split-layout>div {overflow: visible;}')),
    fluidRow(
      column(
        width=12,
        box(
          height='calc(calc(100vh - 140px)/2)',
          width=2,
          title="Normalized data table Preview",
          status = 'primary',
          rHandsontableOutput("table_preview", height = 'calc(calc(100vh - 280px)/2)')
        ),
        box(
          height='calc(calc(100vh - 140px)/2)',
          width=5,
          title="Group design",
          status = 'primary',
          rHandsontableOutput("groups", height = 'calc(calc(100vh - 280px)/2)')
        ),
        box(
          width=5,
          height='calc(calc(100vh - 140px)/2)',
          title="Plot Options and Theme",
          status = 'primary',
          tags$style(HTML(".box {overflow-y:auto}")),
          uiOutput("graph_options")
        )
      ),
      column(
        width=12,
        tabBox(
          width=12,
          title="Plots",
          height = 'calc(calc(100vh - 110px)/2)',
          tabPanel(
            'Growth Plot', 
            plotOutput('plot')
          ),
          tabPanel(
            'AUC Plot',
            column(
              width=2,
              uiOutput("gro_opt")
            ),
            
            column(
              width=5,
              plotOutput('bar_graph')              
            ),
            column(
              width=5,
              plotOutput('tile_graph')
            )
          )
        )
      )
    )
  )
)
