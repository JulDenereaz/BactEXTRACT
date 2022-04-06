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
library(shinyStore)
library(shinyBS)
source('utility.R', local = TRUE)
source('plot.R', local = TRUE)

version <- "0.1"


##### UI #####
ui = dashboardPage(
  dashboardHeader(title = "BactEXTRACT"),
  dashboardSidebar(
    fileInput("files", "Choose TECAN Excel File",
              multiple = TRUE,
              accept = c(".xlsx", ".txt")),
    
    initStore("localStorage", "BactEXTRACT_storage"),
    
    div(style="height:calc(100vh - 250px);",
        width=12,
        uiOutput("multifiles_options"),
        uiOutput("options")
    ),
    tags$hr(style='margin-bottom:5px'),
    column(
      width=12,
      align="center", 
      p('\u00A9 Julien D\u00E9n\u00E9r\u00E9az'),
      p(paste0('2022 - Version ', version))
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
          width=6,
          title="Group design",
          status = 'primary',
          rHandsontableOutput("groups", height = 'calc(calc(100vh - 280px)/2)')
        ),
        tabBox(
          width=6,
          height='calc(calc(100vh - 140px)/2)',
          tabPanel(
            'Settings', 
            div(style = 'overflow-y:auto;height:calc(calc(100vh - 260px)/2)', 
                uiOutput("graph_options")
            )
          ),
          tabPanel(
            'Downloads',
            uiOutput("downloads"),
            
          ),
          tabPanel(
            'Uploads',
            uiOutput("uploads"),
            
          )
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
              uiOutput("AUC_ui")
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







##### Server function #####
server <- function(input, output, session) {
  v <- reactiveValues()
  toListen2 <- reactive({
    list(input$norm,input$norm_baseOD, input$files)
  })
  
  
  observeEvent(toListen2(), {
    req(v$rawdata)
    #Called when normalization input is modified, or when new files is uploaded
    v$df[2:ncol(v$df)] = normalize(isolate(v$rawdata), isolate(input$norm), isolate(input$norm_baseOD))
  })
  
  observeEvent(input$files, {
    req(input$files)
    output$options <- renderUI({
      fluidPage(
        list(
          tags$hr(),
          h3('Options :'),
          # checkboxInput('RLU', 'RLU', value = F),
          selectInput('norm', 'OD Normalisation :', choices=c('Mininum', '1st value', 'Min(wells 1-2-3)', "No Normalisation")),
          selectInput('norm_baseOD', 'Base OD', choices=c(0, 0.001, 0.002, 0.003, 0.004), selected = 0.001),
          textInput('conditionsUI_Input', 'Enter Conditions:', value=paste(input$localStorage$cond, collapse = ","), placeholder = "Strain, Treatment, ..."),
          uiOutput('error_condition'),
          column(12, align="center", p('(Cannot start with number, separated by coma)')),     
          column(12, align="center", actionButton('updateCond', 'Update'))
          # bsTooltip("conditionsUI_Input", "Tooltip works", placement = "bottom", trigger = "hover", options = NULL)
        )
      )
    })
    if(!is.null(v$rawdata_list)) {
      lapply(react, function(reactVal){
        v[[reactVal]] <- NULL
      })
      output$graph_options <- NULL
      output$plot <- NULL
      output$downloads <- NULL
      output$groups <- NULL
    }
    
    tryCatch({
      rawdata_file_list <- do.call(list, lapply(input$files$datapath, function(file) {
        obj <- getFile(file)
        return(obj)
      }))
      names(rawdata_file_list) <- input$files$name
    },
    error = function(e) {
      stop(safeError(e))
    }
    )
    v$rawdata_list <- list()
    #rawdata_list contains the merged files sub-tables, time column in first, followed by any additional OD/luminescence/RLU etc.. tables
    v$timeScale <- rawdata_file_list[[1]]$time
    for (i in names(rawdata_file_list[[1]])[-1]) {
      v$rawdata_list[[i]] <- do.call(cbind, lapply(isolate(input$files$name), function(filename) {
        df <- isolate(rawdata_file_list[[filename]][[i]])
        if(length(input$files$name) > 1) {
          colnames(df) <- paste(tools::file_path_sans_ext(filename), colnames(df), sep="_")
        }
        return(df)
      }))
    }
    #Changing the name of the standard OD to OD. The rest will stay custom, as it can be RLU/Luminescence etc...
    names(v$rawdata_list)[1] <- "OD" 
    
  })
  
  ##### Update button Panel #####
  observeEvent(input$updateCond, {
    req(input$conditionsUI_Input)
    
    
    v$conditions <- formartConditions(input$conditionsUI_Input)
    
    
    #v$groups is a dataframe containing all levels for each condition for each well
    if(!is.null(v$groups)) {
      v$groups[colnames(data.frame(hot_to_r(input$groups)))] <- data.frame(hot_to_r(input$groups))
    }
    
    #add localstorage for groups, if same number of rows
    v$groups <- updateGroup(isolate(v$groups), v$conditions, colnames(v$rawdata_list$OD))
    
    v$interactions <- getInteractions(v$conditions)
    
    #normalization
    v$dataList <- isolate(v$rawdata_list)
    v$dataList$OD = normalize(isolate(v$dataList$OD), input$norm, input$norm_baseOD)
    updateStore(session, name = "cond", value = v$conditions)
    
    
    #SapLine to preview the growth curve of each well
    v$groups$Preview <- apply(v$dataList$OD, 2, function(x) jsonlite::toJSON(list(values=as.vector(x), options=list(type="line", spotRadius=0, chartRangeMin=0, chartRangeMax=1))))
    output$groups <- renderRHandsontable({
      req(v$groups)
      rhandsontable(
        data.frame(v$groups), 
        fillHandle = list(direction='vertical', autoInsertRow=FALSE),
        maxRows = ncol(isolate(v$dataList$OD)),
        useTypes=T) %>%
        hot_col(col="Wells", readOnly = T, allowRowEdit =F, allowColEdit = F) %>%
        hot_col(col="KeepWell", type="dropdown", source=c("Yes", "No"), strict=T, allowInvalid=F, valign='htCenter') %>%
        hot_col(col="Preview",copyable=F, renderer=htmlwidgets::JS("renderSparkline"), valign='htCenter', allowColEdit=F, readOnly=T) %>%
        hot_table(highlightCol = T, highlightRow = T, allowRowEdit =F)
    })
    #To add: from local storage update
    
    
    
    ##### UI graph options #####
    output$graph_options <- renderUI({
      fluidPage(
        list(
          splitLayout(
            column(
              width=12,
              selectInput('color', 'Color:', choices=c("None", v$conditions, v$interactions), selected = v$conditions[1], width='100%'),
              selectInput('linetype', 'Linetype:', choices=c("None", v$conditions), width='100%'),
              selectInput('shape', 'Shape:', choices=c("None", v$conditions), width='100%'),
              selectInput('grouping', 'Grouping:', choices=c("None", v$interactions), width='100%'),
              selectInput('fw', 'Facet Wrap:', choices=c("None", v$conditions, v$interactions), width='100%'),
              uiOutput("refCurveUI")
            ),
            column(
              width=12,
              selectInput('secondaryPlot', 'Additional Plot:', choices=c("None", names(v$dataList)[-1]), width='100%'),
              uiOutput("comparisonMethod"),
              selectInput('se', 'Standart Error Style:', choices=c("None", "Line Range", "Ribbon"), width='100%'),
              
              selectInput('lvlOrderSelect', "Order Levels:", choices = c(v$conditions), width='100%'),
              uiOutput("levelOrderUI", width='100%')
            )
          ),          
          tags$hr(),
          splitLayout(
            column(
              width=12,
              tags$label("Choose a Palette:"),
              palettePicker(
                width="100%",
                inputId = "pal", 
                label = NULL,
                choices = getPalette(8),
                textColor = c(
                  rep("white", 5), rep("black", 4) 
                )
              )
            ),
            textInput('customColorPalette', 'Custom Color Palette:', value=isolate(paste(input$localStorage$customP, collapse=",")), placeholder = "#4b123f, #cb13b2, ...")
          ),
          splitLayout(
            numericInput('height', 'Height (Inches):', value=5, min = 4, max = 50, step = 1),
            numericInput('width', 'Width: (Inches)', value=10, min = 4, max = 22, step = 1),
            checkboxInput('logScale', 'Log10 Transformation', value = T)
            
          ),
          splitLayout(
            sliderInput('range', 'X axis range:', min=0, step=0.5, max=ceiling(max(v$timeScale)), value=c(0, ceiling(max(v$timeScale)))),
            uiOutput("logScaleUI")
            
          ),
          splitLayout(
            sliderInput('size_l', 'Line Size:', min=0, max=4, value=1.2, step = 0.1),
            sliderInput('size_p', 'Point Size:', min=0, max=4, value=2.5, step = 0.1),
            sliderInput('size', 'Text size:', min=1, max=36, value=18)
          ),
          splitLayout(
            selectInput('theme', 'Theme:', choices=themes, width='100%'),
            textInput('title', "Title:", value=Sys.Date())
          ),
          column(
            12, 
            align="center",
            textAreaInput('customThemeUI', 'Add custom ggplot layers (separated by coma):', placeholder = "geom_vline(...), theme(...)")
          )
        )
      )
    })
  })
  
  observeEvent(input$secondaryPlot, {
    if(input$secondaryPlot != "None") {
      output$comparisonMethod <- renderUI({
        list(
          splitLayout(
            checkboxInput('secPlotMethod', paste(input$secondaryPlot, '/OD'), value = F, width='100%'),
            checkboxInput('secPlotDisplay', 'Comparison', value = T, width='100%')
            
          )
        )
      })
    }else {
      output$comparisonMethod <- NULL
    }
  })
  
  
  observeEvent(input$customColorPalette, {
    vec <- gsub(" ", "", unlist(strsplit(input$customColorPalette, ",")))
    
    if(isHex(vec)) {
      v$customP <- vec
      updateStore(session, name = "customP", value = v$customP)
      updatePalettePicker(inputId = "pal", choices = getPalette(8, vec))
    }
  })
  
  ##### Groups #####
  observeEvent(input$groups, {
    #This is called everytime the input$groups table gets modified by the user
    req(input$groups)
    
    v$groupsDF <- data.frame(hot_to_r(input$groups))
    v$groupsDF[-c(1,2,3)] <- lapply(v$groupsDF[-c(1,2,3)], factor)
    v$dataList_melted <- dataMelter(v$dataList, v$groupsDF, v$timeScale)
    if(is.null(v$dataList_melted)) {
      return()
    }
    
    
    ##### Downloads UI #####
    output$downloads <- renderUI({
      list(
        column(12, align="center",
               tags$hr(),
               tags$style(".skin-blue .sidebar a { color: #444; }"),
               downloadButton("downloaddf", label = "Download Full Data"),
               tags$p(""),
               downloadButton("downloadpdf", label = "Download Growth Plot (PDF)"),
               downloadButton("downloadeps", label = "Download Growth Plot (EPS)"),
               tags$hr(),
               downloadButton("downloadquc", label = "Download AUC Data"),
               tags$p(),
               downloadButton("downloadquc_plot", label = "Download AUC Plot")
        )
      )
    })
    output$downloaddf <- downloadHandler(
      filename <- function() {
        "dataframe.csv"
      },
      content <- function(file) {
        write.csv(v$dataList_melted, file, row.names = FALSE)
      }
    )
    output$downloadpdf <- downloadHandler(
      filename = function(){paste(input$title,'.pdf', sep='')},
      content = function(file){
        ggsave(file,plot=v$p, width=input$width, height=input$height, units="in", dpi=300, device=cairo_pdf)
      }
    )    
    output$downloadeps <- downloadHandler(
      filename = function(){paste(input$title,'.eps', sep='')},
      content = function(file){
        ggsave(file,plot=v$p, width=input$width, height=input$height, units="in", dpi=300, device=cairo_ps)
      }
    )
    
    
    
    ##### LevelOrder UI #####
    output$levelOrderUI <- renderUI({
      list(
        rank_list(
          text = paste(input$lvlOrderSelect, ' levels order:'),
          labels = isolate(levels(v$groupsDF[[input$lvlOrderSelect]])),
          input_id = "lvlOrderSorted",
          options = sortable_options(),
          class = "default-sortable"
        )
      )
    })
    observeEvent(input$fw, {
      #Only if facetWrap is select, and at least two different levels in that column
      if(input$fw != "None" && length(isolate(levels(v$dataList_melted[["OD"]][[input$fw]]))) > 1){
        output$refCurveUI <- renderUI({
          list(
            splitLayout(
              selectInput('referenceCurve', 'Ref. Curve:', choices = c("None", isolate(levels(v$dataList_melted[["OD"]][[input$fw]]))), width='100%'),
              numericInput('nRowsFacets', 'N. Rows:', value=1, min = 1, max = 10, step = 1)
            )
          )
        })
      }else {
        output$refCurveUI <- NULL
      }
    })
    
    
    
    ##### Log Scale UI #####
    observeEvent(input$logScale, {
      if(input$logScale) {
        output$logScaleUI <- renderUI({
          list(
            sliderTextInput('yAxisRange', 'Y axis range:', selected =c(0.001, 1), choices = c(0.001, 0.01, 0.1, 1, 10), grid=T)
          )
        })
      }else {
        output$logScaleUI <- renderUI({
          list(
            sliderInput('yAxisRange', 'Y axis range:', min=0, step=0.1, max=ceiling(max(v$dataList_melted$OD$value)), value=c(0, ceiling(max(v$dataList_melted$OD$value))))
          )
        })
      }
    })
    
    ##### AUC UI #####
    output$AUC_ui <- renderUI({
      fluidPage(
        list(
          sliderInput('auc_window', 'Window range AUC [h]:', min=0, step=0.5, max=ceiling(max(v$timeScale)), value=c(0, ifelse(ceiling(max(v$timeScale)) > 6, 6, floor(max(v$timeScale))))),
          selectInput('auc_selector', 'Plot to display:', choices = names(v$dataList), width='100%'),
          selectInput('auc_x_scale', 'X axis group:', choices=v$conditions, width='100%'),
          selectInput('auc_y_scale', 'Y axis group:', choices=v$conditions, width='100%')
        )
      )
    })

    
    
  })
  
  
  ##### lvl Order Sorter #####
  observeEvent(input$lvlOrderSorted, {
    v$groupsDF[input$lvlOrderSelect] <- factor(v$groupsDF[[input$lvlOrderSelect]], levels = c(input$lvlOrderSorted))
    
    #update dataframes levels based on level order
    v$dataList_melted <- lapply(v$dataList_melted, function(df) {
      df[input$lvlOrderSelect] <- factor(df[[input$lvlOrderSelect]], levels=c(input$lvlOrderSorted))
      return(df)
    })
    
  })
  ##### Plot #####
  observeEvent(v$dataList_melted, {
    output$plot <- renderPlot({
      v$customP
      req(input$yAxisRange)
      
      
      df <- v$dataList_melted[["OD"]]
      
      if(input$logScale) {
        df$Upper <- log10(df$value+df$SE)
        df$Lower <- log10(df$value-df$SE)
        df$value <- log10(df$value)
      }else {
        df$Upper <- df$value+df$SE
        df$Lower <- df$value-df$SE
        df$value <- df$value
      }
      pOD <- makePlot(df, input, isolate(v$customP), od=T)
      
      
      if(input$secondaryPlot != "None") {
        dfSec <- v$dataList_melted[[input$secondaryPlot]]
        text <- input$secondaryPlot
        if(input$secPlotMethod) {
          dfSec$value <- isolate(as.numeric(dfSec$value)/as.numeric(v$dataList_melted[["OD"]]$value))
          text <- paste(text, "/OD")
        }
        pSec <- makePlot(dfSec, input, isolate(v$customP), text, od=F)
        if(input$secPlotDisplay) {
          pOD <- ggarrange(pOD, pSec, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "right")
        }else {
          
          v$p <- pSec
          return(pSec)
        }
      }
      
      v$p <- pOD
      return(pOD)
    }, width=reactive(input$width*72), height = reactive(input$height*72))
    
    
    
  })
  
  toListenAUC <- reactive({
    list(v$groupsDF, input$auc_selector)
  })
  

  ##### AUC #####
  
  observeEvent(toListenAUC(), {
    req(input$auc_selector)
    v$auc_list <- lapply(v$dataList, function(subTable) {
      sub <- cbind(data.frame(do.call(rbind, lapply(v$groupsDF$Wells, function(well) {
          return(data.frame(Well=well, AUC=as.numeric(getAUC(v$timeScale, subTable[[well]], input$auc_window))))
        }))), v$groupsDF[v$conditions])
      return(sub)
    })
    
    df <- v$auc_list[[input$auc_selector]]
    
    
    output$bar_graph <- renderPlot({
      p_bar <- ggplot(df, aes_string(y="AUC", x=input$auc_x_scale))
      if(input$color != 'None') {
        x <- nrow(unique(df[unlist(strsplit(input$color,", "))]))
        p_bar <- p_bar + aes_string(col=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")"),
                            fill=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")")) +
          scale_color_manual(values=getPalette(x, v$customP)[[input$pal]], aesthetics = c("colour", "fill"), name=input$color)
      }else {
        
      }

      if (input$fw != "None") {
        p_bar <- p_bar + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), scales="free", nrow = input$nRowsFacets)
      }
      
      p_bar <- p_bar +
        stat_summary(geom="bar", fun = mean, position = "dodge", alpha = 0.3) +
        stat_summary(geom="errorbar", fun.data = mean_se, width = 0.3, position=position_dodge(0.9), colour="black") +
        geom_point(pch=21, position=position_jitterdodge(dodge.width=0.9), col="black", size=input$size_p) +
        scale_y_continuous(expand = c(0,0), limits = c(0,1.03*max(df$AUC))) +
        # ggtitle(names(v$params)[match(input$parameter, v$params)]) +
        getTheme(input$theme, input$size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab(paste('AUC [', input$auc_window[1], 'h - ', input$auc_window[2], 'h]', sep=""))
      v$p_bar <- p_bar
      return(p_bar)
    },  width=reactive(input$width*72), height = reactive(input$height*72))
  })
  
}










shinyApp(ui, server)


