library(ggplot2)
library(gtable)
library(ggpubr)
library(grid)
library(gridExtra)
library(growthcurver)
library(shiny) 
library(shinythemes)
library(shinyStore)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(sortable)
library(esquisse) 
library(tools)
library(rhandsontable) 
library(stringr) 
library(dplyr)
library(tidyr)
library(reshape2) 
library(lemon) 
library(scales) 
library(RColorBrewer)
library(parallel)
library(Cairo)
library(jsonlite)
library(xlsx)
source('utility.R', local = TRUE)
source('plot.R', local = TRUE)


version <- "0.1"

##### UI #####
ui <- dashboardPage(
  dashboardHeader(title = "BactEXTRACT"),
  dashboardSidebar(
    fluidPage(
      list(
        h3('Uploads :')
      )
    ),
    fileInput("files", "Choose TECAN Excel File",
              multiple = TRUE,
              accept = c(".xlsx", ".txt")),
    fileInput("file_settings", "Choose Settings File",
              multiple = FALSE,
              accept = c(".json")),
    
    initStore("localStorage", "BactEXTRACT_storage"),
    uiOutput("downloads"),
    
    div(style="height:130vh;",
        width=12,
        
    ),
    
    tags$hr(style='margin-bottom:0px'),
    column(
      width=12,
      align="center", 
      p('\u00A9 Julien D\u00E9n\u00E9r\u00E9az'),
      p(paste0('2022 - Version ', version))
    )
    
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(120vh - 80px) !important;}"),
    tags$style(HTML('.shiny-split-layout>div {overflow: visible;}')),
    fluidRow(
      column(
        width=12,
        fluidRow(
          column(
            width=9,
            style="padding-left: 0px;padding-right: 0px;",
            box(
              height='50vh',
              width=3,
              title="1. Settings",
              status = 'primary',
              div(style = 'overflow:auto;height:calc(50vh - 60px)',
                  uiOutput("conditionsUI")
              )
            ),
            box(
              height='50vh',
              width=5,
              title="2. Groups Design",
              # title=p("Groups Design", 
              #         actionButton("processGrou", "Apply Modifications", icon = icon("refresh"),
              #                      class = "btn-xs", title = "Update")),
              status = 'primary',
              rHandsontableOutput("groups", height = 'calc(50vh - 60px)')
            ),
            box(
              height='50vh',
              width=4,
              title="3. Plot Settings",
              status = 'primary',
              div(style = 'overflow:auto;height:calc(50vh - 60px)',
                  uiOutput("graph_optionsUI")
              )
            ),
            box(
              width=12,
              title="5. Plots",
              status = 'primary',
              height = '100vh',
              div(
                style="overflow-x:auto;height:100%;",
                plotOutput('plot', height="100%")
                
              )
            )
          ),
          column(
            width=3,
            box(
              height='93vh',
              width=16,
              title="4. Visual Settings",
              status = 'primary',
              div(style = 'overflow:auto;height:calc(calc(200vh - 280px)/2)',
                  uiOutput("themeUI"),
              )
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
  
  v$params <- c(
    "Empirical AUC"="auc_e",
    "Logistic AUC"="auc_l",
    "Trapezoidal AUC" = "AUC",
    "Max value" = "max_val",
    "Max Growth Rate" = "max_gr",
    "Growth Rate (Logistic)"="r",
    "Doubling time (Logistic)"="t_gen",
    "Inflection Point (Logistic)"="t_mid",
    "Carrying capacity (Logistic)"="k",
    "Log initial population size (Logistic)"="n0",
    "Sigma (Logistic)"="sigma"
  )
  
  
  observeEvent(input$files, {
    req(input$files)
    output$conditionsUI <- renderUI({
      fluidPage(
        list(
          splitLayout(
            selectInput('techAggr', 'Tech. Repl. Merging:', choices=c("None", "Horizontal", "Vertical")),
            numericInput('techAggrN', 'By:', value=3, min=2)
          ),
          splitLayout(
            selectInput('norm', 'OD Normalisation:', choices=c('Mininum', '1st well', 'Min(wells 1-2-3)', 'Specific Well(s)', "No Normalisation")),
            selectInput('norm_baseOD', 'Base OD:', choices=c(0, 0.001, 0.002, 0.003, 0.004), selected = 0.001),
          ),
          uiOutput('normByWellsUI'),
          tags$hr(),
          HTML("<b>Enter Conditions:</b>"),
          textInput('conditionsUI_Input', NULL, value=paste(input$localStorage$cond, collapse = ","), placeholder = "Strain, Treatment, ..."),
          bsTooltip("conditionsUI_Input", "Tooltip works", placement = "bottom", trigger = "hover", options = NULL),
          actionButton("updateCond", "Apply Modifications", icon = icon("gears"),class = "btn-xl", title = "Update")
          # column(12, align="center", p('(Cannot start with number, separated by coma)')),     
        )
      )
    })
    if(!is.null(v$rawdata_list)) {
      # lapply(react, function(reactVal){
      #   v[[reactVal]] <- NULL
      # })
      output$graph_optionsUI <- NULL
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
    #rawdata_list contains the merged files sub-tables, OD first, then followed by any additional luminescence/RLU etc.. tables
    v$timeScale <- rawdata_file_list[[1]]$time
    v$subTableNames <- names(rawdata_file_list[[1]])[-1]
    for (i in v$subTableNames) {
      v$rawdata_list[[i]] <- do.call(cbind, lapply(isolate(input$files$name), function(filename) {
        df <- isolate(rawdata_file_list[[filename]][[i]])
        if(length(input$files$name) > 1) {
          colnames(df) <- paste(tools::file_path_sans_ext(filename), colnames(df), sep="_")
        }
        return(df)
      }))
    }
    # showNotification('Files successfully uploaded', type='message')
    
    
  })
  
  observeEvent(input$file_settings, {
    print(fromJSON(input$file_settings$datapath))
  })
  
  
  
  
  toListenNormMerge <- reactive({
    list(input$techAggr, input$norm)
  })
  
  
  observeEvent(toListenNormMerge(), {
    req(input$norm)
    req(input$techAggr)
    v <- names(v$rawdata_list[[1]])
    if(input$techAggr != "None") {
      v <- aggrTechV(v, input$techAggrN, input$techAggr=="Horizontal", multiF = tools::file_path_sans_ext(input$files$name))
    }else {
      output$techAggrUI <- NULL
    }
    if(input$norm == "Specific Well(s)") {

      output$normByWellsUI <- renderUI({
        selectInput('normByWells', label="Wells to use for normalisation:", multiple = T, choices = v)
      })
      
    }else{
      output$normByWellsUI <- NULL
    }
  })
  
  

  
  ##### Update button Panel #####
  observeEvent(input$updateCond, {
    
    req(input$conditionsUI_Input)
    v$conditions <- formartConditions(input$conditionsUI_Input)
    
    #Aggregating technical replicate by mean
    if(input$techAggr != "None") {
      aggdata_list <- aggrTech(v$rawdata_list, input$techAggrN, input$techAggr =="Horizontal", multiF = tools::file_path_sans_ext(input$files$name))
    }else {
      aggdata_list <- v$rawdata_list
    }

    
    if(!is.null(v$groups)) {
      #Reset the v$groups if the user change the settings of technical replicate aggregating
      if(!setequal(colnames(aggdata_list[[1]]),v$groups$Wells)) {
        v$groups <- NULL
      }else {
        v$groups[colnames(data.frame(hot_to_r(input$groups)))] <- data.frame(hot_to_r(input$groups))
      }
    }
    
    #add localstorage for groups, if same number of rows
    
    
    v$groups <- updateGroup(isolate(v$groups), v$conditions, colnames(aggdata_list[[1]]))

    
    v$interactions <- getInteractions(v$conditions)
    
    #normalization
    v$dataList <- aggdata_list
    v$dataList[[1]] = normalize(isolate(v$dataList[[1]]), input$norm, input$norm_baseOD, input$normByWells)
    
    updateStore(session, name = "cond", value = v$conditions)
    
    #SapLine to preview the growth curve of each well
    v$groups$Preview <- apply(v$dataList[[1]], 2, function(x) jsonlite::toJSON(list(values=as.vector(replace(x, is.na(x), 0)), options=list(type="line", spotRadius=0, chartRangeMin=0, chartRangeMax=1))))
    output$groups <- renderRHandsontable({
      req(v$groups)
      rhandsontable(
        data.frame(v$groups), 
        rowHeaders=NULL,
        fillHandle = list(direction='vertical', autoInsertRow=FALSE),
        maxRows = ncol(isolate(v$dataList[[1]])),
        useTypes=T) %>%
        hot_col(col="Wells", readOnly = T, allowRowEdit =F, allowColEdit = F) %>%
        hot_col(col="KeepWell", type="dropdown", source=c("Yes", "No"), strict=T, allowInvalid=F, valign='htCenter') %>%
        hot_col(col="Preview",copyable=F, renderer=htmlwidgets::JS("renderSparkline"), valign='htCenter', allowColEdit=F, readOnly=T) %>%
        hot_table(highlightCol = T, highlightRow = T, allowRowEdit =F)
    })
    #To add: from local storage update

    
    ##### UI graph options #####
    output$graph_optionsUI <- renderUI({
      fluidPage(
        list(
          splitLayout(
            selectInput('type_plot_selector', 'Type of plot:', choices =  c("Growth Plot", "Bar Plot", "Checker Plot", "Logistic Curves"), width='100%'),
            selectInput('data_selector', 'Data to display:', choices = names(v$dataList), width='100%'),
            
          ),
          uiOutput("sec_plot_type_UI"),
          tags$hr(),
          splitLayout(
            column(
              width=12,
              style="padding-left: 0px;padding-right: 0px;",
              selectInput('color', 'Color:', choices=c("None", v$conditions, v$interactions), selected = v$conditions[1], width='100%'),
              selectInput('linetype', 'Linetype:', choices=c("None", v$conditions), width='100%'),
              selectInput('shape', 'Shape:', choices=c("None", v$conditions), width='100%'),
              selectInput('grouping', 'Grouping:', choices=c("None", v$interactions), width='100%'),
              selectInput('fw', 'Facet Wrap:', choices=c("None", v$conditions, v$interactions), width='100%'),
              uiOutput("refCurveUI")
            ),
            column(
              width=12,              
              style="padding-left: 0px;padding-right: 0px;",
              selectInput('se', 'Standart Error Style:', choices=c("None", "Line Range", "Ribbon"), width='100%'),
              selectInput('lvlOrderSelect', "Order Levels:", choices = c(v$conditions), width='100%'),
              uiOutput("levelOrderUI", width='100%')
            )
          ),
          tags$hr(),
          selectInput('param_selector', 'Parameter to display:', choices=c(names(v$params)), width='100%'),
          sliderInput('auc_window', 'Window range parameters [h]:', min=0, step=0.5, max=ceiling(max(v$timeScale)), value=c(0, ifelse(ceiling(max(v$timeScale)) > 6, 6, floor(max(v$timeScale)))))
          # actionButton('calculateParameters', "Calculate Growth Parameters", icon = icon("calculator"))
        )
      )
    })

    output$themeUI <- renderUI({
      fluidPage(
        list(
          splitLayout(
            column(
              width=12,
              style="padding-left: 0px;padding-right: 0px;",
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
            numericInput('width', 'Width: (Inches)', value=10, min = 4, max = 50, step = 1),
            
          ),
          tags$hr(),
          splitLayout(
            textInput('y_axis_title', "X axis Title:"),
            selectInput('params_x_scale', 'X axis selector:', choices=v$conditions, width='100%'),
          ),
          sliderInput('range', 'X axis range:', min=0, step=0.5, max=ceiling(max(v$timeScale)), value=c(0, ceiling(max(v$timeScale)))),
          tags$hr(),
          splitLayout(
            textInput('y_axis_title', "Y axis Title:"),
            selectInput('params_y_scale', 'Y axis selector:', choices=v$conditions, width='100%'),
          ),
          checkboxInput('logScale', 'Log10 Transformation', value = T),
          uiOutput("logScaleUI"),
          tags$hr(),
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
  
  observeEvent(input$data_selector, {
    if(input$data_selector != v$subTableNames[1]) {
      output$sec_plot_type_UI <- renderUI({
        list(
          checkboxInput('secPlotMethod', paste(input$data_selector, '/', v$subTableNames[1]), value = F, width='100%'),
          checkboxInput('secPlotDisplay', paste("Display both ", input$data_selector, ' and ', v$subTableNames[1], 'plots'), value = F, width='100%')
        )
      })
    }else {
      output$sec_plot_type_UI <- NULL
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

    v$groupDF_subset <- v$groupsDF[v$groupsDF$KeepWell=="Yes", -which(names(v$groupsDF) =="Preview")]
    
    
    if(is.null(v$dataList_melted)) {
      return()
    }
    
    ##### Downloads #####
    output$downloads <- renderUI({
      list(
        column(12, 
               # align="center",
               tags$hr(),
               h3("Downloads :"),
               tags$style(".skin-blue .sidebar a { color: #444; }"),
               downloadButton("download_df", label = "Growth Data"),
               downloadButton("downloadparams_df", label = "Growth Parameters Data"),
               tags$p(""),               
               downloadButton("download_settings", label = "Settings"),
               tags$p(""),
               downloadButton("download_pdf", label = "Plot (PDF)"),
               downloadButton("download_eps", label = "Plot (EPS)"),
               tags$hr(),

        )
      )
    })
    output$download_df <- downloadHandler(
      function() {paste0(input$title, "_growthData.csv")},
      function(file) {
        write.csv(v$dataList_melted, file, row.names = FALSE)
      }
    )
    output$download_settings <- downloadHandler(
      function() {paste0(input$title, "_settings.json")},
      function(file) {
        # write.csv(input$localStorage, file, row.names = FALSE)
        writeLines(jsonlite::toJSON(input$localStorage), file)
      }
    )
    output$download_pdf <- downloadHandler(
      function(){paste0(input$title,'.pdf')},
      function(file){
        ggsave(file,plot=v$p, width=input$width, height=input$height, units="in", dpi=300, device=cairo_pdf)
      }
    )
    output$download_eps <- downloadHandler(
      function(){paste0(input$title,'.eps')},
      function(file){
        ggsave(file,plot=v$p, width=input$width, height=input$height, units="in", dpi=300, device=cairo_ps)
      }
    )
    output$downloadparams_df <- downloadHandler(
      function() {paste0(input$title, "_growthParameters.csv")},
      function(file) {
        df <- cbind(
          v$groupDF_subset[-which(names(v$groupDF_subset) == "KeepWell")],
          v$params_list
        )
        write.csv(df, file, row.names = FALSE)
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
      if(input$fw != "None"){
        output$refCurveUI <- renderUI({
          list(
            splitLayout(
              selectInput('referenceCurve', 'Ref. Curve:', choices = c("None", isolate(levels(v$groupsDF[[input$fw]]))), width='100%'),
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
            sliderTextInput('yAxisRange', 'Y axis range:', choices = c(0.0001, 0.001, 0.01, 0.1, 1, 10), selected =c(0.001, 1),  grid=T)
          )
        })
      }else {
        output$logScaleUI <- renderUI({
          list(
            sliderInput('yAxisRange', 'Y axis range:', min=0, step=0.1, max=ceiling(max(v$dataList_melted[[1]]$value, na.rm=T)), value=c(0, ceiling(max(v$dataList_melted[[1]]$value, na.rm=T))))
          )
        })
      }
    })
  })
  
  
  
  
  ##### lvl Order Sorter #####
  observeEvent(input$lvlOrderSorted, {
    v$groupsDF[input$lvlOrderSelect] <- factor(v$groupsDF[[input$lvlOrderSelect]], levels = c(input$lvlOrderSorted))
    v$groupDF_subset[input$lvlOrderSelect] <- factor(v$groupDF_subset[[input$lvlOrderSelect]], levels = c(input$lvlOrderSorted))
    #update dataframes levels based on level order
    v$dataList_melted <- lapply(v$dataList_melted, function(df) {
      df[input$lvlOrderSelect] <- factor(df[[input$lvlOrderSelect]], levels=c(input$lvlOrderSorted))
      return(df)
    })
    
  })
  ##### Plot #####
  observeEvent(input$type_plot_selector, {
    output$plot <- renderPlot({
      req(input$yAxisRange)
      df <- v$dataList_melted[[1]]
      
      if(input$type_plot_selector == "Growth Plot") {
        pOD <- makePlot(df, input, isolate(v$customP), od=T)
        
        # return(plot_exception("sorry, no data is found."))
        
        # pOD <- makePlot(df, input, isolate(v$customP), od=T)
        
        if(!is.null(input$secPlotDisplay) && input$data_selector != v$subTableNames[[1]]) {
          dfSec <- v$dataList_melted[[input$data_selector]]
          text <- input$data_selector
          
          #if RLU/OD
          if(input$secPlotMethod) {
            dfSec$value <- isolate(as.numeric(dfSec$value)/as.numeric(v$dataList_melted[[1]]$value))
            text <- paste(text, "/", v$subTableNames[[1]])
          }
          
          pSec <- makePlot(dfSec, input, isolate(v$customP), text, od=F)
          
          
          if(input$secPlotDisplay) {
            pSec <- ggarrange(pOD, pSec, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "right")
          }
          p <- pSec
          return(p)
        }
        
        p <- pOD
        
        
      }else {
        req(v$params_list) #display they need to update calculation
        
        df_params <- cbind(
          v$groupDF_subset,
          v$params_list[[input$data_selector]]
        )
        
        data_raw <- cbind(data.frame(time=v$timeScale), v$dataList[[input$data_selector]])
        dataOD <- data_raw[which(data_raw$time >= input$auc_window[1] & data_raw$time <= input$auc_window[2]),]
        
        
        p <- makeParametersPlot(input$type_plot_selector, df_params, data_raw, input, isolate(v$params), isolate(v$customP))
        
      }
      v$p <- p

      return(p)
    }, width=reactive(input$width*72), height = reactive(input$height*72))
    
  })
  

  ##### Parameters #####
  observeEvent(input$auc_window, {
    dt <- v$dataList
    #If RLU/OD is selected, normalise the RLU table by OD table
    if(!is.null(input$secPlotMethod) && input$secPlotMethod) {
      dt[[input$data_selector]] <- dt[[input$data_selector]]/df[[1]]
    }
    v$params_list <- lapply(dt, function(subTable) {
      subTable <- subTable[v$groupsDF$KeepWell == "Yes"]
      data <- getLogisticParameters(v$timeScale, subTable, input$auc_window)[2:10]
      data$AUC <- getTrapezoidalAUC(v$timeScale, subTable, input$auc_window)
      data$max_gr <- getMaxGr(v$timeScale, subTable, input$auc_window)
      data$max_val <- getMaxVal(v$timeScale, subTable, input$auc_window)
      return(data)
    })
  })
  
}



shinyApp(ui=ui, server=server)


