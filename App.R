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


version <- "0.2"

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
              # title="2. Groups Design",
              title= p("2. Groups Design", 
                       actionButton('loadLocalStorage', 'Load Local Groups Storage', icon = icon('arrows-rotate'),
                                    class = 'btn-xs', title = '', style = "position: absolute; right: 10px")
              ),
                
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
  v$updateLvl <- F
  
  
  observeEvent(input$files, {
    req(input$files)
    output$conditionsUI <- renderUI({
      fluidPage(
        list(
          splitLayout(
            selectInput('techAggr', 'Tech. Repl. Merging:', choices=c("None", "Horizontal", "Vertical"), selected = input$localStorage$techAggr),
            numericInput('techAggrN', 'By:', value=3, min=2)
          ),
          splitLayout(
            selectInput('norm', 'OD Normalisation:', choices=c('Mininum', '1st well', 'Min(wells 1-2-3)', 'Specific Well(s)', "No Normalisation"), selected = input$localStorage$norm),
            selectInput('norm_baseOD', 'Base OD:', choices=c(0, 0.001, 0.002, 0.003, 0.004), selected = orNull(input$localStorage$norm_baseOD, 0.001)),
          ),
          uiOutput('normByWellsUI'),
          tags$hr(),
          HTML("<b>Enter Conditions:</b>"),
          textInput('conditionsUI_Input', NULL, value=input$localStorage$conditionsUI_Input, placeholder = "Strain, Treatment, ..."),
          bsTooltip("conditionsUI_Input", "Tooltip works", placement = "bottom", trigger = "hover", options = NULL),
          actionButton("updateCond", "Apply Modifications", icon = icon("gears"),class = "btn-xl", title = "Update")
          # column(12, align="center", p('(Cannot start with number, separated by coma)')),     
        )
      )
    })
    if(!is.null(v$rawdata_list)) {
      lapply(react, function(reactVal){
        v[[reactVal]] <- NULL
      })
      output$graph_optionsUI <- NULL
      # output$themeUI <- NULL
      output$plot <- NULL
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
    
    
    timeCols <- lapply(rawdata_file_list, function(rawdt) {
      return(rawdt$time)
    })
    nr <- max(lengths(timeCols))
    
    #rawdata_list contains the merged files sub-tables, OD first, then followed by any additional luminescence/RLU etc.. tables
    v$subTableNames <- names(rawdata_file_list[[1]])[-1] #Not looping through the time vector
    for (subTableName in v$subTableNames) {
      v$rawdata_list[[subTableName]] <- do.call(cbind, lapply(isolate(input$files$name), function(filename) {
        df <- isolate(rawdata_file_list[[filename]][[subTableName]])
        
        #Fill up the DF with NA rows based on the max nr value
        if(nrow(df) < nr) {
          df[nrow(df):nr,] <- NA
        }
        if(length(input$files$name) > 1) {
          colnames(df) <- paste(tools::file_path_sans_ext(filename), colnames(df), sep="_")
        }
        return(df)
      }))
    }
    
    v$timeScale <- timeCols[[which.max(lengths(timeCols))]]
    
    showNotification('File(s) successfully uploaded', type='message')
    
    
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
    if(input$conditionsUI_Input == "") {
      showNotification('You need at lest one condition', type='warning')
    }
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
    
    v$groups <- updateGroup(isolate(v$groups), v$conditions, colnames(aggdata_list[[1]]))

    
    v$interactions <- getInteractions(v$conditions)
    
    #normalization
    v$dataList <- aggdata_list
    v$dataList[[1]] = normalize(isolate(v$dataList[[1]]), input$norm, input$norm_baseOD, input$normByWells)
    
    
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
    
    
    ##### UI graph options #####
    output$graph_optionsUI <- renderUI({
      fluidPage(
        list(
          splitLayout(
            selectInput('type_plot_selector', 'Type of plot:', choices =  c("Growth Plot", "Bar Plot", "Checker Plot", "Logistic Curves"), selected = input$localStorage$type_plot_selector, width='100%'),
            selectInput('data_selector', 'Data to display:', choices = names(v$dataList), width='100%'),
            
          ),
          uiOutput("sec_plot_type_UI"),
          tags$hr(),
          splitLayout(
            column(
              width=12,
              style="padding-left: 0px;padding-right: 0px;",
              selectInput('color', 'Color:', choices=c("None", v$conditions, v$interactions), selected = input$localStorage$color, width='100%'),
              selectInput('linetype', 'Linetype:', choices=c("None", v$conditions), selected = input$localStorage$linetype, width='100%'),
              selectInput('shape', 'Shape:', choices=c("None", v$conditions), selected = input$localStorage$shape, width='100%'),
              selectInput('grouping', 'Grouping:', choices=c("None", v$interactions), selected = input$localStorage$grouping, width='100%'),
              selectInput('fw', 'Facet Wrap:', choices=c("None", v$conditions, v$interactions), selected = input$localStorage$fw, width='100%'),
              uiOutput("refCurveUI")
            ),
            column(
              width=12,              
              style="padding-left: 0px;padding-right: 0px;",
              selectInput('se', 'Standart Error Style:', choices=c("None", "Line Range", "Ribbon"), selected = input$localStorage$se, width='100%'),
              selectInput('lvlOrderSelect', "Order Levels:", selected=input$localStorage$lvlOrderSelect, choices = c(v$conditions), width='100%'),
              uiOutput("levelOrderUI", width='100%')
            )
          ),
          tags$hr(),
          selectInput('param_selector', 'Parameter to display:', choices=c(names(v$params)), width='100%'),
          sliderInput('auc_window', 'Window range parameters [h]:', min=0, step=0.5, max=ceiling(max(v$timeScale)), value=c(0, ifelse(ceiling(max(v$timeScale)) > 6, 6, floor(max(v$timeScale)))))
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
                choices = isolate(getPalette(8, v$customP)),
                selected = orNull(input$localStorage$pal, 'Viridis'),
                textColor = c(
                  rep("white", 5), rep("black", 8) 
                )
              )
            ),
            textInput('customColorPalette', 'Custom Color Palette:', value=paste(isolate(input$localStorage$customP), collapse=","),  placeholder = "#4b123f, #cb13b2, ...")
          ),
          splitLayout(
            numericInput('height', 'Height (Inches):', value=orNull(isolate(input$localStorage$height), 5), min = 4, max = 50, step = 1),
            numericInput('width', 'Width: (Inches)', value=orNull(isolate(input$localStorage$width), 10), min = 4, max = 50, step = 1),
            
          ),
          tags$hr(),
          splitLayout(
            textInput('x_axis_title', "X axis Title:", value = input$localStorage$x_axis_title),
            selectInput('params_x_scale', 'X axis selector:', choices=v$conditions, width='100%'),
          ),
          sliderInput('range', 'X axis range:', min=0, step=0.5, max=ceiling(max(v$timeScale)), value=c(0, ceiling(max(v$timeScale)))),
          tags$hr(),
          splitLayout(
            textInput('y_axis_title', "Y axis Title:", value = input$localStorage$y_axis_title),
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
            textAreaInput('customThemeUI', 'Add custom ggplot layers (separated by coma):', value=input$localStorage$customThemeUI, placeholder = "geom_vline(...), theme(...)")
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
    isolate({
      vec <- gsub(" ", "", unlist(strsplit(input$customColorPalette, ",")))
      
      if(isHex(vec)) {
        v$customP <- vec
        updatePalettePicker(
          inputId = "pal", 
          choices = getPalette(8, vec),
          selected = orNull(input$localStorage$pal, 'Viridis'),
          textColor = c(
            rep("white", 5), rep("black", 8) 
          )
        )
      }
    })
  })
  
  
  ##### Groups #####
  observeEvent(input$groups, {
    
    #This is called everytime the input$groups table gets modified by the user
    req(input$groups)
    
    
    if(v$updateLvl) {
      v$groupsDF[-c(1,3)] <- input$localStorage$groupsDF
      v$groupsDF[-c(1,2,3)] <- lapply(names(v$groupsDF[-c(1,2,3)]), function(colna) {
        col <- factor(v$groupsDF[[colna]], levels=c(input$localStorage$groupsDFLvl[[colna]]))
        return(col)
      })
      v$updateLvl <- F
    }else {
      v$groupsDF <- data.frame(hot_to_r(input$groups))
      v$groupsDF[-c(1,2,3)] <- lapply(v$groupsDF[-c(1,2,3)], factor)
    }
    

    
    v$dataList_melted <- dataMelter(v$dataList, v$groupsDF, v$timeScale)
    
    v$groupDF_subset <- v$groupsDF[v$groupsDF$KeepWell=="Yes", -which(names(v$groupsDF) =="Preview")]
    
    if(!is.null(v$oldWells)) {
      if(length(v$oldWells) != length(v$groupsDF$KeepWell)) {
        calculateParams()
      }else if(any(! v$oldWells == v$groupsDF$KeepWell)) {
        calculateParams()
      }
    }
    v$oldWells <- v$groupsDF$KeepWell
    
    
    
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
               tags$p(""),
               downloadButton("downloadparams_df", label = "Growth Parameters Data"),
               tags$p(""),               
               tags$p(""),
               downloadButton("download_pdf", label = "Plot (PDF)"),
               tags$p(""),
               downloadButton("download_eps", label = "Plot (EPS)"),
               tags$p(""),
               downloadButton("download_settings", label = "Settings"),
               tags$hr(),
               actionButton("save_settings", label = "Save Settings Locally", icon=icon("floppy-disk")),

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
              selectInput('referenceCurve', 'Ref. Curve:', choices = c("None", isolate(levels(v$groupsDF[[input$fw]]))), selected=input$localStorage$referenceCurve, width='100%'),
              numericInput('nRowsFacets', 'N. Rows:', value=orNull(input$localStorage$nRowsFacets, 1), min = 1, max = 10, step = 1)
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
    reactPlot()
  })
  
  ##### Local Storage #####
  
  observeEvent(input$loadLocalStorage, {
    req(v$groups)
    repl <- as.data.frame(input$localStorage$groupsDF)
    int <- intersect(colnames(v$groupsDF), colnames(repl))
    v$groups[,int] <- repl[,int]
    v$updateLvl <- T
    
  })
  
  
  observeEvent(input$file_settings, {
    saveDF <- fromJSON(input$file_settings$datapath)
    updateStore(session, name = "color", value = saveDF$color)
    updateStore(session, name = "linetype", value = saveDF$linetype)
    updateStore(session, name = "shape", value = saveDF$shape)
    updateStore(session, name = "grouping", value = saveDF$grouping)
    updateStore(session, name = "fw", value = saveDF$fw)
    updateStore(session, name = "referenceCurve", value = saveDF$referenceCurve)
    updateStore(session, name = "nRowsFacets", value = saveDF$nRowsFacets)
    updateStore(session, name = "se", value = saveDF$se)
    updateStore(session, name = "conditionsUI_Input", value = saveDF$conditionsUI_Input )
    updateStore(session, name = "customP", value = saveDF$customP)
    updateStore(session, name = "groupsDF", value = saveDF$groupsDF)
    updateStore(session, name = "groupsDFLvl", value = saveDF$groupsDFLvl)
    updateStore(session, name = "height", value = saveDF$height)
    updateStore(session, name = "width", value = saveDF$width)
    updateStore(session, name = "pal", value = saveDF$pal)
    updateStore(session, name = "norm", value = saveDF$norm)
    updateStore(session, name = "norm_baseOD", value = saveDF$norm_baseOD)
    updateStore(session, name = "techAggr", value = saveDF$techAggr)
    updateStore(session, name = "x_axis_title", value = saveDF$x_axis_title)
    updateStore(session, name = "y_axis_title", value = saveDF$y_axis_title)
    updateStore(session, name = "customThemeUI", value = saveDF$customThemeUI)
    updateStore(session, name = "type_plot_selector", value = saveDF$type_plot_selector)
    updateStore(session, name = "lvlOrderSelect", value = saveDF$lvlOrderSelect)
  })
  
  
  observeEvent(input$save_settings, {
    isolate({
      updateStore(session, name = "color", value = input$color)
      updateStore(session, name = "linetype", value = input$linetype)
      updateStore(session, name = "shape", value = input$shape)
      updateStore(session, name = "grouping", value = input$grouping)
      updateStore(session, name = "fw", value = input$fw)
      updateStore(session, name = "referenceCurve", value = input$referenceCurve)
      updateStore(session, name = "nRowsFacets", value = input$nRowsFacets)
      updateStore(session, name = "se", value = input$se)
      updateStore(session, name = "conditionsUI_Input", value = input$conditionsUI_Input )
      updateStore(session, name = "customP", value = v$customP)
      updateStore(session, name = "groupsDF", value = v$groupsDF[-which(names(v$groupsDF) %in% c("Wells", "Preview"))])
      updateStore(session, name = "groupsDFLvl", value = lapply(v$groupsDF[-c(1,2,3)], levels))
      updateStore(session, name = "height", value = input$height)
      updateStore(session, name = "width", value = input$width)
      updateStore(session, name = "pal", value = input$pal)
      updateStore(session, name = "norm", value = input$norm)
      updateStore(session, name = "norm_baseOD", value = input$norm_baseOD)
      updateStore(session, name = "techAggr", value = input$techAggr)
      updateStore(session, name = "x_axis_title", value = input$x_axis_title)
      updateStore(session, name = "y_axis_title", value = input$y_axis_title)
      updateStore(session, name = "customThemeUI", value = input$customThemeUI)
      updateStore(session, name = "type_plot_selector", value = input$type_plot_selector)
      updateStore(session, name = "lvlOrderSelect", value = input$lvlOrderSelect)
    })
    
    showNotification('Successfully saved locally', type='message')
    
    
  })
  
  ##### Parameters #####
  observeEvent(input$auc_window, {
    calculateParams()
  })

  calculateParams <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Parameters", value = 0)
    dt <- v$dataList
    #If RLU/OD is selected, normalise the RLU table by OD table
    if(!is.null(input$secPlotMethod) && input$secPlotMethod) {
      dt[[input$data_selector]] <- dt[[input$data_selector]]/df[[1]]
    }
    n <- length(dt)
    v$params_list <- lapply(dt, function(subTable) {
      progress$inc(1/(n*5), detail = "")
      subTable <- subTable[v$groupsDF$KeepWell == "Yes"]
      progress$inc(1/(n*5), detail = "")
      data <- getLogisticParameters(v$timeScale, subTable, input$auc_window)[2:10]
      progress$inc(1/(n*5), detail = "")
      data$AUC <- getTrapezoidalAUC(v$timeScale, subTable, input$auc_window)
      progress$inc(1/(n*5), detail = "")
      data$max_gr <- getMaxGr(v$timeScale, subTable, input$auc_window)
      progress$inc(1/(n*5), detail = "")
      data$max_val <- getMaxVal(v$timeScale, subTable, input$auc_window)
      return(data)
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
    reactPlot()
  })

  reactPlot <- function() {
    output$plot <- renderPlot({
      # req(input$type_plot_selector)
      req(input$yAxisRange)
      df <- v$dataList_melted[[1]]
      
      if(input$type_plot_selector == "Growth Plot") {
        pOD <- makePlot(df, input, isolate(v$customP), od=T)
        
        # return(plot_exception("sorry, no data is found."))
        
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
  }
  
}



shinyApp(ui=ui, server=server)


