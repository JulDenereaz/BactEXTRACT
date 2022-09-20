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
library(tidyr)
source('utility.R', local = TRUE)
source('plot.R', local = TRUE)


library(ggplot2)
library(gtable)
library(grid)
library(xlsx)



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
            'Growth parameters',
            column(
              width=2,
              uiOutput("growth_param_ui")
            ),
            
            column(
              width=5,
              plotOutput('params_plot')              
            )
          # ),
          # tabPanel(
          #   'GrowthCurver', 
          #   column(
          #     width=2,
          #     uiOutput('growthcurverUI')
          #   ),
          #   
          #   column(
          #     width=5,
          #     plotOutput('growthcurverPlotUI')              
          #   )
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
    output$options <- renderUI({
      fluidPage(
        list(
          tags$hr(),
          h3('Options :'),
          selectInput('norm', 'OD Normalisation :', choices=c('Mininum', '1st value', 'Min(wells 1-2-3)', "No Normalisation")),
          selectInput('norm_baseOD', 'Base OD', choices=c(0, 0.001, 0.002, 0.003, 0.004), selected = 0.001),
          selectInput('techAggr', 'Tech. Repl. Merging:', choices=c("None", "Horizontal", "Vertical")),
          uiOutput('techAggrUI'),
          tags$hr(style='margin-bottom:5px'),
          textInput('conditionsUI_Input', 'Enter Conditions:', value=paste(input$localStorage$cond, collapse = ","), placeholder = "Strain, Treatment, ..."),
          uiOutput('error_condition'),
          column(12, align="center", p('(Cannot start with number, separated by coma)')),     
          column(12, align="center", actionButton('updateCond', 'Update')),
          bsTooltip("conditionsUI_Input", "Tooltip works", placement = "right", trigger = "hover", options = NULL)
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
    #Changing the name of the standard OD to OD. The rest will stay custom, as it can be RLU/Luminescence etc...
    
  })
  
  observeEvent(input$techAggr , {
    if(input$techAggr != "None"){
      output$techAggrUI <- renderUI({
        list(
          numericInput('techAggrN', 'By:', value=3, min=2)
        )
      })
    }else {
      output$techAggrUI <- NULL
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
    
    v$dataList[[1]] = normalize(isolate(v$dataList[[1]]), input$norm, input$norm_baseOD)
    updateStore(session, name = "cond", value = v$conditions)
    
    #SapLine to preview the growth curve of each well
    v$groups$Preview <- apply(v$dataList[[1]], 2, function(x) jsonlite::toJSON(list(values=as.vector(replace(x, is.na(x), 0)), options=list(type="line", spotRadius=0, chartRangeMin=0, chartRangeMax=1))))
    output$groups <- renderRHandsontable({
      req(v$groups)
      rhandsontable(
        data.frame(v$groups), 
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
              selectInput('plot_selector', 'Plot to display:', choices = names(v$dataList), width='100%'),
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
  
  observeEvent(input$plot_selector, {
    if(input$plot_selector != v$subTableNames[1]) {
      output$comparisonMethod <- renderUI({
        list(
          splitLayout(
            checkboxInput('secPlotMethod', paste(input$plot_selector, '/', v$subTableNames[1]), value = F, width='100%'),
            selectInput('secPlotDisplay', "Display:", choices = c("Single", "Dual", "Combined"), width="100%")
            # checkboxInput('secPlotDisplay', paste0('Dual with', v$subTableNames[[1]]), value = F, width='100%'),
            # checkboxInput('secAxis', "Sec. Y Axis", value = F, width='100%')
            
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
    
    v$groupDF_subset <- cbind(
      data.frame(Well = v$groupsDF[v$groupsDF$KeepWell == "Yes", "Wells"]), 
      v$groupsDF[v$groupsDF$KeepWell == "Yes", v$conditions]
    )
    
    if(is.null(v$dataList_melted)) {
      return()
    }
    
    ##### Downloads #####
    output$downloads <- renderUI({
      list(
        column(12, align="center",
               tags$hr(),
               tags$style(".skin-blue .sidebar a { color: #444; }"),
               downloadButton("download_df", label = "Download Full Data"),
               tags$p(""),
               downloadButton("download_pdf", label = "Download Growth Plot (PDF)"),
               downloadButton("download_eps", label = "Download Growth Plot (EPS)"),
               tags$hr(),
               uiOutput('downloadUI_params')

        )
      )
    })
    output$download_df <- downloadHandler(
      function() {paste0(input$title, "_growthData.csv")},
      function(file) {
        write.csv(v$dataList_melted, file, row.names = FALSE)
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
          v$groupDF_subset,
          v$params_list
        )
        write.csv(df, file, row.names = FALSE)
      }
    )
    output$downloadparams_pdf <- downloadHandler(
      function(){paste(input$title, "_", input$param_selector, '.pdf',sep='')},
      function(file){
        ggsave(file,plot=v$p_bar, width=input$width, height=input$height, units="in", dpi=300, device=cairo_pdf)
      }
    )
    output$downloadparams_eps <- downloadHandler(
      function(){paste0(input$title,'.eps')},
      function(file){
        ggsave(file,plot=v$p_bar, width=input$width, height=input$height, units="in", dpi=300, device=cairo_ps)
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
            sliderTextInput('yAxisRange', 'Y axis range:', choices = c(0.001, 0.01, 0.1, 1, 10), selected =c(0.001, 1),  grid=T)
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
    
    ##### Growth Parameters UI #####
    output$growth_param_ui <- renderUI({
      fluidPage(
        list(
          sliderInput('auc_window', 'Window range parameters [h]:', min=0, step=0.5, max=ceiling(max(v$timeScale)), value=c(0, ifelse(ceiling(max(v$timeScale)) > 6, 6, floor(max(v$timeScale))))),
          selectInput('param_plot_selector', "Type of plot:", choices=c("Bar Plot", "Tile Plot", "Logistic Curves")),
          selectInput('param_selector', 'Parameter to display:', choices=c(names(v$params)), width='100%'),
          selectInput('params_x_scale', 'X axis:', choices=v$conditions, width='100%'),
          selectInput('params_y_scale', 'Y axis (For Tile Plot):', choices=v$conditions, width='100%')
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
      req(input$lvlOrderSorted)
      
      
      df <- v$dataList_melted[[1]]
      pOD <- makePlot(df, input, isolate(v$customP), od=T)
      
      
      if(!is.null(input$secPlotDisplay) && input$plot_selector != v$subTableNames[[1]]) {
        dfSec <- v$dataList_melted[[input$plot_selector]]
        text <- input$plot_selector
        
        #if RLU/OD
        if(input$secPlotMethod) {
          dfSec$value <- isolate(as.numeric(dfSec$value)/as.numeric(v$dataList_melted[[1]]$value))
          text <- paste(text, "/", v$subTableNames[[1]])
        }
        dfSec <- getUpLo(dfSec)
        
        pSec <- makePlot(dfSec, input, isolate(v$customP), text, od=F)
        
      
        if(input$secPlotDisplay == "Dual") {
          pSec <- ggarrange(pOD, pSec, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "right")
        # }else if(input$secPlotDisplay == "Combined") {
          # pSec <- makePlot(dfSec, input, isolate(v$customP), text, od=F)
        }
        v$p <- pSec
        return(pSec)
      }
      
      v$p <- pOD
      return(pOD)
    }, width=reactive(input$width*72), height = reactive(input$height*72))
    
  })
  
  toListenParams <- reactive({
    list(v$groupsDF, input$plot_selector, input$auc_window, input$secPlotMethod)
  })
  

  ##### Parameters #####
  
  observeEvent(toListenParams(), {
    req(input$auc_window)

    dt <- v$dataList
  
    #If RLU/OD is selected, normalise the RLU table by OD table
    if(!is.null(input$secPlotMethod) && input$secPlotMethod) {
      dt[[input$plot_selector]] <- dt[[input$plot_selector]]/df[[1]]
    }
    
  
    
    v$params_list <- lapply(dt, function(subTable) {
      subTable <- subTable[v$groupsDF$KeepWell == "Yes"]
      data <- getLogisticParameters(v$timeScale, subTable, input$auc_window)[2:10]
      data$AUC <- getTrapezoidalAUC(v$timeScale, subTable, input$auc_window)
      data$max_gr <- getMaxGr(v$timeScale, subTable, input$auc_window)
      data$max_val <- getMaxVal(v$timeScale, subTable, input$auc_window)
      return(data)
    })
    
    
    output$downloadUI_params <- renderUI({
      list(
        downloadButton("downloadparams_df", label = "Download Growth Parameters Data"),
        tags$p(),
        downloadButton("downloadparams_pdf", label = "Download Growth Parameters (PDF)"),
        downloadButton("downloadparams_eps", label = "Download Growth Parameters (EPS)")
      )
    })
    df <- cbind(
      v$groupDF_subset,
      v$params_list[[input$plot_selector]]
    )
    
    dataOD <- cbind(data.frame(time=v$timeScale), v$dataList[[input$plot_selector]])
    dataOD <- dataOD[which(dataOD$time >= input$auc_window[1] & dataOD$time <= input$auc_window[2]),]
    
    output$params_plot <- renderPlot({
      if(input$param_plot_selector == "Bar Plot") {
        p_bar <- ggplot(df, aes_string(y=v$params[input$param_selector], x=input$params_x_scale))
        if(input$color != 'None') {
          x <- nrow(unique(df[unlist(strsplit(input$color,", "))]))
          p_bar <- p_bar + aes_string(col=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")"),
                              fill=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")")) +
            scale_color_manual(values=getPalette(x, v$customP)[[input$pal]], aesthetics = c("colour", "fill"), name=input$color)
        }
  
        if (input$fw != "None") {
          p_bar <- p_bar + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), nrow = input$nRowsFacets)
        }
        p_bar <- p_bar +
          stat_summary(geom="bar", fun = mean, position = "dodge", alpha = 0.3) +
          stat_summary(geom="errorbar", fun.data = mean_se, width = 0.3, position=position_dodge(0.9), colour="black") +
          geom_point(pch=21, position=position_jitterdodge(dodge.width=0.9), col="black", size=input$size_p) +
          scale_y_continuous(expand = c(0,0), limits = c(0,1.03*max(df[v$params[input$param_selector]], na.rm=T))) +
          # ggtitle(names(v$params)[match(input$parameter, v$params)]) +
          getTheme(input$theme, input$size) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ylab(paste(input$param_selector, ' [', input$auc_window[1], 'h - ', input$auc_window[2], 'h]', sep=""))
        v$p_bar <- p_bar
        return(p_bar)
        
      }else if(input$param_plot_selector == "Tile Plot") {
        
        
        p_tile <- ggplot(df, aes_string(y=input$params_y_scale, x=input$params_x_scale, fill=v$params[input$param_selector])) + 
            scale_fill_gradientn(colours=getPalette(8, v$customP)[[input$pal]], aesthetics = "fill", name=input$param_selector)

        if (input$fw != "None" && !is.null(input$referenceCurve) ) {
          if(input$referenceCurve != "None") {
            p_tile <- p_tile + facet_wrap(~facetRef, nrow = input$nRowsFacets)
          }else {
            p_tile <- p_tile + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), nrow = input$nRowsFacets)
          }
        }
        p_tile <- p_tile +
          geom_raster() +
          scale_y_discrete(expand = c(0,0)) +
          scale_x_discrete(expand = c(0,0)) +
          getTheme(input$theme, input$size) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ggtitle(paste(input$param_selector, ' [', input$auc_window[1], 'h - ', input$auc_window[2], 'h]', sep="")) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        v$p_bar <- p_tile
        return(p_tile)
        
        
        
        
      }else {
        ps <- do.call(grid.arrange, lapply(1:nrow(df), function(sample_index) {
          cp <- "green"
          if(df[sample_index, "note"] != "") {
            cp <- "red"
          }
          k <- df[sample_index, "k"]
          N0 <- df[sample_index, "n0"]
          r <- df[sample_index, "r"]
          currentWell <- df$Well[sample_index]
          p <- ggplot(dataOD, aes_string(x="time", y=sym(currentWell))) +
            geom_line(size=1.2) +
            stat_function(fun = function(t) k / (1 + ((k - N0) / N0) * exp(-r * t)), col=cp) +
            ylim(0, 1.15*max(dataOD[-1], na.rm=T)) +
            theme_classic() +
            annotate(geom = 'text', label = paste(" ", currentWell), x = -Inf, y = Inf, hjust = 0, vjust = 1, col=cp) +
            theme(axis.title=element_blank(),
                  axis.text=element_blank(),
                  axis.ticks=element_blank(),
                  panel.grid=element_blank())

          return(p)
        }))
        v$p_bar <- ps
        return(ps)
      }
    },  width=reactive(input$width*72), height = reactive(input$height*72))
  })
  
}



shinyApp(ui, server)


