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
library(xlsx)
source('utility.R', local = TRUE)

version <- "0.1"
##### UI #####
ui = dashboardPage(
  dashboardHeader(title = "TECAN Extractor"),
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



##### Server function #####
server <- function(input, output, session) {
  v <- reactiveValues()
  toListen2 <- reactive({
    list(input$norm,input$norm_baseOD, input$files)
  })
  v$customP <- NULL
  getPalette <- function(x, pal=NULL) {
    ps = list(
      "Viridis" = viridis_pal(option = "viridis")(x),
      "Magma" = viridis_pal(option = "magma")(x),
      "Plasma" = viridis_pal(option = "plasma")(x),
      "Cividis" = viridis_pal(option = "cividis")(x),
      "Blues (8 max)" = brewer_pal(palette = "Blues")(ifelse(x>8,8,x)),
      "Reds (8 max)" = brewer_pal(palette = "Reds")(ifelse(x>8,8,x)),
      "Paired (8 max)" = brewer_pal(palette = "Paired")(ifelse(x>8,8,x)),
      "Set1 (8 max)" = brewer_pal(palette = "Set1")(ifelse(x>8,8,x)),
      "Set2 (8 max)" = brewer_pal(palette = "Set2")(ifelse(x>8,8,x)),
      "Set3 (8 max)" = brewer_pal(palette = "Set3")(ifelse(x>8,8,x)),
      "Dark2 (8 max)" = brewer_pal(palette = "Dark2")(ifelse(x>8,8,x)),
      "Pastel (4 max)" = c( "#00798c","#66a182", "#edae49", "#d1495b")
    )
    if(!is.null(isolate(v$customP))) {
      ps <- c(
        list(
          "Custom" = isolate(v$customP)
        ),
        ps
      )
    }
    if(is.null(pal)) {
      return(ps)
    }
    
    return(ps[[pal]])
  }
  
  
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
    if(length(input$files$name) == 1) {
      v$rawdata_list <- rawdata_file_list[[1]]
    }else {
      v$rawdata_list$time <- rawdata_file_list[[1]][[1]]
      for (i in names(rawdata_file_list[[1]])[-1]) {
        v$rawdata_list[[i]] <- do.call(cbind, lapply(isolate(input$files$name), function(filename) {
          df <- isolate(rawdata_file_list[[filename]][[i]])
          colnames(df) <- paste(tools::file_path_sans_ext(filename), colnames(df), sep="_")
          return(df)
        }))
      }
    }
    #Changing the name of the standard OD to OD. The rest will stay custom, as it can be RLU/Luminescence etc...
    names(v$rawdata_list)[2] <- "OD"
    
  })

  ##### Update button Panel #####
  observeEvent(input$updateCond, {
    req(input$conditionsUI_Input)
    
    
    v$conditions <- formartConditions(input$conditionsUI_Input)
    
    
    #v$groups is a dataframe containing all levels for each condition for each well
    if(!is.null(v$groups)) {
      v$groups[colnames(data.frame(hot_to_r(input$groups)))] <- data.frame(hot_to_r(input$groups))
    }
    
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
              selectInput('color', 'Color:', choices=c("None", v$conditions, v$interactions), width='100%'),
              selectInput('linetype', 'Linetype:', choices=c("None", v$conditions), width='100%'),
              selectInput('shape', 'Shape:', choices=c("None", v$conditions), width='100%'),
              selectInput('grouping', 'Grouping:', choices=c("None", v$interactions), width='100%'),
              selectInput('fw', 'Facet Wrap:', choices=c("None", v$conditions, v$interactions), width='100%'),
              uiOutput("refCurveUI")
            ),
            column(
              width=12,
              selectInput('plotSelector', 'Plot Selector:', choices=c(names(v$dataList)[-1], getInteractions(names(v$dataList)[-1])), width='100%'),
              selectInput('se', 'Standart Error Style:', choices=c("None", "Line Range", "Ribbon"), width='100%'),
              
              selectInput('lvlOrderSelect', "Order Levels :", choices = c(v$conditions), width='100%'),
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
            textInput('customColorPalette', 'Custom Color Palette:', value="", placeholder = "#4b123f, #cb13b2, ...")
          ),
          splitLayout(
            numericInput('height', 'Height (Inches):', value=5, min = 4, max = 50, step = 1),
            numericInput('width', 'Width: (Inches)', value=10, min = 4, max = 22, step = 1),
            checkboxInput('logScale', 'Log10 Transformation', value = T)
            
          ),
          splitLayout(
            sliderInput('range', 'X axis range:', min=0, step=0.5, max=ceiling(max(v$dataList$time)), value=c(0, ceiling(max(v$dataList$time)))),
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
  
  observeEvent(input$customColorPalette, {
    vec <- gsub(" ", "", unlist(strsplit(input$customColorPalette, ",")))
    if(isHex(vec)) {
      v$customP <- vec
      updatePalettePicker(inputId = "pal", choices = getPalette(8))
    }
  })

  ##### Groups #####
  observeEvent(input$groups, {
    #This is called everytime the input$groups table gets modified by the user
    req(input$groups)

    v$groupsDF <- data.frame(hot_to_r(input$groups))
    v$groupsDF[-c(1,2,3)] <- lapply(v$groupsDF[-c(1,2,3)], factor)
    v$dataList_melted <- dataMelter(v$dataList, v$groupsDF, input$range)
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
    

    
    ##### Log Scale UI #####
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
  
  observeEvent(input$lvlOrderSorted, {
    v$groupsDF[input$lvlOrderSelect] <- factor(v$groupsDF[[input$lvlOrderSelect]], levels = c(input$lvlOrderSorted))
    
    #update dataframes levels based on level order
    v$dataList_melted <- lapply(v$dataList_melted, function(df) {
      df[input$lvlOrderSelect] <- factor(df[[input$lvlOrderSelect]], levels=c(input$lvlOrderSorted))
      return(df)
    })

  })


  observeEvent(v$dataList_melted, {
    output$plot <- renderPlot({
      req(input$yAxisRange)
      v$customP
      v$themes_map <- list(
        "BW" = theme_bw(base_size = input$size),
        "Classic" = theme_classic(base_size = input$size),
        "Light" = theme_light(base_size = input$size),
        "Minimal" = theme_minimal(base_size = input$size),
        "Gray" = theme_gray(base_size = input$size)
      )

      
      df <- v$dataList_melted[[1]]
      
      
      if(!is.null(input$referenceCurve) && input$referenceCurve != "None") {
        df <- addReferenceCurve(input$referenceCurve, input$fw, df)
      }

      if(input$logScale) {
        df$Upper <- log10(df$value+df$SE)
        df$Lower <- log10(df$value-df$SE)
        df$value <- log10(df$value)
      }else {
        df$Upper <- df$value+df$SE
        df$Lower <- df$value-df$SE
        df$value <- df$value
      }
      ##### OD Plot #####
      p <- ggplot(data=df, aes_string(x="time", y="value"))


      if(input$se == "Line Range") {
        p <- p + geom_linerange(aes(ymin=Lower, ymax=Upper))

      }else if(input$se == "Ribbon") {
        p <- p + geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2, colour=NA)
      }
      if(input$color != 'None') {

        x <- nrow(unique(v$groups3[unlist(strsplit(input$color,", "))]))
        p <- p + aes_string(col=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")"),
                            fill=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")")) +
          scale_color_manual(values=getPalette(x, input$pal), aesthetics = c("colour", "fill"), name=input$color)
      }
      if(input$linetype != "None") {
        p <- p + aes_string(linetype=input$linetype) +
          geom_line( size=input$size_l)
      }else if(input$shape == "None") {
        p <- p + geom_line(size=input$size_l)
      }
      if (input$fw != "None") {
        if(!is.null(input$referenceCurve) && input$referenceCurve != "None") {
          if(!is.null(v$RLU) && input$RLUplotSelector == 'Both OD & RLU') {
            p <- p + facet_wrap(~facetRef, scales="free", nrow = 1)
          }else {
            p <- p + facet_wrap(~facetRef, scales="free")

          }
        }else {
          if(!is.null(v$RLU) && input$RLUplotSelector == 'Both OD & RLU') {
            p <- p + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), scales="free", nrow = 1)
          }else {
            p <- p + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), scales="free")

          }
        }
      }

      if(input$shape != "None") {
        p <- p + aes_string(shape=input$shape) +
          geom_point(size=input$size_p)
      }
      if(input$grouping != "None") {
        p <- p +aes_string(group=paste0("interaction(", paste0(unlist(strsplit(input$grouping,", ")), collapse=", "),")"))
      }
      p <- p + v$themes_map[[input$theme]]
      if(input$logScale) {
        tmp = as.numeric(input$yAxisRange)
        if(tmp[1] == 0) {
          return()
        }
        p <-  p +
          scale_y_continuous(limits=c(log10(tmp[1]), log10(tmp[2])), expand=c(0,0), breaks=seq(log10(tmp[1]), log10(tmp[2]), 1), labels=10^seq(log10(tmp[1]), log10(tmp[2]), 1)) +
          annotation_logticks(sides = "l", size = input$size/25, colour="black", outside=T, mid=unit(0.3, "cm"), long=unit(0.4, "cm"), short=unit(0.2, "cm"))
      }else {
        tmp = round(as.numeric(input$yAxisRange), 1)
        p <- p +
          scale_y_continuous(limits=c(tmp[1], tmp[2]), expand=c(0, 0)) +
          theme(axis.ticks = element_line(size=input$size/25),
                axis.ticks.length = unit(0.3, "cm"))
      }

      p <-  p +
        scale_x_continuous(expand=c(0,0), limits = input$range) +
        theme(axis.text.y.left = element_text(margin=margin(t=0, r=10, b=0, l=0))) +
        coord_cartesian(clip = "off") +
        labs(
          linetype=input$linetype,
          shape=input$shape,
          x ="Time [h]",
          y = "Cell Density [OD 595nm]")
      if(!is.null(input$customThemeUI) && nchar(input$customThemeUI) > 0) {
        str = paste0("list(", input$customThemeUI, ")")
        if(inherits(try(p + eval(parse(text=str)), silent=TRUE), "try-error")) {
          showNotification(p + eval(parse(text=str)))
        }else {
          p <- p + eval(parse(text=str))
        }
      }
      v$p <- p
      return(p)
    }, width=reactive(input$width*72), height = reactive(input$height*72))

  })


}


shinyApp(ui, server)



