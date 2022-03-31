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

version <- "0.1"

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



##### Server function #####

server <- function(input, output, session) {
  v <- reactiveValues()
  toListen2 <- reactive({
    list(input$norm,input$norm_TECAN, input$files)
  })
  v$customP <- NULL
  getPalette = function(x, pal=NULL) {
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
    #Called when normalization input is modified, or when new files is uploaded
    if(!is.null(v$rawdata)) {
      v$df[2:ncol(v$df)] = normalize(isolate(v$rawdata), isolate(input$norm), isolate(input$norm_TECAN))
    }
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
          selectInput('norm_TECAN', 'Base OD', choices=c(0, 0.001, 0.002, 0.003, 0.004), selected = 0.001),
          textInput('conditionsUI_Input', 'Enter Conditions:', value="", placeholder = "Strain, Treatment, ..."),
          uiOutput('error_condition'),
          column(12, align="center", p('(Cannot start with number, separated by coma)')),     
          column(12, align="center", actionButton('updateCond', 'Update'))
        )
      )
    })
    if(!is.null(v$rawdata)) {
      lapply(react, function(reactVal){
        v[[reactVal]] <- NULL
      })
    }
    
    tryCatch(
      {
        v$rawdata_list <- do.call(list, lapply(input$files$datapath, function(file) {
          obj <- getFile(file)
          return(obj)
        }))
        names(v$rawdata_list) <- input$files$name
        if(length(input$files$datapath) > 1) {
          output$multifiles_options <- renderUI({
            fluidPage(
              list(
                selectInput('merginFiles', "Files merging method:", choices=c("Standard", "Biological Replicate")),
                numericInput('timeScale', "Time Measurement Interval (min):", value = 10, min=5, max=60, step = 5)
              )
            )
          })
        }else {
          output$multifiles_options <- NULL
          v$rawdata <- v$rawdata_list[[1]]$ODdata
          v$df <-  v$rawdata
          v$RLU <- v$rawdata_list[[1]]$RLUdata
          v$conditions_old <- c()
        }
        output$table_preview <- renderRHandsontable({
          req(v$df)
          if(!is.null(v$df)) {
            rhandsontable(v$df, allowInvalid=T, height = "80%", readOnly = T) %>%
              hot_cols(format = "0.00") %>% 
              hot_table(highlightCol = T, highlightRow = T, minSpareRows = 1)
          }
        })
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  toListenMergin <- reactive({
    list(input$merginFiles, input$timeScale)
  })
  
  observeEvent(toListenMergin(), {
    req(input$merginFiles)
    v$rawdata <- do.call(cbind, lapply(isolate(input$files$name), function(listName) {
      df <- isolate(v$rawdata_list[[listName]][["ODdata"]])
      #Removing time column, cause will create a new one based on user's input
      df <- df[-1]
      colnames(df) <- paste(tools::file_path_sans_ext(listName), colnames(df), sep="_")      
      return(df)
    }))
    v$rawdata <- cbind(time=(input$timeScale*0:(nrow(v$rawdata)-1))/60, isolate(v$rawdata))
    
    v$df <-  v$rawdata
    v$df[2:ncol(v$df)] = normalize(isolate(v$rawdata), isolate(input$norm), isolate(input$norm_TECAN))
    v$conditions_old <- c()
    
  })
  
  ##### Update button Panel #####
  observeEvent(input$updateCond, {
    req(input$conditionsUI_Input)
    v$conditions <- str_trim(unlist(strsplit(input$conditionsUI_Input,",")))
    v$conditions <- gsub("^[0-9]+", '', v$conditions)
    v$conditions <- gsub("KeepWell|SE|time|value|Wells|Preview", '', v$conditions)
    v$conditions <- gsub(" ", "_", v$conditions)
    v$conditions <- unique(v$conditions[v$conditions != ""])
    if(is.null(v$groups)) {
      v$groups <-  data.frame(Wells = colnames(v$df[,-1]), KeepWell="Yes", Preview=NA, stringsAsFactors = F)
    }
    lapply(v$conditions, function(cond) {
      if(!(cond  %in% v$conditions_old)) {
        v$groups[cond] <- "NA"
      }
    })
    if(!is.null(v$groups2)) {
      v$groups[names(v$groups2)] <- v$groups2
    }
    x <- which(!(colnames(v$groups[-c(1,2,3)]) %in% v$conditions)) + 3
    if(length(x) >= 1) {
      v$groups <- v$groups[-x]
    }
    v$conditions_old <- v$conditions
    if(length(v$conditions) > 1) {
      v$interactions <- unlist(apply(combn(v$conditions,2), 2, function(x) {return(paste(x, collapse=", "))}))
    }else {
      v$interactions <- c()
    }
    #SapLine to preview the growth curve of each well
    v$groups$Preview <- apply(v$df[-1], 2, function(x) jsonlite::toJSON(list(values=as.vector(x), options=list(type="line", spotRadius=0, chartRangeMin=0, chartRangeMax=1))))
    
    output$groups <- renderRHandsontable({
      req(v$groups)
      rhandsontable(
        data.frame(v$groups), 
        fillHandle = list(direction='vertical', autoInsertRow=FALSE),
        maxRows = ncol(isolate(v$df))-1,
        useTypes=T) %>%
        hot_col(col="Wells", readOnly = T, allowRowEdit =F, allowColEdit = F) %>%
        hot_col(col="KeepWell", type="dropdown", source=c("Yes", "No"), strict=T, allowInvalid=F, valign='htCenter') %>%
        hot_col(col="Preview",copyable=F, renderer=htmlwidgets::JS("renderSparkline"), valign='htCenter', allowColEdit=F, readOnly=T) %>%
        hot_table(highlightCol = T, highlightRow = T, allowRowEdit =F)
    })
    ##### UI graph options
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
              uiOutput("refCurveUI"),
              uiOutput('RLUoptions')
            ),
            column(
              width=12,
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
            sliderInput('range', 'X axis range:', min=0, step=0.5, max=ceiling(v$df[nrow(v$df),1]), value=c(0, ceiling(v$df[nrow(v$df),1]))),
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
    req(v$RLU)
    output$RLUoptions <- renderUI({
      list(
        selectInput('RLUplotSelector', 'RLU Plots:', choices=c("No", "Only RLU", "Both OD & RLU"), width='100%')
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
  
  observeEvent(input$groups, {
    ##### OD Data #####
    req(input$groups)
    v$groups2 <- data.frame(hot_to_r(input$groups))
    v$groups3 <- isolate(v$groups2[v$groups2$KeepWell == "Yes",])
    v$groups3[v$conditions] <- lapply(v$groups3[v$conditions] , factor)
    if(nrow(v$groups3) == 0) {
      return()
    }
    v$data <-  v$df[which(names(v$df) %in% c(v$groups3$Wells, "time"))]
    v$data <- v$data[which(v$data$time >= input$range[1] & v$data$time <= input$range[2]),]
    v$df_melt = melt(v$df[which(names(v$df) %in% c(v$groups3$Wells, "time"))], id=c('time'))
    ind <- as.vector(match(v$df_melt$variable, v$groups3$Wells))
    lapply(v$conditions, function(cond) {
      v$df_melt[cond] <-  as.factor(as.vector(v$groups3[,cond])[ind])
    })
    v$df_melt <- cbind(
      aggregate(v$df_melt$value, by=v$df_melt[c("time", v$conditions)], FUN=mean),
      aggregate(v$df_melt$value, by=v$df_melt[c("time", v$conditions)], function(x) sd(x)/sqrt(length(x)))[length(v$conditions)+2]
    )
    colnames(v$df_melt)  <-  c("time", v$conditions, "value", "SE")
    
    
    ##### RLU Data #####
    
    if(!is.null(v$RLU)) {
      dataOD <- v$data
      RLU <- v$RLU
      RLU <- cbind(dataOD[1], RLU/dataOD[-1])
      
      RLU <- RLU[which(RLU$time >= input$range[1] & RLU$time <= input$range[2]),]
      RLU_melt <- melt(RLU[which(names(RLU) %in% c(v$groups3$Wells, "time"))], id=c('time'))
      ind <- as.vector(match(RLU_melt$variable, v$groups3$Wells))
      for(cond in v$conditions) {
        RLU_melt[cond] <- as.factor(as.vector(v$groups3[,cond])[ind])
      }
      RLU_melt <- cbind(
        aggregate(RLU_melt$value, by=RLU_melt[c("time", v$conditions)], FUN=mean),
        aggregate(RLU_melt$value, by=RLU_melt[c("time", v$conditions)], function(x) sd(x)/sqrt(length(x)))[length(v$conditions)+2]
      )
      colnames(RLU_melt)  <-  c("time", v$conditions, "RLU", "SE")
      v$RLU_melt <- RLU_melt
      
      req(input$lvlOrderSelect)
    }
    
    
    
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
      filename = function() {
        "dataframe.csv"
      },
      content = function(file) {
        write.csv(v$df_melt, file, row.names = FALSE)
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
            sliderInput('yAxisRange', 'Y axis range:', min=0, step=0.1, max=ceiling(max(v$df_melt$value)), value=c(0, ceiling(max(v$df_melt$value))))
          )
        })
      }
      
    })
    observeEvent(input$lvlOrderSelect, {
      output$levelOrderUI <- renderUI({
        list(
          rank_list(
            text = paste(input$lvlOrderSelect, ' levels order:'),
            labels = isolate(levels(v$groups3[[input$lvlOrderSelect]])),
            input_id = "lvlOrderSorted",
            options = sortable_options(),
            class = "default-sortable"
          )
        )
      })
    })
    
  })
  
  
  observeEvent(v$df_melt, {
    output$plot <- renderPlot({
      req(input$yAxisRange)
      v$RLU_melt
      v$customP
      v$themes_map <- list(
        "BW" = theme_bw(base_size = input$size),      
        "Classic" = theme_classic(base_size = input$size),
        "Light" = theme_light(base_size = input$size),
        "Minimal" = theme_minimal(base_size = input$size),
        "Gray" = theme_gray(base_size = input$size)
      )
      
      df <- v$df_melt
      
      if(!is.null(input$referenceCurve) && input$referenceCurve != "None") {
        #modifying df_melt by adding a new group column, which will act as facetwrap separator, and duplicate the input$referenceCurve with levels names same as other
        ref_df <- df[df[input$fw] == input$referenceCurve,]
        newDF <- df[df[input$fw] != input$referenceCurve,]
        newDF$facetRef = newDF[[input$fw]]
        ref_df <- do.call(rbind, lapply(levels(newDF[[input$fw]]), function(level) {
          if(level != input$referenceCurve) {
            df = ref_df
            df$facetRef = level
            return(df)
          }
        }))
        newDF <- rbind(newDF, ref_df)
        df <- newDF
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
      p <-ggplot(data=df, aes_string(x="time", y="value"))
      
      
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
          #modifying df_melt by adding a new group column, which will act as facetwrap separator, and duplicate the input$referenceCurve with levels names same as other
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
      
      ##### RLU Plot #####
      if(!is.null(v$RLU) && input$RLUplotSelector != 'No') {
        req(v$RLU_melt)
        dfRLU <- v$RLU_melt
        dfRLU$Upper <- dfRLU$RLU+dfRLU$SE
        dfRLU$Lower <- dfRLU$RLU-dfRLU$SE
        
        
        if(!is.null(input$referenceCurve) && input$referenceCurve != "None") {
          #modifying df_melt by adding a new group column, which will act as facetwrap separator, and duplicate the input$referenceCurve with levels names same as other
          ref_df <- dfRLU[dfRLU[input$fw] == input$referenceCurve,]
          newDF <- dfRLU[dfRLU[input$fw] != input$referenceCurve,]
          newDF$facetRef = newDF[[input$fw]]
          
          ref_df <- do.call(rbind, lapply(levels(newDF[[input$fw]]), function(level) {
            if(level != input$referenceCurve) {
              df <- ref_df
              df$facetRef <- level
              return(df)
            }
          }))
          newDF <- rbind(newDF, ref_df)
          dfRLU <- newDF
        }
        
        
        pRLU <- ggplot(data=dfRLU, aes_string(x="time", y="RLU"))
        if(input$se == "Line Range") {
          pRLU <- pRLU + geom_linerange(aes(ymin=Lower, ymax=Upper))
          
        }else if(input$se == "Ribbon") {
          pRLU <- pRLU + geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2, colour=NA)
        }
        if(input$color != 'None') {
          
          x <- nrow(unique(v$groups3[unlist(strsplit(input$color,", "))]))
          pRLU <- pRLU + aes_string(col=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")"),
                                    fill=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")")) + 
            scale_color_manual(values=getPalette(x, input$pal), aesthetics = c("colour", "fill"), name=input$color)
        }
        pRLU <- pRLU + geom_line(size=input$size_l)
        if (input$fw != "None") {
          if(!is.null(input$referenceCurve) && input$referenceCurve != "None") {
            #modifying df_melt by adding a new group column, which will act as facetwrap separator, and duplicate the input$referenceCurve with levels names same as other
            pRLU <- pRLU + facet_wrap(~facetRef, scales="free", nrow=1)
          }else {
            pRLU <- pRLU + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), scales="free", nrow=1)
          }
        }
        
        if(input$grouping != "None") {
          pRLU <- pRLU +aes_string(group=paste0("interaction(", paste0(unlist(strsplit(input$grouping,", ")), collapse=", "),")"))
        }
        pRLU <- pRLU + v$themes_map[[input$theme]] + 
          scale_y_continuous(limits=c(0, max(dfRLU$RLU+dfRLU$SE)), expand=c(0, 0)) +
          theme(axis.ticks = element_line(size=input$size/25), axis.ticks.length = unit(0.3, "cm")) +
          scale_x_continuous(expand=c(0,0), limits = input$range) +
          theme(axis.text.y.left = element_text(margin=margin(t=0, r=10, b=0, l=0))) +
          coord_cartesian(clip = "off") +
          labs(
            x ="Time [h]",
            y = "RLU/OD")
        if(!is.null(input$customThemeUI) && nchar(input$customThemeUI) > 0) {
          str = paste0("list(", input$customThemeUI, ")")
          if(!inherits(try(p + eval(parse(text=str)), silent=TRUE), "try-error")) {
            pRLU <- pRLU + eval(parse(text=str))
          }
        }
      }
      
      if(!is.null(v$RLU) && input$RLUplotSelector == 'Only RLU') {
        p <- pRLU
      }else if(!is.null(v$RLU) && input$RLUplotSelector == 'Both OD & RLU') {
        
        p <- ggarrange(p, pRLU,
                       ncol = 1, nrow = 2, align = "v")
      }      
      v$p <- p
      return(p)
    }, width=reactive(input$width*72), height = reactive(input$height*72))
    
  })
  
  
  toListen3 <- reactive({
    list(input$fw, input$groups)
  })
  observeEvent(toListen3(), {
    req(input$fw)
    #Only if facetWrap is select, and at least two different levels in that column 
    if(input$fw != "None" && length(isolate(levels(v$groups3[[input$fw]]))) > 1){
      output$refCurveUI <- renderUI({
        list(
          selectInput('referenceCurve', 'Reference Facet (copied on each facet):', choices = c("None", isolate(levels(v$groups3[[input$fw]]))), width='100%')
        )
      })
    }
  })
  
  observeEvent(input$lvlOrderSorted, {
    v$groups3[input$lvlOrderSelect] <- isolate(factor(v$groups3[[input$lvlOrderSelect]], levels=c(input$lvlOrderSorted)))
    v$df_melt[input$lvlOrderSelect] <- isolate(factor(v$df_melt[[input$lvlOrderSelect]], levels=c(input$lvlOrderSorted)))
    v$RLU_melt[input$lvlOrderSelect] <- isolate(factor(v$RLU_melt[[input$lvlOrderSelect]], levels=c(input$lvlOrderSorted)))
  })
  
  observeEvent(input$range, {
    output$gro_opt <- renderUI({
      fluidPage(
        list(
          sliderInput('auc_window', 'Window range AUC [h]:', min=0, step=0.5, max=ceiling(v$df[nrow(v$df),1]), value=c(0, ifelse(floor(v$df[nrow(v$df),1])-1>6, 6, floor(v$df[nrow(v$df),1]-1)))),
          selectInput('x_scale', 'X axis group:', choices=v$conditions, width='100%'),
          selectInput('y_scale', 'Y axis group:', choices=v$conditions, width='100%'),
          selectInput('x_fill', 'Fill:', choices=c(v$conditions, v$interactions), width='100%'),
          selectInput('x_facet_wrap', 'Facet wrap:', choices=c("None", v$conditions), width='100%')
        )
      )
    })
  })
  
  
  
  
  toListen <- reactive({
    list(v$data, v$groups3, input$auc_window)
  })
  
  
  ##### AUC Data and Plots #####
  observeEvent(toListen(), {
    req(v$data, v$groups3, input$auc_window)
    
    #Calculate AUC of each well, based on the window size defined by the user, cbind to existing groups with condition
    v$aucs <- cbind(data.frame(do.call(rbind, lapply(v$groups3$Wells, function(well) {
      return(c(well, getAUC(v$data$time, v$data[[well]], input$auc_window)))
    }))), v$groups3[v$conditions])
    colnames(v$aucs) <- c("Well", "AUC", v$conditions)
    #only one parameter for now
    v$aucs$AUC <- as.numeric(v$aucs$AUC)
    
    
    
    
    
    output$bar_graph <- renderPlot({
      req(v$aucs)
      v$themes_map <- list(
        "BW" = theme_bw(base_size = input$size),      
        "Classic" = theme_classic(base_size = input$size),
        "Light" = theme_light(base_size = input$size),
        "Minimal" = theme_minimal(base_size = input$size),
        "Gray" = theme_gray(base_size = input$size)
      )
      p_bar <- ggplot(v$aucs, aes_string(y="AUC", x=input$x_scale))
      if(input$x_fill != 'None') {
        x <- nrow(unique(v$groups3[unlist(strsplit(input$x_fill,", "))]))
        p_bar <- p_bar + aes_string(col=paste0("interaction(", paste0(unlist(strsplit(input$x_fill,", ")), collapse=", "),")"),
                                    fill=paste0("interaction(", paste0(unlist(strsplit(input$x_fill,", ")), collapse=", "),")")) + 
          scale_color_manual(values=getPalette(x, input$pal), aesthetics = c("colour", "fill"), name=input$x_fill)
      }
      
      if (input$x_facet_wrap != "None") {
        p_bar <- p_bar + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$x_facet_wrap,", ")), collapse="+"))), scales="free")
      }
      p_bar <- p_bar +
        stat_summary(geom="bar", fun = mean, position = "dodge", alpha = 0.3) +
        stat_summary(geom="errorbar", fun.data = mean_se, width = 0.3, position=position_dodge(0.9), colour="black") +
        geom_point(pch=21, position=position_jitterdodge(dodge.width=0.9), col="black", size=input$size_p) +
        scale_y_continuous(expand = c(0,0), limits = c(0,1.03*max(v$aucs$AUC))) +
        ggtitle(names(v$params)[match(input$parameter, v$params)]) +
        v$themes_map[[input$theme]] +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab(paste('AUC [', input$auc_window[1], 'h - ', input$auc_window[2], 'h]', sep=""))
      v$p_bar <- p_bar
      return(p_bar)
    },  width=reactive(input$width*72), height = reactive(input$height*72))
    
    
    # output$tile_graph <- renderPlot({
    #   req(v$aucs)
    #   v$themes_map <- list(
    #     "BW" = theme_bw(base_size = input$size),      
    #     "Classic" = theme_classic(base_size = input$size),
    #     "Light" = theme_light(base_size = input$size),
    #     "Minimal" = theme_minimal(base_size = input$size),
    #     "Gray" = theme_gray(base_size = input$size)
    #   )
    #   p_tile <- ggplot(v$aucs, aes_string(y=input$y_scale, x=input$x_scale, fill="AUC")) + 
    #     scale_fill_gradientn(colours=getPalette(8, input$pal), aesthetics = "fill", name="AUC")
    #   
    #   if (input$x_facet_wrap != "None") {
    #     p_tile <- p_tile + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$x_facet_wrap,", ")), collapse="+"))), scales="free")
    #   }
    #   p_tile <- p_tile +
    #     geom_raster() +
    #     scale_y_discrete(expand = c(0,0)) +
    #     scale_x_discrete(expand = c(0,0)) +
    #     v$themes_map[[input$theme]] +
    #     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #     ggtitle(paste('AUC [', input$auc_window[1], 'h - ', input$auc_window[2], 'h]', sep="")) + 
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    #   v$p_tile <- p_tile
    #   return(p_tile)
    # },  width=reactive(input$width*72), height = reactive(input$height*72))
    output$downloadquc <- downloadHandler(
      filename = function() {
        "AUC_data.csv"
      },
      content = function(file) {
        write.csv(v$aucs, file, row.names = FALSE)
      }
    )
    output$downloadquc_plot <- downloadHandler(
      filename = function(){paste(input$title, "_", names(v$params)[match(input$parameter, v$params)], '.pdf',sep='')},
      content = function(file){
        ggsave(file,plot=v$p_bar, width=input$width, height=input$height, units="in", dpi=300, device=cairo_pdf)
      }
    )
    
    # output$downloadquc_tileplot <- downloadHandler(
    #   filename = function(){paste(input$title, "_", names(v$params)[match(input$parameter, v$params)], '.pdf',sep='')},
    #   content = function(file){
    #     ggsave(file,plot=v$p_tile, width=input$width, height=input$height, units="in", dpi=300, device=cairo_pdf)
    #   }
    # )
  })
  
  toListenRLUPlot <- reactive({
    list(v$RLU_melt, input$color, input$pal, input$se, input$referenceCurve, input$grouping, input$fw, input$RLUplotSelector)
  })
  
  # If closing the window, close the file
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
}


shinyApp(ui, server)



