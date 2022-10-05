
makePlot <- function(dfRaw, input, customP, ylabel="", od = T, dfSecRaw = NULL) {
  # getting upper, lower bound from SE, and calculate log10 if necessary
  df <- getUpLo(dfRaw, input$logScale && od)
  
  # #Determining coefficient
  # if(!is.null(dfSecRaw)) {
  #   name <- input$plot_selector
  #   dfSec <- getUpLo(dfSecRaw, log=F)
  #   dfSec <- dfSec[(ncol(dfSec)-3):ncol(dfSec)]
  #   coeff <- max(dfSec$value)/max(dfRaw$value)
  #   if(input$logScale) {
  #     dfSec$value <- log10(dfSec$value/coeff)
  #     dfSec$Upper <- log10(dfSec$Upper/coeff)
  #     dfSec$Lower <- log10(dfSec$Lower/coeff)
  #   }else {
  #     dfSec$value <- dfSec$value/coeff
  #     dfSec$Upper <- dfSec$Upper/coeff
  #     dfSec$Lower <- dfSec$Lower/coeff
  #   }
  #   colnames(dfSec) <- paste0(name, ".", colnames(dfSec))
  #   df <- cbind(df, dfSec)
  # }
  
  
  if(!is.null(input$referenceCurve) && input$referenceCurve != "None") {
    df <- addReferenceCurve(input$referenceCurve, input$fw, df)
    # if(!is.null(dfSec)) {
    #   dfSec <- addReferenceCurve(input$referenceCurve, input$fw, dfSec)
    # }
  }
  
  
  p <- ggplot(data=df, aes_string(x="time", y="value"))
  
  
  if(input$se == "Line Range") {
    p <- p + geom_linerange(aes(ymin=Lower, ymax=Upper))
    
  }else if(input$se == "Ribbon") {
    p <- p + geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2, colour=NA)
  }
  if(input$color != 'None') {
    x <- nrow(unique(df[unlist(strsplit(input$color,", "))]))
    p <- p + aes_string(col=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")"),
                        fill=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")")) +
      scale_color_manual(values=getPalette(x, customP)[[input$pal]], aesthetics = c("colour", "fill"), name=input$color)
  }
  
  

  if(input$linetype != "None") {
    p <- p + aes_string(linetype=input$linetype) +
      geom_line( size=input$size_l)
  }else if(input$shape == "None") {
    p <- p + geom_line(size=input$size_l)
  }
  

  if (input$fw != "None" && !is.null(input$referenceCurve) ) {
    if(input$referenceCurve != "None") {
      p <- p + facet_wrap(~facetRef, nrow = input$nRowsFacets)
    }else {
      p <- p + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), nrow = input$nRowsFacets)
    }
  }
  if(input$shape != "None") {
    p <- p + aes_string(shape=input$shape) +
      geom_point(size=input$size_p)
  }
  
  if(input$grouping != "None") {
    p <- p +aes_string(group=paste0("interaction(", paste0(unlist(strsplit(input$grouping,", ")), collapse=", "),")"))
  }
  

  
  
  

  p <- p + getTheme(input$theme, input$size)
  
  if(od) {
    if(input$logScale) {
      tmp = as.numeric(input$yAxisRange)
      if(!tmp[1]) {
        return(NULL)
      }
      lims <- c(log10(tmp[1]), log10(tmp[2]))
      params <- list(limits=lims, expand=c(0,0), breaks=seq(log10(tmp[1]), log10(tmp[2]), 1), labels=10^seq(log10(tmp[1]), log10(tmp[2]), 1))
      
      if (input$fw != "None" & !is.null(input$nRowsFacets)) {
        a <- annotation_logticks( sides = "l", size = input$size/25, colour="black", outside=T, mid=unit(0.3, "cm"), long=unit(0.4, "cm"), short=unit(0.2, "cm"))
        a$data <- getDFlogticks(input$fw, df, input$nRowsFacets, a$data, input$referenceCurve)
        p <- p + a
      }else {
        p <- p + annotation_logticks( sides = "l", size = input$size/25, colour="black", outside=T, mid=unit(0.3, "cm"), long=unit(0.4, "cm"), short=unit(0.2, "cm"))
      }

    }else {
      tmp = round(as.numeric(input$yAxisRange), 1)
      lims <- c(tmp[1], tmp[2])
      params <- list(limits=lims, expand=c(0, 0))
      p <- p + theme(axis.ticks = element_line(size=input$size/25), axis.ticks.length = unit(0.3, "cm"))
    }

    p <- p + do.call(scale_y_continuous,params)

    p <-  p +
      scale_x_continuous(expand=c(0,0), limits = input$range) +
      theme(axis.text.y.left = element_text(margin=margin(t=0, r=10, b=0, l=0))) +
      coord_cartesian(clip = "off") +
      labs(
        linetype=input$linetype,
        shape=input$shape,
        x ="Time [h]",
        y = "Cell Density [OD 595nm]")
    
    
    
  }else {
    p <- p + 
      scale_y_continuous(limits=c(0, max(df$value+df$SE, na.rm=T)), expand=c(0, 0)) +
      theme(axis.ticks = element_line(size=input$size/25), axis.ticks.length = unit(0.3, "cm")) +
      scale_x_continuous(expand=c(0,0), limits = input$range) +
      theme(axis.text.y.left = element_text(margin=margin(t=0, r=10, b=0, l=0))) +
      coord_cartesian(clip = "off") +
      labs(
        x ="Time [h]",
        y = ylabel) #to modify into given parameter from name of subtable (RLU/GFP...)
  }
  
  if(!is.null(input$customThemeUI) && nchar(input$customThemeUI) > 0) {
    str = paste0("list(", input$customThemeUI, ")")
    if(inherits(try(p + eval(parse(text=str)), silent=TRUE), "try-error")) {
      showNotification(p + eval(parse(text=str)))
    }else {
      p <- p + eval(parse(text=str))
    }
  }
  return(p)
}