
makePlot <- function(dfRaw, input, customP, ylabel="", od = T) {
  # getting upper, lower bound from SE, and calculate log10 if necessary
  df <- getUpLo(dfRaw, input$logScale && od)
  
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
    if(length(getPalette(x, customP)[[input$pal]]) < x) {
      showNotification('Insufficient color values in the selected color palette', type="error")
      return()
    }
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
      params <- list(limits=lims, expand=c(0,0), breaks=seq(log10(tmp[1]), log10(tmp[2]), 1), labels=10^seq(log10(tmp[1]), log10(tmp[2]), 1), minor_breaks = log10(exp(seq(log(tmp[1]), log(tmp[2]), length.out = abs(lims[1]-lims[2])+1))*5))
      
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
        x = ifelse(input$x_axis_title =="", "Time [h]", input$x_axis_title),
        y = ifelse(input$y_axis_title =="", "Cell Density [OD 595nm]", input$y_axis_title),)
    
  }else {
    p <- p + 
      scale_y_continuous(limits=c(0, max(df$value+df$SE, na.rm=T)*1.1), expand=c(0, 0)) +
      theme(axis.ticks = element_line(size=input$size/25), axis.ticks.length = unit(0.3, "cm")) +
      scale_x_continuous(expand=c(0,0), limits = input$range) +
      theme(axis.text.y.left = element_text(margin=margin(t=0, r=10, b=0, l=0))) +
      coord_cartesian(clip = "off") +
      labs(
        x = ifelse(input$x_axis_title =="", "Time [h]", input$x_axis_title),
        y = ifelse(input$y_axis_title =="", ylabel, input$y_axis_title)) #to modify into given parameter from name of subtable (RLU/GFP...)
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


makeParametersPlot <- function(type, df, dataOD, input, params, customP) {
  if(type == "Bar Plot") {
    p_bar <- ggplot(df, aes_string(y=params[input$param_selector], x=input$params_x_scale))
    if(input$color != 'None') {
      x <- nrow(unique(df[unlist(strsplit(input$color,", "))]))
      p_bar <- p_bar + aes_string(col=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")"),
                                  fill=paste0("interaction(", paste0(unlist(strsplit(input$color,", ")), collapse=", "),")")) +
        scale_color_manual(values=getPalette(x, customP)[[input$pal]], aesthetics = c("colour", "fill"), name=input$color)
    }
    
    if (input$fw != "None") {
      p_bar <- p_bar + facet_wrap(as.formula(paste("~", paste0(unlist(strsplit(input$fw,", ")), collapse="+"))), nrow = input$nRowsFacets)
    }
    p_bar <- p_bar +
      stat_summary(geom="bar", fun = mean, position = "dodge", alpha = 0.3) +
      stat_summary(geom="errorbar", fun.data = mean_se, width = 0.3, position=position_dodge(0.9), colour="black") +
      geom_point(pch=21, position=position_jitterdodge(dodge.width=0.9), col="black", size=input$size_p) +
      scale_y_continuous(expand = c(0,0), limits = c(0,1.03*max(df[params[input$param_selector]], na.rm=T))) +
      # ggtitle(names(v$params)[match(input$parameter, v$params)]) +
      getTheme(input$theme, input$size) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylab(paste(input$param_selector, ' [', input$auc_window[1], 'h - ', input$auc_window[2], 'h]', sep=""))
    return(p_bar)
    
  }else if(type == "Checker Plot") {
    
    
    p_tile <- ggplot(df, aes_string(y=input$params_y_scale, x=input$params_x_scale, fill=params[input$param_selector])) + 
      scale_fill_gradientn(colours=getPalette(8, customP)[[input$pal]], aesthetics = "fill", name=input$param_selector)
    
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
    return(p_tile)
    
    
    
    
  }else if (type == "Logistic Curves") { #logistic curves
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
    return(ps)
  }
}


