themes <- c("BW" , "Classic", "Light", "Minimal", "Gray")
react <- c("conditions", "rawdata_list", "dataList", "groups", "interactions", "params_list", "groupDF_subset")

getFile <- function(fileData, lb, nfiles) {
  rawTableList <- list()
  if(file_ext(fileData$datapath) == "txt") {
    rawdata <-  read.table(fileData$datapath, sep="\t", header=T, dec=",")
    rawdata <- rawdata[,colSums(is.na(rawdata))<nrow(rawdata)]
    rawdata <- rawdata[complete.cases(rawdata[,1]),]
    rawdata <- as.data.frame(sapply(rawdata, as.numeric))
    rawTableList$time <- rawdata$time
    rawTableList[["OD"]] <- rawdata[-which(names(rawdata) == "time")]
    
  }else {
    rawdata <- read.xlsx2(file=fileData$datapath, sheetIndex=1, as.data.frame=T, header=F, colIndex = 1:300)
    #rawTableList is a list containing each table, starting with OD, and any additional measurement table, such as RLU, luminescence, or another OD...
    #If tecan is biotek
    biotek <- F
    if(any(grepl("Synergy", rawdata))) {
      #First column empty in synergy software => might wanna double check that
      indexStart <- which(rawdata[-1]=='Time')
      if(any(which(rawdata == "Results"))) {
        indexStop <- which(rawdata == "Results")
        rawdata <- rawdata[1:indexStop,]
      }
      biotek <- T
    }else {
      indexStart <- which(rawdata=='Cycle Nr.')
    }
    if(length(indexStart) == 0) {
      stop(paste0("Could not find the start of the data table in: ", fileData$name))
    }
    for (i in 1:length(indexStart)) {
      if(i == length(indexStart)) {
        subTableDF <- rawdata[indexStart[i]:nrow(rawdata),]
      }else {
        subTableDF <- rawdata[indexStart[i]:indexStart[i+1]-1,]
      }
      #If Time is below Cycle, that means each row is a well, hence we need to translate the table to make each column = one well
      if(!biotek & grepl("Time", rawdata[indexStart[i]+1,1])) {
        subTableDF <- as.data.frame(t(subTableDF))
      }
      #Setting first row as colnames
      colnames(subTableDF) <- subTableDF[1,]
      subTableDF <- subTableDF[-c(1),]
      
      #Removing non-numeric rows and columns
      subTableDF <- as.data.frame(suppressWarnings(sapply(subTableDF, as.numeric)))
      
      #removing columns or rows with only NA in it
      subTableDF <- subTableDF[, colSums(is.na(subTableDF)) != nrow(subTableDF)]
      subTableDF <- subTableDF[rowSums(is.na(subTableDF)) != ncol(subTableDF),]
      
      #Removing Temp and Cycle columns
      rawTableList$time <- subTableDF[, grep("time", tolower(colnames(subTableDF)))]/3600
      
      
      rawTableList$time <- rawTableList$time[!is.na(rawTableList$time)]
      subTableDF <- subTableDF[1:length(rawTableList$time),]
      
      subTableDF <- subTableDF[, -grep("temp|cycle|time|t..", tolower(colnames(subTableDF)))]
      if(biotek) {
        rawTableList$time <- rawTableList$time*86400
      }
      name <- rawdata[indexStart[i]-1,1]
      if(is.na(name) | name == "") {
        #if first subtable not named, it is named OD 
        name <- ifelse(i == 1, "OD", i)
        showNotification(paste0('The 1st table of ', fileData$name, ' was named "OD" by default.'), type="warning")
      }
      
      if(nrow(as.data.frame(subTableDF)) == 0) {
        stop(paste0("N. rows of ", name, "  dataframe is 0 in ", fileData$name))
      }
      #Append to the list
      rawTableList[[name]] <- as.data.frame(subTableDF)
      lb$inc((1/(nfiles))/length(indexStart), detail = "")
    }
    
  }
  
  return(rawTableList)
}

mergeSubTables <- function(rawdata_file_list, subNames, fileNames, nr) {
  out <- list()
  for (subTableName in subNames) {
    out[[subTableName]] <- do.call(cbind, lapply(fileNames, function(filename) {
      df <- isolate(rawdata_file_list[[filename]][[subTableName]])
      #Fill up the DF with NA rows based on the max nr value
      if(nrow(df) < nr) {
        df[nrow(df):nr,] <- NA
      }
      #Paste file name to well name, if multiple files (creating unique well name)
      if(length(fileNames) > 1) {
        colnames(df) <- paste(tools::file_path_sans_ext(filename), colnames(df), sep="_")
      }
      return(df)
    }))
  }
  
  return(out)
  
  
}


detectIfOD <- function(df) {
  if(max(df) < 2) {
    return(T)
  }
  return(F)
}






getDFlogticks <- function(fw, df, nrows, data, refcurve) {
  if(refcurve != "None") {
    facets <- c("facetRef")
  }else {
    facets <- unlist(strsplit(fw,", "))
  }
  
  df_facets <- df[which(colnames(df) %in% facets)]
  if(sum(!duplicated(df_facets)) == 1) {
    return(data)
  }
  tmp <- sapply(df_facets, levels)
  if(ncol(df_facets) > 1 & length(tmp[[1]]) != length(tmp[[2]])) {
    n <- max(length(tmp[[colnames(df_facets[1])]]), length(tmp[[colnames(df_facets[2])]]))
    length(tmp[[colnames(df_facets[1])]]) <- n
    length(tmp[[colnames(df_facets[2])]]) <- n
  }
  tmp <- as.data.frame(tmp)
  df_levels <- expand.grid(rev(tmp))
  
  fin <- semi_join(df_levels, df_facets, by=facets)
  df_final <- cbind(data.frame(x=NA), fin)[seq(1, nrow(fin), by=ceiling((nrow(fin))/nrows)),]
  return(df_final)
}


updateGroup <- function(groups, conditions, wells) {

  if(is.null(groups)) {
    groups <-  data.frame(Wells = wells, KeepWell="Yes", Preview=NA, stringsAsFactors = F)
  }
  currentConds <- colnames(groups[-c(1,2,3)])
  x <- which(!(colnames(groups[-c(1,2,3)]) %in% conditions)) + 3
  if(length(x) >= 1) {
    groups <- groups[-x]
  }
  
  #Only add new columns if a non-existing condition has been entered
  groups[,setdiff(conditions, currentConds)] <- "NA"
  return(groups)
}


#To optimize
aggrTech <- function(dataList, N, horiz=T, multiF=F) {
  #If N not a multiple of total number of wells
  if(ncol(dataList[[1]]) %% N) {
    return(dataList)
  }
  
  dataList_aggre <- lapply(dataList, function(df) {
    
    #horiz => A1, A2, A3
    newDF <- do.call(cbind, lapply(seq(1, ncol(df), by=N), function(colN) {
      subs <- colN:(colN+N-1)
      if(!horiz) {
        nam <- colnames(df)
        x <- match(nam[order(as.numeric(gsub("\\D", "", nam)))], nam)
        subs <- x[subs]
      }
      temp <- data.frame(rowMeans(df[subs]))
      nam <- colnames(df)[subs]
      
      if(length(multiF) > 1) {
        for (ti in paste0(multiF, "_")) {
          if(any(grepl(ti, nam))) {
            nam <- gsub(ti, "", nam)
            colnames(temp) <- paste0(ti, paste(nam, collapse="-"))
          }
        }
      }else {
        colnames(temp) <- paste(nam, collapse="-")
      }
      
      return(temp)
    }))
    
    return(newDF)
  })
  return(dataList_aggre)
}


aggrTechV <- function(v, N, horiz=T, multiF=F) {
  if(length(v) %% N) {
    return(v)
  }
  #horiz => A1, A2, A3
  newV <- do.call(c, lapply(seq(1, length(v), by=N), function(i) {
    subs <- i:(i+N-1)
    if(!horiz) {
      x <- match(v[order(as.numeric(gsub("\\D", "", v)))], v)
      subs <- x[subs]
    }
    nam <- paste(v[subs], collapse="-")
    if(length(multiF) > 1) {
      tmp <- c()
      for (ti in paste0(multiF, "_")) {
        tmp <- c(tmp,paste0(ti, nam))
        
      }
      return(tmp)
    }
    
    return(nam)
  }))
  return(newV)
}





dataMelter <- function(dataList, groups, time) {
  groups <- groups[groups$KeepWell == "Yes",]
  if(nrow(groups) == 0) {
    return(NULL)

  }
  #converting the conditions columns into factor
  # groups[-c(1,2,3)] <- lapply(groups[-c(1,2,3)] , factor)
  
  conditions <- names(groups)[-c(1,2,3)]

  #looping over each subTable, skipping the time vector
  dataList_melted <- lapply(dataList, function(subTable) {
    #subset only selected wells with KeepWell
    subTable <-  subTable[which(names(subTable) %in% c(groups$Wells))]
    # subTable <- subTable[which(time >= timeRange[1] & time <= timeRange[2]),]
    subTable_melt <- melt(cbind(data.frame(time = time), subTable), id=c('time'))
    ind <- as.vector(match(subTable_melt$variable, groups$Wells))
    lapply(conditions, function(cond) {
      subTable_melt[cond] <<-  factor(groups[[cond]][ind], levels=levels(groups[[cond]]))
    })
    #set to 0 negative values
    subTable_melt$value <-(abs(subTable_melt$value)+subTable_melt$value)/2
    #Melt the table and create mean and SE
    subTable_melt <- cbind(
      aggregate(subTable_melt$value, by=subTable_melt[c("time", conditions)], FUN=mean),
      aggregate(subTable_melt$value, by=subTable_melt[c("time", conditions)], function(x) sd(x)/sqrt(length(x)))[length(conditions)+2]
    )
    colnames(subTable_melt)  <-  c("time", conditions, "value", "SE")
    return(subTable_melt)
  })
  return(dataList_melted)
}


orNull <- function(a, b) {
  ifelse(is.null(a), return(b), return(a))
}

getTheme <- function(theme, size) {
  themes_map <- list(
    "BW" = theme_bw(base_size = size),
    "Classic" = theme_classic(base_size = size),
    "Light" = theme_light(base_size = size),
    "Minimal" = theme_minimal(base_size = size),
    "Gray" = theme_gray(base_size = size)
  )
  return(themes_map[[theme]])
}


getInteractions <- function(cond) {
  if(length(cond) == 1) {
    return(c())
  }
  return(unlist(apply(combn(cond,2), 2, function(x) {return(paste(x, collapse=", "))})))
}

formartConditions <- function(input) {
  cond <- str_trim(unlist(strsplit(input,",")))
  cond <- gsub("^[0-9]+", '', cond)
  cond <- gsub("KeepWell|SE|time|value|Wells|Preview", '', cond)
  cond <- gsub(" ", "_", cond)
  cond <- unique(cond[cond != ""])
  return(cond)
}


addReferenceCurve <- function(refLevel, fw, df) {
  ref_df <- df[df[fw] == refLevel,]
  newDF <- df[df[fw] != refLevel,]
  newDF$facetRef = newDF[[fw]]
  ref_df <- do.call(rbind, lapply(levels(newDF[[fw]]), function(level) {
    if(level != refLevel) {
      df <-  ref_df
      df$facetRef <- level
      return(df)
    }
  }))
  newDF <- rbind(newDF, ref_df)
  # newDF[fw] <- newDF$facetRef
  return(newDF)
}



normalise <- function(df=NULL, method=NULL, baseOD=NULL, wells=NULL) {
  if(!detectIfOD(df)) {
    return(df)
  }

  if(method == 'Mininum') {
    return(data.frame(lapply(df, function(well) well - min(well, na.rm=T) + as.numeric(baseOD, na.rm=T)), check.names = F))
  }else if(method == '1st value') {
    return(data.frame(lapply(df, function(well) well - well[1] + as.numeric(baseOD, na.rm=T)), check.names = F))
  }else if(method == 'Min(wells 1-2-3)') {
    return(data.frame(lapply(df, function(well) well - min(well[1:3], na.rm=T) + as.numeric(baseOD, na.rm=T)), check.names = F))
  }else if(method == 'Specific Well(s)') {
    if(is.null(wells)) {
      return(data.frame(lapply(df, function(well) well=well), check.names = F))
    }
    tmp <- data.frame(lapply(df, function(well) well - rowMeans(df[wells], na.rm=T) + as.numeric(baseOD, na.rm=T)), check.names = F)
    return(tmp)
  }
  return(data.frame(lapply(df, function(well) well=well), check.names = F))
}

lagNorm <- function(dl=NULL, threshold=0) {
  fw <- lapply(dl[[1]], function(well) {
    for (i in 1:length(well)) {
      if(well[i] > threshold) {
        return(i)
      }
    }
  })
  
  dl <- lapply(dl, function(dt) {
    daf <- do.call(cbind, lapply(names(dt), function(wellName) {
      if(!is.null(fw[[wellName]])) {
        dt[wellName] <- as.numeric(c(dt[fw[[wellName]]:nrow(dt[wellName]),wellName], rep(NA, fw[[wellName]]-1)))
      }
      return(dt[wellName])
    }))
    return(data.frame(daf, check.names = F))
  })
  return(dl)
}





getPalette <- function(x, customPalette=NULL) {
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
  
  if(!is.null(customPalette)) {
    return(c(list("Custom" = customPalette), ps))
  }
  
  return(ps)
}


getUpLo <- function(df, log=F) {
  if(log) {
    df$Upper <- log10(df$value+df$SE)
    df$Lower <- log10(df$value-df$SE)
    df$value <- log10(df$value)
  }else {
    df$Upper <- df$value+df$SE
    df$Lower <- df$value-df$SE
    df$value <- df$value
  }
  return(df)
}


getTrapezoidalAUC <- function(timeCol, plate, range) {
  AUC <- lapply(colnames(plate), function(well) {
    odCol <- plate[[well]]
    auc <- c()
    start <- which(abs(timeCol-range[1])==min(abs(timeCol-range[1])))+1
    end <-  which(abs(timeCol-range[2])==min(abs(timeCol-range[2])))
    if(end == length(odCol)) {
      end <- end-1
    }
    for (i in start:end) {
      #Formula taken from doi.org/10.1016/j.febslet.2005.02.025
      #Trapezoidal AUC
      x <- ((odCol[i]+odCol[i+1])/2-odCol[1]) * (timeCol[i+1]-timeCol[i])
      if(x < 0 | is.na(x)) {x <- 0}
      auc <- c(auc, x)
    }
    return(as.numeric(sum(auc)))  
  })
  return(as.numeric(AUC))
}

getMaxVal <- function(timeCol, plate, range) {
  max <- lapply(colnames(plate), function(well) {
    
    odCol <- plate[[well]]
    
    odCol <- odCol[which(timeCol >= range[1] & timeCol <= range[2])]
    return(as.numeric(max(odCol)))
  })
  return(as.numeric(max))
}

getMu <- function(N, N0, t, t0) {
  #µ = ( (log10 N - log10 N0) 2.303) / (t - t0)
  if(N<0 || N0<0 || is.na(N)) {
    return(0)
  }
  return(((log10(N) - log10(N0))* 2.303) / (t - t0))
}

getMaxGr <- function(timeCol, plate, range) {
  
  max <- lapply(colnames(plate), function(well) {
    odCol <- plate[[well]]
    odCol <- odCol[which(timeCol >= range[1] & timeCol <= range[2])]
    timeCol <- timeCol[which(timeCol >= range[1] & timeCol <= range[2])]
    
    maxgr <- 0
    for (i in 2:length(odCol)) {
      mu <- getMu(odCol[i], odCol[i-1], timeCol[i], timeCol[i-1])
      if(!is.nan(mu) &  mu > maxgr) {
        maxgr <- mu
      }
    }
    return(maxgr)
  })
  return(as.numeric(max))
  
}

plot_exception <-function(
    ...,
    sep=" ",
    type=c("message","warning","cat","print"),
    color="auto",
    console=TRUE,
    size = 6){      
  type=match.arg(type)
  txt = paste(...,collapse=sep)
  if(console){
    if(type == "message") message(txt)
    if(type == "warning") warning(txt)
    if(type == "cat") cat(txt)
    if(type == "print") print(txt)
  }
  if(color =="auto") color <- if(type == "cat") "black" else "red"
  if(txt == "warning") txt <- paste("warning:",txt)
  print(ggplot2::ggplot() +
          ggplot2::geom_text(ggplot2::aes(x=0,y=0,label=txt),color=color,size=size) + 
          ggplot2::theme_void())
  invisible(NULL)
}


getLogisticParameters <- function(timeCol, plate, range) {
  tmp <- cbind(data.frame(time=timeCol), plate)
  tmp <- tmp[which(tmp$time >= range[1] & tmp$time <= range[2]),]
  
  tmp <- suppressWarnings(SummarizeGrowthByPlate(tmp))
  return(tmp)
}


saveUpdate <- function(session, df, customP=NULL, groupsDF=NULL, groupsDFLvl=NULL) {
  if(!is.null(customP)) {
    updateStore(session, name = "customP", value = customP)
    updateStore(session, name = "groupsDF", value = groupsDF)
    updateStore(session, name = "groupsDFLvl", value = groupsDFLvl)
  }else {
    updateStore(session, name = "customP", value = df$customP)
    updateStore(session, name = "groupsDF", value = df$groupsDF)
    updateStore(session, name = "groupsDFLvl", value = df$groupsDFLvl)
  }
  updateStore(session, name = "color", value = df$color)
  updateStore(session, name = "linetype", value = df$linetype)
  updateStore(session, name = "shape", value = df$shape)
  updateStore(session, name = "grouping", value = df$grouping)
  updateStore(session, name = "fw", value = df$fw)
  updateStore(session, name = "referenceCurve", value = df$referenceCurve)
  updateStore(session, name = "nRowsFacets", value = df$nRowsFacets)
  updateStore(session, name = "se", value = df$se)
  updateStore(session, name = "conditionsUI_Input", value = df$conditionsUI_Input )

  updateStore(session, name = "height", value = df$height)
  updateStore(session, name = "width", value = df$width)
  updateStore(session, name = "pal", value = df$pal)
  updateStore(session, name = "norm", value = df$norm)
  updateStore(session, name = "norm_baseOD", value = df$norm_baseOD)
  updateStore(session, name = "techAggr", value = df$techAggr)
  updateStore(session, name = "x_axis_title", value = df$x_axis_title)
  updateStore(session, name = "y_axis_title", value = df$y_axis_title)
  updateStore(session, name = "customThemeUI", value = df$customThemeUI)
  updateStore(session, name = "type_plot_selector", value = df$type_plot_selector)
  updateStore(session, name = "lvlOrderSelect", value = df$lvlOrderSelect)
  updateStore(session, name = "size_l", value = df$size_l)
  updateStore(session, name = "size_p", value = df$size_p)
  updateStore(session, name = "theme", value = df$theme)
  updateStore(session, name = "params_x_scale", value = df$params_x_scale)
  updateStore(session, name = "params_y_scale", value = df$params_y_scale)
  updateStore(session, name = "range", value = df$range)
  updateStore(session, name = "data_selector", value = df$data_selector)
  updateStore(session, name = "param_selector", value = df$param_selector)
  updateStore(session, name = "secPlotMethod", value = df$secPlotMethod)
  updateStore(session, name = "norm_lagPhase", value = df$norm_lagPhase)
}



updateSettings <- function(df, customP=NULL, groupsDF=NULL, groupsDFLvl=NULL) {
  settings <- list()
  if(!is.null(customP)) {
    settings$customP <-  customP
    settings$groupsDF <-  groupsDF
    settings$groupsDFLvl <-  groupsDFLvl
  }else {
    settings$customP <-  df$customP
    settings$groupsDF <-  df$groupsDF
    settings$groupsDFLvl <-  df$groupsDFLvl
  }
  settings$color <-  df$color
  settings$linetype <-  df$linetype
  settings$shape <-  df$shape
  settings$grouping <-  df$grouping
  settings$fw <-  df$fw
  settings$referenceCurve <-  df$referenceCurve
  settings$nRowsFacets <-  df$nRowsFacets
  settings$se <-  df$se
  settings$conditionsUI_Input <-  df$conditionsUI_Input 
  
  settings$height <-  df$height
  settings$width <-  df$width
  settings$pal <-  df$pal
  settings$norm <-  df$norm
  settings$norm_baseOD <-  df$norm_baseOD
  settings$techAggr <-  df$techAggr
  settings$x_axis_title <-  df$x_axis_title
  settings$y_axis_title <-  df$y_axis_title
  settings$customThemeUI <-  df$customThemeUI
  settings$type_plot_selector <-  df$type_plot_selector
  settings$lvlOrderSelect <-  df$lvlOrderSelect
  settings$size_l <-  df$size_l
  settings$size_p <-  df$size_p
  settings$theme <-  df$theme
  settings$params_x_scale <-  df$params_x_scale
  settings$params_y_scale <-  df$params_y_scale
  settings$range <-  df$range
  settings$data_selector <-  df$data_selector
  settings$param_selector <-  df$param_selector
  settings$secPlotMethod <-  df$secPlotMethod
  settings$norm_lagPhase <-  df$norm_lagPhase
  return(settings)
}







removePattern <- function(df, x, replacem) {
  df <- lapply(names(df), function(colna) {
    if(any(grepl("  ", df[[colna]]))) {
      showNotification(paste0('Some double spaced were found and replaced in column ', colna), type="warning")
      return(as.factor(gsub("  ", " ", df[[colna]])))
    }
    return(df[[colna]])
  })
  return(df)
}
  
  
  
  
  


isHex <- function(vector) {
  if(length(vector) == 0) {
    return(FALSE)
  }
  return(all(str_detect(vector, "^#([a-fA-F0-9]{6})$")))
}