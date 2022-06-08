themes <- c("BW" , "Classic", "Light", "Minimal", "Gray")
react <- c("conditions", "rawdata_list", "dataList", "groups", "names", "interactions", "rawdata", "themes_map", "params_list", "params_df", "groupDF_subset")

getFile <- function(datapath) {
  rawTableList <- list()
  if(file_ext(datapath) == "txt") {
    rawdata <-  read.table(datapath, sep="\t", header=T, dec=",")
    rawdata <- rawdata[,colSums(is.na(rawdata))<nrow(rawdata)]
    rawdata <- rawdata[complete.cases(rawdata[,1]),]
    rawTableList[["OD"]] <- rawdata
    
  }else {
    rawdata <-  as.data.frame(suppressMessages(read_excel(datapath, col_names = F)))
    #rawTableList is a list containing each table, starting with OD, and any additional measurement table, such as RLU, luminescence, or another OD...
    indexStart <- which(rawdata=='Cycle Nr.')
    for (i in 1:length(indexStart)) {
      if(i == length(indexStart)) {
        subTableDF <- rawdata[indexStart[i]:nrow(rawdata),]
      }else {
        subTableDF <- rawdata[indexStart[i]:indexStart[i+1]-1,]
      }
      #From the Tecan i-control software be default, the table is transposed (each row = one well, instead of one column = well and each row is one cycle)
      if(grepl('Tecan i-control', rawdata)[1]) {
        subTableDF <- as.data.frame(t(subTableDF))
      }
      #Setting first row as colnames
      colnames(subTableDF) <- subTableDF[1,]
      subTableDF <- subTableDF[-c(1),]
      
      #Removing non-numeric rows and columns
      subTableDF <- suppressWarnings(sapply(subTableDF, as.numeric))
      
      #removing columns or rows with only NA in it
      subTableDF <- subTableDF[, colSums(is.na(subTableDF)) != nrow(subTableDF)]
      subTableDF <- subTableDF[rowSums(is.na(subTableDF)) != ncol(subTableDF),]
      #Removing Temp and Cycle columns
      rawTableList$time <- subTableDF[, grep("Time", colnames(subTableDF))]/3600
      subTableDF <- subTableDF[, -grep("Temp|Time|Cycle", colnames(subTableDF))]
      
      name <- rawdata[indexStart[i]-1,1]
      if(is.na(name)) {
        name <- ifelse(i == 1, "OD", i)
      }
      
      #Append to the list
      rawTableList[[name]] <- as.data.frame(subTableDF)
    }
  }
  return(rawTableList)
}

getDFlogticks <- function(fw, df, nrows, data) {
  facets <- unlist(strsplit(fw,", "))
  df_facets <- df[which(colnames(df) %in% facets)]
  if(sum(!duplicated(df_facets)) == 1) {
    return(data)
  }
  df_levels <- expand.grid(rev(as.data.frame(sapply(df_facets, levels))))
  fin <- semi_join(df_levels, df_facets, by=facets)
  df_final <- cbind(data.frame(x=NA), fin)[seq(1, nrow(fin), by=ceiling(nrow(fin)/nrows)),]
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
    #showNotification()
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


dataMelter <- function(dataList, groups, time) {
  

  groups <- groups[groups$KeepWell == "Yes",]
  if(nrow(groups) == 0) {
    return(NULL)

  }
  #converting the conditions columns into factor
  groups[-c(1,2,3)] <- lapply(groups[-c(1,2,3)] , factor)
  
  conditions <- names(groups)[-c(1,2,3)]

  #looping over each subTable, skipping the time vector
  dataList_melted <- lapply(dataList, function(subTable) {
    #subset only selected wells with KeepWell
    subTable <-  subTable[which(names(subTable) %in% c(groups$Wells))]
    # subTable <- subTable[which(time >= timeRange[1] & time <= timeRange[2]),]
    subTable_melt = melt(cbind(data.frame(time = time), subTable), id=c('time'))
    ind <- as.vector(match(subTable_melt$variable, groups$Wells))
    lapply(conditions, function(cond) {
      subTable_melt[cond] <<-  as.factor(as.vector(groups[,cond])[ind])
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
  return(newDF)
}



normalize <- function(df=NULL, method=NULL, baseOD=NULL) {
  if(is.null(df) || is.null(method) || is.null(baseOD)) {
    return()
  }
  if(method == 'Mininum') {
    return(data.frame(lapply(df, function(well) well - min(well, na.rm=T) + as.numeric(baseOD, na.rm=T)), check.names = F))
  }else if(method == '1st value') {
    return(data.frame(lapply(df, function(well) well - well[1] + as.numeric(baseOD, na.rm=T)), check.names = F))
  }else if(method == 'Min(wells 1-2-3)') {
    return(data.frame(lapply(df, function(well) well - min(well[1:3], na.rm=T) + as.numeric(baseOD, na.rm=T)), check.names = F))
  }else if(method == 'No Normalisation') {
    return(data.frame(lapply(df, function(well) well=well), check.names = F))
  }
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
      if(x < 0) {x <- 0}
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
  if(N<0 || N0<0) {
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
      if(mu > maxgr) {
        maxgr <- mu
      }
    }
    return(maxgr)
  })
  return(as.numeric(max))
  
}




getLogisticParameters <- function(timeCol, plate, range) {
  tmp <- cbind(data.frame(time=timeCol), plate)
  tmp <- tmp[which(tmp$time >= range[1] & tmp$time <= range[2]),]
  
  tmp <- SummarizeGrowthByPlate(tmp)
  return(tmp)
}





isHex <- function(vector) {
  if(length(vector) == 0) {
    return(FALSE)
  }
  return(all(str_detect(vector, "^#([a-fA-F0-9]{6})$")))
}