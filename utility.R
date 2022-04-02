

themes <- c("BW" , "Classic", "Light", "Minimal", "Gray")
react <- c("conditions", "rawdata_list", "dataList", "groups", "interactions", "names", "rawdata", "themes_map")

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
      
      #Append to the list
      rawTableList[[rawdata[indexStart[i]-1,1]]] <- as.data.frame(subTableDF)
    }
  }
  return(rawTableList)
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


dataMelter <- function(dataList, groups, timeRange) {
  

  groups <- groups[groups$KeepWell == "Yes",]
  if(nrow(groups) == 0) {
    return(NULL)

  }
  #converting the conditions columns into factor
  groups[-c(1,2,3)] <- lapply(groups[-c(1,2,3)] , factor)
  
  conditions <- names(groups)[-c(1,2,3)]

  #looping over each subTable, skipping the time vector
  dataList_melted <- lapply(dataList[-1], function(subTable) {
    
    #subset only selected wells with KeepWell
    subTable <-  subTable[which(names(subTable) %in% c(groups$Wells))]
    subTable <- subTable[which(dataList$time >= timeRange[1] & dataList$time <= timeRange[2]),]
    subTable_melt = melt(cbind(data.frame(time = dataList$time), subTable), id=c('time'))
    ind <- as.vector(match(subTable_melt$variable, groups$Wells))
    lapply(conditions, function(cond) {
      subTable_melt[cond] <<-  as.factor(as.vector(groups[,cond])[ind])
    })
    subTable_melt <- cbind(
      aggregate(subTable_melt$value, by=subTable_melt[c("time", conditions)], FUN=mean),
      aggregate(subTable_melt$value, by=subTable_melt[c("time", conditions)], function(x) sd(x)/sqrt(length(x)))[length(conditions)+2]
    )
    colnames(subTable_melt)  <-  c("time", conditions, "value", "SE")
    return(subTable_melt)
  })
  
  
  
  

  return(dataList_melted)
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






normalize <- function(df=NULL, method=NULL, baseOD=NULL) {
  if(is.null(df) || is.null(method) || is.null(baseOD)) {
    return()
  }
  if(method == 'Mininum') {
    return(data.frame(lapply(df, function(x) x - min(x) + as.numeric(baseOD))))
  }else if(method == '1st value') {
    return(data.frame(lapply(df, function(x) x - x[1] + as.numeric(baseOD))))
  }else if(method == 'Min(wells 1-2-3)') {
    return(data.frame(lapply(df, function(x) x - min(x[1:3]) + as.numeric(baseOD))))
  }else if(method == 'No Normalisation') {
    return(data.frame(lapply(df, function(x) x=x)))
  }
}


getAUC <- function(timeCol, odCol, range) {
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
  return(sum(auc))  
}

isHex <- function(vector) {
  if(length(vector) == 0) {
    return(FALSE)
  }
  return(all(str_detect(vector, "^#([a-fA-F0-9]{6})$")))
}