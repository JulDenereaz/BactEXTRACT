

themes <- c("BW" , "Classic", "Light", "Minimal", "Gray")
react <- c("conditions", "conditions_old", "data", "df", "df_melt", "groups", "RLU", "RLU_melt", "groups2", "groups3", "interactions", "names", "rawdata", "themes_map")

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
      
      #removing columns with only NA in it
      subTableDF <- subTableDF[, colSums(is.na(subTableDF)) != nrow(subTableDF)]
      
      #Removing Temp and Cycle columns
      subTableDF <- subTableDF[, -grep("Temp", colnames(subTableDF))]
      subTableDF <- subTableDF[, -grep("Cycle", colnames(subTableDF))]
      
      #Renaming the column "Time [s]" into "Time", and transforming into hours
      colnames(subTableDF)[grep("Time", colnames(subTableDF))] <- "Time"
      rawdata[colnames(rawdata) == "Time"] <-  rawdata[colnames(rawdata) == "Time"]/3600
      
      #Append to the list
      rawTableList[[rawdata[indexStart[i]-1,1]]] <- subTableDF
    }
  }
  return(rawTableList)
}


normalize <- function(df=NULL, method=NULL, baseOD=NULL) {
  if(is.null(df) || is.null(method) || is.null(baseOD)) {
    return()
  }
  if(method == 'Mininum') {
    return(data.frame(lapply(df[2:ncol(df)], function(x) x - min(x) + as.numeric(baseOD))))
  }else if(method == '1st value') {
    return(data.frame(lapply(df[2:ncol(df)], function(x) x - x[1] + as.numeric(baseOD))))
  }else if(method == 'Min(wells 1-2-3)') {
    return(data.frame(lapply(df[2:ncol(df)], function(x) x - min(x[1:3]) + as.numeric(baseOD))))
  }else if(method == 'No Normalisation') {
    return(data.frame(lapply(df[2:ncol(df)], function(x) x=x)))
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