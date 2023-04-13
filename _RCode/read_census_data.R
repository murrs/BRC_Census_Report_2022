#### Code to read in and prep all years of data
# Edit this file to your local file paths
# Don't pull request changes to this file

library(data.table)
library(survey)

census13 <- fread("C:\\Users\\ashev\\Documents\\census\\Hunter_InProgress\\2013\\Online survey\\csv\\Census2013Fulltab.tsv", sep = "\t", na.strings = c("", "NA"))
census14 <- fread("C:\\Users\\ashev\\Documents\\census\\Hunter_InProgress\\2014\\Online survey\\csv\\Census2014Fulltab.tsv", sep = "\t", na.strings = c("", "NA"))
census15 <- fread("C:\\Users\\ashev\\Documents\\census\\Hunter_InProgress\\2015\\Online survey\\csv\\Clean2015CensusFulltabJul2018.tsv", sep = "\t", na.strings = c("", "NA"))
census16 <- fread("C:\\Users\\ashev\\Documents\\census\\Hunter_InProgress\\2016\\Online survey\\csv\\Census2016Fulltab.tsv", sep = "\t", na.strings = c("", "NA"))
census17 <- fread("C:\\Users\\ashev\\Documents\\census\\Hunter_InProgress\\2017\\Online survey\\csv\\Clean2017CensusFulltabMar2018.tsv", sep = "\t", na.strings = c("", "NA"))
census18 <- fread("C:\\Users\\ashev\\Documents\\census\\Hunter_InProgress\\2018\\Online survey\\csv\\Clean2018CensusFulltabApr2019.tsv", sep = "\t", na.strings = c("", "NA"))
census19 <- fread("C:\\Users\\ashev\\Documents\\census\\Hunter_InProgress\\2019\\Online survey\\csv\\Clean2019CensusFulltabJan2019.tsv", sep = "\t", na.strings = c("", "NA"))
census22 <- fread("C:\\Users\\ashev\\Documents\\census\\report_2022\\census2022_cleaned_weighted.tsv", sep = "\t", na.strings = c("", "NA"))

design13 <- svydesign(ids = ~id, weights = ~weight, data = census13)
design14 <- svydesign(ids = ~id, weights = ~weightbmorg, data = census14)
design15 <- svydesign(ids = ~id, weights = ~weightbmorg1, data = census15)
design16 <- svydesign(ids = ~id, weights = ~weightbmorg1, data = census16)
design17 <- svydesign(ids = ~id, weights = ~weightbfarrival, data = census17)
design18 <- svydesign(ids = ~id, weights = ~weightbfarrival, data = census18)
design19 <- svydesign(ids = ~id, weights = ~weightbfarrival, data = census19)
design22 <- svydesign(ids = ~responseID, weights = ~weights, data = census22)

makePlotDataByLevel <- function(level, design, varName, year){
  if(is.character(level)){
    f <- as.formula(paste0("~", varName, " == ", "\"", level, "\""))
  }
  else if(is.logical(level)){
    if(level){
      f <- as.formula(paste0("~", varName))
    }
    else{
      f <- as.formula(paste0("~", "!", varName))
    }
  }
  ciOut <- svyciprop(f, design = design)
  data.frame(est = as.numeric(ciOut),
             lower = attr(ciOut, "ci")[1],
             upper = attr(ciOut, "ci")[2],
             level = level,
             year = year)
}

makePlotDataByVarLevel <- function(varName, design, year, levels){
  ciDatList <- lapply(levels, makePlotDataByLevel, design = design, 
                      varName = varName, year = year)
  do.call(rbind, ciDatList)
}


makePlotDataByYearVarLevel <- function(i, varNames, designs, years, levels){
  
  ciDatList <- lapply(varNames[[i]], makePlotDataByVarLevel, 
                      design = designs[[i]], levels = levels[[i]], 
                      year = years[i])
  do.call(rbind, ciDatList)
}


makePlotData <- function(varNames, designs, years, levels,
                         labels = NULL, labelOrder = NULL){
  #Get number of years to loop over
  nYears <- length(years)

  #For questions where only one answer may be selected
  #Check if varNames is not a list and convert to list
  if(!is.list(varNames)){
    if(length(varNames == 1)){
      varNames <- rep(varNames, times = nYears)
    }
    varNames <- as.list(varNames)
  }
  
  outDat <- lapply(1:nYears, makePlotDataByYearVarLevel, 
                   varNames = varNames, designs = designs, 
                   years = years, levels = levels)
  outDat <- do.call(rbind, outDat)
  rownames(outDat) <- NULL
  if(is.null(labels)){
    return(outDat)
  }
  else{
    if(!is.null(labelOrder)){
      if(is.character(labelOrder)){
        outDat$labels <- factor(rep(labels, nYears), levels = labelOrder)
      }
      else if(is.numeric(labelOrder)){
        outDat$labels <- factor(rep(labels, nYears), 
                                levels = labels[labelOrder])
      }
      
    }
    else{
      outDat$labels <- factor(rep(labels, nYears))
    }
  }
  return(outDat)
}

