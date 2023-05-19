makePlotData <- function(varName, varNameTable, designs, years, levels,
                         labels = NULL, labelOrder = NULL){
  #Get number of years to loop over
  nYears <- length(years)
  
  #Order years, levels, and designs by decreasing years to match the 
  #  varname lookup
  yearOrder <- order(years, decreasing = TRUE)
  years <- years[yearOrder]
  designs <- designs[yearOrder]
  levels <- levels[yearOrder]
  
  #Get variable names from the crosswalk
  #Determine which years to pull
  yearsToUse <- lapply(years, function(yr, nms){
    grepl(yr, nms)
  }, nms = names(varNameTable))
  yearsToUse <- do.call(rbind, yearsToUse)
  yearsToUse <- apply(yearsToUse, 2, any)
  
  #Grab names.
  varNames <- as.vector(varNameTable[question == varName, ..yearsToUse])
  #Remove any write-in questions
  varNames <- lapply(varNames, function(x){x[!grepl("writeIn", x) & (x != "")]})
  
  #For questions where only one answer may be selected
  #Check if varNames is not a list and convert to list
  if(!is.list(varNames)){
    if(length(varNames == 1)){
      varNames <- rep(varNames, times = nYears)
    }
    varNames <- as.list(varNames)
  }
  
  #If a character vector is supplied for levels, repeat as a list for number
  # of years supplied
  if(is.character(levels)){
    rep(list(levels), times = nYears)
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
