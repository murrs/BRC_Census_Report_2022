formulaVarLevel <- function(level, varName){
  if(is.character(level)){
    out <- paste0("(", varName, " == ", "\"", level, "\"", ")")
  }
  else if(is.logical(level)){
    if(level){
      out <- varName
    }
    else{
      out <- paste0("!", varName)
    }
  }
  return(out)
}

#TODO: makePlotDataByLevel() and makePlotDataByGroup() can be simplified and 
#        combined into a single function

#TODO: check to make sure level is a level of the variable.  If not give an
#        error informing user to check data cleaning script for an error

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

#TODO: The grouping currently does not work for multiple selection questions

makePlotDataByGroup <- function(group, design, varName, year, levels){
  if(is.numeric(group)){
    levelsInGroup <- levels[group]
  }
  else if(is.character(group)){
    levelsInGroup <- levels[levels %in% group]
  }
  fpieces <- lapply(levelsInGroup, formulaVarLevel, varName = varName)
  fpieces <- do.call(paste, c(fpieces, list(sep = " | ")))
  f <- as.formula(paste("~", fpieces))
  ciOut <- svyciprop(f, design = design)
  data.frame(est = as.numeric(ciOut),
             lower = attr(ciOut, "ci")[1],
             upper = attr(ciOut, "ci")[2],
             level = do.call(paste, as.list(levelsInGroup)),
             year = year)
}

makePlotDataByVarLevel <- function(varName, design, year, levels, groups){
  if(is.null(groups)){
    ciDatList <- lapply(levels, makePlotDataByLevel, design = design, 
                        varName = varName, year = year)
  }
  else{
    ciDatList <- lapply(groups, makePlotDataByGroup, design = design,
                        varName = varName, year = year, levels = levels)
  }
  
  do.call(rbind, ciDatList)
}


makePlotDataByYearVarLevel <- function(i, varNames, designs, years, levels,
                                       groups){
  
  ciDatList <- lapply(varNames[[i]], makePlotDataByVarLevel, 
                      design = designs[[i]], levels = levels[[i]], 
                      year = years[i], groups = groups)
  do.call(rbind, ciDatList)
}

format_table_entry <- function(x, format = c("percent", "proportion"),
                               digits = 3, 
                               confInt = c("interval", "plusminus", "none")){
  x <- list(est = as.numeric(x[which(names(x) == "est")]),
            lower = as.numeric(x[which(names(x) == "lower")]),
            upper = as.numeric(x[which(names(x) == "upper")]))
  if(is.na(x$est)){
    return("--")
  }
  
  lessThanPoint1 <- x$est < 0.001
  if(confInt[1] == "interval"){
    if(any(format == "percent")){
      #floating decimal format
      fdf <- paste0("%.", digits - 2, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%", " (", 
                           sprintf(fdf, round(x$lower, digits) * 100), "%, ", 
                           sprintf(fdf, round(x$upper, digits) * 100), "%)")
      outEntries[lessThanPoint1] <- "< 0.1% (--, --)"
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits)), " (", 
                           sprintf(fdf, round(x$lower, digits)), ", ", 
                           sprintf(fdf, round(x$upper, digits)), ")")
      outEntries[lessThanPoint1] <- "< 0.001 (--, --)"
    }
    else{
      stop("Invalid format")
    }
  }
  else if(confInt[1] == "plusminus"){
    if(any(format == "percent")){
      #floating decimal format
      fdf <- paste0("%.", digits - 2, "f")
      plusminus = round(max(c(x$est - x$lower, x$upper - x$est)), digits)
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%", " ± ", 
                           sprintf(fdf, plusminus * 100), "%")
      outEntries[lessThanPoint1] <- "< 0.1%"
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      plusminus = round(max(c(x$est - x$lower, x$upper - x$est)), digits)
      outEntries <- paste0(sprintf(fdf, round(x$est, digits)), " ± ", 
                           sprintf(fdf, plusminus))
      outEntries[lessThanPoint1] <- "< 0.001"
    }
    else{
      stop("Invalid format")
    }
  }
  else if(confInt[1] == "none"){
    if(any(format == "percent")){
      fdf <- paste0("%.", digits - 2, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%")
      outEntries[lessThanPoint1] <- "< 0.1%"
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- sprintf(fdf, round(x$est, digits))
      outEntries[lessThanPoint1] <- "< 0.001"
    }
    else{
      stop("Invalid format")
    }
  }
  else{stop("Invalid confInt format")}
  
  return(linebreak(outEntries))
}


customPlotDat <- function(levels, design, incomeVarName, year, label){
  f <- as.formula(paste0("~ I(", incomeVarName, " %in% c(\"", 
                         do.call(paste, c(as.list(levels), 
                                          list(sep = "\",\""))),"\"))"))
  ciout <- svyciprop(f, design = design)
  data.frame(est = as.numeric(ciout), lower = attr(ciout, "ci")[1],
             upper = attr(ciout, "ci")[2], level = label, 
             year = year, labels = label)
}
