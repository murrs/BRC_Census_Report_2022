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

format_table_entry <- function(x, format = c("percent", "proportion"),
                               digits = 3, confInt = TRUE){
  x <- list(est = as.numeric(x[which(names(x) == "est")]),
            lower = as.numeric(x[which(names(x) == "lower")]),
            upper = as.numeric(x[which(names(x) == "upper")]))
  
  if(confInt){
    if(any(format == "percent")){
      #floating decimal format
      fdf <- paste0("%.", digits - 2, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%", " (", 
                           sprintf(fdf, round(x$lower, digits) * 100), ", ", 
                           sprintf(fdf, round(x$upper, digits) * 100), ")")
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits)), " (", 
                           sprintf(fdf, round(x$lower, digits)), ", ", 
                           sprintf(fdf, round(x$upper, digits)), ")")
    }
    else{
      stop("Invalid format")
    }
  }
  else{
    if(any(format == "percent")){
      fdf <- paste0("%.", digits - 2, "f")
      outEntries <- paste0(sprintf(fdf, round(x$est, digits) * 100), "%")
    }
    else if(format == "proportion"){
      fdf <- paste0("%.", digits, "f")
      outEntries <- sprintf(fdf, round(x$est, digits))
    }
    else{
      stop("Invalid format")
    }
  }
  return(outEntries)
}
