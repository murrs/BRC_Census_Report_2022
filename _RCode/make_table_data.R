make_table_data <- function(plotDat, format = c("percent", "proportion"),
                            digits = 3, confInt = TRUE){
  nYears <- length(unique(plotDat$year))
  nLabels <- length(unique(plotDat$labels))
  cells <- apply(plotDat, 1, format_table_entry, format = format, 
                 digits = digits, confInt = confInt)
  outTable <- as.table(matrix(cells, nrow = nLabels, ncol = nYears))
  rownames(outTable) <- unique(plotDat$labels)
  colnames(outTable) <- unique(plotDat$year)
  outTable <- outTable[,order(unique(plotDat$year))]
  return(outTable)
}