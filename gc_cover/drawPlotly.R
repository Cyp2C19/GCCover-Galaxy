drawChart <- function(sequence, covData, seqName, windowSize, chartType) {
	window.size <- windowSize
	gc <- (rowSums(letterFrequencyInSlidingView(sequence, window.size, c("G","C", "S")))/window.size)*100
	pos <- 1:length(gc)
	coverage <- covData[1:length(gc)]
	if(chartType == "GC_Cov"){
		p <- plot_ly() %>%
			add_lines(x = ~pos, y = ~gc, name = "GC content", line = list(width = 1)) %>%
			add_lines(x = ~pos, y = ~coverage, name = "Coverage", yaxis = "y2", line = list(width = 1)) %>%
  		layout(
				title = seqName,
				yaxis = list(tickfont = list(color = "#337ab7"), title = "GC Content (%)"), 
				yaxis2 = list(tickfont = list(color = "orange"), overlaying = "y", side = "right", title = "Coverage"),
    		xaxis = list(title="Position")
  		)
	}
	else {
		cov_smoothed <- loess(coverage ~ gc)
		p <- plot_ly() %>%
			add_lines(x = ~gc, y = ~fitted(cov_smoothed), line = list(width = 1)) %>%
  		layout(
				title = seqName,
				yaxis = list(title = "Coverage", range = c(0,max(coverage))), 
    		xaxis = list(title="GC Content (%)")
  		)
	}
}

getDataForCsv <- function(sequence, covData, windowSize) {
	window.size <- windowSize
	gc <- (rowSums(letterFrequencyInSlidingView(sequence, window.size, c("G","C", "S")))/window.size)*100
	pos <- 1:length(gc)
	coverage <- covData[1:length(gc)]
	names <- c("Position", paste(c("GC Content (%) -", windowSize, "bp window"), collapse = " "), "Coverage")
	df <- data.frame(pos, gc, coverage, check.names = FALSE)
	colnames(df) = names

	return(df)
}
