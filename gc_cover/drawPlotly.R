drawChart <- function(data, seqName, windowSize, chartType) {
	if(chartType == "GC_Cov"){
		p <- plot_ly() %>%
			add_lines(x = ~data$pos, y = ~data$gc, name = "GC content", line = list(width = 1)) %>%
			add_lines(x = ~data$pos, y = ~data$coverage, name = "Coverage", yaxis = "y2", line = list(width = 1)) %>%
  		layout(
				title = seqName,
				yaxis = list(tickfont = list(color = "#337ab7"), title = "GC Content (%)"), 
				yaxis2 = list(tickfont = list(color = "orange"), overlaying = "y", side = "right", title = "Coverage"),
    		xaxis = list(title="Position")
  		)
	}
	else {
		cov_smoothed <- loess(data$coverage ~ data$gc)
		p <- plot_ly() %>%
			add_lines(x = ~data$gc, y = ~fitted(cov_smoothed), line = list(width = 1)) %>%
  		layout(
				title = seqName,
				yaxis = list(title = "Coverage", range = c(0,max(data$coverage))), 
    		xaxis = list(title="GC Content (%)")
  		)
	}
}

getCsvContent <- function(data, windowSize) {
	names <- c("Position", paste(c("GC Content (%) -", windowSize, "bp window"), collapse = " "), "Coverage")
	df <- data.frame(data$pos, data$gc, data$coverage, check.names = FALSE)
	colnames(df) = names

	return(df)
}

getGcCov <- function(seqString, covData, windowSize) {
	window.size <- windowSize
	sequence <- DNAString(seqString[[1]])
	gc <- (rowSums(letterFrequencyInSlidingView(sequence, window.size, c("G","C", "S")))/window.size)*100
	pos <- 1:length(gc)
	coverage <- covData[1:length(gc)]
	return(list("gc" = gc, "pos" = pos, "coverage" = coverage))
}
