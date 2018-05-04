drawChart <- function(sequence, covData, windowSize, chartType) {
	window.size <- windowSize
	gc <- (rowSums(letterFrequencyInSlidingView(sequence, window.size, c("G","C", "S")))/window.size)*100
	pos <- 1:length(gc)
	coverage <- covData[1:length(gc)]
	if(chartType == "GC_Cov"){
		p <- plot_ly() %>%
  			add_lines(x = ~pos, y = ~gc, name = "GC content", line = list(width = 1)) %>%
			add_lines(x = ~pos, y = ~coverage, name = "Coverage", yaxis = "y2", line = list(width = 1)) %>%
  		layout(
    		title = "GC Content and Coverage",
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
    		title = "GC Content VS Coverage",
			yaxis = list(title = "Coverage", range = c(0,max(coverage))), 
    		xaxis = list(title="GC Content (%)")
  		)
	}
}

getData <- function(sequence, covData, windowSize, chartType) {
	window.size <- windowSize
	gc <- (rowSums(letterFrequencyInSlidingView(sequence, window.size, c("G","C", "S")))/window.size)*100
	pos <- 1:length(gc)
	coverage <- covData[1:length(gc)]
	return(data.frame("Position" = pos, "GC Content (%)" = gc,"Coverage" = coverage, check.names = FALSE))
}
