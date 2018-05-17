library(shiny)
library(plotly)
library(Biostrings)
library(seqinr)
source("drawPlotly.R")
load("data/gcCover.RData", envir=.GlobalEnv)

# Define UI for app that draws a plot ----
ui <- fluidPage(
	includeCSS("styles.css"),
	titlePanel("GC-Cover"),
	hr(),
	plotlyOutput("plot"),
	hr(id = "mainHr"),
	fluidRow(id = "panelInputs",
		column(4,
			radioButtons("chartType", "Chart type :",
						choices = list("GC Content and Coverage" = "GC_Cov", "GC Content VS Coverage" = "GC_Vs_Cov"), 
						selected = "GC_Cov"),
			hr()
		),
		column(4,
			sliderInput("range", 
						label = "Window size",
						min = 25, max = 100, 
						value = 100),
			hr()
		),
		column(4,
			downloadButton("downloadData", "Download data"),
			hr()
		)
	)
)

# Define server logic required to draw a plot ----
server <- function(input, output, session) {
	# Get Data and Draw Chart. Call to "drawPlotly.R" script
	output$plot <- renderPlotly({
		data <- getGcCov(seqString, covData, input$range)
		seqName <- paste(c(gsub("_", " ", seqName, fixed=TRUE), "(Window :", input$range, "bp)"), collapse = " ")
		drawChart(data, seqName, input$range, input$chartType)
	})
	# Downloadable csv of selected dataset ----
	output$downloadData <- downloadHandler(
		filename = function() {
			paste(seqName, ".csv", sep = "")
		},
		content = function(file) {
			data <- getGcCov(seqString, covData, input$range)
			dataSet <- getCsvContent(data, input$range)
			write.csv(dataSet, file, row.names = FALSE)
		}
	)
}

shinyApp(ui = ui, server = server)
