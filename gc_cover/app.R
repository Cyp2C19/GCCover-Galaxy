library(shiny)
library(plotly)
library(Biostrings)
library(seqinr)
source("drawPlotly.R")

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
	seqString <- read.fasta("data/seq.fasta", seqonly=TRUE)
	sequence <- DNAString(seqString[[1]])
	covData <- scan("data/cov_file", integer(), sep =",", skip = 1)
	# Drawing Chart. Call to "drawPlotly.R" script
	output$plot <- renderPlotly({
		drawChart(sequence, covData, input$range, input$chartType)
	})
	# Downloadable csv of selected dataset ----
	output$downloadData <- downloadHandler(
		filename = function() {
			paste("results", ".csv", sep = "")
		},
		content = function(file) {
			dataSet <- getData(sequence, covData, input$range, input$chartType)
			write.csv(dataSet, file, row.names = FALSE)
		}
	)
}

shinyApp(ui = ui, server = server)
