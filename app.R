

## Loading libraries
library(RISmed)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinycssloaders)


## Counts number of articles returned by query in period between start and end
count_in_bin = function(query, start, end)
{
  res = EUtilsSummary(query, type = "esearch", db = "pubmed", datetype = 'pdat', mindate = start, maxdate = end, retmax=1000)
  n = QueryCount(res)
  return(n)
}

## Computes bins between start and end, to get a number of bins approximately equal to nb_bins 
compute_bins = function(start, end, nb_bins)
{
  res = pretty(c(start,end),nb_bins)
  bounds = data.frame(xstart = head(res,-1), xend = res[-1])
  return(bounds)
  
}

user_interface = fluidPage(
 # App title
 titlePanel('Pubmed relative trends'),
 # Sidebar layout with input and output definitions
 sidebarLayout(
  # Sidebar panel for inputs
	sidebarPanel(
		# Enter two valid Pubmed queries.
		# For help with the syntax, look here https://pubmed.ncbi.nlm.nih.gov/help/
		# Text input for query 1
		textInput(inputId = "query1",
                label = "First Pubmed query (numerator):",
                value = "Arabidopsis thaliana"),
		# Text input for query 2
		textInput(inputId = "query2",
                label = HTML("Second Pubmed query (denominator):"),
                value = "Drosophila melanogaster"),
    # Select input for start year
    selectInput(inputId = "start_year", 
        		label = "From:", 
        		choices = seq(1850,as.numeric(format(Sys.Date(), "%Y"))), 
        		selected = "1970"),
    # Select input for end year
    selectInput(inputId = "end_year", 
        		label = "To:", 
        		choices = seq(1850,as.numeric(format(Sys.Date(), "%Y"))), 
        		selected = format(Sys.Date(), "%Y")),
    # Numeric input for approx number of bins to group years
    numericInput(inputId = "nb_bins", 
        		label = "Approximate number of bins:", 
        		min = 3, 
        		max = 50,
        		value = 5),
		# numeric input for minimum count in query 2 to display the ratio
		numericInput(inputId = "min_count",
		             label = "Minimum number of articles from query 2 for display",
		             value = 10,
		             min = 10),
		actionButton(inputId = "plot",
		             label = "Draw Plot"),
	),
 
  # Main panel for displaying outputs
 	mainPanel(
 	# Output: Scatter plot
 	    h4("Ratio of the number of articles returned by the two queries"),
      plotOutput(outputId = "scatterPlot") %>% withSpinner(color="#323637")
			)
	)
)

server = function(input, output)
{
  drawplot = eventReactive(input$plot,
                           {
                             ## Getting input queries and dates
                             query1 = input$query1
                             query2 = input$query2
                             start_year = as.numeric(input$start_year)
                             end_year = as.numeric(input$end_year)
                             nb_bins = as.numeric(input$nb_bins)
                             min_count = as.numeric(input$min_count)
                             
                             bounds = compute_bins(start_year, end_year, nb_bins)
                             
                             count1 = NA
                             count2 = NA

                             for(i in 1:nrow(bounds))
                             {
                               count1 = c(count1,count_in_bin(query1, bounds$xstart[i], bounds$xend[i]))
                               count2 = c(count2,count_in_bin(query2, bounds$xstart[i], bounds$xend[i]))
                             }

                             count1 = count1[-1]
                             count2 = count2[-1]
                             
                             counts = bounds %>% mutate(xmid = (xstart + xend)/2,
                                                        count1 = count1,
                                                        count2 = count2,
                                                        ratio = ifelse(count2 >= min_count, count1/count2, NA))
                             
                               
                             gg = ggplot(counts, aes(xmid,ratio)) + 
                               geom_point() +
                               geom_line() +
                               xlab("Year") +
                               ylab("Ratio")
                             print(gg)
                           })
	output$scatterPlot <- renderPlot({
	drawplot()
    })
	
}






shinyApp(ui = user_interface, server = server)