library(shiny)

# Define UI for miles per gallon application
shinyUI(bootstrapPage(
	tags$style(type='text/css', "#TP, #FP, #FN, #TN { width: 60px;}"),
  
	tagList(
  	  	tags$head(tags$title("2-by-2 Diagram")),
  	  	div(class = "row-fluid",
			div(class="span10", style="padding: 10px 30px;", 
   	 			h1("2-by-2 Diagram")
   	 		),
  			div(class = 'span2', style = "padding: 20px 0px;",
   	 	 		downloadButton('downloadPlot', 'Download Plot')
   	 	 	)
   	 	)
   	),
   	
   	div(class = "row-fluid",
		div(class = 'span2', style = 'padding:0px 30px; width:250px',
				HTML('<table cellpadding = 5 border = 1>
				      <tr><td></td><th> Abnormal </th><th> Normal </th>
				      <tr><th width = 10> Pos Test </th><td>'),
				textInput('TP', '', "10"),
				HTML("</td><td>"),
				textInput('FP', '', "10"),
				HTML("</tr><tr><th> Neg Test </th><td>"),
				textInput('FN', '', "10"),
				HTML("</td><td>"),
				textInput('TN', '', "10"),
				HTML("</td></tr></table>"),
				checkboxInput('square', 'Square Coordinates', TRUE),
				uiOutput('stat_html'),
				plotOutput('legend_plot')
		),
		div(class = 'span8',
				plotOutput('truth', width = 'auto', height = 'auto')
		)
	)	

))