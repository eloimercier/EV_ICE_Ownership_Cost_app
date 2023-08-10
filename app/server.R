
server <- function(input, output, session) {

##############################################################
######################### READ DATA ##########################
##############################################################

	data <- reactiveValues(car_data=NA, rebates=NA, taxes=NA, gas=NA, electricity=NA, fees=NA, sources=NA)

	observe({
		data$car_data <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Cars")
		data$rebates <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Rebates")
		data$taxes <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Taxes")
		data$gas <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Gas")
		data$electricity <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Electricity")
		data$fees <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Fees")
		data$sources <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Sources")
	})

##############################################################
######################### SETUP ##########################
##############################################################

output$setupUI <- renderUI({
	province_list <- unique(data$gas[,1])
        tagList(
          	selectInput('province', 'Province/Territory:', c(Choose='', province_list), selectize=FALSE),
          	conditionalPanel(condition = "input.province == 'British Columbia'",
  				selectInput("income", "Income:", list("Less than 80k$ ind. income", "Between 80k$ and 90k$ ind. income", "More than 90k$ ind. income"))
			)
        )
})

output$rebate_table <- renderDT({
	rebates <- data$rebates
	if(!is.null(input$province) & !identical(input$province,'')){
		if(input$province=='British Columbia'){
			rebates[rebates[,1] == input$province & rebates[,4]==input$income,,drop=FALSE]
		} else {
			rebates[rebates[,1] == input$province,,drop=FALSE]
		}
	}
})

output$rebate_info <- renderUI({
	rebates <- data$rebates
	REBATES <<- rebates
	if(!is.null(input$province) & !identical(input$province,'')){
		federal_rebate <- rebates[rebates[,1]=="Federal","Maximum.amount"]

		if(input$province=='British Columbia'){
			province_rebate <- rebates[rebates[,1] == input$province & rebates[,4]==input$income,"Maximum.amount",drop=FALSE]
		} else {
			province_rebate <- rebates[rebates[,1] == input$province,"Maximum.amount",drop=FALSE]
		}


		HTML(paste0('<b>','Federal Rebate:</b> $',federal_rebate,'<br>',
					'<b>','Provincial Rebate:</b> $',province_rebate,'<br>'
			))

	}



})

##############################################################
######################### CAR LIST ##########################
##############################################################


output$car_table <- renderDT(
	data$car_data
)



}
