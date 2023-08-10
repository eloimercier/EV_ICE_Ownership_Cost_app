
server <- function(input, output, session) {

##############################################################
######################### READ XLSX ##########################
##############################################################

data <- reactiveValues(car_data=NA)

observe({
	a <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Cars")
	TOTO <<- a
 	data$car_data <- a
})

output$car_table <- renderDT(
	data$car_data
	)



}
