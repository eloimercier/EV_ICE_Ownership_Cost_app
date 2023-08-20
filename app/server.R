
server <- function(input, output, session) {

  verbose <- FALSE
  
#TODO:
  #manual overide of tax, gas, electricity rate + add message in top right corner that it's using cusom values
  #message in top right corner if no province selected
  #change Regions to Provinces
  #add walkthrough
  
##############################################################
######################### READ DATA ##########################
##############################################################

	dataTables <- reactiveValues(car_data=NA, rebates=NA, taxes=NA, gas=NA, electricity=NA, fees=NA)
	dataVariable <- reactiveValues(region=NA, federal_rebate=NA, federal_max_msrp=NA,  region_rebate=NA, region_max_msrp=NA, tax=NA, gas_rate=NA, electricity_rate=NA)

	observe({
		dataTables$car_data <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Cars", check.names = TRUE)
		dataTables$rebates <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Rebates")
		dataTables$taxes <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Taxes", rowNames = TRUE)
		dataTables$gas <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Gas", rowNames = TRUE)
		dataTables$electricity <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Electricity", rowNames = TRUE)
		dataTables$delivery_fees <- read.xlsx("../data/EV_list_Canada.xlsx", sheet="Fees", rowNames = TRUE)
	})

##############################################################
######################### SETUP ##########################
##############################################################


observeEvent(input$info, {
    version <- read.table("../VERSION")[1,1]
    print(version)
    shinyalert("EV Comparator App", HTML(paste0(version,"<br><br>
      This document focuses on affordable Electric Vehicles (EVs) available in Canada. It is not meant to encompass all EVs on the market. A few Internal Combustion Engine (ICE) vehicles have been included to the list for comparison.<br><br>
      Prices are in CAD.<br><br>
      <i>The information in this document are provided for information only with no guarantee of accuracy</i>")), type = "info", html=TRUE) 
})

output$githubLink <- renderUI({
  #link to github page
  tags$li(class = "dropdown",
      tags$a(img(src="https://img.icons8.com/?size=512&id=62856&format=png",  width="48", height="48"), href="https://github.com/eloimercier/EV_app")
  )
  })
	
output$setupUI <- renderUI({
	region_list <- unique(dataTables$rebates[,1])
	region_list <- region_list[-c(which(region_list=="Federal"), grep("Source", region_list))]
        tagList(
          	selectInput('region', 'Province/Territory:', c(Choose='', region_list), selectize=FALSE),
          	conditionalPanel(condition = "input.region == 'British Columbia'",
    				    selectInput("income", "Income:", list("Less than 80k$ ind. income", "Between 80k$ and 90k$ ind. income", "More than 90k$ ind. income"))
  			    )
        )
})
	
observe({
  if(!is.null(input$region) & !identical(input$region,'')){
      
    dataVariable$region <- input$region

    #Calculate rebates
    rebates <- dataTables$rebates
    region_occurences <- table(dataTables$rebates[,"Region"])
    region_with_multiple_rebate <- names(region_occurences)[region_occurences>1] #which regions has variable rebates?
    
    dataVariable$federal_rebate <- as.numeric(rebates[rebates[,1] == "Federal","Maximum.amount"])
    dataVariable$federal_max_msrp <- as.numeric(rebates[rebates[,1] == "Federal","If.MSRP.below..."])
    
    if(input$region %in% region_with_multiple_rebate){
      dataVariable$region_rebate <-  as.numeric(rebates[rebates[,1] == input$region & rebates[,4]==input$income,"Maximum.amount",drop=TRUE])
      dataVariable$region_max_msrp <-  as.numeric(rebates[rebates[,1] == input$region & rebates[,4]==input$income,"If.MSRP.below...",drop=TRUE])
    } else {
      dataVariable$region_rebate <-  as.numeric(rebates[rebates[,1] == input$region,"Maximum.amount",drop=TRUE])
      dataVariable$region_max_msrp <-  as.numeric(rebates[rebates[,1] == input$region,"If.MSRP.below...",drop=TRUE])
    }

    #Set rates
    dataVariable$tax <- as.numeric(dataTables$taxes["Federal",1]) + as.numeric(dataTables$taxes[input$region,1])
    dataVariable$gas_rate  <- as.numeric(dataTables$gas[input$region,1])
    dataVariable$electricity_rate <- as.numeric(dataTables$electricity[input$region,1])
    
    if(verbose){
      print(paste0("federal_max_msrp: ", dataVariable$federal_max_msrp))
      print(paste0("region_max_msrp: ", dataVariable$region_max_msrp))
      print(paste0("federal_rebate: ", dataVariable$federal_rebate))
      print(paste0("region_rebate: ", dataVariable$region_rebate))
      print("***********")
    }
  }
  

})


output$rebate_info <- renderUI({
	rebates <- dataTables$rebates
	if(!is.null(input$region) & !identical(input$region,'')){
		federal_rebate <- dataVariable$federal_rebate
		federal_msrp <- dataVariable$federal_max_msrp
		region_rebate <- dataVariable$region_rebate
		region_msrp <- dataVariable$region_max_msrp
		
		federal_rebate_text <- ifelse(federal_rebate>0,
		                              paste0("Up to ",federal_rebate,"$"),
		                              "No rebate.")
		region_rebate_text <- ifelse(region_rebate>0,
		                             paste0("Up to ",region_rebate,"$"),
		                             "No rebate.")
		federal_msrp_text <- ifelse(!is.na(federal_msrp),
                          		  paste0(" on vehicles below $",federal_msrp,"."),
                          		  "")
		region_msrp_text <- ifelse(!is.na(region_msrp),
	                              paste0(" on vehicles below $",region_msrp,"."),
	                              "")
		
		HTML(paste0('<b>','Federal Rebate: </b>',federal_rebate_text, federal_msrp_text,'<br>',
					'<b>','Provincial Rebate: </b>',region_rebate_text, region_msrp_text,'<br>'
		))
	}
})

##############################################################
######################### CAR LIST ##########################
##############################################################

output$car_table <- renderDT({
	car_list <- dataTables$car_data
	car_list <- car_list[,-which(colnames(car_list)=="Link")]
	car_list$delivery_fees <- sapply(car_list$Maker, function(x){dataTables$delivery_fees[x,1]})
	car_list$delivery_fees[is.na(car_list$delivery_fees)] <- 0
	car_list$after_tax_fees <- (car_list[,"MSRP..CAD."] + car_list$delivery_fees) * (1+dataVariable$tax) #(msrp + delivery) * taxes

	car_list$vehicle_federal_rebate <- ifelse(car_list[,"MSRP..CAD."]<=dataVariable$federal_max_msrp & car_list$Engine=="BEV", dataVariable$federal_rebate ,0)
	car_list$vehicle_region_rebate <- ifelse(car_list[,"MSRP..CAD."]<=dataVariable$region_max_msrp & car_list$Engine=="BEV", dataVariable$region_rebate ,0)
	car_list$after_rebates <- car_list$after_tax_fees - car_list$vehicle_federal_rebate - car_list$vehicle_region_rebate
	car_list <- car_list[order(car_list$after_tax_fees,decreasing = FALSE),]
	
	if(is.null(input$region) | identical(input$region,'')){
	  car_list$after_tax_fees <- car_list$after_rebates<- "-"
	}
	
	colnames(car_list) <- c("Maker","Model", "Trim", "Engine", "MSRP", "Delivery fees", "After tax and fees", "Federal rebate", "Region rebate", "After rebates")
	colnames(car_list)[9] <- paste0(dataVariable$region," rebate")
	car_list
	
}, selection = 'single', rownames=FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis'), columnDefs = list(
  list(targets = c(6,8,9), visible = FALSE)
)))

##############################################################
######################### COMPARISON #########################
##############################################################

#tabPanel("Model Variables",  DTOutput("model_table")),
#tabPanel("Comparison",  DTOutput("comparison_table"), plotOutput("comparison_plot"))

output$modelVariableUI <- renderUI({
 
  tagList(
    numericInput("yearly_km", "Km driven yearly:", 10000, min = 1, max = 50000, step=1000),
    numericInput("keep_years", "How many years will you keep the car for:", 10, min = 1, max = 20, step=1),
    actionButton("confimrVariableBttn", "Submit")
  )
})


#### MODEL VARIABLES #### 
output$model_variable_info <- renderUI({
  HTML(paste0('<b>','Here are the defaut parameters used by the model. It might not correspond to your scenari. You cna use custom values by double clicking on a value to edit.','</b>'))
})

output$model_variable_table <- renderDT({
  
  
  default_variables <- c("Yearly km"=12000,
    "Gas ($/L)"=dataVariable$gas_rate,
    "Electricity ($/kWh)"=dataVariable$electricity_rate,
    "Keep car for X years"=10,
    "ICE L/100km"=8.0,
    "ICE maintenance (yearly)"=450,
    "BEV kWh/100km"=15,
    "BEV maintenance (yearly)"=250,
    "Depreciation rate after X years"=0.13)
  
  
  model_variable_table <- data.frame(Parameters=names(default_variables),
                                     Values=default_variables
                                     )
  model_variable_table
  
  
}, selection = 'single', rownames=FALSE, editable=TRUE)






##############################################################
######################### PARAMETERS #########################
##############################################################

#### REBATES #### 
output$rebate_table <- renderDT({
  rebate_table <- dataTables$rebates
  rebate_table <- rebate_table[-grep("Source", rebate_table$Region),,drop=FALSE]
  colnames(rebate_table) <- c("Region", "Maximum Amount", "If MSRP below...", "Condition")
  rebate_table
}, selection = 'single', rownames=FALSE)

output$rebate_source <- renderUI({
  rebate_table <- dataTables$rebates
  rebate_source <- rebate_table[grep("Source", rebate_table$Region),,drop=FALSE]
  HTML(paste0('<b>', '<a href=',rebate_source[,2],'>',rebate_source[,1],'</a>','</b>'))
})

#### TAXES #### 
  output$tax_table <- renderDT({
    tax_table <- dataTables$taxes
    tax_table <- tax_table[-grep("Source", rownames(tax_table)),,drop=FALSE]
    tax_table$Region <- rownames(tax_table)
    tax_table[,2:1]
  }, selection = 'single', rownames=FALSE)
  
  output$tax_source <- renderUI({
    tax_table <- dataTables$taxes
    tax_source <- tax_table[grep("Source", rownames(tax_table)),,drop=FALSE]
    HTML(paste0('<b>', '<a href=',tax_source[,1],'>',rownames(tax_source),'</a>','</b>'))
  })
  
  #### GAS #### 
  output$gas_table <- renderDT({
    gas_table <- dataTables$gas
    gas_table <- gas_table[-grep("Source", rownames(gas_table)),,drop=FALSE]
    gas_table$Region <- rownames(gas_table)
    gas_table[,2:1]
  }, selection = 'single', rownames=FALSE)
  
  output$gas_source <- renderUI({
    gas_table <- dataTables$gas
    gas_source <- gas_table[grep("Source", rownames(gas_table)),,drop=FALSE]
    HTML(paste0('<b>', '<a href=',gas_source[,1],'>',rownames(gas_source),'</a>','</b>'))
  })
  
  #### ELECTRICITY #### 
  output$electricity_table <- renderDT({
    electricity_table <- dataTables$electricity
    electricity_table <- electricity_table[-grep("Source", rownames(electricity_table)),,drop=FALSE]
    electricity_table$Region <- rownames(electricity_table)
    electricity_table[,2:1]
  }, selection = 'single', rownames=FALSE)
  
  output$electricity_source <- renderUI({
    electricity_table <- dataTables$electricity
    electricity_source <- electricity_table[grep("Source", rownames(electricity_table)),,drop=FALSE]
    HTML(paste0('<b>', '<a href=',electricity_source[,1],'>',rownames(electricity_source),'</a>','</b>'))
  })
  
  #### DELIVERY FEES #### 
  
  output$delivery_fees_table <- renderDT({
    delivery_fees_table <- dataTables$delivery_fees
    delivery_fees_table$Region <- rownames(delivery_fees_table)
    delivery_fees_table[,2:1]
  }, selection = 'single', rownames=FALSE)
  

}
