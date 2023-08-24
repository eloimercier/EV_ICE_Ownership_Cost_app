
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
  car_ini <- list(name=NA, MSRP=NA, engine=NA, efficiency=NA, fuel_rate=NA, fuel_increase=NA, maintenance=NA)

	dataTables <- reactiveValues(car_data=NA, rebates=NA, taxes=NA, gas=NA, electricity=NA, fees=NA)
	dataVariable <- reactiveValues(region=NA, federal_rebate=NA, federal_max_msrp=NA, region_rebate=NA, region_max_msrp=NA, tax=NA, gas_rate=NA, electricity_rate=NA, #province specific
                                  ice_efficiency=8, ice_maintenance=450, bev_efficiency=15, bev_maintenance=250, depreciation_rate=0.13, gas_increase=0.05, electricity_increase=0.01) #global
    carSelection <- reactiveValues(car1=car_ini, car2=car_ini, car3=car_ini, car4=car_ini, car5=car_ini)
    modelVariableTable <- reactiveValues(df=NA) #store variable needed for computation

	observe({

    #car_data
		car_data <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Cars", check.names = TRUE)
    rownames(car_data) <- paste(car_data$Make, car_data$Model, car_data$Trim)
    colnames(car_data) <- c("Make","Model", "Trim", "Engine", "MSRP (CAD)", "Link")
    dataTables$car_data <- car_data

		dataTables$rebates <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Rebates")
		dataTables$taxes <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Taxes", rowNames = TRUE)
		dataTables$gas <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Gas", rowNames = TRUE)
		dataTables$electricity <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Electricity", rowNames = TRUE)
		dataTables$delivery_fees <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Fees", rowNames = TRUE)
	})

##############################################################
######################### SETUP ##########################
##############################################################


observeEvent(input$info, {
    version <- read.table("VERSION")[1,1]
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
	car_list$delivery_fees <- sapply(car_list$Make, function(x){dataTables$delivery_fees[x,1]})
	car_list$delivery_fees[is.na(car_list$delivery_fees)] <- 0
	car_list$after_tax_fees <- (car_list[,"MSRP (CAD)"] + car_list$delivery_fees) * (1+dataVariable$tax) #(msrp + delivery) * taxes

	car_list$vehicle_federal_rebate <- ifelse(car_list[,"MSRP (CAD)"]<=dataVariable$federal_max_msrp & car_list$Engine=="BEV", dataVariable$federal_rebate ,0)
	car_list$vehicle_region_rebate <- ifelse(car_list[,"MSRP (CAD)"]<=dataVariable$region_max_msrp & car_list$Engine=="BEV", dataVariable$region_rebate ,0)
	car_list$after_rebates <- car_list$after_tax_fees - car_list$vehicle_federal_rebate - car_list$vehicle_region_rebate
	car_list <- car_list[order(car_list$after_tax_fees,decreasing = FALSE),]
	
	if(is.null(input$region) | identical(input$region,'')){
	  car_list$after_tax_fees <- car_list$after_rebates<- "-"
	}
	
	colnames(car_list) <- c("Make","Model", "Trim", "Engine", "MSRP", "Delivery fees", "After tax and fees", "Federal rebate", "Region rebate", "After rebates")
	colnames(car_list)[9] <- paste0(dataVariable$region," rebate")
	car_list
	
}, selection = 'single', rownames=FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis'), columnDefs = list(
  list(targets = c(6,8,9), visible = FALSE)
)))

##############################################################
######################### COMPARISON #########################
##############################################################

output$modelVariableUI <- renderUI({
  tagList(
    numericInput("yearly_km", "Km driven yearly:", 10000, min = 1, max = 50000, step=1000),
    numericInput("keep_years", "How many years will you keep the car for:", 10, min = 1, max = 20, step=1)
  )
})

#### CAR SELECTION UI #### 

output$CarSelection0UI <- renderUI({
    HTML(paste0('<br><b>Make </b>','<br><br><br><br>',
          '<b>Model </b>','<br><br><br><br>',
          '<b>Trim </b>','<br><br>'
    ))
})

#Car1
output$CarMake1UI <- renderUI({
    makers <- sort(unique(dataTables$car_data$Make))
    selectizeInput('make1', '', choices = c(Choose='',makers))
})
output$CarModel1UI <- renderUI({
    maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make1,"Model"])
    selectizeInput('model1', '', choices = maker_models)
})
output$CarTrim1UI <- renderUI({
    maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make1 & dataTables$car_data$Model==input$model1,"Trim"]
    selectizeInput('trim1', '', choices = maker_model_trims)
})

#Car2
output$CarMake2UI <- renderUI({
    makers <- sort(unique(dataTables$car_data$Make))
    selectizeInput('make2', '', choices = c(Choose='',makers))
})
output$CarModel2UI <- renderUI({
    maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make2,"Model"])
    selectizeInput('model2', '', choices = maker_models)
})
output$CarTrim2UI <- renderUI({
    maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make2 & dataTables$car_data$Model==input$model2,"Trim"]
    selectizeInput('trim2', '', choices = maker_model_trims)
})

#Car 3
output$CarMake3UI <- renderUI({
    makers <- sort(unique(dataTables$car_data$Make))
    selectizeInput('make3', '', choices = c(Choose='',makers))
})
output$CarModel3UI <- renderUI({
    maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make3,"Model"])
    selectizeInput('model3', '', choices = maker_models)
})
output$CarTrim3UI <- renderUI({
    maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make3 & dataTables$car_data$Model==input$model3,"Trim"]
    selectizeInput('trim3', '', choices = maker_model_trims)
})

#Car 4
output$CarMake4UI <- renderUI({
    makers <- sort(unique(dataTables$car_data$Make))
    selectizeInput('make4', '', choices = c(Choose='',makers))
})
output$CarModel4UI <- renderUI({
    maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make4,"Model"])
    selectizeInput('model4', '', choices = maker_models)
})
output$CarTrim4UI <- renderUI({
    maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make4 & dataTables$car_data$Model==input$model4,"Trim"]
    selectizeInput('trim4', '', choices = maker_model_trims)
})

#Car 5
output$CarMake5UI <- renderUI({
    makers <- sort(unique(dataTables$car_data$Make))
    selectizeInput('make5', '', choices = c(Choose='',makers))
})
output$CarModel5UI <- renderUI({
    maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make5,"Model"])
    selectizeInput('model5', '', choices = maker_models)
})
output$CarTrim5UI <- renderUI({
    maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make5 & dataTables$car_data$Model==input$model5,"Trim"]
    selectizeInput('trim5', '', choices = maker_model_trims)
})

#### UPDATE SELECTION #### 

observeEvent(c(input$make1, input$model1, input$trim1),{
    car_long_name <- paste(input$make1, input$model1, input$trim1)
    msrp <- dataTables$car_data[car_long_name,"MSRP (CAD)"]
    engine <- dataTables$car_data[car_long_name,"Engine"]
    efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
    fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
    fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
    maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
    carSelection$car1 <- list(name=car_long_name, MSRP=msrp, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance)
})

observeEvent(c(input$make2, input$model2, input$trim2),{
    car_long_name <- paste(input$make2, input$model2, input$trim2)
    msrp <- dataTables$car_data[car_long_name,"MSRP (CAD)"]
    engine <- dataTables$car_data[car_long_name,"Engine"]
    efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
    fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
    fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
    maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
    carSelection$car2 <- list(name=car_long_name, MSRP=msrp, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance)
})

observeEvent(c(input$make3, input$model3, input$trim3),{
    car_long_name <- paste(input$make3, input$model3, input$trim3)
    msrp <- dataTables$car_data[car_long_name,"MSRP (CAD)"]
    engine <- dataTables$car_data[car_long_name,"Engine"]
    efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
    fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
    fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
    maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
    carSelection$car3 <- list(name=car_long_name, MSRP=msrp, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance)
})

observeEvent(c(input$make4, input$model4, input$trim4),{
    car_long_name <- paste(input$make4, input$model4, input$trim4)
    msrp <- dataTables$car_data[car_long_name,"MSRP (CAD)"]
    engine <- dataTables$car_data[car_long_name,"Engine"]
    efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
    fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
    fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
    maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
    carSelection$car4 <- list(name=car_long_name, MSRP=msrp, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance)
})

observeEvent(c(input$make5, input$model5, input$trim5),{
    car_long_name <- paste(input$make5, input$model5, input$trim5)
    msrp <- dataTables$car_data[car_long_name,"MSRP (CAD)"]
    engine <- dataTables$car_data[car_long_name,"Engine"]
    efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
    fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
    fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
    maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
    carSelection$car5 <- list(name=car_long_name, MSRP=msrp, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance)
})



#### DISPLAY VARIABLE TABLE #### 

output$model_variable_info <- renderUI({
  HTML(paste0('<b>','Change default values by double clicking on a cell to edit.','</b>'))
})


observe({
    if(!is.null(input$yearly_km)){
        c0 <- c("MSRP (CAD)", "Kms driven (yearly)", "Efficency (L/kWh per 100km)", "Fuel rate (CAD per L or kWh)", "Fuel price increase (CAD)", "Yearly Maintenance (CAD)")
        c1 <- data.frame(c(carSelection$car1$MSRP, input$yearly_km, carSelection$car1$efficiency, carSelection$car1$fuel_rate, carSelection$car1$fuel_increase, carSelection$car1$maintenance))
        c2 <- data.frame(c(carSelection$car2$MSRP, input$yearly_km, carSelection$car2$efficiency, carSelection$car2$fuel_rate, carSelection$car1$fuel_increase, carSelection$car2$maintenance))
        c3 <- data.frame(c(carSelection$car3$MSRP, input$yearly_km, carSelection$car3$efficiency, carSelection$car3$fuel_rate, carSelection$car1$fuel_increase, carSelection$car3$maintenance))
        c4 <- data.frame(c(carSelection$car4$MSRP, input$yearly_km, carSelection$car4$efficiency, carSelection$car4$fuel_rate, carSelection$car1$fuel_increase, carSelection$car4$maintenance))
        c5 <- data.frame(c(carSelection$car5$MSRP, input$yearly_km, carSelection$car5$efficiency, carSelection$car5$fuel_rate, carSelection$car1$fuel_increase, carSelection$car5$maintenance))
        car_selected_table <- cbind(c0,c1,c2,c3,c4,c5)
        colnames(car_selected_table) <- c("",carSelection$car1$name, carSelection$car2$name, carSelection$car3$name, carSelection$car4$name, carSelection$car5$name)
        rownames(car_selected_table) <- c0
        modelVariableTable$df <- car_selected_table   
    }
})


output$CarSelectedVariableTable <- renderDataTable({
    car_selected_table <- modelVariableTable$df
    datatable(car_selected_table, colnames = rep("", ncol(car_selected_table)), rownames=FALSE, 
            selection = list(mode = 'single', target = 'column', selectable = c(-1)),
            editable = list(target = "cell", disable = list(columns = c(0))),
            options = list( dom = 't', ordering=FALSE, autoWidth = F,
                columnDefs = list(list(className = 'text-center', width="150px", targets = c(0)),
                                list(className = 'text-center', width="200px", targets = c(1,2,3,4,5))
                    ) 
            )                
    )    
},server = TRUE)

observeEvent(input$CarSelectedVariableTable_cell_edit, {
    i <- input$CarSelectedVariableTable_cell_edit$row
    j <- input$CarSelectedVariableTable_cell_edit$col+1
    modelVariableTable$df[i,j] <- input$CarSelectedVariableTable_cell_edit$value
})




#### CAR COMPARISON TABLE #### 

comparisonData <- reactiveValues(df=NA)

compute_ownership_cost <- function(purchase_price, kms, kept_years, fuel_per_100km, fuel_rate, fuel_increase, maintenance){
#formula to get cost of ownership over X years
    fuel_cost <- kms * fuel_per_100km/100 * fuel_rate/100 * kept_years 
    fuel_increase_cost <- kms * fuel_per_100km/100 * fuel_increase * kept_years
    maintenance_cost <- maintenance * kept_years
    ownership_cost <- purchase_price + fuel_cost + fuel_increase_cost + maintenance_cost
    ownership_cost <- round(ownership_cost,2)
    return(ownership_cost)
}

observe({
    if(!is.null(input$keep_years)){
        df <- data.frame(matrix(ncol=6, nrow=input$keep_years))
        df[,1] <- seq_len(input$keep_years)
        car_selected_table <- modelVariableTable$df
        i=2
    print(paste0("fuel_rate 1:", car_selected_table["Fuel rate (CAD per L or kWh)",i]))
    print(paste0("fuel_increase 1:", car_selected_table["Fuel price increase (CAD)",i]))
            TOTO <<- compute_ownership_cost(purchase_price=car_selected_table["MSRP (CAD)",i], kms=input$yearly_km, kept_years=seq_len(input$keep_years), fuel_per_100km=car_selected_table["Efficency (L/kWh per 100km)",i], fuel_rate=car_selected_table["Fuel rate (CAD per L or kWh)",i], fuel_increase=car_selected_table["Fuel price increase (CAD)",i], maintenance=car_selected_table["Yearly Maintenance (CAD)",i])
        for (i in 2:6){
        df[,i] <- compute_ownership_cost(purchase_price=car_selected_table["MSRP (CAD)",i], kms=input$yearly_km, kept_years=seq_len(input$keep_years), fuel_per_100km=car_selected_table["Efficency (L/kWh per 100km)",i], fuel_rate=car_selected_table["Fuel rate (CAD per L or kWh)",i], fuel_increase=car_selected_table["Fuel price increase (CAD)",i], maintenance=car_selected_table["Yearly Maintenance (CAD)",i])
        }
        colnames(df) <- c("Year", carSelection$car1$name, carSelection$car2$name, carSelection$car3$name, carSelection$car4$name, carSelection$car5$name)
        comparisonData$df <- df
    }
})


output$CarCostTableTEST <- renderDataTable({
  car_comparison_table <- comparisonData$df
  datatable(car_comparison_table, rownames=FALSE, 
        selection = list(mode = 'single'),
        options = list(ordering=FALSE, autoWidth = F, pageLength = 20, dom = 't',
            columnDefs = list(list(className = 'dt-head-center', targets = '_all'), #centered colnames
                              list(className = 'text-center', width="150px", targets = c(0)), #defined first column
                              list(className = 'text-center', width="200px", targets = c(1,2,3,4,5)) #defined remaining columns
                ) 
        )                
    ) 
})


#### CAR COMPARISON PLOT #### 


output$CarCostplotTEST <- renderPlot({
    plot(1)
})




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
  
  #### DEFAULT MODEL VARIABLES #### 
  output$default_model_variable_table <- renderDT({
      default_variables <- c(
        "ICE L/100km"=dataVariable$ice_efficiency,
        "ICE maintenance (CAD, yearly)"=dataVariable$ice_maintenance,
        "BEV kWh/100km"=dataVariable$bev_efficiency,
        "BEV maintenance (CAD, yearly)"=dataVariable$bev_maintenance,
        "Depreciation rate (yearly)"=dataVariable$depreciation_rate,
        "Gas price increase (CAD, yearly)"=dataVariable$gas_increase)
      model_variable_table <- data.frame(Parameters=names(default_variables),
                                         Values=default_variables)
      model_variable_table
  }, selection = 'single', rownames=FALSE, options = list(autoWidth = T))
  
}
