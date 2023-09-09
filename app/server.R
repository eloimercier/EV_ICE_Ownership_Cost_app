
server <- function(input, output, session) {

  verbose <- FALSE
  
#TODO:
  #add depreciation in model variable
  #message in top right corner if no province selected
  #change Regions to Provinces
  #add walkthrough
  #option to select miles or km, mpg or L/km
  #better naming scheme
  
##############################################################
########################### SETUP ############################
##############################################################

    countrySpecificData <- reactiveValues(names_for_regions="Province/Territory", currency_name="CAD", currency_symbol="$", currency_symbol_cent="¢", distance="kms",
        gas_efficiency="L/100kms", gas_rate="¢/L",   electricity_efficiency="kWh/100kms", electricity_rate="¢/kW"
        )

    car_ini <- list(name="", MSRP=NA, rebates=NA, purchase_price=NA, engine=NA, efficiency=NA, fuel_rate=NA, fuel_increase=NA, maintenance=NA, yearly_kms=NA, depreciation_x_years=NA)

	dataTables <- reactiveValues(car_data=NA, rebates=NA, taxes=NA, gas=NA, electricity=NA, fees=NA)
	dataVariable <- reactiveValues(region=NA, federal_rebate=NA, federal_max_msrp=NA, region_rebate=NA, region_max_msrp=NA, tax=NA, gas_rate=NA, electricity_rate=NA, #province specific
                                  ice_efficiency=8, ice_maintenance=350, bev_efficiency=15, bev_maintenance=150, depreciation_rate=13, depreciation_value_10year=25, gas_increase=5, electricity_increase=1) #global
    carSelection <- reactiveValues(car1=car_ini, car2=car_ini, car3=car_ini, car4=car_ini, car5=car_ini)
    modelVariableTable <- reactiveValues(df=NA) #store variable needed for computation
    comparisonData <- reactiveValues(cost_over_years=NA, final_cost=NA)

    reVal <- reactiveValues(current_step="step0", step_completed=c(1:4))


##############################################################
######################### READ DATA ##########################
##############################################################

	observe({
    #car_data
		car_data <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Cars", check.names = TRUE)
        rownames(car_data) <- paste(car_data$Make, car_data$Model, car_data$Trim)
        colnames(car_data) <- c("Make","Model", "Trim", "Engine", "MSRP", "Link", "Traction","Range (km)", "AC Charging rate (kW)", "DC Fast Charging rate (kW)","HP")
        dataTables$car_data <- car_data

		dataTables$rebates <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Rebates")
		dataTables$taxes <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Taxes", rowNames = TRUE)
		dataTables$gas <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Gas", rowNames = TRUE)
		dataTables$electricity <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Electricity", rowNames = TRUE)
		dataTables$delivery_fees <- read.xlsx("data/EV_list_Canada.xlsx", sheet="Fees", rowNames = TRUE)
	})


##############################################################
######################## WALKTHROUGH #########################
##############################################################

walkthrough <- Conductor$new(
    exitOnEsc = FALSE,
    keyboardNavigation = TRUE)$
    step(title="Welcome!", text="Welcome to the Electric Vehicle Comparison Tool!<br><br><h5>Press `Escape` to exit this walkthrough.</h5>",id = "1")$
    step(title="Select your region", text="This is where you select your region. <i>This will determine the parameters such has potential rebates, electricity price, etc.</i>", el="#region", id = "2")$
    step(title="Enter the kms driven", text="This is the number of kilometers that you expect to be driven each year, in average.<br><br>You can modify this value later to see how it affects the model.", el="#yearly_kms", id = "3")$
    step(title="Enter the duration of ownership", text="This is how long you are planning on keeping your next vehicle. This will determine the resale value of the vehicle.", el="#keep_years", id = "4")$
    step(title="Comparison tool", cancelIcon = list(enabled = TRUE, label = "Close"),text="This is where you compare cars.", id = "5")$
    step(title="Select cars for comparison", text="Select up to 5 cars for comaprison among the list available", el="#CarMake1UI", id = "6")$
    step(title="Update model parameters", text="Default parameters are automatically applied. You can edit them by double-clicking a cell to enter your own value.", id = "7")$
    step(title="Cost of ownership", text="This is how much you can expect to have spent at the end of the period.", el="#CostOfOwnershipTable", id = "8")$
    step(title="Cost of ownership after resale", text="Assuming you resale the vehicle at the end of the period, this is how much it did cost you.", el="#CarFinalCostTable", id = "9")$
    step(title="Plot of cost of ownership", text="This is another representation of the cost of ownership.", id = "10")$
    step(title="AAAA", text="This is another step", id = "11")$
    step(title="BBBB", text="One last step", id = "12")

    # step(title="Model parameters", text="Your country and region selection determines the potential rebates and costs. You can find all the information in the different tabs here.",  id = "5")$
    # step(title="Model parameters", text="Youre.",  id = "6")$
    # step(title="Model parameters", text="Your country and region selection determines the potential rebates and costs. You can find all the information in the different tabs here. \n You will be able to adjust them later if they do not correspond to your situation", el="#default_model_variable_table", id = "7")

    observeEvent(input$walkthroughBtn,{
        #start walktorugh
        walkthrough$init()$start()
        reVal$current_step <- walkthrough$getCurrentStep()
    })


    observe({
        current_step <- walkthrough$getCurrentStep()
        # if(identical(current_step, "5")){
        #     updateTabItems(session, inputId="sidebarID", selected="params")
        #     reVal$current_step <- current_step
        # } 

        # if(identical(current_step, "6")){
        #     updateTabsetPanel(session, inputId="param_panels", selected="default_model_variable")
        # }
        if(identical(current_step, "2")){
          updateSelectInput(session,"region", selected = "Ontario")

        } else if(identical(current_step, "5") & !(5 %in% reVal$step_completed)){
            updateTabItems(session, inputId="sidebarID", selected="compare")
            updateSelectInput(session,"make1", selected = "Audi")
            updateSelectInput(session,"make2", selected = "Honda")
            reVal$step_completed <- 1:5
        } else if(identical(current_step, "7")){
            updateCollapse(session, id="model_variable_table_collapsible", open = "model_variable_collapse", close = NULL, style = NULL)
        } else if(identical(current_step, "10")){
            updateTabsetPanel(session, inputId="comparison_panels", selected="plot")
        } else if (identical(current_step, "11")) { #reset options when we reach end of tour or if tour cancel
            updateSelectInput(session,"region", selected = "")
            # updateSelectInput(session,"make1", selected = "")
            # updateSelectInput(session,"make2", selected = "")
        }

    })


  # observeEvent(walkthrough$isActive(), {
  #    updateSelectInput(session,"region", selected = "Ontario")

  # })




# observe({
#     is_walktrhough_active <- walkthrough$isActive()
#     step <- reVal$current_step
#     if(identical(is_walktrhough_active,TRUE)){ 
#         updateSelectInput(session,"region", selected = "Ontario")
#         updateSelectInput(session,"make1", selected = "Audi")
#         updateSelectInput(session,"make2", selected = "Honda")
#     } else if(identical(step, "step0")){
#     updateSelectInput(session,"region", selected = "")
#     updateSelectInput(session,"make1", selected = "")
#     updateSelectInput(session,"make2", selected = "")
# reVal$current_step <- "TOTO"
#     }
#     })

# #check whether walkthrough is running or not
# is_walktrhough_active <- walkthrough$isActive()
# if(identical(is_walktrhough_active,TRUE)){ #if running change setting
# updateSelectizeInput(session,"dataset_selected", selected = dataSetList$datasets[[1]][1])
# dataTable$selected_row <- 2
# dataTable$show_nrows <- 6
# } else { #reset default setting
# updateSelectizeInput(session,"dataset_selected", selected = "")
# dataTable$selected_row <- NULL
# dataTable$show_nrows <- 25
# }

##############################################################
######################### USER INFO ##########################
##############################################################


observeEvent(input$info, {
    version <- read.table("VERSION")[1,1]
    shinyalert("EV Comparator App", HTML(paste0(version,"<br><br>
      This document focuses on affordable Battery Electric Vehicles (BEVs) available in Canada. It is not meant to encompass all EVs on the market. A few Internal Combustion Engine (ICE) vehicles have been included to the list for comparison.<br><br>
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

output$modelVariableUI <- renderUI({
  tagList(
    numericInput("yearly_kms", "Km driven yearly:", 10000, min = 1, max = 100000, step=1000),
    numericInput("keep_years", "How many years do you intend to keep the car for:", 10, min = 1, max = 20, step=1)
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
      region_max_msrp <-  as.numeric(rebates[rebates[,1] == input$region & rebates[,4]==input$income,"If.MSRP.below...",drop=TRUE])
      dataVariable$region_max_msrp <- ifelse(is.na(region_max_msrp),Inf, region_max_msrp)
    } else {
      dataVariable$region_rebate <-  as.numeric(rebates[rebates[,1] == input$region,"Maximum.amount",drop=TRUE])
      region_max_msrp <-  as.numeric(rebates[rebates[,1] == input$region,"If.MSRP.below...",drop=TRUE])
      dataVariable$region_max_msrp <- ifelse(is.na(region_max_msrp),Inf, region_max_msrp)
    }

    #Set rates
    dataVariable$tax <- 1 + as.numeric(dataTables$taxes["Federal",1]) + as.numeric(dataTables$taxes[input$region,1])
    dataVariable$gas_rate  <- as.numeric(dataTables$gas[input$region,1])
    dataVariable$electricity_rate <- as.numeric(dataTables$electricity[input$region,1])
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
		federal_msrp_text <- ifelse(federal_msrp<Inf,
                          		  paste0(" on BEVs below $",federal_msrp,"."),
                          		  "")
		region_msrp_text <- ifelse(region_msrp>Inf,
	                              paste0(" on BEVs below $",region_msrp,"."),
	                              "")
        max_rebate <- paste0("$",federal_rebate + region_rebate)
		
		HTML(paste0('<p style="font-size:20px;"><b>','You can benefit from up to <span style="background-color: #FFFF00">', max_rebate ,'</span> on the purchase of a new Battery Electric Vehicle (BEV)!','</b></p>',
                    '<p style="font-size:14px;"><b>','Federal Rebate: </b>',federal_rebate_text, federal_msrp_text,'</p>',
        			'<p style="font-size:14px;"><b>','Provincial Rebate: </b>',region_rebate_text, region_msrp_text,'</p><br>'
		))
	} else {
        HTML('<p style="font-size:20px;"><b>Select your Province or Territory to see how much you can save on the purchase of a new Battery Electric Vehicles (BEV)!</b></p>')
    }
})

##############################################################
######################### CAR LIST ##########################
##############################################################

output$car_table <- renderDataTable({
	car_list <- dataTables$car_data
	car_list <- car_list[,-which(colnames(car_list)=="Link")]

	car_list$delivery_fees <- sapply(car_list$Make, function(x){dataTables$delivery_fees[x,1]})
	car_list$delivery_fees[is.na(car_list$delivery_fees)] <- 0
	car_list$after_tax_fees <- round((car_list[,"MSRP"] + car_list$delivery_fees) * (dataVariable$tax) ,2)

	car_list$vehicle_federal_rebate <- ifelse(car_list[,"MSRP"]<=dataVariable$federal_max_msrp & car_list$Engine=="BEV", dataVariable$federal_rebate ,0)
	car_list$vehicle_region_rebate <- ifelse(car_list[,"MSRP"]<=dataVariable$region_max_msrp & car_list$Engine=="BEV", dataVariable$region_rebate ,0)
	car_list$after_rebates <- round(car_list$after_tax_fees - car_list$vehicle_federal_rebate - car_list$vehicle_region_rebate,2)


	if(is.null(input$region) | identical(input$region,'')){
	  car_list$after_tax_fees <- car_list$after_rebates<- "-"
	}

    msrp_colname <- paste0("MSRP (", countrySpecificData$currency_name,")")
    purchase_price_colname <- paste0("Purchase Price (", countrySpecificData$currency_name,")")
    region_rebate_colname <- paste0(dataVariable$region," rebate")
    range_colname <- paste0("Range (",countrySpecificData$distance,")")
	colnames(car_list) <- c("Make", "Model", "Trim", "Engine", msrp_colname, "Traction", range_colname, "AC Charging rate (kw)", "DC Fast Charging rate (kW)", "HP", "Delivery Fees", "Price after Tax & Fees", "Federal Rebate", paste0(dataVariable$region," rebate"), purchase_price_colname)
    car_list <- car_list[,c("Make", "Model", "Trim", "Engine", msrp_colname,  "Delivery Fees", "Price after Tax & Fees", "Federal Rebate", region_rebate_colname, purchase_price_colname, "Traction", range_colname, "AC Charging rate (kw)", "DC Fast Charging rate (kW)", "HP")] #reorder columns
    car_list <- car_list[order(car_list[,purchase_price_colname],decreasing = FALSE),]
    hide_columns <- which(colnames(car_list) %in% c("Delivery Fees", "Price after Tax & Fees", "Federal Rebate", region_rebate_colname, "Traction", "AC Charging rate (kw)")) - 1 #columns hidden by default, 0-based

    #create factors for better filtering
    col_names_to_convert <- c("Make", "Engine", "Traction")
    car_list[col_names_to_convert] <- lapply(car_list[col_names_to_convert] , factor)

    #create column mouseover info
    mouseover_info = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
            th('Make', title = 'Make'),
            th('Model', title = 'Model'),
            th('Trim', title = 'Trim'),
            th('Engine', title = 'Type of engine'),
            th(msrp_colname, title = "Manufacturer's Suggested Retail Price"),
            th('Delivery Fees', title = 'Addtional delivery fees - manufacturer specific'),
            th('Price after Tax & Fees', title = 'Price accounting for delivery fees and tax'),
            th('Federal Rebate', title = 'Federal rebate if applicable'),
            th(region_rebate_colname, title = 'Regional rebate if applicable'),
            th(purchase_price_colname, title = 'Final price after rebates'),
            th('Traction', title = 'Car drivetrain'),
            th(range_colname, title = 'Distance on a full charge'),
            th('AC Charging rate (kw)', title = 'AC Charging rate (kw)'),
            th('DC Fast Charging rate (kW)', title = 'DC Fast Charging rate (kW)'),
            th('HP', title = 'Horsepower')
        )
      )
    ))

	datatable(car_list, selection = 'single', rownames=FALSE, extensions = 'Buttons', filter="top", container = mouseover_info,
        options = list(pageLength = 20, lengthMenu = c(10, 20, 50, 100), dom = 'Bfrtlip', buttons = I('colvis'), columnDefs = list(list(targets = hide_columns, visible = FALSE)))) %>% 
            formatStyle(msrp_colname, background = styleColorBar(car_list[,msrp_colname], rgb(0,0.8,0,0.3)),  backgroundSize = '98% 88%',   backgroundRepeat = 'no-repeat',  backgroundPosition = 'left')  %>% 
            formatStyle(purchase_price_colname, background = styleColorBar(car_list[,purchase_price_colname], rgb(0,0.8,0,0.3)),  backgroundSize = '98% 88%',   backgroundRepeat = 'no-repeat',  backgroundPosition = 'left')

	
})

##############################################################
######################### COMPARISON #########################
##############################################################


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

observeEvent(c(input$region, input$yearly_kms, input$make1, input$model1, input$trim1),{
    if(!is.null(input$region) & !is.null(input$trim1) & !identical(input$trim1,"")){
        car_long_name <- paste(input$make1, input$model1, input$trim1)
        engine <- dataTables$car_data[car_long_name,"Engine"]
        msrp <- dataTables$car_data[car_long_name,"MSRP"]
        if(identical(engine, "BEV")){
            rebates <- ifelse(msrp <= dataVariable$federal_max_msrp | is.na(dataVariable$federal_max_msrp), dataVariable$federal_rebate, 0) + ifelse(msrp <= dataVariable$region_max_msrp  | is.na(dataVariable$region_max_msrp),  dataVariable$region_rebate, 0)
        } else {
            rebates <- 0
        }
        delivery_fees <- dataTables$delivery_fees[input$make1,"Amount"]
        purchase_price <- round((msrp + delivery_fees) * dataVariable$tax - rebates,2)
        efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
        fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
        fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
        maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
        depreciation_rate <- calculate_depreciation_rate (dataVariable$depreciation_value_10year, input$keep_years)
        depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
        carSelection$car1 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
    }
})

observeEvent(c(input$region, input$yearly_kms, input$make2, input$model2, input$trim2),{
    if(!is.null(input$region) & !is.null(input$trim2) & !identical(input$trim2,"")){
        car_long_name <- paste(input$make2, input$model2, input$trim2)
        engine <- dataTables$car_data[car_long_name,"Engine"]
        msrp <- dataTables$car_data[car_long_name,"MSRP"]
        if(identical(engine, "BEV")){
            rebates <- ifelse(msrp <= dataVariable$federal_max_msrp | is.na(dataVariable$federal_max_msrp), dataVariable$federal_rebate, 0) + ifelse(msrp <= dataVariable$region_max_msrp  | is.na(dataVariable$region_max_msrp),  dataVariable$region_rebate, 0)
        } else {
            rebates <- 0
        }
        delivery_fees <- dataTables$delivery_fees[input$make2,"Amount"]
        purchase_price <- round((msrp + delivery_fees) * dataVariable$tax - rebates,2)
        efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
        fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
        fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
        maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
        depreciation_rate <- calculate_depreciation_rate (dataVariable$depreciation_value_10year, input$keep_years)
        depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
        carSelection$car2 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
    }
})

observeEvent(c(input$region, input$yearly_kms, input$make3, input$model3, input$trim3),{
    if(!is.null(input$region) & !is.null(input$trim3) & !identical(input$trim3,"")){
        car_long_name <- paste(input$make3, input$model3, input$trim3)
        engine <- dataTables$car_data[car_long_name,"Engine"]
        msrp <- dataTables$car_data[car_long_name,"MSRP"]
        if(identical(engine, "BEV")){
            rebates <- ifelse(msrp <= dataVariable$federal_max_msrp | is.na(dataVariable$federal_max_msrp), dataVariable$federal_rebate, 0) + ifelse(msrp <= dataVariable$region_max_msrp  | is.na(dataVariable$region_max_msrp),  dataVariable$region_rebate, 0)
        } else {
            rebates <- 0
        }
        delivery_fees <- dataTables$delivery_fees[input$make3,"Amount"]
        purchase_price <- round((msrp + delivery_fees) * dataVariable$tax - rebates,2)
        efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
        fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
        fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
        maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
        depreciation_rate <- calculate_depreciation_rate (dataVariable$depreciation_value_10year, input$keep_years)
        depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
        carSelection$car3 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
    }
})

observeEvent(c(input$region, input$yearly_kms, input$make4, input$model4, input$trim4),{
    if(!is.null(input$region) & !is.null(input$trim4) & !identical(input$trim4,"")){
        car_long_name <- paste(input$make4, input$model4, input$trim4)
        engine <- dataTables$car_data[car_long_name,"Engine"]
        msrp <- dataTables$car_data[car_long_name,"MSRP"]
        if(identical(engine, "BEV")){
            rebates <- ifelse(msrp <= dataVariable$federal_max_msrp | is.na(dataVariable$federal_max_msrp), dataVariable$federal_rebate, 0) + ifelse(msrp <= dataVariable$region_max_msrp  | is.na(dataVariable$region_max_msrp),  dataVariable$region_rebate, 0)
        } else {
            rebates <- 0
        }
        delivery_fees <- dataTables$delivery_fees[input$make4,"Amount"]
        purchase_price <- round((msrp + delivery_fees) * dataVariable$tax - rebates,2)   
        efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
        fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
        fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
        maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
        depreciation_rate <- calculate_depreciation_rate (dataVariable$depreciation_value_10year, input$keep_years)
        depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
        carSelection$car4 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
    }
})

observeEvent(c(input$region, input$yearly_kms, input$make5, input$model5, input$trim5),{
    if(!is.null(input$region) & !is.null(input$trim5) & !identical(input$trim5,"")){
        car_long_name <- paste(input$make5, input$model5, input$trim5)
        engine <- dataTables$car_data[car_long_name,"Engine"]
        msrp <- dataTables$car_data[car_long_name,"MSRP"]
        if(identical(engine, "BEV")){
            rebates <- ifelse(msrp <= dataVariable$federal_max_msrp | is.na(dataVariable$federal_max_msrp), dataVariable$federal_rebate, 0) + ifelse(msrp <= dataVariable$region_max_msrp  | is.na(dataVariable$region_max_msrp),  dataVariable$region_rebate, 0)
        } else {
            rebates <- 0
        }
        delivery_fees <- dataTables$delivery_fees[input$make5,"Amount"]
        purchase_price <- round((msrp + delivery_fees) * dataVariable$tax - rebates,2)
        efficiency <- ifelse(engine=="BEV", dataVariable$bev_efficiency, dataVariable$ice_efficiency)
        fuel_rate <- ifelse(engine=="BEV", dataVariable$electricity_rate, dataVariable$gas_rate)
        fuel_increase <- ifelse(engine=="BEV", dataVariable$electricity_increase, dataVariable$gas_increase) 
        maintenance <- ifelse(engine=="BEV", dataVariable$bev_maintenance, dataVariable$ice_maintenance)
        depreciation_rate <- calculate_depreciation_rate (dataVariable$depreciation_value_10year, input$keep_years)
        depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
        carSelection$car5 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
    }
})



#### DISPLAY VARIABLE TABLE #### 

output$model_variable_info <- renderUI({
  HTML(paste0("<b>The default variables are taken from the Parameter tables based on your ", countrySpecificData$names_for_regions," and the type of vehicle. Double click on a cell to edit.</b>"))
})

observe({
    if(!is.null(input$yearly_kms)){
        #prepares the model variable table
        c0 <-c( paste0("Purchase Price (",countrySpecificData$currency_name,")"), 
            paste0(countrySpecificData$distance," driven (yearly)"), 
            paste0("Efficency (",countrySpecificData$gas_efficiency," or ", countrySpecificData$electricity_efficiency,")"), 
            paste0("Fuel/energy rate (",countrySpecificData$gas_rate," or ", countrySpecificData$electricity_rate,")"), 
            paste0("Yearly fuel/energy price increase (",countrySpecificData$currency_symbol_cent,")"), 
            paste0("Yearly Maintenance (",countrySpecificData$currency_name,")"),
            paste0("Remaining value at ", input$keep_years, " years (in % of MSRP)"))
        c1 <- data.frame(c(carSelection$car1$purchase_price, carSelection$car1$yearly_kms, carSelection$car1$efficiency, carSelection$car1$fuel_rate, carSelection$car1$fuel_increase, carSelection$car1$maintenance, carSelection$car1$depreciation_x_years))
        c2 <- data.frame(c(carSelection$car2$purchase_price, carSelection$car2$yearly_kms, carSelection$car2$efficiency, carSelection$car2$fuel_rate, carSelection$car2$fuel_increase, carSelection$car2$maintenance, carSelection$car2$depreciation_x_years))
        c3 <- data.frame(c(carSelection$car3$purchase_price, carSelection$car3$yearly_kms, carSelection$car3$efficiency, carSelection$car3$fuel_rate, carSelection$car3$fuel_increase, carSelection$car3$maintenance, carSelection$car3$depreciation_x_years))
        c4 <- data.frame(c(carSelection$car4$purchase_price, carSelection$car4$yearly_kms, carSelection$car4$efficiency, carSelection$car4$fuel_rate, carSelection$car4$fuel_increase, carSelection$car4$maintenance, carSelection$car4$depreciation_x_years))
        c5 <- data.frame(c(carSelection$car5$purchase_price, carSelection$car5$yearly_kms, carSelection$car5$efficiency, carSelection$car5$fuel_rate, carSelection$car5$fuel_increase, carSelection$car5$maintenance, carSelection$car5$depreciation_x_years))
        car_selected_table <- data.frame(cbind(c0,c1,c2,c3,c4,c5))
        colnames(car_selected_table) <- c("",carSelection$car1$name, carSelection$car2$name, carSelection$car3$name, carSelection$car4$name, carSelection$car5$name)
        rownames(car_selected_table) <- c("purchase_price", "distance", "efficiency", "fuel_rate", "fuel_price_increase", "maintenance", "depreciation_x_years")
        modelVariableTable$df <- car_selected_table   
    }
})


output$CarSelectedVariableTable <- renderDataTable({
    car_selected_table <- modelVariableTable$df
    not_editable_cols <- which(sapply(c(input$trim1,input$trim2, input$trim3, input$trim4, input$trim5), function(x){identical(x,"")}, USE.NAMES=F)) #columns without a selection should not be editable
    car_selected_table$Tips <- c("Purchase price after accounting for MSRP, delivery fees, taxes and rebates", "The distance you drive in a year", "The car efficiency i.e. how much electricity or gas it uses", "Cost of electricty or gas in your area", "An estimation of the yearly electricity or gas increase", "Estimated yearly maintenance cost", "% of MSRP remaining at the end of the period")
    datatable(car_selected_table, colnames = rep("", ncol(car_selected_table)), rownames=FALSE,
            selection = list(mode = 'single', target = 'column', selectable = c(-1)),
            editable = list(target = "cell", disable = list(columns = c(0, not_editable_cols))),
            options = list( dom = 't', ordering=FALSE, autoWidth = F, pageLength=20,
                columnDefs = list(
                                list(className = 'text-center', width="150px", targets = c(0)),
                                list(className = 'text-center', width="200px", targets = c(1,2,3,4,5)),
                                list(targets = 6, visible = FALSE)),
                rowCallback = JS(
                      "function(row, data) {",
                      "var full_text = data[6];",
                      "$('td:eq(0)', row).attr('title', full_text);",
                                            "}")
            )                
    )    
},server = TRUE)

#handles the update of the model variable table based on user input
observeEvent(input$CarSelectedVariableTable_cell_edit, {
    i <- input$CarSelectedVariableTable_cell_edit$row
    j <- input$CarSelectedVariableTable_cell_edit$col+1
    val <- ifelse(input$CarSelectedVariableTable_cell_edit$value!="", input$CarSelectedVariableTable_cell_edit$value, NA)
    modelVariableTable$df[i,j] <- suppressWarnings(as.numeric(val)) #coerce non-numeric to numeric or NA
})


#### CAR COMPARISON TABLE #### 

observe({
    if(!is.null(input$keep_years)){

        #Cost over X years
        df <- data.frame(matrix(ncol=6, nrow=input$keep_years))
        df[,1] <- seq_len(input$keep_years)
        model_variables_table <- modelVariableTable$df
        for (i in 2:6){
            df[,i] <- compute_ownership_cost(
                purchase_price=model_variables_table["purchase_price",i], 
                kms=input$yearly_kms, 
                kept_years=seq_len(input$keep_years), 
                fuel_per_100km=model_variables_table["efficiency",i], 
                fuel_rate=model_variables_table["fuel_rate",i], 
                fuel_increase=model_variables_table["fuel_price_increase",i], 
                maintenance=model_variables_table["maintenance",i])
        }
        colnames(df) <- c("Year",carSelection$car1$name, carSelection$car2$name, carSelection$car3$name, carSelection$car4$name, carSelection$car5$name)
        comparisonData$cost_over_years <- df

        #final cost after resale
        purchase_price <- as.numeric(model_variables_table["purchase_price",-1])
        final_ownership_cost <- round(as.numeric(tail(df, 1)[-1]),2)
        depreciation_x_years <- as.numeric(model_variables_table["depreciation_x_years",-1]) 
        resale_value <- round(as.numeric(purchase_price * depreciation_x_years/100),2)
        depreciation_cost <-  purchase_price - resale_value #how much of the car value was lost
        operational_cost <- final_ownership_cost - purchase_price #how much did it cost to use the car
        final_cost <- round(final_ownership_cost - resale_value,2)
        df2 <- data.frame(matrix(c(
            "Purchase Price", purchase_price,
            "Ownership Cost", final_ownership_cost, 
            "Remaining Value", resale_value, 
            "Depreciation Cost", depreciation_cost,
            "Operational Cost", operational_cost,
            "Final Cost", final_cost), nrow=6, byrow=T))
        rownames(df2) <- c("Purchase Price", "Ownership Cost", "Remaining Value","Depreciation Cost","Operational Cost","Final Cost")
        colnames(df2) <- c("Breakdown", carSelection$car1$name, carSelection$car2$name, carSelection$car3$name, carSelection$car4$name, carSelection$car5$name)
        comparisonData$final_cost <- df2
    }
})





output$car_comparison_info <- renderUI({
  HTML(paste0('<b>','Cost of ownership over the years based on model variables.','</b>'))
})

#inital cost of the car
output$purchasePriceTable <- renderDataTable({
    purchase_price_table <- modelVariableTable$df["purchase_price",,drop=FALSE]

    purchase_price_table$Tips <- "Purchase price after accounting for MSRP, delivery fees, taxes and rebates"
    colnames(purchase_price_table) <- LETTERS[1:ncol(purchase_price_table)] #placeholder for rowCallback which doesn't accept empty colnames

    df <- datatable(purchase_price_table, rownames=FALSE, colnames = rep("", ncol(purchase_price_table)),
        selection = list(mode = 'single'),
        options = list(ordering=FALSE, autoWidth = F, pageLength = 20, dom = 't',
                columnDefs = list(
                                list(className = 'text-center', width="150px", targets = c(0)),
                                list(className = 'text-center', width="200px", targets = c(1,2,3,4,5)),
                                list(targets = 6, visible = FALSE)),
                rowCallback = JS(
                      "function(row, data) {",
                      "var full_text = data[6];",
                      "$('td:eq(0)', row).attr('title', full_text);",
                                            "}")
            ) 
        ) 

    #create breaks for coloring cells
    if(!all(is.na(purchase_price_table[,2:6]))){
        xr <- as.numeric(purchase_price_table[,2:6])
        brks <- seq(from=min(xr, na.rm=T), to=max(xr, na.rm=T), length.out=19) 
        clrs <- colorRampPalette(c("#99fa8d","#fa8d8d"))(20) #green to red
        #color cells based on values
        df %>% formatStyle(names(purchase_price_table)[2:6],  backgroundColor = styleInterval(brks, clrs),  default="white") 
    } else {
        df
    }
})

#Compare cost of car ownership over X year
output$CarComparisonTable <- renderDataTable({
    car_comparison_table <- comparisonData$cost_over_years  
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

#Cost of ownership at the end of X years
output$CostOfOwnershipTable <- renderDataTable({
    cost_ownership_table <- tail(comparisonData$cost_over_years,1)
    cost_ownership_table[1,1] <- "Cost of ownership" #change the label
    cost_ownership_table$Tips <- c(paste0("Total cost of ownership after ", input$keep_years,". Accounts for purchase price, electricity/gas cost and maintenance."))
    colnames(cost_ownership_table) <- LETTERS[1:ncol(cost_ownership_table)] #placeholder for rowCallback which doesn't accept empty colnames

    df <- datatable(cost_ownership_table, rownames=FALSE, colnames = rep("", ncol(cost_ownership_table)),
        selection = list(mode = 'single'),
        options = list(ordering=FALSE, autoWidth = F, pageLength = 20, dom = 't',
                columnDefs = list(
                                list(className = 'text-center', width="150px", targets = c(0)),
                                list(className = 'text-center', width="200px", targets = c(1,2,3,4,5)),
                                list(targets = 6, visible = FALSE)),
                rowCallback = JS(
                      "function(row, data) {",
                      "var full_text = data[6];",
                      "$('td:eq(0)', row).attr('title', full_text);",
                                            "}")
            ) 
        ) 

    #create breaks for coloring cells
    if(!all(is.na(cost_ownership_table[,2:6]))){
        xr <- as.numeric(cost_ownership_table[,2:6])
        brks <- seq(from=min(xr, na.rm=T), to=max(xr, na.rm=T), length.out=19) 
        clrs <- colorRampPalette(c("#99fa8d","#fa8d8d"))(20) #green to red
        #color cells based on values
        df %>% formatStyle(names(cost_ownership_table)[2:6],  backgroundColor = styleInterval(brks, clrs),  default="white") 
    } else {
        df
    }

})




output$car_resell_info <- renderUI({
  HTML(paste0('<b>','Cost of ownership after accounting for resale value','</b>'))
})

#Final cost after resell and breakdown
output$BreakdownCostTable <- renderDataTable({
    breakdown_cost_table <- comparisonData$final_cost[c("Purchase Price", "Operational Cost", "Remaining Value"),]

    breakdown_cost_table$Tips <- c( "Purchase price after accounting for MSRP, delivery fees, taxes and rebates", paste0("Total expenses over ", input$keep_years," years of ownership"), paste0("Resale value. Account for purchase price and depreciation."))
    df <- datatable(breakdown_cost_table, rownames=FALSE, selection = list(mode = 'single'),
        options = list(ordering=FALSE, autoWidth = F, pageLength = 20, dom = 't',
            rowCallback = JS(
                      "function(row, data) {",
                      "var full_text = data[6];",
                      "$('td:eq(0)', row).attr('title', full_text);",
                                            "}"),
            columnDefs = list(list(className = 'dt-head-center', targets = '_all'), #centered colnames
                              list(className = 'text-center', width="150px", targets = c(0)), #defined first column
                              list(className = 'text-center', width="200px", targets = c(1,2,3,4,5)), #defined remaining columns
                              list(targets = 6, visible = FALSE) #hide tips
            ) 
        )                
    )
    df 
})



output$CarFinalCostTable <- renderDataTable({
    final_cost_table <- comparisonData$final_cost["Final Cost",1:6,drop=FALSE] 
    colnames(final_cost_table) <- LETTERS[1:ncol(final_cost_table)] #placeholder

    final_cost_table[1,1] <- "Cost of ownership after resale" #change the label
    final_cost_table$Tips <- c(paste0("Cost of ownership after accounting for purchase price, operational cost, minus the resale value"))
    colnames(final_cost_table) <- LETTERS[1:ncol(final_cost_table)] #placeholder for rowCallback which doesn't accept empty colnames

    df <- datatable(final_cost_table, rownames=FALSE, colnames = rep("", ncol(final_cost_table)),
        selection = list(mode = 'single'),
        options = list(ordering=FALSE, autoWidth = F, pageLength = 20, dom = 't',
                columnDefs = list(
                                list(className = 'text-center', width="150px", targets = c(0)),
                                list(className = 'text-center', width="200px", targets = c(1,2,3,4,5)),
                                list(targets = 6, visible = FALSE)),
                rowCallback = JS(
                      "function(row, data) {",
                      "var full_text = data[6];",
                      "$('td:eq(0)', row).attr('title', full_text);",
                                            "}")
            ) 
        ) 

    #create breaks for coloring cells
    if(!all(is.na(final_cost_table[,2:6]))){
        xr <- as.numeric(final_cost_table[,2:6])
        brks <- seq(from=min(xr, na.rm=T), to=max(xr, na.rm=T), length.out=19) 
        clrs <- colorRampPalette(c("#99fa8d","#fa8d8d"))(20) #green to red
        #color cells based on values
        df %>% formatStyle(names(final_cost_table)[2:6],  backgroundColor = styleInterval(brks, clrs),  default="white") 
    } else {
        df
    }

 })


#### CAR COMPARISON PLOT #### 

car_comparison_plot <- reactive({
    df <- comparisonData$cost_over_years
    colnames(df) <- make.unique(colnames(df))
    df$point_type <- "Cost of ownership" #"circle"

# DF1 <<- df


    CD <<- comparisonData$final_cost
    purchase_price <- CD["Purchase Price",,drop=FALSE]
    purchase_price$point_type <- "Purchase Price" # "triangle"
    colnames(purchase_price) <- colnames(df)
    purchase_price[1,"Year"] <- 0
    df <- rbind(df,purchase_price)

    df_long <- melt(df, id.vars=c("Year", "point_type"), na.rm=TRUE)
    colnames(df_long) <- c("Year", "point_type", "Model", "Cost")
    df_long$Cost <- as.numeric(df_long$Cost)
    df_long$Year <- as.numeric(df_long$Year)

    p <- ggplot(df_long, aes(x=Year, color=Model, y=Cost, shape=point_type)) + geom_line(lwd=1) + geom_point(size=3) #assign arbitrary alpha, we will define the range later
    p <- p + scale_y_continuous(limits = c(0, max(df_long$Cost))) + scale_x_continuous(limits = c(0, input$keep_years), breaks = seq(from=0, to=input$keep_years, by=5), minor_breaks = seq(from=0, to=input$keep_years, by=1))
    p <- p + theme_minimal() + ylab("Ownership Cost") + xlab("Years")
    p <- p + theme(panel.grid.major.x = element_line(size = 2), panel.grid.minor.x = element_line(color="grey"))
    # p <- p + scale_alpha_identity() #define the range of the alpa values here
    p
 })

car_final_cost_plot <- reactive({
    df <- comparisonData$final_cost[c("Depreciation Cost", "Operational Cost"),]
    colnames(df) <- make.unique(colnames(df))
DF2 <<- df
    df_long <- melt(as.matrix(df[,-which(colnames(df)=="Breakdown")]), na.rm=T , varnames="Breakdown")
    colnames(df_long) <- c("Breakdown", "Model", "Cost")
    df_long$Cost <- as.numeric(df_long$Cost)

    p <- ggplot(df_long, aes(x=Model, y=Cost, fill=Breakdown)) + geom_bar(stat = "identity", lwd=3)
    max_y <- max(comparisonData$cost_over_years, na.rm=TRUE)  #set ylim the same as other plot
    p <- p + scale_y_continuous(limits = c(0, max_y))
    p <- p + theme_minimal() + ylab("Cost") + xlab(NULL)
    p <- p + theme(panel.grid.major.y = element_line(size = 2), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    p <- p + scale_fill_manual(values=c("Depreciation Cost"="#ff9d4c", "Operational Cost"="#9452a9")) #+ ggtitle("Cost of ownership after resale")
    p
})


output$CarComparisonPlot <- renderPlotly({
    p1 <- car_comparison_plot() 
    p2 <- car_final_cost_plot()
P1 <<-   p1
P2 <<- p2

    #merge plots
    gp1 <- ggplotly(p1, height=800)
    gp2 <- ggplotly(p2, height=800)
    sp <- subplot(gp1,gp2, widths = c(0.8,0.2), shareY = T, margin=0)
    # sp <- sp %>% layout(hovermode='x', legend = list(orientation = 'h', title="none", y=1, yanchor="bottom", x=0.5, xanchor="center", margin = list(l = 20, r = 100)) )

    #FORMAT THE LEGEND NICELY:
    df_long2 <- p1$data
    #create minimal datasets to force legend display
    model.df <- data.frame(Year=-1, Cost=0, Model=unique(df_long2$Model))
    cost_type.df <- data.frame(Year=-1, Cost=0, cost_type=c("Cost of ownership", "Purchase price"))
    breakdown.df <- data.frame(Year=-1, Cost=0, Breakdown=c("Depreciation Cost", "Operational Cost"))

    markercolors <- gg_color_hue(nrow(model.df))

    #force legend in plotly
    sp <- sp %>%
      layout( #hide gglotly legend
        legend = list(
          title = list(text = ''),
          itemclick = FALSE,
          itemdoubleclick = FALSE,
          groupclick = FALSE
        )
      ) %>%
      add_trace(
        data = model.df, x = ~ Year, y = ~ Cost,
        inherit = FALSE, type = "scatter", mode = "markers",
        marker = list(color = markercolors, size = 14, opacity = 0.6, symbol = "circle"),
        name = ~ Model, legendgroup = "Model",  legendgrouptitle = list(text = "Model")
      ) %>%
      add_trace(
        data = cost_type.df, x = ~ Year, y = ~ Cost,
        inherit = FALSE, type = "scatter",  mode = "markers",
        marker = list(color = "darkgrey", size = 14, opacity = 0.6, symbol = c("circle", "triangle-up")),
        name = ~cost_type, legendgroup = "Type", legendgrouptitle = list(text = "Type")
      )  %>%
      add_trace( 
        data = breakdown.df, x = ~ Year, y = ~ Cost,
        inherit = FALSE, type = "scatter",  mode = "markers",
        marker = list(color = c("#ff9d4c", "#9452a9"), size = 14, opacity = 0.6, symbol = c("square")),
        name = ~Breakdown, legendgroup = "Breakdown", legendgrouptitle = list(text = "Breakdown")
      )  %>% 
      style(showlegend = FALSE, traces = 1:(nrow(breakdown.df)+2*nrow(model.df))) #remove the 2 Breakdown and the 2 x models legend points

        sp <- sp %>% layout(hovermode='x', legend = list(orientation = 'h', title="none", y=1, yanchor="bottom", x=0.5, xanchor="center", margin = list(l = 20, r = 100)) )

    #Add titles
    annotations = list( 
      list( 
        x = 0.35,  
        y = 0.95,  
        text = "Cost of ownership",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.88,  
        y = 0.95,  
        text = "Cost of ownership after resale",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )
    )
    sp %>% layout(annotations = annotations) 

})

##############################################################
######################### PARAMETERS #########################
##############################################################

#### REBATES #### 
output$rebate_table <- renderDT({
  rebate_table <- dataTables$rebates
  rebate_table <- rebate_table[-grep("Source", rebate_table$Region),,drop=FALSE]
  colnames(rebate_table) <- c(countrySpecificData$names_for_regions, paste0("Maximum Amount (",countrySpecificData$currency_name,")"), "If MSRP below...", "Condition")
  rebate_table
}, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))

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
    tax_table$Rate <- paste0(as.numeric(tax_table$Rate) * 100, "%")
    colnames(tax_table) <- c("Rate", countrySpecificData$names_for_regions)
    tax_table[,2:1]
  }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))
  
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
    colnames(gas_table) <- c(paste0(countrySpecificData$gas_rate," (",countrySpecificData$currency_name,")"), countrySpecificData$names_for_regions)
    gas_table[,2:1]
  }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))
  
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
    colnames(electricity_table) <- c(paste0(countrySpecificData$electricity_rate, " (",countrySpecificData$currency_name,")"), countrySpecificData$names_for_regions)
    electricity_table[,2:1]
  }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))
  
  output$electricity_source <- renderUI({
    electricity_table <- dataTables$electricity
    electricity_source <- electricity_table[grep("Source", rownames(electricity_table)),,drop=FALSE]
    HTML(paste0('<b>', '<a href=',electricity_source[,1],'>',rownames(electricity_source),'</a>','</b>'))
  })
  
  #### DELIVERY FEES #### 
  output$delivery_fees_table <- renderDT({
    delivery_fees_table <- dataTables$delivery_fees
    delivery_fees_table$Region <- rownames(delivery_fees_table)
    colnames(delivery_fees_table) <- c(paste0("Amount (",countrySpecificData$currency_name,")"),countrySpecificData$names_for_regions)
    delivery_fees_table[,2:1]
  }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))
  
  #### DEFAULT MODEL VARIABLES #### 
  output$default_model_variable_table <- renderDT({
      default_variables <- c(
            "ICE efficiency"=paste(dataVariable$ice_efficiency, countrySpecificData$gas_efficiency),
            "ICE maintenance (yearly)"=paste0(countrySpecificData$currency_symbol, dataVariable$ice_maintenance), 
            "BEV efficiency"=paste(dataVariable$bev_efficiency, countrySpecificData$electricity_efficiency),
            "BEV maintenance (yearly)"=paste0(countrySpecificData$currency_symbol, dataVariable$bev_maintenance), 
            "Gas price increase (yearly)"=paste0(dataVariable$gas_increase, countrySpecificData$currency_symbol_cent),
            "Electricity price increase (yearly)"=paste0(dataVariable$electricity_increase, countrySpecificData$currency_symbol_cent),
            "Depreciation rate (yearly)"=paste0(dataVariable$depreciation_rate, "%"), 
            "Depreciation value at 10 years"=paste0(dataVariable$depreciation_value_10year, "%")) 
            #Depreciation at 10 years: 25%
      model_variable_table <- data.frame(Parameters=names(default_variables),
                                         Values=default_variables)
      model_variable_table
  }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))



}
