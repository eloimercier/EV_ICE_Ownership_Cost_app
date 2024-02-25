
server <- function(input, output, session) {

  verbose <- FALSE
  

##############################################################
######################### WELCOME ############################
##############################################################

    observe({
        shinyalert("Welcome to the EV Comparator App", HTML("<br><br>
            This app aims to provide an overview of the ownership cost of Battery Electric Vehicles (BEVs) in comparison to Internal Combustion Engine (ICE). <br><br>
            <b>Start by selecting your country and the expected average yearly distance driven.</b><br><br>
            <i>The information in this document are provided for information only with no guarantee of accuracy</i>"), type = "info", html=TRUE) 
    }) 

##############################################################
########################### SETUP ############################
##############################################################

    # Available countries
    countryInfo <- list(
        USA = list(names_for_regions="State", is_federation=TRUE, tax_included=FALSE,
                currency_name="USD", currency_symbol="$", currency_symbol_cent="¢", distance="kms",
                gas_efficiency="mpg", gas_rate="¢/L", electricity_efficiency="m/kWh", electricity_rate="¢/kW", ice_maintenance=250, bev_maintenance=100),
        Canada = list(names_for_regions="Province/Territory", is_federation=TRUE, tax_included=FALSE,
                currency_name="CAD", currency_symbol="$", currency_symbol_cent="¢", distance="kms",
                gas_efficiency="L/100kms", gas_rate="¢/L", electricity_efficiency="kWh/100kms", electricity_rate="¢/kW", ice_maintenance=350, bev_maintenance=150),
        France = list(names_for_regions="Region", is_federation=FALSE, tax_included=TRUE,
                currency_name="Euro", currency_symbol="€", currency_symbol_cent=" cent", distance="kms",
                gas_efficiency="L/100kms", gas_rate=" cents/L", electricity_efficiency="kWh/100kms", electricity_rate=" cents/kW", ice_maintenance=250, bev_maintenance=100)
    )

    countrySpecificData <- reactiveValues(names_for_regions=NA, is_federation=NA, tax_included=NA,
                                        currency_name=NA, currency_symbol=NA, currency_symbol_cent=NA, distance=NA,
                                        gas_efficiency=NA, gas_rate=NA,   electricity_efficiency=NA, electricity_rate=NA,
                                        region_list=NA )
    generalModelData <- reactiveValues(country=NA,  region=NA, rebate_condition=NA,
                                        country_wide_rebate=NA, country_wide_max_msrp=NA, region_rebate=NA, region_max_msrp=NA, tax=NA, gas_rate=NA, electricity_rate=NA, #region specific
                                        ice_efficiency=8, ice_maintenance=NA, bev_efficiency=15, bev_maintenance=NA, depreciation_rate=13, depreciation_value_10year=25, gas_increase=5, electricity_increase=1) #fixed

    dataTables <- reactiveValues(car_data=data.frame(), rebates=NA, taxes=NA, gas=NA, electricity=NA, fees=NA, delivery_fees=NA)

    car_ini <- list(name="", MSRP=NA, rebates=NA, purchase_price=NA, engine=NA, efficiency=NA, fuel_rate=NA, fuel_increase=NA, maintenance=NA, yearly_kms=NA, depreciation_x_years=NA)
    carSelection <- reactiveValues(car1=car_ini, car2=car_ini, car3=car_ini, car4=car_ini, car5=car_ini)
    modelVariableTable <- reactiveValues(df=NA) #store variable needed for computation
    comparisonData <- reactiveValues(cost_over_years=NA, final_cost=NA)


##############################################################
######################## WALKTHROUGH #########################
##############################################################

    walkthrough <- Conductor$new(
        exitOnEsc = FALSE,
        keyboardNavigation = TRUE)$
        step(title="<b>Welcome!</b>", text="Welcome to the Electric Vehicle Comparison Tool!",id = "first")$
        step(title="<b>Enter your info</b>", text="This is where you select your country, region, the distance driven annually and how long you intend to keep the car for.", el="#country", 
            id = "user_info.1")$
        step(title="<b>Enter your info</b>", text="This will determine the model parameters such has potential rebates, electricity price, resale value, etc.", el="#country", 
            id = "user_info.2")$
        step(title="<b>Comparison tool</b>",text="This is the comparison tool. It allows you to compare up to 5 cars and see how much it will cost you at the end of the period.", 
            id = "comparator.transistion", buttons = list(list(action = "next",text = "Next")))$
        step(title="<b>Select cars for comparison</b>", text="Select the make, model and trim among the available cars.", el="#CarMake1UI", 
            id = "comparator.1")$
        step(title="<b>Default model parameters</b>", text="Default parameters are automatically applied based on the user info.<br><br><i>Note: the default parameters are taken from the tables in the 'Parameters' tab.</i>",el="#CarSelectedVariableTable", 
            id = "comparator.2")$
        step(title="<b>Customize model parameters</b>", text="You can fully customize the model if the default values do not match your profile.<br><br><i>Edit the parameters by double-clicking a cell and enter a new value.</i>",el="#CarSelectedVariableTable", 
            id = "comparator.3")$
        step(title="<b>Cost of ownership</b>", text="The tool calculates the cost of ownership over the period.<br><br>This is how much you can expect to have spent at the end of the period.", el="#CarComparisonTable", 
            id = "comparator.4")$
        step(title="<b>Cost of ownership after resale</b>", text="Assuming you resale the vehicle at the end of the period, this is how much it did cost you.", el="#CarFinalCostTable", 
            id = "comparator.5")$
        step(title="<b>Comparison plot</b>", text="This is a graphical representation of the cost of ownership.", el="#plot",
            id = "compararison_plot.transistion", buttons = list(list(action = "next",text = "Next")))$
        step(title="<b>Comparison plot</b>", text="In this scenario, the Honda CRV ends up being more expensive that the Hyundai IONIQ5 after 7 years despite a lower MSRP.", el="#combinedCarPlot", 
            id = "compararison_plot.2")$
        step(title="<b>Change your info</b>", text="You can always change the parameters here to see how it affects the model.", 
            id = "clear_walkthrough_options")$
        step(title="<b>Questions or Comments?</b>", text='Visit my <a href="https://github.com/eloimercier/EV_app">github repo</a> if you have any questions or comments.', 
            id = "last", buttons = list(list(action = "back",secondary = TRUE, text = "Previous"),list(action = "next",text = "Finish")))

    observeEvent(input$walkthroughBtn,{
        #start walktorugh
        walkthrough$init()$start()
    })

    observe({
        current_step <- walkthrough$getCurrentStep()
        btn <- input$walkthroughBtn #to force update

        #step options for walkthrough
        if(identical(current_step, "first")){
            updateSelectInput(session,"country", selected = "Canada")
            updateSelectInput(session,"region", selected = "Ontario")
        }

        #reset options at the end
        if (identical(current_step, "clear_walkthrough_options")) { #reset options when we reach end of tour or if tour cancel
            updateSelectInput(session,"region", selected = "")
            updateSelectInput(session,"make1", selected = "")
            updateSelectInput(session,"make2", selected = "")
            updateCollapse(session, id="model_variable_table_collapsible", open = NULL, close = "model_variable_collapse", style = NULL)
            updateTabsetPanel(session, inputId="tabsetPanel", selected="user_info")
            updateTabsetPanel(session, inputId="comparison_panels", selected="table")
            updateSelectInput(session,"country", selected = "")
        }

        #actions at specific steps
        if(identical(current_step, "comparator.transistion")){
            updateTabsetPanel(session, inputId="tabsetPanel", selected="comparison_tab")
            updateSelectInput(session,"make1", selected = "Honda")
            updateSelectInput(session,"make2", selected = "Hyundai")
            updateSelectInput(session,"model1", selected = "CRV")
            updateSelectInput(session,"model2", selected = "IONIQ 5")
            updateCollapse(session, id="model_variable_table_collapsible", open = "model_variable_collapse", close = NULL, style = NULL)
        }
        if(identical(current_step, "compararison_plot.transistion")){
            updateTabsetPanel(session, inputId="comparison_panels", selected="plot")
        }

    })

##############################################################
######################### APP INFO ##########################
##############################################################

    observeEvent(input$info, {
        version <- read.table("VERSION")[1,1]
        shinyalert("EV Comparator App", HTML(paste0(version,"<br><br>
            This document focuses on affordable Battery Electric Vehicles (BEVs) available in Canada. It is not meant to encompass all EVs on the market. A few Internal Combustion Engine (ICE) vehicles have been included to the list for comparison.<br><br>
            <i>The information in this document are provided for information only with no guarantee of accuracy</i>")), type = "info", html=TRUE) 
    })

    output$githubLink <- renderUI({
        #link to github page
        tags$li(class = "dropdown",
          tags$a(img(src="https://img.icons8.com/?size=512&id=62856&format=png",  width="48", height="48"), href="https://github.com/eloimercier/EV_app")
        )
    })
        

##############################################################
######################### USER INFO ##########################
##############################################################

######### Setup country specific data
    output$user_countryUI <- renderUI({
        country_list <- names(countryInfo)
        selectInput('country', 'Country:', c(Choose='', country_list), selectize=FALSE)            
    })

    observeEvent(input$country,{
        if(!is.null(input$country) & !identical(input$country,"")){

            if(verbose) print("*********************")
            if(verbose) print("Setting up country parameters")

            generalModelData$region <- generalModelData$country #set region = country by default

            #set up country specific parameters
            country_info <- countryInfo[[input$country]]
            for (i in 1:length(country_info)){
                var <- names(country_info)[i]
                countrySpecificData[[var]] <- country_info[[var]]
            }
            generalModelData$ice_maintenance <- country_info$ice_maintenance
            generalModelData$bev_maintenance <- country_info$bev_maintenance

            #read specific spreadsheet
            country_file <- paste0("data/EV_list_",input$country,".xlsx")
            dataTables$rebates <- read.xlsx(country_file, sheet="Rebates")
            dataTables$taxes <- read.xlsx(country_file, sheet="Taxes", rowNames = TRUE)
            dataTables$gas <- read.xlsx(country_file, sheet="Gas", rowNames = TRUE)
            dataTables$electricity <- read.xlsx(country_file, sheet="Electricity", rowNames = TRUE)
            dataTables$delivery_fees <- read.xlsx(country_file, sheet="Fees", rowNames = TRUE)

            #format car data
            car_data <- read.xlsx(country_file, sheet="Cars", check.names = TRUE)
            rownames(car_data) <- paste(car_data$Make, car_data$Model, car_data$Trim)
            colnames(car_data) <- c("Make","Model", "Trim", "Engine", "MSRP", "Link", "Traction","Range (km)", "AC Charging rate (kW)", "DC Fast Charging rate (kW)","HP")
            dataTables$car_data <- car_data

            #format rebate table
            dataTables$rebates <- read.xlsx(country_file, sheet="Rebates")
            # rownames(dataTables$rebates) <- paste0(dataTables$rebates$Region,"_", dataTables$rebates$Condition) #make unique names

            #set up region list
            all_regions <- dataTables$rebates[,1]
            if(countrySpecificData$is_federation){ #remove federal
                all_regions <- all_regions[all_regions!="Federal"]
            }
            all_regions <- all_regions[-grep("Source", all_regions)] #remove source info
            countrySpecificData$region_list <- unique(all_regions)

            #set country wide rebate
            tax_table <- dataTables$taxes
            rebates_table <- dataTables$rebates

            if(identical(countrySpecificData$is_federation, TRUE)){
                generalModelData$country_wide_rebate <- as.numeric(rebates_table[rebates_table[,"Region"] == "Federal", "Maximum.amount"])
                country_wide_max_msrp <- as.numeric(rebates_table[rebates_table[,"Region"] == "Federal", "If.MSRP.below..."])
                if(is.na(country_wide_max_msrp)) country_wide_max_msrp <- Inf
                generalModelData$country_wide_max_msrp <- country_wide_max_msrp
                generalModelData$region_rebate <- 0 # we will set it up later
                generalModelData$region_max_msrp <- 0 # we will set it up later
                generalModelData$tax <- as.numeric(tax_table["Federal",1]) + 0 # we will add region tax later
                generalModelData$gas_rate  <- 0 # we will set it up later
                generalModelData$electricity_rate <- 0 # we will set it up later                  
            } else {
                generalModelData$country_wide_rebate <- as.numeric(rebates_table[rebates_table[,"Region"] == input$country,"Maximum.amount"])
                country_wide_max_msrp <- as.numeric(rebates_table[rebates_table[,"Region"] == input$country,"If.MSRP.below..."])
                if(is.na(country_wide_max_msrp)) country_wide_max_msrp <- Inf
                generalModelData$country_wide_max_msrp <- country_wide_max_msrp
                generalModelData$region_rebate <- 0
                generalModelData$region_max_msrp <- 0 
                generalModelData$tax <- as.numeric(tax_table[input$country,1])
                generalModelData$gas_rate  <- as.numeric(dataTables$gas[input$country,1])
                generalModelData$electricity_rate <- as.numeric(dataTables$electricity[input$country,1])
                if(verbose) print(paste0("Tax/Gas/Electricity rates: ", generalModelData$tax, "/", generalModelData$gas_rate, "/", generalModelData$electricity_rate))
            }
        } else {
            #reset data
            countrySpecificData$names_for_regions <- countrySpecificData$is_federation <- countrySpecificData$tax_included <- countrySpecificData$currency_name  <- countrySpecificData$currency_symbol <- countrySpecificData$currency_symbol_cent <- NA
            countrySpecificData$distance <- countrySpecificData$gas_efficiency <- countrySpecificData$gas_rate <- countrySpecificData$electricity_efficiency <- countrySpecificData$electricity_rate <- countrySpecificData$region_list <- NA
            generalModelData$region <- ""
            dataTables$car_data=data.frame()
            dataTables$rebates <- dataTables$taxes <- dataTables$gas <- dataTables$electricity <- dataTables$fees <- dataTables$delivery_fees <- NA
        }
    })


######### Setup region specific data

    output$user_regionUI <- renderUI({
        if(all(!is.na(countrySpecificData$region_list))){
            region_list <- countrySpecificData$region_list #when country not yet selected
        } else {
            region_list <- NULL
        }
        if(length(region_list)>1){
            selectInput('region', paste0(countrySpecificData$names_for_regions,":"), c(Choose='', region_list), selectize=FALSE)
        }
    })

    output$user_region_specificUI <- renderUI({
        if(!is.null(input$region) & !identical(input$region,"") & !identical(generalModelData$region,"")){
            conditions <-  dataTables$rebates[dataTables$rebates[,"Region"]==input$region,"Condition"]
            if(length(conditions)>1){ 
                selectInput("rebate_condition", "Income:", as.list(conditions))
            } 
        }
    })


    observeEvent(c(input$region, input$rebate_condition),{

        # 3 situations: 
        # country is not federation -> get rebate matching country
        # country is federation -> get rebate matching region
        # country is federation + multiple rebate conditions -> get rebate matching region + condition
        if(!is.null(input$country) & !identical(input$country,"")){ #country has been selected

            if(!countrySpecificData$is_federation) {
                stop("This should only be executed when country is a federation. What went wrong?")
            }
            if(!is.null(input$region) & !identical(input$region,"")){

                if(verbose) print("Setting up region parameters")
                rebate_table <- dataTables$rebates

                rebate_info_federal <- rebate_table[rebate_table$Region == "Federal",,drop=FALSE]
                country_wide_rebate <- as.numeric(rebate_info_federal[1,"Maximum.amount"])
                country_wide_max_msrp <- as.numeric(rebate_info_federal[1,"If.MSRP.below..."])

                region <- input$region
                region_info_rebate <- rebate_table[rebate_table[,"Region"]==region,,drop=FALSE]
                if(nrow(region_info_rebate)>1){ # get rebate matching region + condition
                    if(verbose) print("** Multiple rebates exist for this region **")
                    region_info_rebate_specific <- region_info_rebate[region_info_rebate[,4]==input$rebate_condition,,drop=FALSE]
                    region_rebate <- as.numeric(region_info_rebate_specific[1,"Maximum.amount"])
                    region_max_msrp <- as.numeric(region_info_rebate_specific[1,"If.MSRP.below..."])
                } else { #get rebate matching region
                    if(verbose) print("** There is only one rebate for that region **")
                    region_rebate <- as.numeric(region_info_rebate[1,"Maximum.amount"])
                    region_max_msrp <- as.numeric(region_info_rebate[1,"If.MSRP.below..."])
                }


                # if rebate not specified (i.e. NA), set rebate to 0
                if(is.na(country_wide_rebate)) country_wide_rebate <- 0
                if(is.na(region_rebate)) region_rebate <- 0

                # if max amount not specified (i.e. NA), set max eligible MSRP to Infinite
                # if(is.na(country_wide_max_msrp)) country_wide_max_msrp <- Inf
                if(is.na(region_max_msrp)) region_max_msrp <- Inf

                generalModelData$country_wide_rebate <- country_wide_rebate
                generalModelData$country_wide_max_msrp <- country_wide_max_msrp

                generalModelData$region_rebate <- region_rebate
                generalModelData$region_max_msrp <- region_max_msrp
                generalModelData$region <- region

                ############## Get utility rates
                tax_table <- dataTables$taxes
                country_wide_tax <- generalModelData$tax 
                region_tax <- as.numeric(tax_table[input$region,1])
                total_tax <- country_wide_tax + region_tax
                gas_rate  <- as.numeric(dataTables$gas[input$region,1])
                electricity_rate <- as.numeric(dataTables$electricity[input$region,1])                
                if(verbose) print(paste0("Tax/Gas/Electricity rates: ", generalModelData$tax, "/", generalModelData$gas_rate, "/", generalModelData$electricity_rate))

                generalModelData$tax <- total_tax
                generalModelData$gas_rate <- gas_rate
                generalModelData$electricity_rate <- electricity_rate

            }

        }

    })

######### Setup distance and time

    output$user_kms_yearsUI <- renderUI({
      tagList(
        numericInput("yearly_kms", "Km driven yearly:", 10000, min = 1, max = 100000, step=1000),
        numericInput("keep_years", "How many years do you intend to keep the car for:", 10, min = 1, max = 20, step=1)
      )
    })

######### Display rebate info

    output$rebate_info <- renderUI({
    	rebates <- dataTables$rebates
        if(is.null(input$country) | identical(input$country,"")){
            HTML('<p style="font-size:20px;"><b>Select your country to see how much you can save on the purchase of a new Battery Electric Vehicles (BEV).</b></p>')
        } else if(( is.null(input$region) | identical(input$region,'') ) & countrySpecificData$is_federation){
            HTML('<p style="font-size:20px;"><b>Select your ',countrySpecificData$names_for_regions,' to see how much you can save on the purchase of a new Battery Electric Vehicles (BEV).</b></p>')
        } else {
    		country_wide_rebate <- generalModelData$country_wide_rebate
    		federal_msrp <- generalModelData$country_wide_max_msrp
    		region_rebate <- generalModelData$region_rebate
    		region_msrp <- generalModelData$region_max_msrp
    		
            if(verbose){
                print(paste0("Country wide rebate: ", country_wide_rebate))
                print(paste0("Regional rebate: ", region_rebate))
            }

    		country_wide_rebate_text <- ifelse(country_wide_rebate>0,
    		                              paste0("Up to ",country_wide_rebate,"$"),
    		                              "No rebate.")
    		region_rebate_text <- ifelse(region_rebate>0,
    		                             paste0("Up to ",region_rebate,"$"),
    		                             "No rebate.")
    		federal_msrp_text <- ifelse(federal_msrp<Inf,
                              		  paste0(" on BEVs below $",federal_msrp,"."),
                              		  "")
    		region_msrp_text <- ifelse(region_msrp<Inf,
    	                              paste0(" on BEVs below $",region_msrp,"."),
    	                              "")

            if(countrySpecificData$is_federation){
                rebate_max_text <- paste0('<p style="font-size:20px;"><b>','You can benefit from up to <span style="background-color: #FFFF00">', paste0(sum(country_wide_rebate, region_rebate, na.rm=TRUE), countrySpecificData$currency_symbol) ,'</span> on the purchase of a new Battery Electric Vehicle (BEV).','</b></p>')
                rebate_federal_text <- paste0('<p style="font-size:14px;"><b>','Federal Rebate: </b>',country_wide_rebate_text, federal_msrp_text,'</p>')
                rebate_region_text <- paste0('<p style="font-size:14px;"><b>',generalModelData$region,' Rebate: </b>',region_rebate_text, region_msrp_text,'</p><br>')
            } else {
                rebate_max_text <- paste0('<p style="font-size:20px;"><b>','You can benefit from up to <span style="background-color: #FFFF00">', paste0(country_wide_rebate, countrySpecificData$currency_symbol) ,'</span> on the purchase of a new Battery Electric Vehicle (BEV).','</b></p>')
                 rebate_federal_text <- rebate_region_text <- NULL
            }

    		HTML(c(rebate_max_text, rebate_federal_text, rebate_region_text))
    	} 
    })

##############################################################
######################### CAR LIST ##########################
##############################################################

    output$car_table <- renderDataTable({
    	car_list <- dataTables$car_data
        validate(need(nrow(car_list)>0, "Select a country first!"))

    	car_list <- car_list[,-which(colnames(car_list)=="Link")]

        ########## Calculate price after delivery fees and tax (if not included in MSRP)
    	car_list$delivery_fees <- sapply(car_list$Make, function(x){dataTables$delivery_fees[x,1]})
    	car_list$delivery_fees[is.na(car_list$delivery_fees)] <- 0

        if(!countrySpecificData$tax_included){
    	   car_list$after_tax_and_fees <- round((car_list[,"MSRP"] + car_list$delivery_fees) * (1+generalModelData$tax) ,2)
        } else {
            car_list$after_tax_and_fees <- round((car_list[,"MSRP"] + car_list$delivery_fees),2)
        }

        ########## Calculate price after rebate
   
        vehicle_country_wide_rebate <- ifelse(car_list[,"MSRP"]<=generalModelData$country_wide_max_msrp & car_list$Engine=="BEV", generalModelData$country_wide_rebate ,0)
        if(countrySpecificData$is_federation){
            vehicle_region_rebate <- ifelse(car_list[,"MSRP"]<=generalModelData$region_max_msrp & car_list$Engine=="BEV", generalModelData$region_rebate ,0)
        } else {
            vehicle_region_rebate <- rep(0, nrow(car_list))
        }

        car_list$eligible_rebate <- vehicle_country_wide_rebate + vehicle_region_rebate
        car_list$after_rebates <- round(car_list$after_tax_and_fees - car_list$eligible_rebate,2)

        # rename columns
        msrp_colname <- paste0("MSRP (", countrySpecificData$currency_name,")")
        purchase_price_colname <- paste0("Purchase Price (", countrySpecificData$currency_name,")")
        # region_rebate_colname <- paste0(generalModelData$region," rebate")
        range_colname <- paste0("Range (",countrySpecificData$distance,")")
    	colnames(car_list) <- c("Make", "Model", "Trim", "Engine", msrp_colname, "Traction", range_colname, "AC Charging rate (kw)", "DC Fast Charging rate (kW)", "HP", "Delivery Fees", "Price after Tax & Fees", "Eligible rebate", purchase_price_colname)
        # reorder table
        car_list <- car_list[,c("Make", "Model", "Trim", "Engine", msrp_colname,  "Delivery Fees", "Price after Tax & Fees", "Eligible rebate", purchase_price_colname, "Traction", range_colname, "AC Charging rate (kw)", "DC Fast Charging rate (kW)", "HP")] 
        car_list <- car_list[order(car_list[,purchase_price_colname],decreasing = FALSE),]
        hide_columns <- which(colnames(car_list) %in% c("Delivery Fees", "Price after Tax & Fees", "Eligible rebate", "Traction", "AC Charging rate (kw)")) - 1 #columns hidden by default, 0-based

        #create factors for better filtering options
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
                th('Eligible Rebate', title = 'Combined amount of all eligible rebates'),
                th(purchase_price_colname, title = 'Final price after rebates'),
                th('Traction', title = 'Car drivetrain'),
                th(range_colname, title = 'Distance on a full charge'),
                th('AC Charging rate (kw)', title = 'AC Charging rate (kw)'),
                th('DC Fast Charging rate (kW)', title = 'DC Fast Charging rate (kW)'),
                th('HP', title = 'Horsepower')
            )
          )
        ))

    	DT::datatable(car_list, filter = list(position = 'top'),selection = 'single', rownames=FALSE, extensions = 'Buttons', container = mouseover_info, 
             options = list(pageLength = 20, lengthMenu = c(10, 20, 50, 100), dom = 'Bfrtlip', buttons = I('colvis'), columnDefs = list(list(targets = hide_columns, visible = FALSE)))) %>% 
                 formatStyle(msrp_colname, background = styleColorBar(car_list[,msrp_colname], rgb(0,0.8,0,0.3)),  backgroundSize = '98% 88%',   backgroundRepeat = 'no-repeat',  backgroundPosition = 'left') %>% 
                 formatStyle(purchase_price_colname, background = styleColorBar(car_list[,purchase_price_colname], rgb(0,0.8,0,0.3)),  backgroundSize = '98% 88%',   backgroundRepeat = 'no-repeat',  backgroundPosition = 'left')	
    })

##############################################################
######################### COMPARISON #########################
##############################################################


    #### CAR SELECTION UI #### 

    output$CarSelection0UI <- renderUI({
        HTML(paste0('<br><b>Make </b>','<br><p style="height: 40px"></p>',
                    '<b>Model </b>','<br><p style="height: 40px"></p>',
                    '<b>Trim </b>','<br><br>'
        ))
    })

    #Car1
    output$CarMake1UI <- renderUI({
        makers <- sort(unique(dataTables$car_data$Make))
        selectInput('make1', '', choices = c(Choose='',makers))
    })
    output$CarModel1UI <- renderUI({
        maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make1,"Model"])
        pickerInput('model1', '', choices = maker_models)
    })
    output$CarTrim1UI <- renderUI({
        maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make1 & dataTables$car_data$Model==input$model1,"Trim"]
        pickerInput('trim1', '', choices = maker_model_trims)
    })

    #Car2
    output$CarMake2UI <- renderUI({
        makers <- sort(unique(dataTables$car_data$Make))
        pickerInput('make2', '', choices = c(Choose='',makers))
    })
    output$CarModel2UI <- renderUI({
        maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make2,"Model"])
        pickerInput('model2', '', choices = maker_models)
    })
    output$CarTrim2UI <- renderUI({
        maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make2 & dataTables$car_data$Model==input$model2,"Trim"]
        pickerInput('trim2', '', choices = maker_model_trims)
    })

    #Car 3
    output$CarMake3UI <- renderUI({
        makers <- sort(unique(dataTables$car_data$Make))
        pickerInput('make3', '', choices = c(Choose='',makers))
    })
    output$CarModel3UI <- renderUI({
        maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make3,"Model"])
        pickerInput('model3', '', choices = maker_models)
    })
    output$CarTrim3UI <- renderUI({
        maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make3 & dataTables$car_data$Model==input$model3,"Trim"]
        pickerInput('trim3', '', choices = maker_model_trims)
    })

    #Car 4
    output$CarMake4UI <- renderUI({
        makers <- sort(unique(dataTables$car_data$Make))
        pickerInput('make4', '', choices = c(Choose='',makers))
    })
    output$CarModel4UI <- renderUI({
        maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make4,"Model"])
        pickerInput('model4', '', choices = maker_models)
    })
    output$CarTrim4UI <- renderUI({
        maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make4 & dataTables$car_data$Model==input$model4,"Trim"]
        pickerInput('trim4', '', choices = maker_model_trims)
    })

    #Car 5
    output$CarMake5UI <- renderUI({
        makers <- sort(unique(dataTables$car_data$Make))
        pickerInput('make5', '', choices = c(Choose='',makers))
    })
    output$CarModel5UI <- renderUI({
        maker_models <- unique(dataTables$car_data[dataTables$car_data$Make==input$make5,"Model"])
        pickerInput('model5', '', choices = maker_models)
    })
    output$CarTrim5UI <- renderUI({
        maker_model_trims <- dataTables$car_data[dataTables$car_data$Make==input$make5 & dataTables$car_data$Model==input$model5,"Trim"]
        pickerInput('trim5', '', choices = maker_model_trims)
    })

#### UPDATE SELECTION #### 

    observeEvent(c(input$country, input$yearly_kms, input$make1, input$model1, input$trim1),{
        if(!is.null(input$country) & !is.null(input$trim1) & !identical(input$trim1,"")){
            car_long_name <- paste(input$make1, input$model1, input$trim1)
            engine <- dataTables$car_data[car_long_name,"Engine"]
            msrp <- dataTables$car_data[car_long_name,"MSRP"]
            if(identical(engine, "BEV")){
                rebates <- ifelse(msrp <= generalModelData$country_wide_max_msrp | is.na(generalModelData$country_wide_max_msrp), generalModelData$country_wide_rebate, 0) + ifelse(msrp <= generalModelData$region_max_msrp  | is.na(generalModelData$region_max_msrp),  generalModelData$region_rebate, 0)
            } else {
                rebates <- 0
            }
            delivery_fees <- dataTables$delivery_fees[input$make1,"Amount"]
            purchase_price <- round((msrp + delivery_fees) * (1+generalModelData$tax) - rebates,2)
            efficiency <- ifelse(engine=="BEV", generalModelData$bev_efficiency, generalModelData$ice_efficiency)
            fuel_rate <- ifelse(engine=="BEV", generalModelData$electricity_rate, generalModelData$gas_rate)
            fuel_increase <- ifelse(engine=="BEV", generalModelData$electricity_increase, generalModelData$gas_increase) 
            maintenance <- ifelse(engine=="BEV", generalModelData$bev_maintenance, generalModelData$ice_maintenance)
            depreciation_rate <- calculate_depreciation_from_10year_rate (generalModelData$depreciation_value_10year)
            depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
            carSelection$car1 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
        } else {
             carSelection$car1 <- car_ini
        }
    })

    observeEvent(c(input$country, input$yearly_kms, input$make2, input$model2, input$trim2),{
        if(!is.null(input$country) & !is.null(input$trim2) & !identical(input$trim2,"")){
            car_long_name <- paste(input$make2, input$model2, input$trim2)
            engine <- dataTables$car_data[car_long_name,"Engine"]
            msrp <- dataTables$car_data[car_long_name,"MSRP"]
            if(identical(engine, "BEV")){
                rebates <- ifelse(msrp <= generalModelData$country_wide_max_msrp | is.na(generalModelData$country_wide_max_msrp), generalModelData$country_wide_rebate, 0) + ifelse(msrp <= generalModelData$region_max_msrp  | is.na(generalModelData$region_max_msrp),  generalModelData$region_rebate, 0)
            } else {
                rebates <- 0
            }
            delivery_fees <- dataTables$delivery_fees[input$make2,"Amount"]
            purchase_price <- round((msrp + delivery_fees) * (1+generalModelData$tax) - rebates,2)
            efficiency <- ifelse(engine=="BEV", generalModelData$bev_efficiency, generalModelData$ice_efficiency)
            fuel_rate <- ifelse(engine=="BEV", generalModelData$electricity_rate, generalModelData$gas_rate)
            fuel_increase <- ifelse(engine=="BEV", generalModelData$electricity_increase, generalModelData$gas_increase) 
            maintenance <- ifelse(engine=="BEV", generalModelData$bev_maintenance, generalModelData$ice_maintenance)
            depreciation_rate <- calculate_depreciation_from_10year_rate (generalModelData$depreciation_value_10year)
            depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
            carSelection$car2 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
        } else {
             carSelection$car2 <- car_ini
        }
    })

    observeEvent(c(input$country, input$yearly_kms, input$make3, input$model3, input$trim3),{
        if(!is.null(input$country) & !is.null(input$trim3) & !identical(input$trim3,"")){
            car_long_name <- paste(input$make3, input$model3, input$trim3)
            engine <- dataTables$car_data[car_long_name,"Engine"]
            msrp <- dataTables$car_data[car_long_name,"MSRP"]
            if(identical(engine, "BEV")){
                rebates <- ifelse(msrp <= generalModelData$country_wide_max_msrp | is.na(generalModelData$country_wide_max_msrp), generalModelData$country_wide_rebate, 0) + ifelse(msrp <= generalModelData$region_max_msrp  | is.na(generalModelData$region_max_msrp),  generalModelData$region_rebate, 0)
            } else {
                rebates <- 0
            }
            delivery_fees <- dataTables$delivery_fees[input$make3,"Amount"]
            purchase_price <- round((msrp + delivery_fees) * (1+generalModelData$tax) - rebates,2)
            efficiency <- ifelse(engine=="BEV", generalModelData$bev_efficiency, generalModelData$ice_efficiency)
            fuel_rate <- ifelse(engine=="BEV", generalModelData$electricity_rate, generalModelData$gas_rate)
            fuel_increase <- ifelse(engine=="BEV", generalModelData$electricity_increase, generalModelData$gas_increase) 
            maintenance <- ifelse(engine=="BEV", generalModelData$bev_maintenance, generalModelData$ice_maintenance)
            depreciation_rate <- calculate_depreciation_from_10year_rate (generalModelData$depreciation_value_10year)
            depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
            carSelection$car3 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
        } else {
             carSelection$car3 <- car_ini
        }
    })

    observeEvent(c(input$country, input$yearly_kms, input$make4, input$model4, input$trim4),{
        if(!is.null(input$country) & !is.null(input$trim4) & !identical(input$trim4,"")){
            car_long_name <- paste(input$make4, input$model4, input$trim4)
            engine <- dataTables$car_data[car_long_name,"Engine"]
            msrp <- dataTables$car_data[car_long_name,"MSRP"]
            if(identical(engine, "BEV")){
                rebates <- ifelse(msrp <= generalModelData$country_wide_max_msrp | is.na(generalModelData$country_wide_max_msrp), generalModelData$country_wide_rebate, 0) + ifelse(msrp <= generalModelData$region_max_msrp  | is.na(generalModelData$region_max_msrp),  generalModelData$region_rebate, 0)
            } else {
                rebates <- 0
            }
            delivery_fees <- dataTables$delivery_fees[input$make4,"Amount"]
            purchase_price <- round((msrp + delivery_fees) * (1+generalModelData$tax) - rebates,2)   
            efficiency <- ifelse(engine=="BEV", generalModelData$bev_efficiency, generalModelData$ice_efficiency)
            fuel_rate <- ifelse(engine=="BEV", generalModelData$electricity_rate, generalModelData$gas_rate)
            fuel_increase <- ifelse(engine=="BEV", generalModelData$electricity_increase, generalModelData$gas_increase) 
            maintenance <- ifelse(engine=="BEV", generalModelData$bev_maintenance, generalModelData$ice_maintenance)
            depreciation_rate <- calculate_depreciation_from_10year_rate (generalModelData$depreciation_value_10year)
            depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
            carSelection$car4 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
        } else {
             carSelection$car4 <- car_ini
        }
    })

    observeEvent(c(input$country, input$yearly_kms, input$make5, input$model5, input$trim5),{
        if(!is.null(input$country) & !is.null(input$trim5) & !identical(input$trim5,"")){
            car_long_name <- paste(input$make5, input$model5, input$trim5)
            engine <- dataTables$car_data[car_long_name,"Engine"]
            msrp <- dataTables$car_data[car_long_name,"MSRP"]
            if(identical(engine, "BEV")){
                rebates <- ifelse(msrp <= generalModelData$country_wide_max_msrp | is.na(generalModelData$country_wide_max_msrp), generalModelData$country_wide_rebate, 0) + ifelse(msrp <= generalModelData$region_max_msrp  | is.na(generalModelData$region_max_msrp),  generalModelData$region_rebate, 0)
            } else {
                rebates <- 0
            }
            delivery_fees <- dataTables$delivery_fees[input$make5,"Amount"]
            purchase_price <- round((msrp + delivery_fees) * (1+generalModelData$tax) - rebates,2)
            efficiency <- ifelse(engine=="BEV", generalModelData$bev_efficiency, generalModelData$ice_efficiency)
            fuel_rate <- ifelse(engine=="BEV", generalModelData$electricity_rate, generalModelData$gas_rate)
            fuel_increase <- ifelse(engine=="BEV", generalModelData$electricity_increase, generalModelData$gas_increase) 
            maintenance <- ifelse(engine=="BEV", generalModelData$bev_maintenance, generalModelData$ice_maintenance)
            depreciation_rate <- calculate_depreciation_from_10year_rate (generalModelData$depreciation_value_10year)
            depreciation_x_years <- calculate_depreciation_n_years(depreciation_rate, input$keep_years)
            carSelection$car5 <- list(name=car_long_name, MSRP=msrp, rebates=rebates, purchase_price=purchase_price, engine=engine, efficiency=efficiency, fuel_rate=fuel_rate, fuel_increase=fuel_increase, maintenance=maintenance, yearly_kms=input$yearly_kms, depreciation_x_years=depreciation_x_years)
        } else {
             carSelection$car5 <- car_ini
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
                paste0(firstup(countrySpecificData$distance)," driven (yearly)"), 
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
        if(!all(is.na(car_selected_table[1,-1]))){ #prevent generation of the table before cars have been selected - e.g. whem running walkthrough
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
        }
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
                    kept_years=input$keep_years, 
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

        validate(need(!is.null(input$country) & !identical(input$country,""), "Select a country first!"))
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
        if(!is.null(input$country) & !identical(input$country,"")){ #country has been selected
            car_comparison_table <- comparisonData$cost_over_years
            colnames(car_comparison_table)[1] <- "End of Year"
            datatable(car_comparison_table, rownames=FALSE, 
                selection = list(mode = 'single'),
                options = list(ordering=FALSE, autoWidth = F, pageLength = 20, dom = 't',
                    columnDefs = list(list(className = 'dt-head-center', targets = '_all'), #centered colnames
                                      list(className = 'text-center', width="150px", targets = c(0)), #defined first column
                                      list(className = 'text-center', width="200px", targets = c(1,2,3,4,5)) #defined remaining columns
                    )
                )                
            ) 
        }
    })

    #Cost of ownership at the end of X years
    output$CostOfOwnershipTable <- renderDataTable({
        if(!is.null(input$country) & !identical(input$country,"")){ #country has been selected

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
        }
    })




    output$car_resell_info <- renderUI({
        if(!is.null(input$country) & !identical(input$country,"")){ #country has been selected
            HTML(paste0('<b>','Cost of ownership after accounting for resale value','</b>'))
        }
    })

    #Final cost after resell and breakdown
    output$BreakdownCostTable <- renderDataTable({
        if(!is.null(input$country) & !identical(input$country,"")){ #country has been selected

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
        }
    })



    output$CarFinalCostTable <- renderDataTable({
        if(!is.null(input$country) & !identical(input$country,"")){ #country has been selected
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
        }
     })


#### CAR COMPARISON PLOT #### 

    purchase_price_plot <- reactive({
        df <- comparisonData$final_cost[c("Purchase Price"),,drop=FALSE]
DF <<- df        
        colnames(df) <- make.unique(colnames(df))
        df_long <- melt(as.matrix(df[,-which(colnames(df)=="Breakdown")]), na.rm=T , varnames="Breakdown")
        colnames(df_long) <- c("Breakdown", "Model", "Cost")
        df_long$Cost <- as.numeric(df_long$Cost)
        point_cols <- gg_color_hue(length(unique(df_long$Model))); names(point_cols) <- unique(df_long$Model)

        if(nrow(df_long)>0){
            p <- ggplot(df_long, aes(x=Model, y=Cost, fill=Breakdown, text=paste0(Breakdown,": ", Cost))) + geom_bar(stat = "identity", lwd=3) + geom_point(aes(x=Model, y=Cost, color=Model, fill=Model), stroke=0, size=5)
            max_y <- max(comparisonData$cost_over_years, na.rm=TRUE)  #set ylim the same as other plot
            p <- p + scale_y_continuous(limits = c(0, max_y))
            p <- p + theme_minimal() + ylab("Cost") + xlab(NULL)
            p <- p + theme(panel.grid.major.y = element_line(size = 2), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            p <- p + scale_fill_manual(values=c("Purchase Price"=rgb(0,0.3,0.9,1), point_cols)) 
            p
        }
    })


    cost_over_years_plot <- reactive({
        df <- comparisonData$cost_over_years
        colnames(df) <- make.unique(colnames(df))
        #add purchase price
        df0 <- comparisonData$final_cost[c("Purchase Price"),,drop=FALSE]
        df0[1,1] <- 0
        colnames(df0) <- colnames(df)
        df <- rbind(df0, df)

        df_long <- melt(df, id.vars=c("Year"), na.rm=TRUE)
        colnames(df_long) <- c("Year", "Model", "Cost")
        df_long$Cost <- as.numeric(df_long$Cost)
        df_long$Year <- as.numeric(df_long$Year)

        if(nrow(df_long)>1){
            p <- ggplot(df_long, aes(x=Year, color=Model, y=Cost, group=Model, text=paste0(Model,"\n",c("Purchase Price", paste0("End of year ", seq_len(input$keep_years))),": ",Cost))) + geom_line(lwd=1) + geom_point(size=3)
            p <- p + scale_y_continuous(limits = c(0, max(df_long$Cost))) + scale_x_continuous(limits = c(0, input$keep_years), breaks = seq(from=0, to=input$keep_years, by=5), minor_breaks = seq(from=0, to=input$keep_years, by=1))
            p <- p + theme_minimal() + ylab("Ownership Cost") + xlab(NULL)
            p <- p + theme(panel.grid.major.x = element_line(size = 2), panel.grid.minor.x = element_line(color="grey"))
            p
        }
     })

    final_cost_plot <- reactive({
        df <- comparisonData$final_cost[c("Depreciation Cost", "Remaining Value", "Operational Cost", "Final Cost"),]
        colnames(df) <- make.unique(colnames(df))
        df_long <- melt(as.matrix(df[,-which(colnames(df)=="Breakdown")]), na.rm=T , varnames="Breakdown")
        colnames(df_long) <- c("Breakdown", "Model", "Cost")
        df_long$Cost <- as.numeric(df_long$Cost)
        df_long$Breakdown <- factor(df_long$Breakdown, levels=c("Remaining Value", "Depreciation Cost", "Operational Cost", "Final Cost"))
        point_cols <- gg_color_hue(length(unique(df_long$Model))); names(point_cols) <- unique(df_long$Model)

        if(nrow(df_long)>1){
            p <- ggplot(subset(df_long, Breakdown!="Final Cost"), aes(x=Model, y=Cost, fill=Breakdown, text=paste0(Breakdown,": ", Cost))) + geom_bar(stat = "identity", lwd=3) + geom_point(data=subset(df_long, Breakdown=="Final Cost"), aes(x=Model, y=Cost, color=Model, fill=Model), stroke=0, size=5)
            max_y <- max(comparisonData$cost_over_years, na.rm=TRUE)  #set ylim the same as other plot
            p <- p + scale_y_continuous(limits = c(0, max_y))
            p <- p + theme_minimal() + ylab("Cost") + xlab(NULL)
            p <- p + theme(panel.grid.major.y = element_line(size = 2), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            p <- p + scale_fill_manual(values=c("Depreciation Cost"=rgb(0,0.2,0.5,1), "Operational Cost"=rgb(0.9,0.3,0.3,1), "Remaining Value"=rgb(0,0.5,1,0.2),point_cols)) 
            p
        }

    })


    output$combinedCarPlot <- renderPlotly({
        p0 <- purchase_price_plot()
        p1 <- cost_over_years_plot() 
        p2 <- final_cost_plot()

        if(all(c(!is.null(p1), !is.null(p2)))){
            #merge plots
            gp0 <- ggplotly(p0, height=800, tooltip = "text")
            gp1 <- ggplotly(p1, height=800, tooltip = "text")
            gp2 <- ggplotly(p2, height=800, tooltip = "text")
GP1 <<- gp1
GP2 <<- gp2
GP0 <<- gp0
            sp <- subplot(gp0, gp1,gp2, widths = c(0.15,0.7,0.15), shareY = T, margin=0)
SP <<- sp

            #FORMAT THE LEGEND NICELY:
            df_long2 <- p1$data
DFLONG2 <<- df_long2            
            #create minimal datasets to force legend display
            model.df <- data.frame(Year=-1, Cost=0, Model=unique(df_long2$Model), color=gg_color_hue(length(unique(df_long2$Model))))
            breakdown.df <- data.frame(Year=-1, Cost=0, Breakdown=c("Purchase Price","Resale Value", "Depreciation Cost", "Operational Cost"), color=c(rgb(0,0.3,0.9,1), rgb(0,0.5,1,0.2), rgb(0,0.2,0.5,1), rgb(0.9,0.1,0.1,1)))
            breakdown.df$Breakdown <- factor(breakdown.df$Breakdown, levels=c("Purchase Price","Resale Value", "Depreciation Cost", "Operational Cost"))
# "Depreciation Cost"=rgb(0,0.2,0.5,1), "Operational Cost"=rgb(0.9,0.3,0.3,1), "Remaining Value"=rgb(0,0.5,1,0.2)
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
                marker = list(color = model.df$color, size = 14, opacity = 0.6, symbol = "circle"),
                name = ~ Model, legendgroup = "Model",  legendgrouptitle = list(text = "Model")
              ) %>%
              add_trace( 
                data = breakdown.df, x = ~ Year, y = ~ Cost,
                inherit = FALSE, type = "scatter",  mode = "markers",
                marker = list(color = breakdown.df$color, size = 14, opacity = 0.6, symbol = c("square")),
                name = ~Breakdown, legendgroup = "Breakdown", legendgrouptitle = list(text = "Breakdown")
              )  %>% 
              style(showlegend = FALSE, traces = 1:(nrow(breakdown.df) + 3*nrow(model.df))) #remove the Breakdown labels + the car names (1 for each point) legends

            sp <- sp %>% layout(hovermode='x', legend = list(orientation = 'h', title="none", y=1, yanchor="bottom", x=0.5, xanchor="center", margin = list(l = 20, r = 100)), xaxis = list(title = '') )

            #Add titles
            annotations = list(
              list( 
                x = 0.08,  
                y = 0.97,  
                text = "Purchase Price",  
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),                
              list( 
                x = 0.50,  
                y = 0.97,  
                text = "Cost of ownership",  
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = 0.92,  
                y = 0.97,  
                text = "Cost of ownership after resale",  
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              )
            )
            sp %>% layout(annotations = annotations) 
        }

    })

##############################################################
######################### PARAMETERS #########################
##############################################################

    #### REBATES #### 
    output$rebate_table <- renderDT({
        rebate_table <- dataTables$rebates
        validate(need(!all(is.na(rebate_table)), "Select a country first!"))
        rebate_table <- rebate_table[-grep("Source", rebate_table$Region),,drop=FALSE]
        colnames(rebate_table) <- c(countrySpecificData$names_for_regions, paste0("Maximum Amount (",countrySpecificData$currency_name,")"), "If MSRP below...", "Income")
        rebate_table
    }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))

    output$rebate_source <- renderUI({
        rebate_table <- dataTables$rebates
        if (!all(is.na(rebate_table))){
            rebate_source <- rebate_table[grep("Source", rebate_table$Region),,drop=FALSE]
            HTML(paste0('<b>', '<a href=',rebate_source[,2],'>',rebate_source[,1],'</a>','</b>'))
        }
    })

    #### TAXES #### 
    output$tax_table <- renderDT({
        tax_table <- dataTables$taxes
        validate(need(!all(is.na(tax_table)), "Select a country first!"))
        tax_table <- tax_table[-grep("Source", rownames(tax_table)),,drop=FALSE]
        tax_table$Region <- rownames(tax_table)

        if(tax_table$Rate==0){
            tax_table$Rate <- "Included"
        } else {
            tax_table$Rate <- paste0(as.numeric(tax_table$Rate) * 100, "%")
        }
        colnames(tax_table) <- c("Rate", countrySpecificData$names_for_regions)
        tax_table[,2:1]
    }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))

    output$tax_source <- renderUI({
        tax_table <- dataTables$taxes
        if (!all(is.na(tax_table))){
            tax_source <- tax_table[grep("Source", rownames(tax_table)),,drop=FALSE]
            HTML(paste0('<b>', '<a href=',tax_source[,1],'>',rownames(tax_source),'</a>','</b>'))
        }
    })

    #### GAS #### 
    output$gas_table <- renderDT({
        gas_table <- dataTables$gas
        validate(need(!all(is.na(gas_table)), "Select a country first!"))
        gas_table <- gas_table[-grep("Source", rownames(gas_table)),,drop=FALSE]
        gas_table$Region <- rownames(gas_table)
        colnames(gas_table) <- c(paste0(countrySpecificData$gas_rate," (",countrySpecificData$currency_name,")"), countrySpecificData$names_for_regions)
        gas_table[,2:1]
    }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))

    output$gas_source <- renderUI({
        gas_table <- dataTables$gas
        if (!all(is.na(gas_table))){
            gas_source <- gas_table[grep("Source", rownames(gas_table)),,drop=FALSE]
            HTML(paste0('<b>', '<a href=',gas_source[,1],'>',rownames(gas_source),'</a>','</b>'))
        }
    })

    #### ELECTRICITY #### 
    output$electricity_table <- renderDT({
        electricity_table <- dataTables$electricity
        validate(need(!all(is.na(electricity_table)), "Select a country first!"))
        electricity_table <- electricity_table[-grep("Source", rownames(electricity_table)),,drop=FALSE]
        electricity_table$Region <- rownames(electricity_table)
        colnames(electricity_table) <- c(paste0(countrySpecificData$electricity_rate, " (",countrySpecificData$currency_name,")"), countrySpecificData$names_for_regions)
        electricity_table[,2:1]
    }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))

    output$electricity_source <- renderUI({
        electricity_table <- dataTables$electricity
        if (!all(is.na(electricity_table))){
            electricity_source <- electricity_table[grep("Source", rownames(electricity_table)),,drop=FALSE]
            HTML(paste0('<b>', '<a href=',electricity_source[,1],'>',rownames(electricity_source),'</a>','</b>'))
        }
    })

    #### DELIVERY FEES #### 
    output$delivery_fees_table <- renderDT({
        delivery_fees_table <- dataTables$delivery_fees
        validate(need(!all(is.na(delivery_fees_table)), "Select a country first!"))
        delivery_fees_table$Region <- rownames(delivery_fees_table)
        colnames(delivery_fees_table) <- c(paste0("Amount (",countrySpecificData$currency_name,")"),countrySpecificData$names_for_regions)
        delivery_fees_table[,2:1]
    }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))

    #### DEFAULT MODEL VARIABLES #### 
    output$default_model_variable_table <- renderDT({
        validate(need(!is.na(countrySpecificData$currency_symbol), "Select a country first!"))

      default_variables <- c(
            "ICE efficiency"=paste(generalModelData$ice_efficiency, countrySpecificData$gas_efficiency),
            "ICE maintenance (yearly)"=paste0(generalModelData$ice_maintenance, countrySpecificData$currency_symbol), 
            "BEV efficiency"=paste(generalModelData$bev_efficiency, countrySpecificData$electricity_efficiency),
            "BEV maintenance (yearly)"=paste0(generalModelData$bev_maintenance, countrySpecificData$currency_symbol), 
            "Gas price increase (yearly)"=paste0(generalModelData$gas_increase, countrySpecificData$currency_symbol_cent),
            "Electricity price increase (yearly)"=paste0(generalModelData$electricity_increase, countrySpecificData$currency_symbol_cent),
            "Depreciation rate (yearly)"=paste0(generalModelData$depreciation_rate, "%"), 
            "Remaining value at 10 years"=paste0(generalModelData$depreciation_value_10year, "%")) 
            #Depreciation at 10 years: 25%
      model_variable_table <- data.frame(Parameters=names(default_variables),
                                         Values=default_variables)
      model_variable_table
    }, selection = 'single', rownames=FALSE, options = list(pageLength = 20, autoWidth = F, dom="t"))



}
