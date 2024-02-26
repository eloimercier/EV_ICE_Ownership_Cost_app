
ui <- fluidPage(

    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"), #to load all fonts
    useConductor(), #for dynamic walkthrough

##############################################################
############################ LEFT PANEL ##########################
##############################################################

    titlePanel("User Info"),


    sidebarLayout(
    
##############################################################
############################ LEFT PANEL ##########################
##############################################################

    sidebarPanel(width=2,
        actionButton("walkthroughBtn", label="Interactive Walkthrough", icon.library="font awesome",css.class='sc-button'),
        uiOutput("user_countryUI"),
        uiOutput("user_regionUI"),
        uiOutput("user_kms_yearsUI"),
        div(style = "height:20px"),
        uiOutput("rebate_info")
    ),

##############################################################
############################ BODY ############################
##############################################################

      
        mainPanel(
            tabsetPanel(id="tabsetPanel",

                ############################
                #CAR LIST
                ############################
                tabPanel("Car Overview",value="car_list_tab", 
                    HTML("<h4>Here is a list of selected BEV and ICE vehicles that are available for comparison.<br>
                        <i>Car prices are given for reference only and might not reflect current pricing. But don't worry, you can update the price in the Comparator options.</i></h4>"),
                    actionButton("add_new_data_btn", "Add new car"),        
                    dataTableOutput("car_table")
                ), 

                ############################
                #COMPARISON
                ############################
                tabPanel("Car Comparator",value="comparison_tab", 
                    fluidRow(
                        #Car selection UI
                        column(12, 
                            fluidRow(
                                column(2,uiOutput("CarSelection0UI")), # rownames
                                column(2,uiOutput("CarMake1UI"), uiOutput("CarModel1UI"), uiOutput("CarTrim1UI")), 
                                column(2,uiOutput("CarMake2UI"), uiOutput("CarModel2UI"), uiOutput("CarTrim2UI")), 
                                column(2,uiOutput("CarMake3UI"), uiOutput("CarModel3UI"), uiOutput("CarTrim3UI")), 
                                column(2,uiOutput("CarMake4UI"), uiOutput("CarModel4UI"), uiOutput("CarTrim4UI")), 
                                column(2,uiOutput("CarMake5UI"), uiOutput("CarModel5UI"), uiOutput("CarTrim5UI")) 
                            )
                        ),

                        #Model variables
                        column(12, 
                            div(style = "height:20px"),
                            
                            HTML("<b>Parameters used to estimate cost of ownership</b>"),
                            bsCollapse(id = "model_variable_table_collapsible", open = NULL,
                                bsCollapsePanel("Model Variables  <click here to edit>", value="model_variable_collapse",
                                    uiOutput("model_variable_info"),
                                    dataTableOutput("CarSelectedVariableTable"), 
                                    style="info")
                            ),
                            div(style = "height:20px")
                        ),

                        #Comparison table and plot
                        column(12,
                            HTML('<b>Cost of ownership based on model variables</b>'),

                            tabsetPanel(id="comparison_panels",
                                tabPanel("Table", value="table",

                                    fluidRow(

                                        #pruchase price
                                        dataTableOutput("purchasePriceTable"),
                                        #cost of ownsership over the years
                                        dataTableOutput("CarComparisonTable"),
                                        #final cost of ownership
                                        dataTableOutput("CostOfOwnershipTable"),

                                        #Final cost after resale
                                        div(style = "height:30px"),
                                        column(12, uiOutput("car_resell_info")),
                                        dataTableOutput("BreakdownCostTable"),
                                        dataTableOutput("CarFinalCostTable")
                                    )
                                ),
                                tabPanel(title="Plot", value="plot",
                                    plotlyOutput("combinedCarPlot")
                                )      
                            )
                        )
                    )

                ),

                ############################
                #PARAMETERS
                ############################
                tabPanel("Default Parameters", value="params_tab", 
                    tabPanel(title="Rebates",  DTOutput("rebate_table", width="800px"), uiOutput("rebate_source"))
                )


            ) #end tabsetPanel
        ) #end mainPanel
    ) #end sidebarLayout
) #end fluidPage