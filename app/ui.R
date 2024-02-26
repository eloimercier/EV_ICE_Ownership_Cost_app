
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
                    uiOutput("car_table_textUI"),
                    uiOutput("add_new_carUI"),                        
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
                        uiOutput("CarParamsTableUI"),

                        #Comparison table and plot
                        uiOutput("CarComparisonResultsUI")
                    )

                ),

                ############################
                #PARAMETERS
                ############################

                tabPanel("Default Parameters", value="params_tab", 
uiOutput("defaultParamTablesUI")
                )


            ) #end tabsetPanel
        ) #end mainPanel
    ) #end sidebarLayout
) #end fluidPage