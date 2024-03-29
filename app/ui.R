
ui <- fluidPage(

    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"), #to load all fonts
    useConductor(), #for dynamic walkthrough

##############################################################
############################ LEFT PANEL ##########################
##############################################################

    titlePanel("EV and ICE Ownership Cost Calculator"),


    sidebarLayout(
    
##############################################################
############################ LEFT PANEL ##########################
##############################################################

        sidebarPanel(width=2, id="sidebar_panel",
            actionButton("walkthroughBtn", label="Interactive Walkthrough", icon.library="font awesome",css.class='sc-button'),
            HTML("<h3>User info:</h3>"),
            uiOutput("user_infoUI"),
            uiOutput("user_distanceUI"),
            div(style = "height:20px"),
            uiOutput("rebate_info"),
            div(style = "height:20px")
        ),

##############################################################
############################ BODY ############################
##############################################################

      
        mainPanel(
            tabsetPanel(id="tabsetPanel",

                ############################
                #CAR LIST
                ############################
                tabPanel("Car List",value="car_list_tab", 
                    uiOutput("car_table_textUI"),
                    div(style = "height:20px"),
                    uiOutput("add_new_carUI"),                        
                    dataTableOutput("car_table")
                ), 

                ############################
                #COMPARISON
                ############################
                tabPanel("Comparator Tool",value="comparison_tab", 
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
    ), #end sidebarLayout

    div(
        class = "footer",
        includeHTML("footer.html")
    )
) #end fluidPage