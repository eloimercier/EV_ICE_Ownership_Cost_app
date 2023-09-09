
ui <- dashboardPage(skin = "green",

##############################################################
############################ HEADER ##########################
##############################################################

    dashboardHeader(title = "EV List and Comparator", titleWidth = 300,
                    tags$li(class = "dropdown",tags$a(
                            actionButton("info", "App Info",  style="color: black; background-color: transparent; border-color: transparent; margin:0px 0px 0px 0px; padding:0px 0px 0px 0px; font-size:100%"),  style=" margin:0px 0px 0px 0px; padding:13px 10px 10px 10px; font-size:100%"
                        )),
                    tags$li(class = "dropdown", tags$a(
                        img(src="https://img.icons8.com/?size=512&id=62856&format=png",  width="30", height="30"), href = "https://github.com/eloimercier/EV_app", style="margin:0px 0px 0px 0px; padding:10px 10px 10px 10px;"
                        ))
),

##############################################################
########################### SIDEBAR ##########################
##############################################################

    dashboardSidebar(
      tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"), #to load all fonts
        sidebarMenu(id="sidebarID",
            menuItem("User Info", tabName = "setup", icon = icon("user")),
            menuItem("Car List", tabName = "carlist", icon = icon("list")),
            menuItem("Compare", tabName = "compare", icon = icon("chart-line")),
            menuItem("Parameters", tabName = "params", icon = icon("database"))
        )
    ), #end dashboardSidebar

##############################################################
############################ BODY ############################
##############################################################

    dashboardBody(
      
     useConductor(), #for dynamic walkthrough
      
        tabItems(
############################
#SETUP
############################
        tabItem(tabName = "setup",
            actionButton("walkthroughBtn", label="Interactive Walkthrough", icon.library="font awesome",css.class='sc-button'),
            uiOutput("setupUI"),
            uiOutput("modelVariableUI"),
            div(style = "height:20px"),
            uiOutput("rebate_info")
        ),

############################
#CAR LIST
############################

        tabItem(tabName = "carlist",
           dataTableOutput("car_table")
        ),

############################
#COMPARISON
############################

tabItem(tabName = "compare",
        fluidRow(

        #Car selection UI
            column(12, 
                fluidRow(
                    column(2,uiOutput("CarSelection0UI")), #, DTOutput("RownamesCarCostTable")),
                    column(2,uiOutput("CarMake1UI"), uiOutput("CarModel1UI"), uiOutput("CarTrim1UI")), #, DTOutput("CarCostTable1")),
                    column(2,uiOutput("CarMake2UI"), uiOutput("CarModel2UI"), uiOutput("CarTrim2UI")), #, DTOutput("CarCostTable2")),
                    column(2,uiOutput("CarMake3UI"), uiOutput("CarModel3UI"), uiOutput("CarTrim3UI")), #, DTOutput("CarCostTable3")),
                    column(2,uiOutput("CarMake4UI"), uiOutput("CarModel4UI"), uiOutput("CarTrim4UI")), #, DTOutput("CarCostTable4")),
                    column(2,uiOutput("CarMake5UI"), uiOutput("CarModel5UI"), uiOutput("CarTrim5UI")) #, DTOutput("CarCostTable5"))
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
                tabPanel("Table", 

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
                    plotlyOutput("CarComparisonPlot")
                )      
            )
        )

    )

      

        
),


############################
#PARAMETERS
############################

        tabItem(tabName = "params",
                tabsetPanel(id="param_panels",
                  tabPanel(title="Rebates",  DTOutput("rebate_table", width="800px"), uiOutput("rebate_source")),
                  tabPanel(title="Taxes",  DTOutput("tax_table", width="600px"), uiOutput("tax_source")),
                  tabPanel(title="Gas",  DTOutput("gas_table", width="600px"), uiOutput("gas_source")),
                  tabPanel(title="Electricity",  DTOutput("electricity_table", width="600px"), uiOutput("electricity_source")),
                  tabPanel(title="Delivery Fees",  DTOutput("delivery_fees_table", width="600px")),
                  tabPanel(title="Default Model Variables", value="default_model_variable", DTOutput("default_model_variable_table", width="600px"))
                )
        )


############################
        ) #end tabItems
    ) #end dashboardBody

)