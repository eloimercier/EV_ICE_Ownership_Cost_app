
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
        sidebarMenu(
            menuItem("Setup", tabName = "setup", icon = icon("circle-info")),
            menuItem("Car List", tabName = "carlist", icon = icon("list")),
            menuItem("Compare", tabName = "compare", icon = icon("chart-line")),
            menuItem("Parameters", tabName = "params", icon = icon("database"))
        )
    ), #end dashboardSidebar

##############################################################
############################ BODY ############################
##############################################################

    dashboardBody(
      
      # tags$head(
      #     tags$style(HTML("
      #             .btn {
      #               height: 5px;
      #               }
      #           "))
      # ),
      
        tabItems(
############################
#SETUP
############################
        tabItem(tabName = "setup",
            uiOutput("setupUI"),
            uiOutput("rebate_info")
        ),

############################
#CAR LIST
############################

        tabItem(tabName = "carlist",
           DTOutput("car_table")
        ),

############################
#COMPARISON
############################

tabItem(tabName = "compare",
        fluidRow(
          column(12,
            box(uiOutput("modelVariableUI"))
          ),
          column(12,
            tabsetPanel(
              tabPanel("Comparison", 
                    fluidRow(

                        column(2,uiOutput("CarSelection0UI"), DTOutput("RownamesCarInfoTable"), DTOutput("RownamesCarCostTable")),
                        column(2,uiOutput("CarMake1UI"), uiOutput("CarModel1UI"), uiOutput("CarTrim1UI"), DTOutput("CarInfoTable1"), DTOutput("CarCostTable1")),
                        column(2,uiOutput("CarMake2UI"), uiOutput("CarModel2UI"), uiOutput("CarTrim2UI"), DTOutput("CarInfoTable2"), DTOutput("CarCostTable2")),
                        column(2,uiOutput("CarMake3UI"), uiOutput("CarModel3UI"), uiOutput("CarTrim3UI"), DTOutput("CarInfoTable3"), DTOutput("CarCostTable3")),
                        column(2,uiOutput("CarMake4UI"), uiOutput("CarModel4UI"), uiOutput("CarTrim4UI"), DTOutput("CarInfoTable4"), DTOutput("CarCostTable4")),
                        column(2,uiOutput("CarMake5UI"), uiOutput("CarModel5UI"), uiOutput("CarTrim5UI"), DTOutput("CarInfoTable5"), DTOutput("CarCostTable5"))
                    ),
                    

                   DTOutput("comparison_table"), 
                   plotOutput("comparison_plot")
               ),

              tabPanel("Model Variables", uiOutput("model_variable_info"), DTOutput("model_variable_table"))
            )
          )
        )
),


############################
#PARAMETERS
############################

        tabItem(tabName = "params",
                tabsetPanel(
                  tabPanel("Rebates",  DTOutput("rebate_table"), uiOutput("rebate_source")),
                  tabPanel("Taxes",  DTOutput("tax_table"), uiOutput("tax_source")),
                  tabPanel("Gas",  DTOutput("gas_table"), uiOutput("gas_source")),
                  tabPanel("Electricity",  DTOutput("electricity_table"), uiOutput("electricity_source")),
                  tabPanel("Delivery Fees",  DTOutput("delivery_fees_table"))
                )
        )


############################
        ) #end tabItems
    ) #end dashboardBody

)