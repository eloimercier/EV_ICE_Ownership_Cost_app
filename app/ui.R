
ui <- dashboardPage(skin = "green",

##############################################################
############################ HEADER ##########################
##############################################################

    dashboardHeader(title = "EV List",
                    uiOutput("appInfoUI")),

##############################################################
########################### SIDEBAR ##########################
##############################################################

    dashboardSidebar(
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
              tabPanel("Comparison",  DTOutput("comparison_table"), plotOutput("comparison_plot")),
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