
ui <- dashboardPage(

##############################################################
############################ HEADER ##########################
##############################################################

  dashboardHeader(title = "EV List and Comparator"),

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

############################
#CAR LIST
############################
  tabItem(tabName = "carlist",
    DTOutput("car_table")
  )


  ) #end dashboardBody

)