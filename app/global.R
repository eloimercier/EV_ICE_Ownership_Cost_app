
##############################################################
########################## LIBRARIES #########################
##############################################################

library(shiny)
library(shinydashboard)
library(openxlsx)
library(DT)
library(shinyWidgets)
library(shinyalert)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinyBS)




compute_ownership_cost <- function(purchase_price, kms, kept_years, fuel_per_100km, fuel_rate, fuel_increase, maintenance){
#formula to get cost of ownership over X years
    fuel_cost <- kms * fuel_per_100km/100 * fuel_rate/100 * kept_years 
    fuel_increase_cost <- kms * fuel_per_100km/100 * fuel_increase/100 * (kept_years-1)
    maintenance_cost <- maintenance * kept_years
    ownership_cost <- purchase_price + fuel_cost + fuel_increase_cost + maintenance_cost
    ownership_cost <- round(ownership_cost,2)
    return(ownership_cost)
}