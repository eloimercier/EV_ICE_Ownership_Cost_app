
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
library(conductor)

calculate_depreciation_rate <- function(depreciation_n_years, n_years){
	#e.g. depreciation after 10 years is 25% of MSRP, yearly depreciation is 0.25^(1/10)=0.87
	y <- 1 - (depreciation_n_years/100) ^ (1/n_years)
	y * 100 #as a percentage
}

calculate_depreciation_n_years <- function(depreciation_rate, n_years){
	#e.g. depreciation rate is 0.13; after 10 years the remaining resale % o the MSRP is (1-0.13)^10=0.25
	y <- (1-depreciation_rate/100) ^ n_years
	y * 100 #as a percentage
}

compute_ownership_cost <- function(purchase_price, kms, kept_years, fuel_per_100km, fuel_rate, fuel_increase, maintenance){
#formula to get cost of ownership over X years
    fuel_cost <- kms * fuel_per_100km/100 * fuel_rate/100 * kept_years 
    fuel_increase_cost <- kms * fuel_per_100km/100 * fuel_increase/100 * (kept_years-1)
    maintenance_cost <- maintenance * kept_years
    ownership_cost <- purchase_price + fuel_cost + fuel_increase_cost + maintenance_cost
    ownership_cost <- round(ownership_cost,2)
    return(ownership_cost)
}


gg_color_hue <- function(n) {
	#emulate ggplot2 color scheme
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}