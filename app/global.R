
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


format_currency <- function(values, unit_data, unit_target=c("unit","cent","thousand")){
#takes a vector of numerical values and add symbols based on currency data
	# values <- format(values, big.mark=unit_data$format["big.mark"], small.mark=unit_data$format["small.mark"], nsmall=unit_data$format["nsmall"])
	if(!all(is.na(values))){
		if(unit_target[1]=="thousand"){
			values <- paste0(round(values/1000),"k")
		}
	    currency_symbol_before <- ifelse(unit_data$position[[unit_target[1]]] == "left", unit_data$symbols[[unit_target[1]]], "")
	    currency_symbol_after <- ifelse(unit_data$position[[unit_target[1]]] == "right", unit_data$symbols[[unit_target[1]]], "")
	    sapply(values, function(x){paste0(currency_symbol_before,x,currency_symbol_after)})
    } else { #keep NAs
    	values
    }
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


