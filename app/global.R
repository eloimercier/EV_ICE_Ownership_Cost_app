
##############################################################
########################## LIBRARIES #########################
##############################################################

library(shiny)
library(shinyalert)
library(openxlsx)
library(DT)
library(shinyWidgets)
library(shinyalert)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinyBS)
library(conductor)
library(shinyWidgets)

calculate_depreciation_from_10year_rate <- function(depreciation_10_years, n_years=10){
	#e.g. depreciation after 10 years is 25% of MSRP, yearly depreciation is 0.25^(1/10)=0.87
	y <- 1 - (depreciation_10_years/100) ^ (1/n_years)
	y * 100 #as a percentage
}

calculate_depreciation_n_years <- function(depreciation_rate, n_years){
	#e.g. depreciation rate is 0.13; after 10 years the remaining resale % o the MSRP is (1-0.13)^10=0.25
	y <- (1-depreciation_rate/100) ^ n_years
	round(y * 100,2) #as a percentage
}

compute_ownership_cost <- function(purchase_price, kms, kept_years, fuel_per_100km, fuel_rate, fuel_increase, maintenance){
#formula to get cost of ownership over X years
	kept_years_vec <- seq_len(kept_years)
	cumulative_ownership_cost_over_years <- yearly_spending <- rep(NA, kept_years)

	for (i in kept_years_vec){
		yearly_spending[i] <- maintenance + kms * fuel_per_100km/100 * (fuel_rate/100 + fuel_increase/100 * (i-1)) 
	}
	cumulative_ownership_cost_over_years <- round(purchase_price + cumsum(yearly_spending),2)
    return(cumulative_ownership_cost_over_years)
}

gg_color_hue <- function(n) {
#emulate ggplot2 color scheme
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

firstup <- function(x) {
#Capitalize first letter of a word
	if(!is.null(x) & !is.na(x)){
		substr(x, 1, 1) <- toupper(substr(x, 1, 1))
		x
	}
}


convert_distance <- function(distance, convert_from=c("metrics", "imperial"), convert_to=c("metrics", "imperial")){
#convert kilometers to miles and vice versa
	if(convert_from[1]=="metrics" & convert_to[1]=="imperial"){
		converted_distance <- distance * 0.621371
		attr(converted_distance, "unit") <- "miles"
	} else if(convert_from[1]=="imperial" & convert_to[1]=="metrics"){
		converted_distance <- distance / 0.621371
		attr(converted_distance, "unit") <- "kilometers"
	} else if(convert_from[1]=="metrics" & convert_to[1]=="metrics"){
		converted_distance <- distance
		attr(converted_distance, "unit") <- "kilometers"
	}else if(convert_from[1]=="imperial" & convert_to[1]=="imperial"){
		converted_distance <- distance
		attr(converted_distance, "unit") <- "miles"
	}
	return(converted_distance)
}

convert_gas_consumption <- function(gas_consumption, convert_from=c("metrics", "imperial"), convert_to=c("metrics", "imperial")){
#convert L/100km to mpg and vice versa
	if(convert_from[1]=="metrics" & convert_to[1]=="imperial"){
		converted_consumption <- 235.215 / gas_consumption
		attr(converted_consumption, "unit") <- "mpg"
	} else if(convert_from[1]=="imperial" & convert_to[1]=="metrics"){
		converted_consumption <- 235.215 / gas_consumption
		attr(converted_consumption, "unit") <- "L/100km"		
	} else if(convert_from[1]=="metrics" & convert_to[1]=="metrics"){
		converted_consumption <- gas_consumption
		attr(converted_consumption, "unit") <- "L/100km"
	}else if(convert_from[1]=="imperial" & convert_to[1]=="imperial"){
		converted_consumption <- gas_consumption
		attr(converted_consumption, "unit") <- "mpg"
	}
	return(converted_consumption)
}


