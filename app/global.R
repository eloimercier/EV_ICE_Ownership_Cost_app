
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
	kept_years_vec <- seq_len(kept_years)
	cumulative_ownership_cost_over_years <- yearly_spending <- rep(NA, kept_years)

	for (i in kept_years_vec){
		yearly_spending[i] <- maintenance + kms * fuel_per_100km/100 * (fuel_rate/100 + fuel_increase/100 * (i-1)) 
	}
	cumulative_ownership_cost_over_years <- purchase_price + cumsum(yearly_spending)
    return(cumulative_ownership_cost_over_years)
}

gg_color_hue <- function(n) {
	#emulate ggplot2 color scheme
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}