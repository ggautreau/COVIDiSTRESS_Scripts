#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(countrycode)
library(wpp2019)
data(pop)
data(e0F)
data(e0M)

args <- commandArgs(TRUE)
args <-
  ifelse(length(args) == 0, "../COVIDiSTRESS_May_30_clean.csv", args)

data = read.csv(args, header = T, stringsAsFactors = F)

stress_map <- function(){

    processed_data = data %>%
                    group_by(Country) %>%
                    summarise(mean_stress_score = mean(PSS10_avg,na.rm=T),sd_stress_score = sd(PSS10_avg,na.rm=T),
                    nb_answers = n()) %>%
      mutate(
      Country_iso3c = countrycode(
        sourcevar = Country,
        origin = "country.name.en",
        destination = "iso3c",
        custom_match = c(
          "Sudan, South" = "SSD",
          "other" = NA,
          "Kosovo" = NA
        )
      ),
      Country_code = countrycode(
        sourcevar = Country_iso3c,
        origin = "iso3c",
        destination = "un",
        custom_match = c("SSD" = 728, "TWN" = 158)
      ),
      population_2020 = pop[match(Country_code, pop$country_code), "2020"],
      life_expectancy_M = e0M[match(Country_code, e0M$country_code), "2015-2020"],
      life_expectancy_F = e0F[match(Country_code, e0F$country_code), "2015-2020"],
    )
        
  fig <- plot_geo(processed_data) %>%
    add_trace(
      locations =  ~ Country_iso3c,
      z = ~ mean_stress_score,
      color = ~ mean_stress_score,
      hovertemplate = ~ paste0(
        "<b>Mean stress score: ",
        round(mean_stress_score, 2),
        "\n",
        "Std dev. stress score: ",
        round(sd_stress_score, 2),
        "</b>\n",
        "Population size: ",
        format(round(population_2020), big.mark = " "),
        "\n",
        "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
        "\n",
        "# of answers: ",
        nb_answers,
        '<extra>',
        Country,
        '</extra>'
      ),
      colorscale = 'RdBu'
    ) 
    
    return(fig)
        
}


fig_stress = stress_map()

fig_stress_africa = fig_stress %>% layout(geo = list(scope = 'africa'))
fig_stress_asia = fig_stress %>% layout(geo = list(scope = 'asia'))
fig_stress_europe = fig_stress %>% layout(geo = list(scope = 'europe'))
fig_stress_north_america = fig_stress %>% layout(geo = list(scope = 'north america'))
fig_stress_south_america = fig_stress %>% layout(geo = list(scope = 'south america'))
fig_stress_oceania = fig_stress %>% layout(geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7)))
