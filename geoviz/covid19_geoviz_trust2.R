#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
source("get_world_maps.R")

args <- commandArgs(TRUE)
args <- ifelse(length(args)==0,"../COVIDiSTRESS_May_30_clean.csv",args)

data = read.csv(args, header=T, stringsAsFactors=F)

trust_map <- function(){

    processed_data = data %>%
                    group_by(Country) %>%
                    summarise(mean_trust_score = mean(Trust_countrymeasure,na.rm=T), sd_trust_score = sd(Trust_countrymeasure,na.rm=T),
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
      z = ~ mean_trust_score,
      color = ~ mean_trust_score,
      hovertemplate = ~ paste0(
        "<b>Mean trust score: ",
        round(mean_trust_score, 2),
        "\n",
        "Std dev. trust score: ",
        round(sd_trust_score, 2),
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
      colorscale = list(c(0, 0.5, 1), c('rgb(51, 153, 255)', 'rgb(128, 255, 128)', 'rgb(255, 51, 51)')),
      zmin = 0,
      zmax = 10,
      colorbar = list(
        title = 'All things considered, do you believe that\nthe government has taken the appropriate\n measures in response to Coronavirus ?',
        tickvals = 0:10,
        ticktext = c(
          "0 - Too little",
          "1",
          "2",
          "3",
          "4",
          "5 - Appropriate",
          "6",
          "7",
          "8",
          "9",
          "10 - Too mush"
        )
      )
    ) 
    
    return(fig)
}

fig_trust = trust_map()

fig_trust_africa = fig_trust %>% layout(geo = list(scope = 'africa'))
fig_trust_asia = fig_trust %>% layout(geo = list(scope = 'asia'))
fig_trust_europe = fig_trust %>% layout(geo = list(scope = 'europe'))
fig_trust_north_america = fig_trust %>% layout(geo = list(scope = 'north america'))
fig_trust_south_america = fig_trust %>% layout(geo = list(scope = 'south america'))
fig_trust_oceania = fig_trust %>% layout(geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7)))
