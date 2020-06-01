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

isolation_map <- function(){
    
    processed_data = data %>%
                    mutate(isolation_score=recode(Dem_islolation,"Life carries on as usual"=0,"Life carries on with minor changes"=1,"Isolated"=2,"Isolated in medical facility of similar location"=3,.default=as.numeric(NA))) %>%
                    group_by(Country) %>%
                    summarise(mean_isolation_score = mean(isolation_score,na.rm=T),sd_isolation_score = sd(isolation_score,na.rm=T),
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
      z = ~ mean_isolation_score,
      color = ~ mean_isolation_score,
      hovertemplate = ~ paste0(
        "<b>Mean isolation score: ",
        round(mean_isolation_score, 2),
        "\n",
        "Std dev. isolation score: ",
        round(sd_isolation_score, 2),
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
      colorscale = list(c(0, 0.5, 1), c('rgb(255, 255, 77)', 'rgb(255, 210, 77)', 'rgb(230, 0, 0)')),
      zmin = 0,
      zmax = 3,
      colorbar = list(
        title = 'Isolation score',
        tickvals = 0:3,
        ticktext = c(
          "0 - Life carries on as usual",
          "1 - Life carries on with minor changes",
          "2 - Isolated",
          "3 - Isolated in medical facility of similar location"
        )
      )
    ) 
    
    return(fig)

}


fig_isolation = isolation_map()

fig_isolation_africa = fig_isolation %>% layout(geo = list(scope = 'africa'))
fig_isolation_asia = fig_isolation %>% layout(geo = list(scope = 'asia'))
fig_isolation_europe = fig_isolation %>% layout(geo = list(scope = 'europe'))
fig_isolation_north_america = fig_isolation %>% layout(geo = list(scope = 'north america'))
fig_isolation_south_america = fig_isolation %>% layout(geo = list(scope = 'south america'))
fig_isolation_oceania = fig_isolation %>% layout(geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7)))
