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
  ifelse(length(args) == 0, "../COVIDiSTRESS_May_11_clean.csv", args)

data = read.csv(args, header = T, stringsAsFactors = F)

concern_map <- function() {
  processed_data = data %>%
    group_by(Country) %>%
    summarise(
      mean_concern_score_themself = mean(Corona_concerns_1, na.rm = T),
      sd_concern_score_themself = sd(Corona_concerns_1, na.rm =
                                       T),
      mean_concern_score_friend = mean(Corona_concerns_2, na.rm =
                                         T),
      sd_concern_score_friend = sd(Corona_concerns_2, na.rm =
                                     T),
      mean_concern_score_family = mean(Corona_concerns_3, na.rm =
                                         T),
      sd_concern_score_family = sd(Corona_concerns_3, na.rm =
                                     T),
      mean_concern_score_country = mean(Corona_concerns_4, na.rm =
                                          T),
      sd_concern_score_country = sd(Corona_concerns_4, na.rm =
                                      T),
      mean_concern_score_othercountries = mean(Corona_concerns_5, na.rm =
                                                 T),
      sd_concern_score_othercountries = sd(Corona_concerns_5, na.rm =
                                             T),
      nb_answers = n()
    ) %>%
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
      z = ~ mean_concern_score_themself,
      color = ~ mean_concern_score_themself,
      hovertemplate = ~ paste0(
        "<b>Mean concern score for themself: ",
        round(mean_concern_score_themself, 2),
        "\n",
        "Std dev. concern score for themself: ",
        round(sd_concern_score_themself, 2),
        "</b>\n",
        "Mean concern score for close friends: ",
        round(mean_concern_score_friend, 2),
        "\n",
        "Std dev. concern score for close friends: ",
        round(sd_concern_score_friend, 2),
        "\n",
        "Mean concern score for family: ",
        round(mean_concern_score_family, 2),
        "\n",
        "Std dev. concern score for family: ",
        round(sd_concern_score_family, 2),
        "\n",
        "Mean concern score for his country: ",
        round(mean_concern_score_country, 2),
        "\n",
        "Std dev. concern score for his country: ",
        round(sd_concern_score_country, 2),
        "\n",
        "Mean concern score for other countries: ",
        round(mean_concern_score_othercountries, 2),
        "\n",
        "Std dev. concern score for other countries: ",
        round(sd_concern_score_othercountries, 2),
        "\n",
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
      colorscale = 'RdBu',
      zmin = 1,
      zmax = 6,
      colorbar = list(
        title = 'Concern for themself',
        tickvals = 1:6,
        ticktext = c(
          "1 - Strongly disagree",
          "2 - Disagree",
          "3 - Slightly disagree",
          "4 - Slightly agree",
          "5 - Agree",
          "6 - Strongly agree"
        )
      )
    ) %>%
    add_trace(
      locations =  ~ Country_iso3c,
      z = ~ mean_concern_score_friend,
      color = ~ mean_concern_score_friend,
      hovertemplate = ~ paste0(
        "Mean concern score for themself: ",
        round(mean_concern_score_themself, 2),
        "\n",
        "Std dev. concern score for themself: ",
        round(sd_concern_score_themself, 2),
        "\n",
        "<b>Mean concern score for close friends: ",
        round(mean_concern_score_friend, 2),
        "\n",
        "Std dev. concern score for close friends: ",
        round(sd_concern_score_friend, 2),
        "</b>\n",
        "Mean concern score for family: ",
        round(mean_concern_score_family, 2),
        "\n",
        "Std dev. concern score for family: ",
        round(sd_concern_score_family, 2),
        "\n",
        "Mean concern score for his country: ",
        round(mean_concern_score_country, 2),
        "\n",
        "Std dev. concern score for his country: ",
        round(sd_concern_score_country, 2),
        "\n",
        "Mean concern score for other countries: ",
        round(mean_concern_score_othercountries, 2),
        "\n",
        "Std dev. concern score for other countries: ",
        round(sd_concern_score_othercountries, 2),
        "\n",
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
      colorscale = 'RdBu',
      zmin = 1,
      zmax = 6,
      visible = FALSE,
      colorbar = list(
        title = 'Concern for their friends',
        tickvals = 1:6,
        ticktext = c(
          "1 - Strongly disagree",
          "2 - Disagree",
          "3 - Slightly disagree",
          "4 - Slightly agree",
          "5 - Agree",
          "6 - Strongly agree"
        )
      )
    ) %>%
    add_trace(
      locations =  ~ Country_iso3c,
      z = ~ mean_concern_score_family,
      color = ~ mean_concern_score_family,
      hovertemplate = ~ paste0(
        "Mean concern score for themself: ",
        round(mean_concern_score_themself, 2),
        "\n",
        "Std dev. concern score for themself: ",
        round(sd_concern_score_themself, 2),
        "\n",
        "Mean concern score for close friends: ",
        round(mean_concern_score_friend, 2),
        "\n",
        "Std dev. concern score for close friends: ",
        round(sd_concern_score_friend, 2),
        "\n",
        "<b>Mean concern score for family: ",
        round(mean_concern_score_family, 2),
        "\n",
        "Std dev. concern score for family: ",
        round(sd_concern_score_family, 2),
        "</b>\n",
        "Mean concern score for his country: ",
        round(mean_concern_score_country, 2),
        "\n",
        "Std dev. concern score for his country: ",
        round(sd_concern_score_country, 2),
        "\n",
        "Mean concern score for other countries: ",
        round(mean_concern_score_othercountries, 2),
        "\n",
        "Std dev. concern score for other countries: ",
        round(sd_concern_score_othercountries, 2),
        "\n",
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
      colorscale = 'RdBu',
      zmin = 1,
      zmax = 6,
      visible = FALSE,
      colorbar = list(
        title = 'Concern for their family',
        tickvals = 1:6,
        ticktext = c(
          "1 - Strongly disagree",
          "2 - Disagree",
          "3 - Slightly disagree",
          "4 - Slightly agree",
          "5 - Agree",
          "6 - Strongly agree"
        )
      )
    ) %>%
    add_trace(
      locations =  ~ Country_iso3c,
      z = ~ mean_concern_score_country,
      color = ~ mean_concern_score_country,
      hovertemplate = ~ paste0(
        "Mean concern score for themself: ",
        round(mean_concern_score_themself, 2),
        "\n",
        "Std dev. concern score for themself: ",
        round(sd_concern_score_themself, 2),
        "\n",
        "Mean concern score for close friends: ",
        round(mean_concern_score_friend, 2),
        "\n",
        "Std dev. concern score for close friends: ",
        round(sd_concern_score_friend, 2),
        "\n",
        "Mean concern score for family: ",
        round(mean_concern_score_family, 2),
        "\n",
        "Std dev. concern score for family: ",
        round(sd_concern_score_family, 2),
        "\n",
        "<b>Mean concern score for his country: ",
        round(mean_concern_score_country, 2),
        "\n",
        "Std dev. concern score for his country: ",
        round(sd_concern_score_country, 2),
        "</b>\n",
        "Mean concern score for other countries: ",
        round(mean_concern_score_othercountries, 2),
        "\n",
        "Std dev. concern score for other countries: ",
        round(sd_concern_score_othercountries, 2),
        "\n",
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
      colorscale = 'RdBu',
      zmin = 1,
      zmax = 6,
      visible = FALSE,
      colorbar = list(
        title = 'Concern for their country',
        tickvals = 1:6,
        ticktext = c(
          "1 - Strongly disagree",
          "2 - Disagree",
          "3 - Slightly disagree",
          "4 - Slightly agree",
          "5 - Agree",
          "6 - Strongly agree"
        )
      )
    ) %>%
    add_trace(
      locations =  ~ Country_iso3c,
      z = ~ mean_concern_score_othercountries,
      color = ~ mean_concern_score_othercountries,
      hovertemplate = ~ paste0(
        "Mean concern score for themself: ",
        round(mean_concern_score_themself, 2),
        "\n",
        "Std dev. concern score for themself: ",
        round(sd_concern_score_themself, 2),
        "\n",
        "Mean concern score for close friends: ",
        round(mean_concern_score_friend, 2),
        "\n",
        "Std dev. concern score for close friends: ",
        round(sd_concern_score_friend, 2),
        "\n",
        "Mean concern score for family: ",
        round(mean_concern_score_family, 2),
        "\n",
        "Std dev. concern score for family: ",
        round(sd_concern_score_family, 2),
        "\n",
        "Mean concern score for his country: ",
        round(mean_concern_score_country, 2),
        "\n",
        "Std dev. concern score for his country: ",
        round(sd_concern_score_country, 2),
        "\n",
        "<b>Mean concern score for other countries: ",
        round(mean_concern_score_othercountries, 2),
        "\n",
        "Std dev. concern score for other countries: ",
        round(sd_concern_score_othercountries, 2),
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
      colorscale = 'RdBu',
      zmin = 1,
      zmax = 6,
      visible = FALSE,
      colorbar = list(
        title = 'Concern for other countries',
        tickvals = 1:6,
        ticktext = c(
          "1 - Strongly disagree",
          "2 - Disagree",
          "3 - Slightly disagree",
          "4 - Slightly agree",
          "5 - Agree",
          "6 - Strongly agree"
        )
      )
    ) %>%
    layout(
      title = "Concern map",
      geo = list(showland = T,
                 landcolor = toRGB("grey50")),
      updatemenus = list(list(
        buttons = list(
          list(
            method = 'restyle',
            args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE)),
            label = "Themself"
          ),
          list(
            method = 'restyle',
            args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE)),
            label = "Friends"
          ),
          list(
            method = 'restyle',
            args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE)),
            label = "Family"
          ),
          list(
            method = 'restyle',
            args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE)),
            label = "Country"
          ),
          list(
            method = 'restyle',
            args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE)),
            label = "Other countries"
          )
        )
      ))
    )
  
  return(fig)
  
}

fig_concern = concern_map()

fig_concern_africa = fig_concern %>% layout(geo = list(scope = 'africa'))
fig_concern_asia = fig_concern %>% layout(geo = list(scope = 'asia'))
fig_concern_europe = fig_concern %>% layout(geo = list(scope = 'europe'))
fig_concern_north_america = fig_concern %>% layout(geo = list(scope = 'north america'))
fig_concern_south_america = fig_concern %>% layout(geo = list(scope = 'south america'))
fig_concern_oceania = fig_concern %>% layout(geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7)))
