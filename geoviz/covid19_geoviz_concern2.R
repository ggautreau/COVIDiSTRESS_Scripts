#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
library(countrycode)
library(wpp2019)

args <- commandArgs(TRUE)
args <- ifelse(length(args)==0,"../COVIDiSTRESS_May_04_clean.csv",args)

data = read.csv(args, header=T, stringsAsFactors=F)

concern_map <- function(world="world", who="themself"){
    #world = "world" means a atlantic-centred map
    #world = "world2" means a pacific-centred map
    #who="themself" 
    #who="family" 
    #who="friends" 
    #who="country"
    #who="othercountries"

    processed_data = data %>%
                    group_by(Country) %>%                  
                    summarise(mean_concern_score_themself = mean(Corona_concerns_1, na.rm=T),
                              sd_concern_score_themself = sd(Corona_concerns_1,na.rm=T),
                              mean_concern_score_friend = mean(Corona_concerns_2, na.rm=T),
                              sd_concern_score_friend = sd(Corona_concerns_2, na.rm=T),
                              mean_concern_score_family = mean(Corona_concerns_3, na.rm=T),
                              sd_concern_score_family = sd(Corona_concerns_3, na.rm=T),
                              mean_concern_score_country = mean(Corona_concerns_4, na.rm=T),
                              sd_concern_score_country = sd(Corona_concerns_4, na.rm=T),
                              mean_concern_score_othercountries = mean(Corona_concerns_5, na.rm=T),
                              sd_concern_score_othercountries = sd(Corona_concerns_5, na.rm=T),
                              nb_answers = n()) %>%
                mutate(
                Country_iso3c = countrycode(sourcevar = Country,
                        origin = "country.name.en",
                        destination = "iso3c",
                        custom_match=c("Sudan, South"="SSD","other"=NA,"Germany"="DEU","Kosovo"=NA)),
                Country_code = countrycode(sourcevar = Country_iso3c,
                        origin = "iso3c",
                        destination = "un",
                        custom_match=c("SSD"=728,"TWN"=158)))

                processed_data %>%
                mutate(population_2020 = pop[match(processed_data$Country_code,pop$country_code),"2020"])

                country_text = paste0(
                "Country: ", Country, "\n",
                "Mean concern score for herself/themself: ", round(mean_concern_score_themself,2), "\n",
                "Std dev. concern score for herself/themself: ", round(sd_concern_score_themself,2),"\n",
                "Mean concern score for close friends: ", round(mean_concern_score_friend,2), "\n",
                "Std dev. concern score for close friends: ", round(sd_concern_score_friend,2), "\n",
                "Mean concern score for family: ", round(mean_concern_score_family,2), "\n",
                "Std dev. concern score for family: ", round(sd_concern_score_family,2), "\n",
                "Mean concern score for his country: ", round(mean_concern_score_country,2), "\n",
                "Std dev. concern score for his country: ", round(sd_concern_score_country,2), "\n",
                "Mean concern score for other countries: ", round(mean_concern_score_othercountries,2), "\n",
                "Std dev. concern score for other countries: ", round(sd_concern_score_othercountries,2), "\n",
                "# of answers: ", nb_answers))

colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(6, "RdBu")))(6)

fig <- plot_geo(processed_data) %>%
       add_trace(locations=~Country_iso3c, 
                 z = ~mean_concern_score_themself,
                 color = ~mean_concern_score_themself,
                 hovertemplate = ~paste('<b>Mean concern score for themself: </b> %{z:.2f}<br><b>Std dev. concern score for themself</b>',round(sd_concern_score_themself,2),"<br># of answers: ", nb_answers,'<extra>',Country,'</extra>'),
                 colorscale='RdBu',
                 zmin=1,
                 zmax=6,
                 colorbar = list(title='Concern for themself', tickvals = 1:6, ticktext=c("1 - Strongly disagree","2 - Disagree","3 - Slightly disagree","4 - Slightly agree","5 - Agree","6 - Strongly agree"))) %>%
       add_trace(locations=~Country_iso3c, 
                 z = ~mean_concern_score_friend, 
                 color = ~mean_concern_score_friend, 
                 colorscale='RdBu',
                 zmin=1,
                 zmax=6,
                 visible = FALSE,
                 colorbar = list(title='Concern for friends',tickvals = 1:6, ticktext=c("1 - Strongly disagree","2 - Disagree","3 - Slightly disagree","4 - Slightly agree","5 - Agree","6 - Strongly agree"))) %>%
       add_trace(locations=~Country_iso3c, 
                 z = ~mean_concern_score_family, 
                 color = ~mean_concern_score_family, 
                 colorscale='RdBu',
                 zmin=1,
                 zmax=6,
                 visible = FALSE,
                 colorbar = list(title='Concern for family', tickvals = 1:6, ticktext=c("1 - Strongly disagree","2 - Disagree","3 - Slightly disagree","4 - Slightly agree","5 - Agree","6 - Strongly agree"))) %>%
       add_trace(locations=~Country_iso3c, 
                 z = ~mean_concern_score_country, 
                 color = ~mean_concern_score_country, 
                 colorscale='RdBu',
                 zmin=1,
                 zmax=6,
                 visible = FALSE,
                 colorbar = list(title='Concern for their country', tickvals = 1:6, ticktext=c("1 - Strongly disagree","2 - Disagree","3 - Slightly disagree","4 - Slightly agree","5 - Agree","6 - Strongly agree"))) %>%
       add_trace(locations=~Country_iso3c, 
                 z = ~mean_concern_score_othercountries, 
                 color = ~mean_concern_score_othercountries, 
                 colorscale='RdBu',
                 zmin=1,
                 zmax=6,
                 visible = FALSE,
                 colorbar = list(title='Concern for other countries',tickvals = 1:6, ticktext=c("1 - Strongly disagree","2 - Disagree","3 - Slightly disagree","4 - Slightly agree","5 - Agree","6 - Strongly agree")))%>%
       layout(
        title = "who",
        updatemenus = list(
          list(
            buttons = list(
                   list(method = 'restyle',
                   args = list("visible", list(TRUE,FALSE,FALSE,FALSE,FALSE)),
                   label = "themself"),
                   list(method = 'restyle',
                   args = list("visible", list(FALSE,TRUE,FALSE,FALSE,FALSE)),
                   label = "friends"),
                   list(method = 'restyle',
                   args = list("visible", list(FALSE,FALSE,TRUE,FALSE,FALSE)),
                   label = "family"),
                   list(method = 'restyle',
                   args = list("visible", list(FALSE,FALSE,FALSE,TRUE,FALSE)),
                   label = "country"),
                   list(method = 'restyle',
                   args = list("visible", list(FALSE,FALSE,FALSE,FALSE,TRUE)),
                   label = "other countries")
                )
            )
        )
    )

fig = fig %>% layout(
    geo = list(
  scope = 'africa',
  showland = T,
  landcolor = toRGB("grey50"))
  )

}

world_map_1_concern_themself <- ggplotly(concern_map(who="themself"), tooltip="text")
world_map_1_concern_friend <- ggplotly(concern_map(who="friend"), tooltip="text")
world_map_1_concern_family <- ggplotly(concern_map(who="family"), tooltip="text")
world_map_1_concern_country <- ggplotly(concern_map(who="country"), tooltip="text")
world_map_1_concern_othercountries <- ggplotly(concern_map(who="othercountries"), tooltip="text")

world_map_2_concern_themself <- ggplotly(concern_map(who="themself",world="world2"), tooltip="text")
world_map_2_concern_friend <- ggplotly(concern_map(who="friend",world="world2"), tooltip="text")
world_map_2_concern_family <- ggplotly(concern_map(who="family",world="world2"), tooltip="text")
world_map_2_concern_country <- ggplotly(concern_map(who="country",world="world2"), tooltip="text")
world_map_2_concern_othercountries <- ggplotly(concern_map(who="othercountries",world="world2"), tooltip="text")

save(world_map_1_concern_themself,
     world_map_1_concern_friend,
     world_map_1_concern_family,
     world_map_1_concern_country,
     world_map_1_concern_othercountries,
     world_map_2_concern_themself,
     world_map_2_concern_friend,
     world_map_2_concern_family,
     world_map_2_concern_country,
     world_map_2_concern_othercountries,
     file="world_maps_concern.Rdata")

#load("world_maps_concern.Rdata")

#europe
#world_map_1_concern %>% layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72)))

#asia
#world_map_1_concern %>% layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90)))

#africa
#world_map_1_concern %>% layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40)))

#america
#world_map_1_concern %>% layout(xaxis=list(range = c(-180,-20)),yaxis=list(range =  c(-60,80)))

#oceania
#world_map_2_concern %>% layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80)))

#world
#world_map_1_concern