#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
source("get_world_maps.R")

args <- commandArgs(TRUE)
args <- ifelse(length(args)==0,"../COVIDiSTRESS_April_27_clean.csv",args)

data = read.csv(args, header=T, stringsAsFactors=F)

trust_map <- function(world="world"){
    #world = "world" means a atlantic-centred map
    #world = "world2" means a pacific-centred map
    data = data %>%
           mutate(Country=recode(Country,"Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))

    processed_data = data %>%
                    group_by(Country) %>%
                    summarise(mean_trust_score = mean(Trust_countrymeasure,na.rm=T), sd_trust_score = sd(Trust_countrymeasure,na.rm=T),
                    nb_answers = n())

    processed_world_map = get_world_map(world)
    processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
                          mutate(country_text = paste0(
                "Country: ", country, "\n",
                "Region: ", region, "\n",
                "Mean trust score: ", round(mean_trust_score,2), "\n",
                "Std dev. trust score: ", round(sd_trust_score,2), "\n",
                "# of answers: ", nb_answers))

    p <- ggplot(as.data.frame(processed_world_map)) +
        geom_polygon(aes( x = long, y = lat, group = group, fill = mean_trust_score, text = country_text), colour = "black", size = 0.2)+
        scale_fill_distiller(palette="RdYlBu", name = "All things considered, do you believe that\nthe government has taken the appropriate\n measures in response to Coronavirus ?", limits = c(0, 10), breaks = 0:10, labels= c("0 - Too little","1","2","3","4","5 - Appropriate","6","7","8","9","10 - Too mush")) +
        theme_void()
}

world_map_1_trust <- ggplotly(trust_map(), tooltip="text")
world_map_2_trust <- ggplotly(trust_map(world="world2"), tooltip="text")

save(world_map_1_trust,
     world_map_2_trust,
     file="world_maps_trust.Rdata")

#load("world_maps_trust.Rdata")

#europe
#world_map_1_trust %>% layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72)))

#asia
#world_map_1_trust %>% layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90)))

#africa
#world_map_1_trust %>% layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40)))

#america
#world_map_1_trust %>% layout(xaxis=list(range = c(-180,-20)),yaxis=list(range =  c(-60,80)))

#oceania
#world_map_2_trust %>% layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80)))

#world
#world_map_1_trust