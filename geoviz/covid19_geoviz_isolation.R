#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
source("get_world_maps.R")

args <- commandArgs(TRUE)
args <- ifelse(length(args)==0,"../COVIDiSTRESS_April_27_clean.csv",args)

data = read.csv(args, header=T, stringsAsFactors=F)

isolation_map <- function(world="world"){
    #world = "world" means a atlantic-centred map
    #world = "world2" means a pacific-centred map
    data = data %>%
           mutate(Country=recode(Country,"Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))

    processed_data = data %>%
                    mutate(isolation_score=recode(Dem_islolation,"Life carries on as usual"=0,"Life carries on with minor changes"=1,"Isolated"=2,"Isolated in medical facility of similar location"=3,.default=as.numeric(NA))) %>%
                    group_by(Country) %>%
                    summarise(mean_isolation_score = mean(isolation_score,na.rm=T),sd_isolation_score = sd(isolation_score,na.rm=T),
                    nb_answers = n())

    processed_world_map = get_world_map(world)
    processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
                          mutate(country_text = paste0(
                "Country: ", country, "\n",
                "Region: ", region, "\n",
                "Mean isolation score: ", round(mean_isolation_score,2), "\n",
                "Std dev. isolation score: ", round(sd_isolation_score,2), "\n",
                "# of answers: ", nb_answers))

    p <- ggplot(as.data.frame(processed_world_map)) +
        geom_polygon(aes( x = long, y = lat, group = group, fill = mean_isolation_score, text = country_text), colour = "black", size = 0.2)+
        scale_fill_distiller(palette="RdYlBu", name = "isolation score", limits = c(0, 3), breaks = c(0,1,2,3), labels= c("0 - Life carries on as usual","1 - Life carries on with minor changes","2 - Isolated","3 - Isolated in medical facility of similar location"),values=c(0,0.45,0.55,1))+
        theme_void()

}

world_map_1_isolation <- ggplotly(isolation_map())
world_map_2_isolation <- ggplotly(isolation_map(world="world2"))

save(world_map_1_isolation, world_map_2_isolation,file="world_maps_isolation.Rdata")

#load("world_maps_isolation.Rdata")
#europe
#world_map_1_isolation %>% layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72)))

#asia
#world_map_1_isolation %>% layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90)))

#africa
#world_map_1_isolation %>% layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40)))

#america
#world_map_1_isolation %>% layout(xaxis=list(range = c(-180,-20)),yaxis=list(range =  c(-60,80)))

#oceania
#world_map_2_isolation %>% layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80)))

#world
#world_map_1_isolation