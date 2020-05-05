#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
source("get_world_maps.R")

args <- commandArgs(TRUE)
args <- ifelse(length(args)==0,"../COVIDiSTRESS_April_27_clean.csv",args)

data = read.csv(args, header=T, stringsAsFactors=F)

concern_map <- function(world="world", who="himself"){
    #world = "world" means a atlantic-centred map
    #world = "world2" means a pacific-centred map
    #who="himself" 
    #who="family" 
    #who="friends" 
    #who="country"
    #who="othercountries"
    data = data %>%
           mutate(Country=recode(Country,"Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))

    processed_data = data %>%
                    group_by(Country) %>%                  
                    summarise(mean_concern_score_himself = mean(Corona_concerns_1, na.rm=T),
                              sd_concern_score_himself = sd(Corona_concerns_1,na.rm=T),
                              mean_concern_score_friend = mean(Corona_concerns_2, na.rm=T),
                              sd_concern_score_friend = sd(Corona_concerns_2, na.rm=T),
                              mean_concern_score_family = mean(Corona_concerns_3, na.rm=T),
                              sd_concern_score_family = sd(Corona_concerns_3, na.rm=T),
                              mean_concern_score_country = mean(Corona_concerns_4, na.rm=T),
                              sd_concern_score_country = sd(Corona_concerns_4, na.rm=T),
                              mean_concern_score_othercountries = mean(Corona_concerns_5, na.rm=T),
                              sd_concern_score_othercountries = sd(Corona_concerns_5, na.rm=T),
                              nb_answers = n())

    processed_world_map = get_world_map(world)
    processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
                          mutate(country_text = paste0(
                "Country: ", country, "\n",
                "Region: ", region, "\n",
                ifelse(who=="himself","<b>",""), "Mean concern score for herself/himself: ", round(mean_concern_score_himself,2), "\n",
                "Std dev. concern score for herself/himself: ", round(sd_concern_score_himself,2), ifelse(who=="himself","</b>","") ,"\n",
                ifelse(who=="friend","<b>",""),"Mean concern score for close friends: ", round(mean_concern_score_friend,2), "\n",
                "Std dev. concern score for close friends: ", round(sd_concern_score_friend,2), ifelse(who=="friend","</b>",""), "\n",
                ifelse(who=="family","<b>",""),"Mean concern score for family: ", round(mean_concern_score_family,2), "\n",
                "Std dev. concern score for family: ", round(sd_concern_score_family,2), ifelse(who=="family","</b>",""), "\n",
                ifelse(who=="country","<b>",""),"Mean concern score for his country: ", round(mean_concern_score_country,2), "\n",
                "Std dev. concern score for his country: ", round(sd_concern_score_country,2), ifelse(who=="country","</b>",""), "\n",
                ifelse(who=="orthercountries","<b>",""),"Mean concern score for other countries: ", round(mean_concern_score_othercountries,2), "\n",
                "Std dev. concern score for other countries: ", round(sd_concern_score_othercountries,2), ifelse(who=="othercountries","</b>",""), "\n",
                "# of answers: ", nb_answers))

    p <- ggplot(as.data.frame(processed_world_map))+
         switch(who,
               himself=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_himself, text = country_text), colour = "black", size = 0.2),
               friend=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_friend, text = country_text), colour = "black", size = 0.2),
               family=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_family, text = country_text), colour = "black", size = 0.2),
               country=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_country, text = country_text), colour = "black", size = 0.2),
               othercountries=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_othercountries, text = country_text), colour = "black", size = 0.2))+
        scale_fill_distiller(palette="RdYlBu", name = "Concerned about consequences of the coronavirus ?", limits = c(0, 5), breaks = c(0,1,2,3,4,5), labels= c("0 - Strongly disagree","1 - Disagree","2 - Slightly disagree","3 - Slightly agree","4 - Agree","5 - Strongly agree"))+#,values=c(0,0.45,0.55,1)
        theme_void()
}

world_map_1_concern_himself <- ggplotly(concern_map(who="himself"), tooltip="text")
world_map_1_concern_friend <- ggplotly(concern_map(who="friend"), tooltip="text")
world_map_1_concern_family <- ggplotly(concern_map(who="family"), tooltip="text")
world_map_1_concern_country <- ggplotly(concern_map(who="country"), tooltip="text")
world_map_1_concern_othercountries <- ggplotly(concern_map(who="othercountries"), tooltip="text")

world_map_2_concern_himself <- ggplotly(concern_map(who="himself",world="world2"), tooltip="text")
world_map_2_concern_friend <- ggplotly(concern_map(who="friend",world="world2"), tooltip="text")
world_map_2_concern_family <- ggplotly(concern_map(who="family",world="world2"), tooltip="text")
world_map_2_concern_country <- ggplotly(concern_map(who="country",world="world2"), tooltip="text")
world_map_2_concern_othercountries <- ggplotly(concern_map(who="othercountries",world="world2"), tooltip="text")

save(world_map_1_concern_himself,
     world_map_1_concern_friend,
     world_map_1_concern_family,
     world_map_1_concern_country,
     world_map_1_concern_othercountries,
     world_map_2_concern_himself,
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