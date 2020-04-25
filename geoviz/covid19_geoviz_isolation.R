#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)

data = read.csv("../covid_06042020_choice_values.csv", header=T, stringsAsFactors=F)
data = data[3:nrow(data),]


world_map <- map_data("world")
world_map$country = world_map$region

world_map[world_map$region == "Grenadines","country"] <- "Saint Vincent and the Grenadines"
world_map[world_map$region == "Saint Vincent","country"] <- "Saint Vincent and the Grenadines"
world_map[world_map$region == "Antigua","country"] <- "Antigua and Barbuda"
world_map[world_map$region == "Barbuda","country"] <- "Antigua and Barbuda"

world_map[world_map$region == "Aruba","country"] <- "Netherlands"
world_map[world_map$region == "Curacao","country"] <- "Netherlands"
world_map[world_map$region == "Bonaire","country"] <- "Netherlands"
world_map[world_map$region == "Sint Eustatius","country"] <- "Netherlands"
world_map[world_map$region == "Saba","country"] <- "Netherlands"
world_map[world_map$region == "Sint Maarten","country"] <- "Netherlands"

world_map[world_map$region == "Anguilla","country"] <- "UK"
world_map[world_map$region == "Bermuda","country"] <- "UK"
world_map[world_map$region == "Falkland Islands","country"] <- "UK"
world_map[world_map$region == "Chagos Archipelago","country"] <- "UK"
world_map[world_map$region == "Pitcairn Islands","country"] <- "UK"
world_map[world_map$region == "South Sandwich Islands","country"] <- "UK"
world_map[world_map$region == "Saint Helena","country"] <- "UK"
world_map[world_map$region == "Ascension Island","country"] <- "UK"
world_map[world_map$region == "Turks and Caicos Islands","country"] <- "UK"

world_map[world_map$region == "French Southern and Antarctic Lands","country"] <- "France"
world_map[world_map$region == "Saint Barthelemy","country"] <- "France"
world_map[world_map$region == "Reunion","country"] <- "France"
world_map[world_map$region == "Mayotte","country"] <- "France"
world_map[world_map$region == "French Guiana","country"] <- "France"
world_map[world_map$region == "Martinique","country"] <- "France"
world_map[world_map$region == "Guadeloupe","country"] <- "France"
world_map[world_map$region == "Saint Martin","country"] <- "France"
world_map[world_map$region == "New Caledonia","country"] <- "France"
world_map[world_map$region == "French Polynesia","country"] <- "France"
world_map[world_map$region == "Saint Pierre and Miquelon","country"] <- "France"
world_map[world_map$region == "Wallis and Futuna","country"] <- "France"

world_map[world_map$region == "Canary Islands","country"] <- "Spain"
world_map[world_map$region == "Montserrat","country"] <- "Spain"

world_map[world_map$region == "Azores","country"] <- "Portugal"

world_map[world_map$region == "Guam","country"] <- "USA"
world_map[world_map$region == "Puerto Rico","country"] <- "USA"

world_map[world_map$region == "Heard Island","country"] <- "Australia"
world_map[world_map$region == "Cocos Islands","country"] <- "Australia"
world_map[world_map$region == "Christmas Island","country"] <- "Australia"
world_map[world_map$region == "Norfolk Island","country"] <- "Australia"

world_map[world_map$region == "Siachen Glacier","country"] <- "India"

world_map[world_map$region == "Trinidad","country"] <- "Trinidad and Tobago"
world_map[world_map$region == "Tobago","country"] <- "Trinidad and Tobago"


isolation_map <- function(country_list = setdiff(world_map$country,c("")), x_continent=c(-200,200), y_continent=c(-80,100)){

data = data %>%
     mutate(Country=recode(Country,"- other"="NA","Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))

processed_world_map = world_map %>%
                      filter(country%in%country_list)

processed_data = data %>%
                 filter(Country%in%country_list) %>%
                 mutate(isolation_score=replace(Dem_islolation,Dem_islolation=="1",NA)) %>%
                 mutate(isolation_score=replace(isolation_score,isolation_score=="",NA)) %>%
                 mutate(isolation_score=replace(isolation_score,isolation_score=="Life carries on as usual",0)) %>%
                 mutate(isolation_score=replace(isolation_score,isolation_score=="Life carries on with minor changes",1)) %>%
                 mutate(isolation_score=replace(isolation_score,isolation_score=="Isolated", 2)) %>%
                 mutate(isolation_score=replace(isolation_score,isolation_score=="Isolated in medical facility of similar location",3)) %>%
                 mutate(isolation_score=as.numeric(isolation_score)) %>%
                 group_by(Country) %>%
                 summarise(mean_isolation_score = mean(isolation_score,na.rm=T),sd_isolation_score = sd(isolation_score,na.rm=T),
                 nb_answers = n())

processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
        mutate(country_text = paste0(
               "Country: ", country, "\n",
               "Region: ", region, "\n",
               "Mean isolation score: ", round(mean_isolation_score,2), "\n",
               "Std dev. isolation score: ", round(sd_isolation_score,2), "\n",
               "# of answers: ", nb_answers))

p <- ggplot(as.data.frame(processed_world_map)) +
     geom_polygon(aes( x = long, y = lat, group = group, fill = mean_isolation_score, text = country_text), colour = "black", size = 0.2)+
     scale_fill_distiller(palette="RdYlBu", name = "isolation score", limits = c(0, 3), breaks = c(0,1,2,3), labels= c("0 - Life carries on as usual","1 - Life carries on with minor changes","2 - Isolated","3 - Isolated in medical facility of similar location"),values=c(0,0.45,0.55,1))+#breaks=c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000), guide=guide_colorbar(barwidth = 0.8, barheight = 18), trans = "log", limits=c(10, NA), direction=-1
     scale_x_continuous(limits=x_continent)+
     scale_y_continuous(limits=y_continent)+
     theme_void()
}

x_europe = c(-25,50)
y_europe = c(35,80)
europe = c("Netherlands","UK","Albania","Finland","Andorra","France","Austria","Belgium","Bulgaria","Bosnia and Herzegovina","Switzerland","Czech Republic","Germany","Denmark","Spain","Estonia","Faroe Islands","Georgia","Guernsey","Greece","Croatia","Hungary","Iceland","Italy","San Marino","Jersey","Lithuania","Monaco","Luxembourg","Moldova","Macedonia","Malta","Norway","Portugal","Romania","Serbia","Slovakia","Sweden","Slovenia","Turkey","Ukraine","Vatican","Ireland","Poland","Cyprus","Russia","Belarus","Latvia","Albania","Montenegro","Kosovo") 

ggplotly(isolation_map(europe,x_europe,y_europe), tooltip="text")
