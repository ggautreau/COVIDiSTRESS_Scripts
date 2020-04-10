#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
library(ggrepel)
library(scatterpie)

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

data$Country <- replace(data$Country, 
                                    data$Country %in% c("- other","Cabo Verde","Congo, Democratic Republic of the","Congo, Republic of the","Côte d’Ivoire","East Timor (Timor-Leste)","Korea, North","Korea, South","Micronesia, Federated States of","North Macedonia","Sudan, South","The Bahamas","United Kingdom","United States"),
                                    c(NA,"Cape Verde","Democratic Republic of the Congo","Republic of Congo","Ivory Coast","Timor-Leste","North Korea","South Korea","Micronesia","Macedonia","South Sudan","Bahamas","UK","USA"))

nb_rep_by_country <- table(data$Country)
nb_rep_by_country <- nb_rep_by_country[names(nb_rep_by_country) != ""]

world_map$nb_answers <- 0
world_map$nb_answers <- as.vector(nb_rep_by_country[match(world_map$country,names(nb_rep_by_country))])

world_map <- world_map %>%
        mutate(country_text = paste0(
               "Country: ", country, "\n",
               "Region: ", region, "\n",
               "# of answers: ", nb_answers))

world_cities <- data(world.cities)

enough_answers_countries <- unique(world_map[world_map$nb_answers>=50,"country"])
coord_capital <- world.cities[world.cities$capital==1 & world.cities$country.etc %in% enough_answers_countries,]

for(a in unique(data[,"Dem_gender"])){
        if(a!=""){
            coord_capital[, a] <- 0
        }
}

for(country in enough_answers_countries){
answers = table(data[data$Country == country,"Dem_gender"])
        for(a in names(answers)){
                if(a!=""){
                        coord_capital[coord_capital$country.etc == country, a] <- answers[a]
                }
        }
}

coord_capital$nb_answers <- as.vector(nb_rep_by_country[match(coord_capital$country.etc,names(nb_rep_by_country))])

scale=5
p <- ggplot() +
     geom_polygon(data=world_map,aes( x = long, y = lat, group = group, text = country_text),fill="grey90", color="grey60", size = 0.1)+
     geom_point(data=coord_capital,aes( x = long, y = lat),size=0.1)+
     geom_scatterpie(data=coord_capital,
                     aes(x=long, y=lat, r=sqrt(nb_answers)/(pi*scale)),  cols=setdiff(unique(data[,"Dem_gender"]),""),
                     color=NA,
                     alpha=0.5,
                     legend_name="Gender")+
     geom_text_repel(data=coord_capital,aes(x=long, y=lat, label=country.etc),size=2)+
     geom_scatterpie_legend(sqrt(coord_capital$nb_answers)/(pi*scale), -150,-50, n = 5,labeller = function(x){round((x*pi*scale)^2)})+
     #geom_scatterpie_legend(sqrt(coord_capital$nb_answers)/(pi*scale), -10,72, n = 5,labeller = function(x){round((x*pi*scale)^2)})+
     #scale_x_continuous(limits=c(-20,50))+
     #scale_y_continuous(limits=c(25,75))+
     theme_void()
     #, labeller = function(x){x*x*31.4}

scale=5
p <- ggplot() +
     geom_polygon(data=world_map,aes( x = long, y = lat, group = group, text = country_text),fill="grey90", color="grey60", size = 0.1)+
     geom_point(data=coord_capital,aes( x = long, y = lat),size=0.1)+
     geom_scatterpie(data=coord_capital,
                     aes(x=long, y=lat, r=sqrt(nb_answers)/(pi*scale)),  cols=setdiff(unique(data[,"Dem_gender"]),""),
                     color=NA,
                     alpha=0.5,
                     legend_name="Gender")+
     geom_text_repel(data=coord_capital,aes(x=long, y=lat, label=country.etc),size=2)+
     geom_scatterpie_legend(sqrt(coord_capital$nb_answers)/(pi*scale), -150,-50, n = 5,labeller = function(x){round((x*pi*scale)^2)})+
     #geom_scatterpie_legend(sqrt(coord_capital$nb_answers)/(pi*scale), -10,72, n = 5,labeller = function(x){round((x*pi*scale)^2)})+
     #scale_x_continuous(limits=c(-20,50))+
     #scale_y_continuous(limits=c(25,75))+
     theme_void()


ggplotly(p, tooltip="text")
