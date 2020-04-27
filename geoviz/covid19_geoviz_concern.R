#!/usr/bin/env Rscript

library(ggplot2)
library(plotly)
library(dplyr)
library(maps)

data = read.csv("../covid_06042020_choice_values.csv", header=T, stringsAsFactors=F)
data = data[3:nrow(data),]

get_world_map <- function(s){
    world_map <- map_data(s)
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
    return(world_map)
}

concern_map <- function(world="world", who="himself"){
    #world = "world" means a atlantic-centred map
    #world = "world2" means a pacific-centred map
    #who="himself" 
    #who="family" 
    #who="friends" 
    #who="country"
    #who="othercountries"
    data = data %>%
           mutate(Country=recode(Country,"- other"="NA","Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))

    processed_data = data %>%
                    filter(Country!="") %>%
                    group_by(Country) %>%
                    mutate(score_Corona_concerns_himself=recode(Corona_concerns_1,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%
                    mutate(score_Corona_concerns_friend=recode(Corona_concerns_2,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%
                    mutate(score_Corona_concerns_family=recode(Corona_concerns_3,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%  
                    mutate(score_Corona_concerns_country=recode(Corona_concerns_4,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%     
                    mutate(score_Corona_concerns_othercountry=recode(Corona_concerns_5,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%                   
                    summarise(mean_concern_score_himself = mean(score_Corona_concerns_himself, na.rm=T),
                              sd_concern_score_himself = sd(score_Corona_concerns_himself,na.rm=T),
                              mean_concern_score_friend = mean(score_Corona_concerns_friend, na.rm=T),
                              sd_concern_score_friend = sd(score_Corona_concerns_friend, na.rm=T),
                              mean_concern_score_family = mean(score_Corona_concerns_family, na.rm=T),
                              sd_concern_score_family = sd(score_Corona_concerns_family, na.rm=T),
                              mean_concern_score_country = mean(score_Corona_concerns_country, na.rm=T),
                              sd_concern_score_country = sd(score_Corona_concerns_country, na.rm=T),
                              mean_concern_score_othercountries = mean(score_Corona_concerns_othercountry, na.rm=T),
                              sd_concern_score_othercountries = sd(score_Corona_concerns_othercountry, na.rm=T),
                              nb_answers = n())

    processed_world_map = get_world_map("world")
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

#europe
ggplotly(concern_map(), tooltip="text") %>% layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72)))

#asia
ggplotly(concern_map(), tooltip="text") %>% layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90)))

#africa
ggplotly(concern_map(), tooltip="text") %>% layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40)))

#america
ggplotly(concern_map(), tooltip="text") %>% layout(xaxis=list(range = c(-80,10)),yaxis=list(range =  c(-80,80)))

#oceania
ggplotly(concern_map(world="world2"), tooltip="text") %>% layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80)))

#world
ggplotly(concern_map(world="world",who="family"), tooltip="text")