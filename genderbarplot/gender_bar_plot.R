library(ggplot2)
library(plotly)
library(dplyr)

data <- read.csv("../COVIDiSTRESS_May_04_clean.csv", header = T, stringsAsFactors = F)

gender_plot <- function(country_list){
    
    genders <- c("Female","Male","Other/would rather not say")

    processed_data = data %>%
                    filter(Country%in%country_list,Dem_gender != "") %>%
                    group_by(Country,Dem_gender) %>%
                    summarise(nb_surveyed=n()) %>%
                    ungroup() %>%
                    group_by(Country) %>%
                    mutate(perc_surveyed_by_country = (nb_surveyed / sum(nb_surveyed))) %>%
                    ungroup() %>%
                    mutate(country_gender_text = paste0(
                            "Country: ", Country, "\n",
                            "Gender: ", Dem_gender, "\n",
                            "# of surveyed: ", nb_surveyed, "\n",
                            "% of surveyed: ", round(perc_surveyed_by_country*100, 2), "%\n"))
    processed_data$Country <- factor(processed_data$Country, levels = rev(country_list))
    processed_data$Dem_gender <- factor(processed_data$Dem_gender, levels = rev(genders))

    

    p <- ggplot(data = processed_data) +
        geom_bar(aes(x = Country, y = perc_surveyed_by_country, fill = Dem_gender, text = country_gender_text), stat="identity", size=0.5, color="grey20") +
        scale_fill_manual(name="Gender", values=c("Female" = "#00c7b8ff", "Male" = "#31233bff","Other/would rather not say" = "#fbedcdff")) +
        coord_flip() +
        scale_y_continuous(breaks=seq(0,1,0.1),labels = scales::percent_format(accuracy = 1),expand=c(0,0))+
        scale_x_discrete(expand=c(0,0))+
        labs(x = "Country", y = "% Gender") +
        theme_classic() +
        theme(legend.position="top")

    return(ggplotly(p, tooltip = "text"))
}

#test
gender_plot(c("France","Mexico"))