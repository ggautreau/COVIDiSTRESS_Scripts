library(ggplot2)
library(plotly)
library(dplyr)

data <- read.csv("../COVIDiSTRESS_May_04_clean.csv", header = T, stringsAsFactors = F)

education_plot <- function(country_list){
    
    education <- c("PhD/Doctorate", "College degree, bachelor, master", "Some College, short continuing education or equivalent", "Up to 12 years of school ", "Up to 9 years of school", "Up to 6 years of school", "None")

    processed_data = data %>%
                    filter(Country%in%country_list, Dem_edu %in% education) %>%
                    group_by(Country,Dem_edu) %>%
                    summarise(nb_surveyed=n()) %>%
                    ungroup() %>%
                    group_by(Country) %>%
                    mutate(perc_surveyed_by_country = (nb_surveyed / sum(nb_surveyed))) %>%
                    ungroup() %>%
                    mutate(country_edu_text = paste0(
                            "Country: ", Country, "\n",
                            "Education: ", Dem_edu, "\n",
                            "# of surveyed: ", nb_surveyed, "\n",
                            "% of surveyed: ", round(perc_surveyed_by_country*100, 2), "%\n"))
    processed_data$Country <- factor(processed_data$Country, levels = rev(country_list))
    processed_data$Dem_edu <- factor(processed_data$Dem_edu, levels = rev(education))

    pEdu <- ggplot(data = processed_data) +
        geom_bar(aes(x = Country, y = perc_surveyed_by_country, fill = Dem_edu, text = country_edu_text), stat="identity", size=0.5, color="grey20") +
        scale_fill_manual(name="Education", values=c("PhD/Doctorate" = "#31233bff", "College degree, bachelor, master" = "#50456cff","Some College, short continuing education or equivalent" = "#6b6099ff","Up to 12 years of school " = "#9392b7ff","Up to 9 years of school" = "#b0b0d1ff","Up to 6 years of school" = "#bec0d4ff", "None" = "#f8f8ffff"))+
        coord_flip() +
        scale_y_continuous(breaks=seq(0,1,0.1),labels = scales::percent_format(accuracy = 1),expand=c(0,0))+
        scale_x_discrete(expand=c(0,0))+
        labs(x = "Country", y = "% Education") +
        theme_classic() +
        theme(legend.position="top")

    return(pEdu)
}

#test
education_plot(c("France","Mexico"))