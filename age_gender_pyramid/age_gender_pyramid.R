library(ggplot2)
library(plotly)
library(stringr)

data <- read.csv("../covid_06042020_choice_values.csv", header = T, stringsAsFactors = F)

age_gender_pyramid <- function(country_list){
  processed_data <- data[data$Country%in%country_list,]
  processed_data$Dem_age_sliced <- cut(as.numeric(processed_data$Dem_age), breaks = seq(0, 100, 5), right = FALSE)
  processed_data <- processed_data[!is.na(processed_data$Dem_age_sliced),]

  label_ages <- function(x){x <- str_replace(x,"\\)", "[")
                            str_replace(x,",", "-")}
                            
  p_age_gender_pyramid <- ggplot(data = processed_data, aes(x = Dem_age_sliced, fill=Dem_gender)) +
                          geom_bar(data = subset(processed_data, Dem_gender == "Female"), aes(y = ..count.. * (-1), text = ..count..)) +
                          geom_bar(data = subset(processed_data, Dem_gender == "Male"), aes(y = ..count.. , text = ..count..)) +
                          scale_fill_manual(name="Gender", values = c("Female" = "#00c7b8ff", "Male" = "#31233bff")) +
                          scale_y_continuous(labels = abs) +
                          scale_x_discrete(labels = label_ages) +
                          geom_hline(yintercept=0, size=0.1) +
                          coord_flip() +
                          labs(x = "Age ranges", y = "# of surveyed") +
                          theme_classic()
  return(p_age_gender_pyramid)
}

#test
ggplotly(age_gender_pyramid(c("France","Italy")),tooltip="text")