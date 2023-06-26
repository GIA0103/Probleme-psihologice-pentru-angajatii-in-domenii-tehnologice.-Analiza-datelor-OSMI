#install.packages('vcd')
library (vcd)
library(tidyverse)
library(readxl)
library(lubridate)
options(scipen = 999)



df_mentalhealth_employmentstatus <- df3 %>%
  select(id, year, self_employed,
         had_a_mental_health_disorder_in_the_past,
         currently_have_a_mental_health_disorder,
         diagnosed_with_a_mental_health_disorder
  ) %>%
  mutate(self_employed = case_when(
    self_employed == 1 ~ "Freelancer",
    self_employed == 0 ~ "Employee",
    TRUE ~ as.character(self_employed)
  ))




#Distribution of Employees That Currently Have a Mental Health Disorder Based on Employment Status

data <- df_mentalhealth_employmentstatus %>%
  filter(currently_have_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(self_employed) %>%
  summarise(n_of_mentalillness_employmentstatus = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_employmentstatus * 100) / sum(n_of_mentalillness_employmentstatus))
data


ggplot(data = data, aes(x = self_employed, y = percentage,
                        fill = self_employed)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Employment Status") + ylab("Percentage of Employees that Currently Have a Mental Health Disorder") + 
  ggtitle("Distribution of Employees That Currently Have a Mental Health Disorder Based on Employment Status") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 )) 




#Distribution of Employees That Had a Mental Health Disorder Based on Employment Status

data <- df_mentalhealth_employmentstatus %>%
  filter(had_a_mental_health_disorder_in_the_past == 'Yes') %>%
  dplyr::group_by(self_employed) %>%
  summarise(n_of_mentalillness_employmentstatus = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_employmentstatus * 100) / sum(n_of_mentalillness_employmentstatus))
data


ggplot(data = data, aes(x = self_employed, y = percentage,
                        fill = self_employed)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Employment Status") + ylab("Percentage of Employees that Had a Mental Health Disorder") + 
  ggtitle("Distribution of Employees That Had a Mental Health Disorder Based on Employment Status") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 )) 




#Distribution of Employees That Were Diagnosed with a Mental Health Problem vs. Employment Status

data <- df_mentalhealth_employmentstatus %>%
  filter(diagnosed_with_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(self_employed) %>%
  summarise(n_of_mentalillness_employmentstatus = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_employmentstatus * 100) / sum(n_of_mentalillness_employmentstatus))
data



ggplot(data = data, aes(x = self_employed, y = percentage,
                        fill = self_employed)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Employment Status") + ylab("Percentage of Employees that Were Diagnosed with a Mental Health Disorder") + 
  ggtitle("Distribution of Employees That Were Diagnosed with a Mental Health Problem vs. Employment Status") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 )) 
  