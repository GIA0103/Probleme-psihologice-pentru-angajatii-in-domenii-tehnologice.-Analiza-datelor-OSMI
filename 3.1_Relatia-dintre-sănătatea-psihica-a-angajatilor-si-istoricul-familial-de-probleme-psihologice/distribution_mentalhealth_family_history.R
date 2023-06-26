library(tidyverse)
library(readxl)
library(lubridate)
options(scipen = 999)



df_mentalhealth_vs_familyhistory <- df3 %>%
  select(id, year, family_history_of_mental_illness,
         had_a_mental_health_disorder_in_the_past,
         currently_have_a_mental_health_disorder,
         diagnosed_with_a_mental_health_disorder
  )




#Distribution of Employees That Currently Have a Mental Health Disorder Based on Family History of Mental Illness

data <- df_mentalhealth_vs_familyhistory %>%
  filter (currently_have_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(family_history_of_mental_illness) %>%
  summarise(n_of_mentalillness_familyhistory = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_familyhistory * 100) / sum(n_of_mentalillness_familyhistory))
data


ggplot(data = data, aes(x = family_history_of_mental_illness, y = percentage,
                        fill = family_history_of_mental_illness)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Family History of Mental Illness") + ylab("Percentage of Persons that Currently Have a Mental Health Disorder") + 
  ggtitle("Distribution of Employees That Currently Have a Mental Health Disorder Based on Family History of Mental Illness") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 )) 




#Distribution of Employees Who Had a Mental Health Disorder and Family History of Mental Illness

data <- df_mentalhealth_vs_familyhistory %>%
  filter (had_a_mental_health_disorder_in_the_past == 'Yes') %>%
  dplyr::group_by(family_history_of_mental_illness) %>%
  summarise(n_of_mentalillness_familyhistory = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_familyhistory * 100) / sum(n_of_mentalillness_familyhistory))
data


ggplot(data = data, aes(x = family_history_of_mental_illness, y = percentage,
                        fill = family_history_of_mental_illness)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Family History of Mental Illness") + ylab("Percentage of Persons that Had a Mental Health Disorder In the Past") + #!!nu stiu daca e corecta denumirea axei y
  ggtitle("Distribution of Employees Who Had a Mental Health Disorder and Family History of Mental Illness") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 )) 






#Distribution_of_Employees_Diagnosed_with_a_Mental_Health_Disorder_and_Family_History_of_Mental_Illness

data <- df_mentalhealth_vs_familyhistory %>%
  filter (diagnosed_with_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(family_history_of_mental_illness) %>%
  summarise(n_of_mentalillness_familyhistory = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_familyhistory / sum(n_of_mentalillness_familyhistory)) * 100)
data


ggplot(data = data, aes(x = family_history_of_mental_illness, y = percentage,
                        fill = family_history_of_mental_illness)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Family History of Mental Illness") + ylab("Percentage of Persons Diagnosed with a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Diagnosed with a Mental Health Disorder and Family History of Mental Illness") +
  theme(axis.text.x = element_text(angle = 30, 
                                   vjust = 1,
                                   hjust = 1 )) 


