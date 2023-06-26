#install.packages('vcd')
library (vcd)
library(tidyverse)
library(readxl)
library(lubridate)
options(scipen = 999)
#install.packages("ggstatsplot")
library(ggstatsplot)
library(scales)

# giving up scientific notation (1.6e+07)
options(scipen = 999)



df_mentalhealth_basedon_gender <- df3 %>%
  select(id, year, gender,
         had_a_mental_health_disorder_in_the_past,
         currently_have_a_mental_health_disorder,
         diagnosed_with_a_mental_health_disorder
  )



glimpse(df3)

table(df3$productivity__affected_by__mental_health_issue)



df_mentalhealth_basedon_gender_1 <- df_mentalhealth_basedon_gender %>%
  select(id, gender, had_a_mental_health_disorder_in_the_past,
         currently_have_a_mental_health_disorder,
         diagnosed_with_a_mental_health_disorder) %>%
  mutate(gender = tolower(gender)) %>%
  mutate(gender = case_when(
    str_detect(gender, "no.*binary") ~ "other",
    str_detect(gender, "trans") ~ "other",
    gender %in% c("female", "f") | str_detect(gender, "female|woman|femalw|femmina|femile|fm|fem|My sex is female.|I identify as female.|I identify as female") ~ "female",
    gender %in% c("male", "m") | str_detect(gender, "male|man|mail|masculine|masculino|malr|cisdude|dude|m|") ~ "male",
    str_detect(gender, "fluid|contextual") ~ "other",
    gender %in% c("n/a", "none") ~ NA_character_,
    TRUE ~ "other"
  ))





#Distribution of Employees That Currently Have a Mental Health Disorder Based on Gender

data <- df_mentalhealth_basedon_gender_1 %>%
  filter (gender %in% c('male', 'female', 'non-binary', 'transgender', 'other')) %>%
  filter(currently_have_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(gender) %>%
  summarise(n_of_mentalillness_gender = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_gender * 100) / sum(n_of_mentalillness_gender))
data


ggplot(data = data, aes(x = gender, y = percentage,
                        fill = gender)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Gender") + ylab("Percentage of Employees Currently Have a Mental Health Disorder") + 
  ggtitle("Distribution of Employees That Currently Have a Mental Health Disorder Based on Gender") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1,
                                   hjust = 1 )) 




#Distribution of Employees Who Had a Mental Health Disorder and Gender
data <- df_mentalhealth_basedon_gender_1 %>%
  filter (gender %in% c('male', 'female', 'non-binary', 'transgender', 'other')) %>%
  filter(had_a_mental_health_disorder_in_the_past == 'Yes') %>%
  dplyr::group_by(gender) %>%
  summarise(n_of_mentalillness_gender = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_gender * 100) / sum(n_of_mentalillness_gender))
data


ggplot(data = data, aes(x = gender, y = percentage,
                        fill = gender)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Gender") + ylab("Percentage of Employees that Had a Mental Health Disorder") +
  ggtitle("Distribution of Employees Who Had a Mental Health Disorder and Gender") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 )) 




#Distribution of Employees Diagnosed with a Mental Health Disorder and Gender
data <- df_mentalhealth_basedon_gender_1 %>%
  filter (gender %in% c('male', 'female', 'non-binary', 'transgender', 'other')) %>%
  filter(diagnosed_with_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(gender) %>%
  summarise(n_of_mentalillness_gender = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_mentalillness_gender * 100) / sum(n_of_mentalillness_gender))
data


ggplot(data = data, aes(x = gender, y = percentage,
                        fill = gender)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Gender") + ylab("Percentage of Employees Diagnosed with a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Diagnosed with a Mental Health Disorder and Gender") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 )) 


