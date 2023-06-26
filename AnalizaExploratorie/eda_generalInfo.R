library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)
library(ggplot2)

names(general_info)

#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- general_info %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(general_info), 2))


# now, the plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")




# compute the frequencies for each categorical variables and values
eda_factors <- general_info %>%
  mutate(talk_to_us_about_experiences_mental_health_issues_in_tech_industry = case_when(
    talk_to_us_about_experiences_mental_health_issues_in_tech_industry == 1 ~ "Yes",
    talk_to_us_about_experiences_mental_health_issues_in_tech_industry == 0 ~ "No",
    TRUE ~ as.character(talk_to_us_about_experiences_mental_health_issues_in_tech_industry)
  )) %>%
  mutate_if(is.factor, as.character) %>%
  select_if(., is.character ) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(general_info),2)) %>%
  arrange(variable, value)
View(eda_factors)


glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')

table(general_info$have_previous_employers)




#talk_to_us_about_experiences_mental_health_issues_in_tech_industry 
table(general_info$talk_to_us_about_experiences_mental_health_issues_in_tech_industry)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "talk_to_us_about_experiences_mental_health_issues_in_tech_industry") %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 3) +
  facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')

