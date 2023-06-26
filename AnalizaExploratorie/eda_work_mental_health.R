library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)


glimpse(work_and_mentalhealth)


#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- work_and_mentalhealth %>%
  select(-id, -year) %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(work_and_mentalhealth), 2))


#the plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")



#first, compute the frequencies for each categorical variables and values
eda_factors <- work_and_mentalhealth %>%
  select(-year) %>%
  mutate(mental_health_issue_affected_your_career = case_when(
    mental_health_issue_affected_your_career == 1 ~ "Yes",
    mental_health_issue_affected_your_career == 0 ~ "No",
    TRUE ~ as.character(mental_health_issue_affected_your_career)
  )) %>%
  mutate(how_affected_your_career = as.character(how_affected_your_career)) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(work_and_mentalhealth),2)) %>%
  arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')

table(work_and_mentalhealth$mental_health_issue_affected_your_career)

names(work_and_mentalhealth)
                                                                                                                                                            



# plot only the factors with less than 20 distinct values 
eda_factors %>%
  mutate(variable = str_replace_all(variable, "_", " ")) %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (n_of_values <= 20) %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors %>%
               mutate(variable = str_replace_all(variable, "_", " "))
  ) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 3.5) +
  facet_wrap(~ variable, scale = "free", labeller = label_wrap_gen(width=50, multi_line = TRUE)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 8)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')




#productivity__affected_by__mental_health_issue 
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "productivity__affected_by__mental_health_issue") %>%    
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



#if_yes_percentage_work_time_performing_job_functions_affected_by_mental_health_issue
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "if_yes_percentage_work_time_performing_job_functions_affected_by_mental_health_issue") %>%    
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



#identified_as_a_person_with_mental_health_issue_would_hurt_your_career
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "identified_as_a_person_with_mental_health_issue_would_hurt_your_career") %>%    
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



#how_often_it_interferes_with_work_when_treated_effectively
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "how_often_it_interferes_with_work_when_treated_effectively") %>%    
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




#how_often_it_interferes_with_work_when_not_treated_effectively_when_experiencing_symptoms
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "how_often_it_interferes_with_work_when_not_treated_effectively_when_experiencing_symptoms") %>%    
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




#if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_being_treated_effectively                                                                             
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_being_treated_effectively") %>%    
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




# if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_em_not_em_being_treated_effectively_i_e_when_you_are_experiencing_symptoms                           
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_em_not_em_being_treated_effectively_i_e_when_you_are_experiencing_symptoms") %>%    
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





# if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_strong_em_strong_not_strong_em_strong_being_treated_effectively_i_e_when_you_are_experiencing_symptoms
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_strong_em_strong_not_strong_em_strong_being_treated_effectively_i_e_when_you_are_experiencing_symptoms") %>%    
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




#mental_health_issue_affected_your_career   
table(work_and_mentalhealth$mental_health_issue_affected_your_career)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "mental_health_issue_affected_your_career") %>%    
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



#how_affected_your_career 
table(work_and_mentalhealth$how_affected_your_career)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "how_affected_your_career") %>%    
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