library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)
library(ggplot2)

glimpse(cowokers_curent_workplace)

names(cowokers_curent_workplace)

  

#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- cowokers_curent_workplace %>%
  select(-year, -id) %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(cowokers_curent_workplace), 2))


#plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")


# first, compute the frequencies for each integer variables and values
eda_factors <- cowokers_curent_workplace %>%
  select(-year, -conversation_with_coworkers_about_their_mental_health_reactions,
         -conversation_with_coworkers_about_your_mental_health_reactions) %>% 
  mutate(discussed_your_mental_health_with_coworkers = case_when(
    discussed_your_mental_health_with_coworkers == 1 ~ "Yes",
    discussed_your_mental_health_with_coworkers == 0 ~ "No",
    TRUE ~ as.character(discussed_your_mental_health_with_coworkers)
  )) %>%
  mutate(coworker_discuss_their_another_s_mental_health_with_you = case_when(
    coworker_discuss_their_another_s_mental_health_with_you == 1 ~ "Yes",
    coworker_discuss_their_another_s_mental_health_with_you == 0 ~ "No",
    TRUE ~ as.character(coworker_discuss_their_another_s_mental_health_with_you)
  )) %>%
  mutate(if_they_knew_you_suffered_how_do_you_think_that_coworkers_would_react = as.character(if_they_knew_you_suffered_how_do_you_think_that_coworkers_would_react)) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  #  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(cowokers_curent_workplace),2)) %>%
  arrange(variable, value)
View(eda_factors)

summary(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')


# plot only the factors with less than distinct values 
eda_factors %>%
  mutate(variable = str_replace_all(variable, "_", " ")) %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors %>%
               mutate(variable = str_replace_all(variable, "_", " "))
  ) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 3) +
  facet_wrap(~ variable, scale = "free", labeller = label_wrap_gen(width=50, multi_line = TRUE)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 10)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')


#### plots for each attribute ####

# comfortable_discussing_a_mental_health_issue_with_coworkers   
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "comfortable_discussing_a_mental_health_issue_with_coworkers") %>%    
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



# heard_observed_negative_consequences_for_coworkers_open_about_mental_health_issues_in_workplace
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "heard_observed_negative_consequences_for_coworkers_open_about_mental_health_issues_in_workplace") %>%    
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



# coworkers_would_view_negatively_if_knew_you_suffered_mental_health_issue            
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "coworkers_would_view_negatively_if_knew_you_suffered_mental_health_issue") %>%    
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



# comfortable_talking_to_coworkers_about_physical_health_or_mental_health     
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "comfortable_talking_to_coworkers_about_physical_health_or_mental_health") %>%    
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



# conversation_with_coworkers_about_their_mental_health_reactions 
# este o intrebare cu raspuns deschis
table(cowokers_curent_workplace$conversation_with_coworkers_about_their_mental_health_reactions)




# conversation_with_coworkers_about_your_mental_health_reactions  
# este o intrebare cu raspuns deschis
table(cowokers_curent_workplace$conversation_with_coworkers_about_your_mental_health_reactions)




# coworker_discuss_their_another_s_mental_health_with_you    
table(cowokers_curent_workplace$coworker_discuss_their_another_s_mental_health_with_you)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "coworker_discuss_their_another_s_mental_health_with_you") %>%    
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





# if_they_knew_you_suffered_how_do_you_think_that_coworkers_would_react
table(cowokers_curent_workplace$if_they_knew_you_suffered_how_do_you_think_that_coworkers_would_react)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "if_they_knew_you_suffered_how_do_you_think_that_coworkers_would_react") %>%    
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




# conversation_with_coworkers_about_your_mental_health_reactions    
# este o intrebare cu raspuns deschis
table(cowokers_curent_workplace$conversation_with_coworkers_about_your_mental_health_reactions)





# discussed_your_mental_health_with_coworkers      
table(cowokers_curent_workplace$discussed_your_mental_health_with_coworkers)

eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "discussed_your_mental_health_with_coworkers") %>%    
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