library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)


glimpse(mental_health)


#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- mental_health %>%
  select(-year, -id) %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(mental_health), 2))


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
eda_factors <- mental_health %>%
  select(-year) %>%
  mutate(sought_treatment_from_a_mental_health_professional = case_when(
    sought_treatment_from_a_mental_health_professional == 1 ~ "Yes",
    sought_treatment_from_a_mental_health_professional == 0 ~ "No",
    TRUE ~ as.character(sought_treatment_from_a_mental_health_professional)
  )) %>%
  mutate(openly_identified_with_mental_health_issue = case_when(
    openly_identified_with_mental_health_issue == 1 ~ "Yes",
    openly_identified_with_mental_health_issue == 0 ~ "No",
    TRUE ~ as.character(openly_identified_with_mental_health_issue)
  )) %>%
  mutate_if(is.factor, as.character) %>%
  # select_if(., is.character ) %>%
  mutate(how_willing_to_share_with_friends_family_that_you_have_mental_illness = as.character(how_willing_to_share_with_friends_family_that_you_have_mental_illness)) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(mental_health),2)) %>%
  arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')

table(mental_health$currently_have_a_mental_health_disorder)



# plot only the factors with less than 35 distinct values 
eda_factors %>%
  mutate(variable = str_replace_all(variable, "_", " ")) %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (n_of_values <= 35) %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors %>%
               mutate(variable = str_replace_all(variable, "_", " "))
  ) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 4) +
  facet_wrap(~ variable, scale = "free", labeller = label_wrap_gen(width=50, multi_line = TRUE)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 10)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')


#currently_have_a_mental_health_disorder                               
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "currently_have_a_mental_health_disorder") %>%    
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



#had_a_mental_health_disorder_in_the_past
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "had_a_mental_health_disorder_in_the_past") %>%    
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


#diagnosed_with_a_mental_health_disorder
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "diagnosed_with_a_mental_health_disorder") %>%    
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



#family_history_of_mental_illness
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "family_history_of_mental_illness") %>%    
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




#what_disorder_were_diagnosed_with
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "what_disorder_were_diagnosed_with") %>%    
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



table(mental_health$what_disorder_were_diagnosed_with)



df_disorder_were_diagnosed_with <- mental_health %>%
  select(id, what_disorder_were_diagnosed_with) %>%
  mutate(what_disorder_were_diagnosed_with = str_split(what_disorder_were_diagnosed_with, "\\|")) %>%
  unnest(what_disorder_were_diagnosed_with)



table(df_disorder_were_diagnosed_with$what_disorder_were_diagnosed_with)


eda_factors_4 <- df_disorder_were_diagnosed_with %>%
  select(id, what_disorder_were_diagnosed_with) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_disorder_were_diagnosed_with),2)) %>%
  arrange(variable, value)
View(eda_factors_4)



eda_factors_4 %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "what_disorder_were_diagnosed_with") %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors_4) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 5) +
  facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 12, angle = 70, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')



###########################################################################################

#if_possibly_what_disorder_believe_you_have
table(mental_health$if_possibly_what_disorder_believe_you_have)



df_disorder_believe_you_have <- mental_health %>%
  select(id, if_possibly_what_disorder_believe_you_have) %>%
  mutate(what_disorder_believe_you_have = str_split(if_possibly_what_disorder_believe_you_have, "\\|")) %>%
  unnest(what_disorder_believe_you_have)



table(df_disorder_believe_you_have$what_disorder_believe_you_have)
#este intrebare cu raspuns multiplu 



eda_factors_4 <- df_disorder_believe_you_have %>%
  select(id, what_disorder_believe_you_have) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_disorder_believe_you_have),2)) %>%
  arrange(variable, value)
View(eda_factors_4)



eda_factors_4 %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "what_disorder_believe_you_have") %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors_4) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 5) +
  facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 12, angle = 70, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')

##########################################################################################

#if_so_what_disorder_s_were_you_diagnosed_with
#este intrebare la care s-a putut da raspuns multiplu 

table(mental_health$if_so_what_disorder_s_were_you_diagnosed_with)


#if_so_what_disorder_s_were_you_diagnosed_with
table(mental_health$if_so_what_disorder_s_were_you_diagnosed_with)

df_disorder_were_diagnosed_with <- mental_health %>%
  select(id, if_so_what_disorder_s_were_you_diagnosed_with) %>%
  mutate(disorder_were_diagnosed_with = str_split(if_so_what_disorder_s_were_you_diagnosed_with, "\\|")) %>%
  unnest(disorder_were_diagnosed_with)


table(df_disorder_were_diagnosed_with$disorder_were_diagnosed_with)

#este intrebare cu raspuns multiplu 

eda_factors_4 <- df_disorder_were_diagnosed_with %>%
  select(id, disorder_were_diagnosed_with) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_disorder_were_diagnosed_with),2)) %>%
  arrange(variable, value)
View(eda_factors_4)



eda_factors_4 %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "disorder_were_diagnosed_with") %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors_4) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 5) +
  facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 12, angle = 70, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')



###############################################
#if_yes_what_condition_diagnosed_with_currently
#este intrebare cu raspuns multiplu 
table(mental_health$if_yes_what_condition_diagnosed_with_currently)


#if_yes_what_condition_diagnosed_with_currently
table(mental_health$if_yes_what_condition_diagnosed_with_currently)

df_condition_diagnosed_with_currently <- mental_health %>%
  select(id, if_yes_what_condition_diagnosed_with_currently) %>%
  mutate(condition_diagnosed_with_currently = str_split(if_yes_what_condition_diagnosed_with_currently, "\\|")) %>%
  unnest(condition_diagnosed_with_currently)


table(df_condition_diagnosed_with_currently$condition_diagnosed_with_currently)

 

eda_factors_4 <- df_condition_diagnosed_with_currently %>%
  select(id, condition_diagnosed_with_currently) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_condition_diagnosed_with_currently),2)) %>%
  arrange(variable, value)
View(eda_factors_4)



eda_factors_4 %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "condition_diagnosed_with_currently") %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors_4) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 5) +
  facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 12, angle = 70, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')

########################################################################################################################

#sought_treatment_from_a_mental_health_professional
table(mental_health$sought_treatment_from_a_mental_health_professional)


#the plot for sought_treatment_from_a_mental_health_professional
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "sought_treatment_from_a_mental_health_professional") %>%    
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




#openly_identified_with_mental_health_issue 
table(mental_health$openly_identified_with_mental_health_issue)


#the plot for openly_identified_with_mental_health_issue
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "openly_identified_with_mental_health_issue") %>%    
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





#how_willing_to_share_with_friends_family_that_you_have_mental_illness 
table(mental_health$how_willing_to_share_with_friends_family_that_you_have_mental_illness)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "how_willing_to_share_with_friends_family_that_you_have_mental_illness") %>%    
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

