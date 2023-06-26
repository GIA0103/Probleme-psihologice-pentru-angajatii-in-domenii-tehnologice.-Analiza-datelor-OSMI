library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
library(ggplot2)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)


names(demographycs)


#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- demographycs %>%
  select(-id, -year) %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(demographycs), 2))


# now, the plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")


glimpse(demographycs)

# compute the frequencies for each categorical variables and values
eda_factors <- demographycs %>%
  mutate_if(is.factor, as.character) %>%
  select_if(., is.character ) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(demographycs),2)) %>%
  arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')

table(demographycs$gender)

# plot only the factors with less than 100 distinct values 
eda_factors %>%
  mutate(variable = str_replace_all(variable, "_", " ")) %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (n_of_values <= 100) %>%    
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



#plot gender distribution
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "gender") %>%    
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


#procesare gender
df_gender <- demographycs %>%
  select(id, gender) %>%
  mutate(gender = tolower(gender)) %>%
  mutate(gender = case_when(
    str_detect(gender, "no.*binary") ~ "non-binary",
    str_detect(gender, "trans") ~ "transgender",
    gender %in% c("female", "f") | str_detect(gender, "female|woman|femalw|femmina|femile|fm|fem|My sex is female.|I identify as female.|I identify as female") ~ "female",
    gender %in% c("male", "m") | str_detect(gender, "male|man|mail|masculine|masculino|malr|cisdude|dude|m|") ~ "male",
    str_detect(gender, "fluid|contextual") ~ "fluid",
    gender %in% c("n/a", "none") ~ NA_character_,
    TRUE ~ "other"
  ))
# distinct(gender)

test <- demographycs %>%
  filter(str_detect(gender, "trans"))

test <- demographycs %>%
  filter(str_detect(gender, "n/a"))

table(df_gender$gender)


eda_factors <- df_gender %>%
  mutate_if(is.factor, as.character) %>%
  select_if(., is.character ) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_gender),2)) %>%
  arrange(variable, value)
View(eda_factors)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "gender") %>%    
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


test <- df_gender %>%
  filter(str_detect(gender, "binary"))



#race
table(df3$race)

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
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 4) +
  facet_wrap(~ variable, scale = "free", labeller = label_wrap_gen(width=50, multi_line = TRUE)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 10)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')




#procesare country
df_country <- demographycs %>%
  transmute(id, country = country_live_in)
  
  
table(df_country$country)


eda_factors <- df_country %>%
  mutate_if(is.factor, as.character) %>%
  select_if(., is.character ) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate(value = if_else(n_value <= 16, "other", value)) %>%
  group_by(variable, value) %>%
  summarise (n_value = sum(n_value)) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_country),2)) %>%
  arrange(variable, value)
View(eda_factors)



eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "country") %>%    
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


eda_factors %>%
  filter (variable == 'country') %>%    
  distinct (variable) %>%
  inner_join(eda_factors) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(x = value, y = n_value, 
                 label = n_value, hjust = 1.1), size = 4) +
  geom_text (aes(x = value, y = n_value, 
                 label = paste0('(', round(percent,0), '%)'), 
                 hjust = -0.05), size = 3.25) +
  coord_flip() +
  # facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')



#procesare age

num_variables <- demographycs %>%
  mutate(age = if_else(age > 75, NA_integer_, age)) %>%
  select_if(., is.numeric ) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) 
View(num_variables)


#separate histogram for each numeric value; free scale
num_variables %>%
  ggplot(., aes(x = value, fill = variable)) +
  geom_histogram() +
  facet_wrap(~ variable, scale = "free") +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 9)) +
  theme(strip.text.x = element_text(size = 12)) +
  xlab("") + ylab("frequency") 


  
