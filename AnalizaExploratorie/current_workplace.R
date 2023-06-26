library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)
library(ggplot2)

glimpse(curent_workplace)

#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- curent_workplace %>%
  select(-year, -id) %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(curent_workplace), 2))


#the plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")


#glimpse(curent_workplace)


#compute the frequencies for each categorical variables and values
eda_factors <- curent_workplace %>%
  mutate(is_employer_primarily_a_tech_company_organization = case_when(
    is_employer_primarily_a_tech_company_organization == 1 ~ "Yes",
    is_employer_primarily_a_tech_company_organization == 0 ~ "No",
    TRUE ~ as.character(is_employer_primarily_a_tech_company_organization)
  )) %>%
  mutate(is_primary_role_within_company_related_to_tech_it = case_when(
    is_primary_role_within_company_related_to_tech_it == 1 ~ "Yes",
    is_primary_role_within_company_related_to_tech_it == 0 ~ "No",
    TRUE ~ as.character(is_primary_role_within_company_related_to_tech_it)
  )) %>%
  mutate(self_employed = case_when(
    self_employed == 1 ~ "Yes",
    self_employed == 0 ~ "No",
    TRUE ~ as.character(self_employed)
  )) %>%
  mutate_if(is.factor, as.character) %>%
  select_if(., is.character ) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(curent_workplace),2)) %>%
  arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')

table(curent_workplace$self_employed)


#plot only the factors with less than 100 distinct values 
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
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 3.5) +
  facet_wrap(~ variable, scale = "free", labeller = label_wrap_gen(width=50, multi_line = TRUE)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 8)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')


#plot work_remotely distribution
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "work_remotely") %>%    
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

#names(curent_workplace)

#plot how_many_employees_company distribution
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "how_many_employees_company") %>%    
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


#plot country_work_in distribution
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "country_work_in") %>%    
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



#procesam tara in care lucreaza
df_country_work <- curent_workplace %>%
  transmute(id, country_work = country_work_in)


table(df_country_work$country_work)


eda_factors <- df_country_work %>%
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


#plot country_work
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "country_work") %>%    
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


#which_describes_work_position
table(curent_workplace$which_describes_work_position)

df_work_position <- curent_workplace %>%
  select(id, which_describes_work_position) %>%
  mutate(position = str_split(which_describes_work_position, "\\|")) %>%
  unnest(position)

table(df_work_position$position)

eda_factors_2 <- df_work_position %>%
  select(id, position) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_work_position),2)) %>%
  arrange(variable, value)
View(eda_factors_2)


eda_factors_2 %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "position") %>%    
  ungroup() %>%
  select (variable) %>%
  inner_join(eda_factors_2) %>%
  ggplot(., aes(x = value, y = n_value, fill = value)) +
  geom_col() +
  geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 3) +
  facet_wrap(~ variable, scale = "free") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 13)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')
