library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)
library(ggplot2)



#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- previous_workplace %>%
  select(-year, -id) %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(previous_workplace), 2))


# now, the plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")



#glimpse(previous_workplace)



# first, compute the frequencies for each categorical variables and values
eda_factors <- previous_workplace %>%
  select(-year) %>%
  mutate(do_you_have_previous_employers = case_when(
    do_you_have_previous_employers == 1 ~ "Yes",
    do_you_have_previous_employers == 0 ~ "No",
    TRUE ~ as.character(do_you_have_previous_employers)
  )) %>%
  mutate(have_previous_employers = case_when(
    have_previous_employers == 1 ~ "Yes",
    have_previous_employers == 0 ~ "No",
    TRUE ~ as.character(have_previous_employers)
  )) %>%
  mutate(was_your_employer_primarily_a_tech_company_organization = case_when(
    was_your_employer_primarily_a_tech_company_organization == 1 ~ "Yes",
    was_your_employer_primarily_a_tech_company_organization == 0 ~ "No",
    TRUE ~ as.character(was_your_employer_primarily_a_tech_company_organization)
  )) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(previous_workplace),2)) %>%
  arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')

table(previous_workplace$have_previous_employers)


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
                 vjust = if_else(n_value > 300, 1.5, -0.5)), size = 4) +
  facet_wrap(~ variable, scale = "free", labeller = label_wrap_gen(width=50, multi_line = TRUE)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(strip.text.x = element_text(size = 10)) +
  xlab("") + ylab("frequency") +
  theme(legend.position = 'none')


#names(previous_workplace)

#glimpse(previous_workplace)



#do_you_have_previous_employers
table(previous_workplace$do_you_have_previous_employers)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "do_you_have_previous_employers") %>%    
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


#have_previous_employers distribution
table(previous_workplace$have_previous_employers)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "have_previous_employers") %>%    
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


# was_your_employer_primarily_a_tech_company_organization
table(previous_workplace$was_your_employer_primarily_a_tech_company_organization)


eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (variable == "was_your_employer_primarily_a_tech_company_organization") %>%    
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


