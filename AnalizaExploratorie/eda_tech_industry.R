library(tidyverse) 
library(corrr)
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)
library(ggplot2)


names(tech_industry)


#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- tech_industry %>%
  select(-id, -year) %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(tech_industry), 2))


# now, the plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")


#glimpse(tech_industry)


# compute the frequencies for each categorical variables and values
eda_factors <- tech_industry %>%
  select(-year, -to_improve_mental_health_support_for_employees) %>%
  mutate(how_well_tech_industry_supports_employees_with_mental_health_issues = as.character(how_well_tech_industry_supports_employees_with_mental_health_issues)) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(tech_industry),2)) %>%
  arrange(variable, value)
View(eda_factors)


glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')

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


#to_improve_mental_health_support_for_employees
#este o intrebare cu raspuns deschis, motiv pentru care nu se poate realiza un plot lizibil
table(tech_industry$to_improve_mental_health_support_for_employees)


