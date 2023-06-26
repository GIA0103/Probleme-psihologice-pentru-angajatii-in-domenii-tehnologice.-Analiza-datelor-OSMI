library(tidyverse) 
library(corrr)
#install.packages("tidymodels")
library(tidymodels) 
library(readxl)
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)
library(ggplot2)
#install.packages("viridis")
#install.packages("hrbrthemes")
library(viridis)
library(hrbrthemes)
#install.packages("reshape2")
#install.packages("ggpubr")
library(reshape2)
library(ggpubr)



glimpse(mental_health_multiple_answers)



#calculating the number and percentage of null values corresponding to attributes in the dataset
# (not NA, but 'N/A'):
missing_vals <- mental_health_multiple_answers %>%
  map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate (percent_missing = round(n_missing * 100 / 
                                    nrow(mental_health_multiple_answers), 2))


#the plot
ggplot(missing_vals, 
       aes (x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, '%'), 
                hjust = if_else(percent_missing > 3, 1.02, -0.03), 
                vjust = 0.5), size = 4 ) +
  theme(legend.position="none")




# compute the frequencies for each categorical variables and values
eda_factors <- mental_health_multiple_answers %>%
  mutate_if(is.factor, as.character) %>%
  select_if(., is.character ) %>%
  mutate (id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(mental_health_multiple_answers),2)) %>%
  arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)

test <- eda_factors %>%
  filter (is.na(value))

test <- eda_factors %>%
  filter (value == 'N/A')


# plot only the factors with less than 20 distinct values 
eda_factors %>%
  group_by(variable) %>%
  summarise(n_of_values = n()) %>%
  filter (n_of_values <= 20) %>%    
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



################################################################################################################

#anxiety_disorder_generalized_social_phobia_etc
df_anxiety <- mental_health_multiple_answers %>%
  select(
    anxiety_disorder_generalized_social_phobia_etc,
    anxiety_disorder_generalized_social_phobia_etc_1,
    anxiety_disorder_generalized_social_phobia_etc_2
  )

View(df_anxiety)


# transformam toate coloanele in randuri de tipul: id, year, variable, value
all_answers_as_rows <- melt(mental_health_multiple_answers, id.vars = c("id", "year")) %>%
  arrange(id, year) 
View(all_answers_as_rows)

# Dataframe doar cu atributele legate de anxietate. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_anx_2 <- 
  filter(all_answers_as_rows, grepl("anxiety_disorder_generalized_social_phobia_etc", variable, fixed = TRUE))
View(df_anx_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_anx_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_anx_factors <- df_anx_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_anx_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_anx_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

#Do you currently have a mental health disorder?
#If yes, what condition(s) have you been diagnosed with? 
Q1_data = filter(df_anx_factors, variable == "anxiety_disorder_generalized_social_phobia_etc")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


#If maybe, what condition(s) do you believe you have?
#if_possibly_what_disorder_believe_you_have este attributul folosit in setul de date din 2016
Q2_data = filter(df_anx_factors, variable == "anxiety_disorder_generalized_social_phobia_etc_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


#Have you been diagnosed with a mental health condition by a medical professional?
#If so, what condition(s) were you diagnosed with?
Q3_data = filter(df_anx_factors, variable == "anxiety_disorder_generalized_social_phobia_etc_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Stacked plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="stack", stat="identity")
theme_ipsum() +
  #ggtitle("Distributie pe ani asociat diagnosticului pozitiv") +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="stack", stat="identity")
theme_ipsum() +
  #ggtitle("Distributie pe ani asociat x") +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="stack", stat="identity")
theme_ipsum() +
  #ggtitle("Distributie pe ani asociat parerilor personale") +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot




#mood_disorder_depression_bipolar_disorder_etc 
df_mood_disorder <- mental_health_multiple_answers %>%
  select(mood_disorder_depression_bipolar_disorder_etc,
         mood_disorder_depression_bipolar_disorder_etc_1,
         mood_disorder_depression_bipolar_disorder_etc_2
  )

# transformam toate coloanele in randuri de tipul: id, year, variable, value


# Dataframe doar cu atributele legate de depresie. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_depression_2 <- 
  filter(all_answers_as_rows, grepl("mood_disorder_depression_bipolar_disorder_etc", variable, fixed = TRUE))
View(df_depression_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_depression_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_depression_factors <- df_depression_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_depression_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_depression_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

#Do you currently have a mental health disorder?
#If yes, what condition(s) have you been diagnosed with? 
Q1_data = filter(df_anx_factors, variable == "mood_disorder_depression_bipolar_disorder_etc")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


#If maybe, what condition(s) do you believe you have?
Q2_data = filter(df_anx_factors, variable == "mood_disorder_depression_bipolar_disorder_etc_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


#Have you been diagnosed with a mental health condition by a medical professional?
Q3_data = filter(df_anx_factors, variable == "mood_disorder_depression_bipolar_disorder_etc_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

#Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot



#psychotic_disorder_schizophrenia_schizoaffective_etc
df_psychotic_disorder <- mental_health_multiple_answers %>%
  select(psychotic_disorder_schizophrenia_schizoaffective_etc,
         psychotic_disorder_schizophrenia_schizoaffective_etc_1,
         psychotic_disorder_schizophrenia_schizoaffective_etc_2
  )


df_psychotic_2 <- 
  filter(all_answers_as_rows, grepl("psychotic_disorder_schizophrenia_schizoaffective_etc", variable, fixed = TRUE))
View(df_psychotic_2)

#raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_psychotic_2 %>% distinct(variable))


df_psychotic_factors <- df_psychotic_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_psychotic_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_psychotic_factors)

#Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_psychotic_factors, variable == "psychotic_disorder_schizophrenia_schizoaffective_etc")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_psychotic_factors, variable == "psychotic_disorder_schizophrenia_schizoaffective_etc_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_psychotic_factors, variable == "psychotic_disorder_schizophrenia_schizoaffective_etc_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot



#eating_disorder_anorexia_bulimia_etc
df_eating_disorder <- mental_health_multiple_answers %>%
  select(eating_disorder_anorexia_bulimia_etc,
         eating_disorder_anorexia_bulimia_etc_1,
         eating_disorder_anorexia_bulimia_etc_2
  )



# Dataframe doar cu atributele legate de schizofrebie. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_eatdsdr_2 <- 
  filter(all_answers_as_rows, grepl("eating_disorder_anorexia_bulimia_etc", variable, fixed = TRUE))
View(df_eatdsdr_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_eatdsdr_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_eatdsdr_factors <- df_eatdsdr_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_eatdsdr_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_eatdsdr_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_eatdsdr_factors, variable == "eating_disorder_anorexia_bulimia_etc")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_eatdsdr_factors, variable == "eating_disorder_anorexia_bulimia_etc_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_eatdsdr_factors, variable == "eating_disorder_anorexia_bulimia_etc_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot



#############################################################################################
df_ADHD <- mental_health_multiple_answers %>%
  select(attention_deficit_hyperactivity_disorder,
         attention_deficit_hyperactivity_disorder_1,
         attention_deficit_hyperactivity_disorder_2
  )



# Dataframe doar cu atributele legate de schizofrebie. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_adhd_2 <- 
  filter(all_answers_as_rows, grepl("attention_deficit_hyperactivity_disorder", variable, fixed = TRUE))
View(df_adhd_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_adhd_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_adhd_factors <- df_adhd_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_adhd_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_adhd_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_adhd_factors, variable == "attention_deficit_hyperactivity_disorder")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_adhd_factors, variable == "attention_deficit_hyperactivity_disorder_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_adhd_factors, variable == "attention_deficit_hyperactivity_disorder_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot



##############################
#personality_disorder_borderline_antisocial_paranoid_etc
df_personality_disorder <- mental_health_multiple_answers %>%
  select(personality_disorder_borderline_antisocial_paranoid_etc,
         personality_disorder_borderline_antisocial_paranoid_etc_1,
         personality_disorder_borderline_antisocial_paranoid_etc_2
  )



# Dataframe doar cu atributele legate de tulburarea de personalitate. Ne bazam pe faptul ca atributele incep toate cu aceleasi nume 
df_personalitydsdr_2 <- 
  filter(all_answers_as_rows, grepl("personality_disorder_borderline_antisocial_paranoid_etc", variable, fixed = TRUE))
View(df_personalitydsdr_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_personalitydsdr_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_personalitydsdr_factors <- df_personalitydsdr_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_personalitydsdr_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_personalitydsdr_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_personalitydsdr_factors, variable == "personality_disorder_borderline_antisocial_paranoid_etc")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_personalitydsdr_factors, variable == "personality_disorder_borderline_antisocial_paranoid_etc_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_personalitydsdr_factors, variable == "personality_disorder_borderline_antisocial_paranoid_etc_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot




###################################
df_OCD <- mental_health_multiple_answers %>%
  select(obsessive_compulsive_disorder,
         obsessive_compulsive_disorder_1,
         obsessive_compulsive_disorder_2
  )



# Dataframe doar cu atributele legate de ocd. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_ocd_2 <- 
  filter(all_answers_as_rows, grepl("obsessive_compulsive_disorder", variable, fixed = TRUE))
View(df_ocd_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_ocd_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_ocd_factors <- df_ocd_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_ocd_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_ocd_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_ocd_factors, variable == "obsessive_compulsive_disorder")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_ocd_factors, variable == "obsessive_compulsive_disorder_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_ocd_factors, variable == "obsessive_compulsive_disorder_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot



# ########################
#post_traumatic_stress_disorder
df_PTSD <- mental_health_multiple_answers %>%
  select(post_traumatic_stress_disorder,
         post_traumatic_stress_disorder_1,
         post_traumatic_stress_disorder_2
  )


df_ptsd_2 <- 
  filter(all_answers_as_rows, grepl("post_traumatic_stress_disorder", variable, fixed = TRUE))
View(df_ptsd_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_ptsd_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_ptsd_factors <- df_ptsd_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_ptsd_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_ptsd_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_ptsd_factors, variable == "post_traumatic_stress_disorder")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_ptsd_factors, variable == "post_traumatic_stress_disorder_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_ptsd_factors, variable == "post_traumatic_stress_disorder_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot



#####################################
#stress_response_syndromes
df_stress <- mental_health_multiple_answers %>%
  select(stress_response_syndromes,
         stress_response_syndromes_1,
         stress_response_syndromes_2
  )


# Dataframe doar cu atributele legate de stress. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_stress_2 <- 
  filter(all_answers_as_rows, grepl("stress_response_syndromes", variable, fixed = TRUE))
View(df_stress_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_stress_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_stress_factors <- df_stress_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_stress_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_stress_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_stress_factors, variable == "stress_response_syndromes")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_stress_factors, variable == "stress_response_syndromes_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_stress_factors, variable == "stress_response_syndromes_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot


############################
#dissociative_disorder
df_dissociative_disorder <- mental_health_multiple_answers %>%
  select(dissociative_disorder,
         dissociative_disorder_1,
         dissociative_disorder_2
  )



# Dataframe doar cu atributele legate de disociere. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_dissociative_2 <- 
  filter(all_answers_as_rows, grepl("dissociative_disorder", variable, fixed = TRUE))
View(df_dissociative_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_dissociative_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_dissociative_factors <- df_dissociative_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_dissociative_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_dissociative_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_dissociative_factors, variable == "dissociative_disorder")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_dissociative_factors, variable == "dissociative_disorder_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_dissociative_factors, variable == "dissociative_disorder_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot



#########################################################################
#substance_use_disorder
df_substance_use_disorder <- mental_health_multiple_answers %>%
  select(substance_use_disorder,
         substance_use_disorder_1,
         substance_use_disorder_2
  )



# Dataframe doar cu atributele legate de abuzul de substante. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_subst_use_2 <- 
  filter(all_answers_as_rows, grepl("substance_use_disorder", variable, fixed = TRUE))
View(df_eatdsdr_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_subst_use_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_subst_use_factors <- df_subst_use_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_subst_use_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_subst_use_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_subst_use_factors, variable == "substance_use_disorder")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_subst_use_factors, variable == "substance_use_disorder_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_subst_use_factors, variable == "substance_use_disorder_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot

##########################################################
#addictive_disorder
df_addictive_disorder <- mental_health_multiple_answers %>%
  select(addictive_disorder,
         addictive_disorder_1,
         addictive_disorder_2
  )


# Dataframe doar cu atributele legate de adictie. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_addictivedsdr_2 <- 
  filter(all_answers_as_rows, grepl("addictive_disorder", variable, fixed = TRUE))
View(df_eatdsdr_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_addictivedsdr_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_addictivedsdr_factors <- df_addictivedsdr_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_addictivedsdr_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_addictivedsdr_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_addictivedsdr_factors, variable == "addictive_disorder")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_addictivedsdr_factors, variable == "addictive_disorder_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_addictivedsdr_factors, variable == "addictive_disorder_2")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot

#########################
df_other <- mental_health_multiple_answers %>%
  select(other,
         other_1,
         other_3
  )

# Dataframe doar cu atributele legate de alte variante de rasp. Ne bazam pe faptul ca atributele incep toate cu acleasi nume 
df_otherdsdr_2 <- 
  filter(all_answers_as_rows, grepl("other", variable, fixed = TRUE))
View(df_otherdsdr_2)

# raportam procentul final la nr total de intrari pentru fiecare atribut  
nr_distinct_attributes = nrow(df_otherdsdr_2 %>% distinct(variable))

# Obtinem procentele raportate la nr total de raspunsuri pe toti anii - poate fi sau nu o problema
df_otherdsdr_factors <- df_otherdsdr_2 %>%
  mutate (value = coalesce(value, 'N/A')) %>%
  group_by(variable, value, year) %>%
  summarise (n_value = n()) %>%
  ungroup() %>%
  mutate (percent = round(n_value * 100 / nrow(df_otherdsdr_2) * nr_distinct_attributes, 2)) %>%
  arrange(variable, year)
View(df_otherdsdr_factors)

## Desenam un plot grupat pe 3 proprietati (procent, nume atribut, valoare atribut)

#Filtram datele pentru intrebarea dorita

Q1_data = filter(df_otherdsdr_factors, variable == "other")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q2_data = filter(df_otherdsdr_factors, variable == "other_1")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)


Q3_data = filter(df_otherdsdr_factors, variable == "other_3")  %>%
  select(year, value, percent) %>%
  rename(An = year, Legenda = value, Procent = percent)

# Dodge plot
Q1_plot <- ggplot(Q1_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q2_plot <- ggplot(Q2_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

Q3_plot <- ggplot(Q3_data, aes(fill=Legenda, y=Procent, x=An)) + 
  geom_bar(position="dodge", stat="identity")
theme_ipsum() +
  xlab("")

combined_plot <- ggarrange(Q1_plot, Q2_plot, Q3_plot,
                           labels = c("mental_health_disorder_currently_diagnosed_with",
                                      "mental_health_disorder_believe_you_have", 
                                      "mental_health_disorder_diagnosed_by_a_medical_professional"),
                           ncol = 1, nrow = 3)
combined_plot

