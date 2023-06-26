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


glimpse(df3)



#Problema de cercetare nr. 1

df_mentalhealth_basedon_gender <- df3 %>%
  select(id, year, gender,
         had_a_mental_health_disorder_in_the_past,
         currently_have_a_mental_health_disorder,
         diagnosed_with_a_mental_health_disorder
  )



df_mentalhealth_basedon_gender_1 <- df_mentalhealth_basedon_gender %>%
  select(id, gender, had_a_mental_health_disorder_in_the_past) %>%
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


#Problema de cercetare nr.1: exista vreo asociere intre existenta in trecut a unei probleme psihice si identitatea de gen a persoanei?
#pentru aceasta problema vom folosi un test de independenta intre variabilele gender si had_a_mental_health_disorder_in_the_past

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_mentalhealth_basedon_gender_1,
  gender,
  had_a_mental_health_disorder_in_the_past
)


# p-value = 1.32 * 10^(-11). Intrucat p-value este mult sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.
#Marimea efectului (V cramer) este insa foarte mica. Cu alte cuvinte, asocierea dintre cele doua variabile 
#este semnificativa din punct de vedere statistic, insa foarte slab.



######################################################################################################################
#Problema de cercetare nr.2


df_mentalhealth_openess_age <- df3 %>%
  select(id, year, age, how_willing_to_share_with_friends_family_that_you_have_mental_illness)

#Problema de cercetare nr.2: 
#Exista o asociere intre varsta si nivelul de deschidere .... familie?
#Pentru evaluarea statistica a asocierii intre aceste doua variabile se foloseste un test de corelatie.
#Daca cele doua variabile au o distributie normala si varianta omogena se foloseste un test parametric. 
#In caz contrar se foloseste un coeficient/test neparametric.
#

#Testam daca variabila age este distribuita normal.


shapiro.test(df_mentalhealth_openess_age$age)
#p-value este mult mai mic decat 0.05 deci varsta nu are o distributie normala. Prin urmare vom folosi un test neparametric.

#Ipoteza nula a testului de corelatie H0 este ca NU exista o asociere intre cele doua variabile. 


# non-parametric correlation coefficient
ggscatterstats(
  data = df_mentalhealth_openess_age,
  x = age,
  y = how_willing_to_share_with_friends_family_that_you_have_mental_illness,
  type = "np"
)


ggscatterstats(
  data = df_mentalhealth_openess_age,
  x = how_willing_to_share_with_friends_family_that_you_have_mental_illness,
  y = age,
  type = "np"
)

#Intrucat p-value este 0.53, cu mult peste pragul de semnificatie de 0.05, ipoteza nula nu poate fi respinsa. 
#Cu alte cuvinte nivelul de deschidere nu pare a fi corelat cu varsta respondentilor. 




###########################################################################################################################################################
#Problema de cercetare nr.3

df_mentalhealth_openess_gender <- df3 %>%
  select(id, year, gender, how_willing_to_share_with_friends_family_that_you_have_mental_illness) %>%
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


### RQ3: Daca exista vreo asociere intre gen si nivelul de deschidere fa??a de familie
#Verificam daca nivelul de deschidere are o distributie normala

shapiro.test(df_mentalhealth_openess_gender$how_willing_to_share_with_friends_family_that_you_have_mental_illness)
#p-value este mult mai mic decat 0.05 deci nivelul de deschidere fa??a de familie nu are o distributie normala. Prin urmare vom folosi un test neparametric.



ggbetweenstats(
  data = df_mentalhealth_openess_gender,
  x = gender,
  y = how_willing_to_share_with_friends_family_that_you_have_mental_illness
)

#p-value > 0.05 adica 0.78, H0 nu poate fi respinsa. Prin urmare nivelul de deschidere fa??a de familie nu depinde nici de genul persoanei. 



#######################################################################################
##Problema de cercetare nr.4


table(df3$how_many_employees_company)


df_companyDimension_mentalHealth <- df3 %>%
  select(id, year, how_many_employees_company,
         diagnosed_with_a_mental_health_disorder
  )


#problema de cercetare nr.4: exista vreo asociere intre diagnosticarea persoanei cu o problema psihica
#si dimensiunea companiei in care aceasta lucreaza?
#pentru aceasta problema vom folosi un test de independenta intre variabilele how_many_employees_company si diagnosed_with_a_mental_health_disorder

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_companyDimension_mentalHealth,
  how_many_employees_company,
  diagnosed_with_a_mental_health_disorder
)


# p-value = 8.57 * 10^(-6). Intrucat p-value este mult sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.
#Marimea efectului (V Cramer) este insa moderata (0.12). Cu alte cuvinte, asocierea dintre cele doua variabile 
#este semnificativa din punct de vedere statistic, insa intr-o masura foarte mica.


###########################################################################################################################
#Problema de cercetare nr. 5


table(df3$productivity__affected_by__mental_health_issue)


df_productivityaffected_gender <- df3 %>%
  select(id, year, gender,
         productivity__affected_by__mental_health_issue
  ) %>%
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


#problema de cercetare nr.5: exista vreo asociere intre 
#nivelul de afectare a productivitatii angajatului care se confrunta cu o problema psihologica
#si identitatea de gen a acestuia?
#pentru aceasta problema vom folosi un test de independenta intre variabilele productivity__affected_by__mental_health_issue si gender

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_productivityaffected_gender,
  gender,
  productivity__affected_by__mental_health_issue
)


# p-value = 0.03. Intrucat p-value este sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.
#Marimea efectului (V Cramer) este moderata (0.08). Cu alte cuvinte, relatia dintre cele doua variabile 
#este semnificativa din punct de vedere statistic, insa foarte slab.



######################################################################################################
#Problema de cercetare nr. 6

table(df3$sought_treatment_from_a_mental_health_professional)



df_gender_treatment_mentalhealth <- df3 %>% 
  select(id, -year, sought_treatment_from_a_mental_health_professional, gender) %>%
  mutate(gender = tolower(gender)) %>%
  mutate(gender = case_when(
    str_detect(gender, "no.*binary") ~ "other",
    str_detect(gender, "trans") ~ "other",
    gender %in% c("female", "f") | str_detect(gender, "female|woman|femalw|femmina|femile|fm|fem|My sex is female.|I identify as female.|I identify as female") ~ "female",
    gender %in% c("male", "m") | str_detect(gender, "male|man|mail|masculine|masculino|malr|cisdude|dude|m|") ~ "male",
    str_detect(gender, "fluid|contextual") ~ "other",
    gender %in% c("n/a", "none") ~ NA_character_,
    TRUE ~ "other"
  )) %>%
  mutate(sought_treatment_from_a_mental_health_professional = case_when(
    sought_treatment_from_a_mental_health_professional == 1 ~ "Yes",
    sought_treatment_from_a_mental_health_professional == 0 ~ "No",
    TRUE ~ as.character(sought_treatment_from_a_mental_health_professional)
  ))

#problema de cercetare nr.6: exista vreo asociere intre dorinta de a apela la tratament/tratare 
#pentru o problema psihologica si genul/sexul persoanei?
#pentru aceasta problema vom folosi un test de independenta intre variabilele gender si sought_treatment_from_a_mental_health_professional

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_gender_treatment_mentalhealth,
  gender,
  sought_treatment_from_a_mental_health_professional
)

# p-value = 4.35 * 10^(-20). Intrucat p-value este mult sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.
#Marimea efectului (V Cramer) este moderata (0.16). Cu alte cuvinte, relatia dintre cele doua variabile 
#este semnificativa din punct de vedere statistic, insa nu este foarte puternica.


####################################################################################################################################################
#Problema de cercetare nr. 7


df_familly_diagnosedmentalhealth <- df3 %>%
  select(id, year, diagnosed_with_a_mental_health_disorder,
         family_history_of_mental_illness
  ) 


#problema de cercetare nr.7: exista vreo asociere intre diagnosticarea cu o problema psihologica
#si istoricul familial de boli psihice?
#pentru aceasta problema vom folosi un test de independenta intre variabilele diagnosed_with_a_mental_health_disorder si family_history_of_mental_illness

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_familly_diagnosedmentalhealth,
  diagnosed_with_a_mental_health_disorder,
  family_history_of_mental_illness
)


# p-value = 5.52 * 10 (-77). Intrucat p-value este sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.
#Marimea efectului (V Cramer) este moderata (0.40). Cu alte cuvinte, relatia dintre cele doua variabile 
#este puternic semnificativa din punct de vedere statistic.



#############################################################################################################################################################
#Problema de cercetare nr. 8

df_familly_currentlymentalhealth <- df3 %>%
  select(id, year, currently_have_a_mental_health_disorder,
         family_history_of_mental_illness
  ) 




#problema de cercetare nr.8: exista vreo asociere intre a avea in prezent o problema psihologica
#si istoricul familial de boli psihice a persoanei in cauza?
#pentru aceasta problema vom folosi un test de independenta intre variabilele currently_have_a_mental_health_disorder si family_history_of_mental_illness

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_familly_currentlymentalhealth,
  currently_have_a_mental_health_disorder,
  family_history_of_mental_illness
)


# p-value = 4.70 * 10 (-141). Intrucat p-value este sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.
#Marimea efectului (V Cramer) este moderata (0.32). Cu alte cuvinte, relatia dintre cele doua variabile 
#este semnificativa din punct de vedere statistic.



###############################################################################################################
#Problema de cercetare nr. 9


table(df3$how_affected_your_career)

df_gender_mentalhealthImpact <- df3 %>%
  select(id, year, how_affected_your_career, gender) %>%
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



### RQ9: Daca exista vreo asociere intre gen si impactul problemelor psihologice asupra carierei persoanei
#Verificam daca nivelul de deschidere are o distributie normala

shapiro.test(df_gender_mentalhealthImpact$how_affected_your_career)
#p-value este mult mai mic decat 0.05 deci impactul problemelor psihologice nu are o distributie normala.
#Prin urmare vom folosi un test neparametric.



ggbetweenstats(
  data = df_gender_mentalhealthImpact,
  x = gender,
  y = how_affected_your_career
)

#p-value < 0.05 adica 0.04, H0 poate fi respinsa. Prin urmare impactul problemelor psihologice
#asupra performantei profesionale a individlui depinde de identitatea de gen a acestuia. 

# p-value = 0.04. Intrucat p-value este sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.


############################################################################################################################
#Problema de cercetare nr. 10


table(df3$comfortable_discussing_a_mental_health_issue_with_coworkers)


df_gender_discuss_mentalhealth <- df3 %>% 
  select(id, year, comfortable_discussing_a_mental_health_issue_with_coworkers, gender) %>%
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

#problema de cercetare nr. 10: exista vreo asociere intre nivelul de deschidere cu privire la 
#a discuta despre o problema psihologica cu un coleg de munca  si genul/sexul persoanei?
#pentru aceasta problema vom folosi un test de independenta intre variabilele gender si comfortable_discussing_a_mental_health_issue_with_coworkers

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_gender_discuss_mentalhealth,
  gender,
  comfortable_discussing_a_mental_health_issue_with_coworkers
)

#p-value > 0.05 adica 0.40, H0 nu poate fi respinsa. 
#Prin urmare nivelul de deschidere cu privire la a discuta despre o problema psihologica cu un coleg de munca
# NU depinde de identitatea de gen a persoanei. 





###################################################################################################################
#Problema de cercetare nr. 11


table(df3$coworker_discuss_their_another_s_mental_health_with_you)


df_discussanothercoworker_mentalhealth_gender <- df3 %>% 
  select(id, -year, coworker_discuss_their_another_s_mental_health_with_you, gender) %>%
  mutate(gender = tolower(gender)) %>%
  mutate(gender = case_when(
    str_detect(gender, "no.*binary") ~ "other",
    str_detect(gender, "trans") ~ "other",
    gender %in% c("female", "f") | str_detect(gender, "female|woman|femalw|femmina|femile|fm|fem|My sex is female.|I identify as female.|I identify as female") ~ "female",
    gender %in% c("male", "m") | str_detect(gender, "male|man|mail|masculine|masculino|malr|cisdude|dude|m|") ~ "male",
    str_detect(gender, "fluid|contextual") ~ "other",
    gender %in% c("n/a", "none") ~ NA_character_,
    TRUE ~ "other"
  )) %>%
  mutate(coworker_discuss_their_another_s_mental_health_with_you = case_when(
    coworker_discuss_their_another_s_mental_health_with_you == 1 ~ "Yes",
    coworker_discuss_their_another_s_mental_health_with_you == 0 ~ "No",
    TRUE ~ as.character(coworker_discuss_their_another_s_mental_health_with_you)
  ))


#problema de cercetare nr.11: exista vreo asociere intre nivelul de deschidere cu privire la 
#a discuta despre o problema psihologica cu un coleg de munca si identitatea de gen a colegului cu care se discuta? ##reformulare
#pentru aceasta problema vom folosi un test de independenta intre variabilele gender si coworker_discuss_their_another_s_mental_health_with_you

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_discussanothercoworker_mentalhealth_gender,
  gender,
  coworker_discuss_their_another_s_mental_health_with_you
)

# p-value = 2.12 * 10^(-3). Intrucat p-value este mult sub pragul de semnificatie de 0,05, 
#putem sa respingem ipoteza nula (H0). Prin urmare putem sa spunem ca exista o asociere intre cele doua variabile.
#Marimea efectului (V Cramer) este moderata (0.08). Cu alte cuvinte, relatia dintre cele doua variabile 
#este semnificativa din punct de vedere statistic, insa foarte slab.




##########################################################################################################################################################
#Problema de cercetare nr. 12


table(df3$discussing_mental_health_disorder_with_employer_negative_consequences)


df_discussmh_negativeconsequences_gender <- df3 %>%
  select(id, year, gender,
         discussing_mental_health_disorder_with_employer_negative_consequences
  ) %>%
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



#problema de cercetare nr.12: exista vreo asociere intre consecintele negative pe care angajatii 
#le intampina cand discuta cu angajatorii despre problemele psihologice cu care se confrunta 
#si identitatea de gen a persoanei?
#pentru aceasta problema vom folosi un test de independenta intre variabilele gender si discussing_mental_health_disorder_with_employer_negative_consequences

#ipoteza nula (H0) a testului de independenta este ca NU EXISTA vreo asociere intre cele doua variabile

ggbarstats(
  data = df_discussmh_negativeconsequences_gender,
  gender,
  discussing_mental_health_disorder_with_employer_negative_consequences
)


#p-value > 0.05 adica 0.10, H0 nu poate fi respinsa. 
#Prin urmare consecintele negative pe care angajatii 
#le intampina cand discuta cu angajatorii despre problemele psihologice cu care se confrunta
# NU depinde de identitatea de gen a persoanei. 


