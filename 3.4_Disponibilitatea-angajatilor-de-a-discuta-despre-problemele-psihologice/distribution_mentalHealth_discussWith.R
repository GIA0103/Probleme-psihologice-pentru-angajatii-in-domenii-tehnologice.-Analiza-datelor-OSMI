#install.packages('vcd')
library (vcd)
library(tidyverse)
library(readxl)
library(lubridate)
options(scipen = 999)
#install.packages("ggstatsplot")
library(ggstatsplot)
library(scales)
#library(vcdExtra)
#install.packages('ggmosaic')
#library(ggmosaic)

# giving up scientific notation (1.6e+07)
options(scipen = 999)




df_mentalhealth_openess_wp <- df3 %>%
  select(id, year,
         would_bring_up_mental_health_with_a_potential_employer_in_an_interview,
         comfortable_discussing_a_mental_health_issue_with_coworkers,
         comfortable_discussing_a_mental_health_issue_with_direct_supervisor,
         how_willing_to_share_with_friends_family_that_you_have_mental_illness,
         currently_have_a_mental_health_disorder,
         diagnosed_with_a_mental_health_disorder
  ) 



#currently_have_a_mental_health_disorder

#Distribution of Employees Who Currently Have a Mental Health Disorder 
#and Their Willingness to Bring Up Mental Health in an Interview

data <- df_mentalhealth_openess_wp %>%
  filter (currently_have_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(would_bring_up_mental_health_with_a_potential_employer_in_an_interview) %>%
  summarise(n_of_discussed_mh_interview = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_discussed_mh_interview * 100) / sum(n_of_discussed_mh_interview))
data


ggplot(data = data, aes(x = would_bring_up_mental_health_with_a_potential_employer_in_an_interview, y = percentage,
                        fill = would_bring_up_mental_health_with_a_potential_employer_in_an_interview)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Willingness to Bring Up Mental Health in an Interview") + ylab("Percentage of Persons that Currently Have a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Who Currently Have a Mental Health Disorder and Their Willingness to Bring Up Mental Health in an Interview") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 ))





#Distribution of Employees Who Currently Have a Mental Health Disorder 
#and Their level of comfortableness to Discuss a Mental Health Issue with Coworkers

data <- df_mentalhealth_openess_wp %>%
  filter (currently_have_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(comfortable_discussing_a_mental_health_issue_with_coworkers) %>%
  summarise(n_of_discussed_mh_coworkers = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_discussed_mh_coworkers * 100) / sum(n_of_discussed_mh_coworkers))
data


ggplot(data = data, aes(x = comfortable_discussing_a_mental_health_issue_with_coworkers, y = percentage,
                        fill = comfortable_discussing_a_mental_health_issue_with_coworkers)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Confortable to Disscus a Mental Health Issue with Coworkers") + ylab("Percentage of Persons that Currently Have a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Who Currently Have a Mental Health Disorder and Their level of comfortableness to Discuss a Mental Health Issue with Coworkers") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 ))





#Distribution of Employees Who Currently Have a Mental Health Disorder 
#and Their level of comfortableness to Discuss a Mental Health Issue with a Direct Supervisor

data <- df_mentalhealth_openess_wp %>%
  filter (currently_have_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(comfortable_discussing_a_mental_health_issue_with_direct_supervisor) %>%
  summarise(n_of_discussed_mh_supervisor = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_discussed_mh_supervisor * 100) / sum(n_of_discussed_mh_supervisor))
data


ggplot(data = data, aes(x = comfortable_discussing_a_mental_health_issue_with_direct_supervisor, y = percentage,
                        fill = comfortable_discussing_a_mental_health_issue_with_direct_supervisor)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Confortable to Discuss a Mental Health Issue with a Direct Supervisor") + ylab("Percentage of Persons that Currently Have a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Who Currently Have a Mental Health Disorder and Their level of comfortableness to Discuss a Mental Health Issue with a Direct Supervisor") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1,
                                   hjust = 1 ))




############################################################################################################
#diagnosed_with_a_mental_health_disorder

#Distribution of Employees Diagnosed with a Mental Health Disorder 
#and Their Willingness to Brig Up Mental Health in an Interview

data <- df_mentalhealth_openess_wp %>%
  filter (diagnosed_with_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(would_bring_up_mental_health_with_a_potential_employer_in_an_interview) %>%
  summarise(n_of_discussed_mh_interview = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_discussed_mh_interview * 100) / sum(n_of_discussed_mh_interview))
data


ggplot(data = data, aes(x = would_bring_up_mental_health_with_a_potential_employer_in_an_interview, y = percentage,
                        fill = would_bring_up_mental_health_with_a_potential_employer_in_an_interview)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Willingness to Brig Up Mental Health in an Interview") + ylab("Percentage of Persons Diagnosed with a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Diagnosed with a Mental Health Disorder and Their Willingness to Brig Up Mental Health in an Interview") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 ))





#Distribution of Employees Diagnosed with a Mental Health Disorder 
#and Their level of comfortableness to Discuss a Mental Health Issue with Coworkers

data <- df_mentalhealth_openess_wp %>%
  filter (diagnosed_with_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(comfortable_discussing_a_mental_health_issue_with_coworkers) %>%
  summarise(n_of_discussed_mh_coworkers = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_discussed_mh_coworkers * 100) / sum(n_of_discussed_mh_coworkers))
data


ggplot(data = data, aes(x = comfortable_discussing_a_mental_health_issue_with_coworkers, y = percentage,
                        fill = comfortable_discussing_a_mental_health_issue_with_coworkers)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Confortable to Disscus a Mental Health Issue with Coworkers") + ylab("Percentage of Persons Diagnosed with a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Diagnosed with a Mental Health Disorder and Their level of comfortableness to Discuss a Mental Health Issue with Coworkers") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1, 
                                   hjust = 1 ))




#Distribution of Employees Diagnosed with a Mental Health Disorder 
#and Their level of comfortableness to Discuss a Mental Health Issue with a Direct Supervisor

data <- df_mentalhealth_openess_wp %>%
  filter (diagnosed_with_a_mental_health_disorder == 'Yes') %>%
  dplyr::group_by(comfortable_discussing_a_mental_health_issue_with_direct_supervisor) %>%
  summarise(n_of_discussed_mh_supervisor = n()) %>%
  ungroup() %>%
  mutate(percentage = (n_of_discussed_mh_supervisor * 100) / sum(n_of_discussed_mh_supervisor))
data


ggplot(data = data, aes(x = comfortable_discussing_a_mental_health_issue_with_direct_supervisor, y = percentage,
                        fill = comfortable_discussing_a_mental_health_issue_with_direct_supervisor)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") + # this will remove the legend
  xlab("Confortable to Discuss a Mental Health Issue with a Direct Supervisor") + ylab("Percentage of Persons Diagnosed with a Mental Health Disorder") + 
  ggtitle("Distribution of Employees Diagnosed with a Mental Health Disorder and Their level of comfortableness to Discuss a Mental Health Issue with a Direct Supervisor") +
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 1,
                                   hjust = 1 ))

