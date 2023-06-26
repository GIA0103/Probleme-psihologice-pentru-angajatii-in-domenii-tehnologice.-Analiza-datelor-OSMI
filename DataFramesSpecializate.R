#libraries
library(tidyverse)
library(dplyr)
#install.packages("janitor")
library(janitor)
#install.packages("rio")
library(rio)
#install_formats()
library(readxl)

getwd()

#crearea DataFrame-urilor specializate 
curent_workplace <- df3 %>%
  select(id, year, self_employed, 
         how_many_employees_company,
         is_employer_primarily_a_tech_company_organization,
         is_primary_role_within_company_related_to_tech_it,
         country_work_in,
         us_state_territory_work_in,
         which_describes_work_position,
         work_remotely
  )


mentalhealth_curent_workplace <- df3 %>%
  select(id, year, employer_mental_health_benefits_healthcare_coverage,
         mental_health_care_employer_provided_coverage,
         employer_formally_discussed_mental_health_campaign,
         employer_offer_resources_options_help,
         anonymity_protected_if_take_advantage_mental_health_treatment_resources_provided_employer,
         prompted_to_request_a_medical_leave_from_work_how_easy_difficult_to_ask_for,
         discussing_physical_health_issue_with_employer_negative_consequences,
         comfortable_discussing_a_mental_health_issue_with_direct_supervisor,
         employer_takes_mental_health_as_seriously_as_physical_health,
         if_diagnosed_treated_for_a_mental_health_disorder_do_you_reveal_this_to_coworkers_employees,
         if_revealed_a_mental_health_issue_to_a_coworker_employee_do_you_believe_this_impacted_you_negatively,
         would_bring_up_mental_health_with_a_potential_employer_in_an_interview,
         why_or_why_not_1,
         observed_experienced_unsupportive_response_to_a_mental_health_issue_in_current_previous_workplace,
         observations_another_individual_discussed_mental_made_you_not_reveal_mental_health_current_workplace,
         discussed_your_mental_health_with_employer,
         conversation_with_employer_about_your_mental_health_reactions_and_actions,
         how_much_importance_employer_place_on_mental_health,
         if_revealed_a_mental_health_disorder_to_a_coworker_employee_how_impacted_you_the_relationship,
         if_you_have_been_diagnosed_or_treated_for_a_mental_health_disorder_do_you_ever_reveal_this_to_coworkers_or_employees,
         have_you_observed_or_experienced_an_unsupportive_or_badly_handled_response_to_a_mental_health_issue_in_your_current_or_previous_workplace,
         circumstances_of_unsupportive_response,
         have_you_observed_or_experienced_supportive_or_well_handled_response_to_a_mental_health_issue_in_your_current_or_previous_workplace,
         circumstances_of_supportive_response
  )


physicalhealth_curent_workplace <- df3 %>%
  select(id, year, discussing_physical_health_issue_with_employer_negative_consequences,
         willing_to_bring_up_a_physical_health_issue_with_employer_in_an_interview,
         why_or_why_not,
         how_much_importance_employer_place_on_physical_health
         )


cowokers_curent_workplace <- df3 %>%
  select(id, year, comfortable_discussing_a_mental_health_issue_with_coworkers,
         heard_observed_negative_consequences_for_coworkers_open_about_mental_health_issues_in_workplace,
         coworkers_would_view_negatively_if_knew_you_suffered_mental_health_issue,
         comfortable_talking_to_coworkers_about_physical_health_or_mental_health,
         discussed_your_mental_health_with_coworkers:conversation_with_coworkers_about_their_mental_health_reactions,
         if_they_knew_you_suffered_how_do_you_think_that_coworkers_would_react
  )


previous_workplace <- df3 %>%
  select(id, year, have_previous_employers,
         do_you_have_previous_employers:have_your_previous_employers_provided_mental_health_benefits
         )


mentalhealth_previous_workplace <- df3 %>%
  select(id, year, previous_employers_provided_mental_health_benefits:discussing_mental_health_disorder_previous_employer_negative_consequences,
         willing_to_discuss_your_mental_health_with_direct_supervisor,
         previous_employer_takes_mental_health_as_seriously_as_physical_health,
         were_you_aware_of_the_options_for_mental_health_care_provided_by_your_previous_employers,
         ever_discuss_your_mental_health_with_previous_employer,
         conversation_with_previous_employer_about_your_mental_health_reactions_and_actions,
         ever_discuss_your_mental_health_with_previous_coworker,
         how_much_importance_previous_employer_place_on_mental_health
         )


physicalhealth_previous_workplace <- df3 %>%
  select(id, year, 
         discussing_physical_health_issue_with_previous_employer_negative_consequences,
         more_comfortable_talking_to_previous_employer_about_your_physical_health_or_mental_health,
         how_much_importance_previous_employer_place_on_physical_health
         )


cowokers_previous_workplace <- df3 %>%
  select(id, year, willing_to_discuss_a_mental_health_with_previous_coworkers,
         hear_or_observe_negative_consequences_for_coworkers_with_mental_health_issues_in_previous_workplaces,
         would_you_have_been_willing_to_discuss_your_mental_health_with_your_coworkers_at_previous_employers,
         conversation_with_previous_coworkers_about_your_mental_health_reactions:conversation_with_coworkers_about_their_mental_health_reactions_1
  )


clients <- df3 %>%
  select(id, year,
         if_diagnosed_treated_for_a_mental_health_disorder_do_you_reveal_this_to_clients,
         if_revealed_a_mental_health_issue_to_a_client_do_you_believe_this_impacted_you_negatively,
         if_you_have_been_diagnosed_or_treated_for_a_mental_health_disorder_do_you_ever_reveal_this_to_clients_or_business_contacts,
         if_revealed_a_mental_health_disorder_to_a_client_how_affected_you_the_relationship
         )


work_and_mentalhealth <- df3 %>%
  select(id, year, productivity__affected_by__mental_health_issue,
         if_yes_percentage_work_time_performing_job_functions_affected_by_mental_health_issue,
         identified_as_a_person_with_mental_health_issue_would_hurt_your_career,
         how_often_it_interferes_with_work_when_treated_effectively,
         how_often_it_interferes_with_work_when_not_treated_effectively_when_experiencing_symptoms,
         if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_being_treated_effectively,
         if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_em_not_em_being_treated_effectively_i_e_when_you_are_experiencing_symptoms,
         mental_health_issue_affected_your_career,
         how_affected_your_career,
         if_you_have_a_mental_health_disorder_how_often_do_you_feel_that_it_interferes_with_your_work_strong_when_strong_em_strong_not_strong_em_strong_being_treated_effectively_i_e_when_you_are_experiencing_symptoms
         )


demographycs <- df3 %>%
  select(id, year, age:us_state_territory_live_in,
         race,
         other_2
         )


mental_health <- df3 %>%
  select(id, year, how_willing_to_share_with_friends_family_that_you_have_mental_illness,
         family_history_of_mental_illness:sought_treatment_from_a_mental_health_professional,
         openly_identified_with_mental_health_issue,
         what_disorder_s_have_you_been_diagnosed_with:if_so_what_disorder_s_were_you_diagnosed_with
         )


mental_health_multiple_answers <- df3 %>%
  select(id, year, 
         anxiety_disorder_generalized_social_phobia_etc,
         anxiety_disorder_generalized_social_phobia_etc_1,
         anxiety_disorder_generalized_social_phobia_etc_2,
         mood_disorder_depression_bipolar_disorder_etc, 
         mood_disorder_depression_bipolar_disorder_etc_1, 
         mood_disorder_depression_bipolar_disorder_etc_2, 
         psychotic_disorder_schizophrenia_schizoaffective_etc, 
         psychotic_disorder_schizophrenia_schizoaffective_etc_1, 
         psychotic_disorder_schizophrenia_schizoaffective_etc_2, 
         eating_disorder_anorexia_bulimia_etc,
         eating_disorder_anorexia_bulimia_etc_1,
         eating_disorder_anorexia_bulimia_etc_2,
         attention_deficit_hyperactivity_disorder,
         attention_deficit_hyperactivity_disorder_1,
         attention_deficit_hyperactivity_disorder_2,
         personality_disorder_borderline_antisocial_paranoid_etc,
         personality_disorder_borderline_antisocial_paranoid_etc_1,
         personality_disorder_borderline_antisocial_paranoid_etc_2,
         obsessive_compulsive_disorder,
         obsessive_compulsive_disorder_1,
         obsessive_compulsive_disorder_2,
         post_traumatic_stress_disorder,
         post_traumatic_stress_disorder_1,
         post_traumatic_stress_disorder_2,
         stress_response_syndromes,
         stress_response_syndromes_1,
         stress_response_syndromes_2,
         dissociative_disorder,
         dissociative_disorder_1,
         dissociative_disorder_2,
         substance_use_disorder,
         substance_use_disorder_1,
         substance_use_disorder_2,
         addictive_disorder,
         addictive_disorder_1,
         addictive_disorder_2,
         other,
         other_1,
         other_3
         )


extern_resources <- df3 %>%
  select(id, year, medical_coverage_includes_treatment_mental_health_disorders,
         know_local_online_resources_help
         )


general_info <- df3 %>%
  select(id, year, x, anything_else_to_tell_us,
         talk_to_us_about_experiences_mental_health_issues_in_tech_industry,
         start_date_utc:network_id
         )


tech_industry <- df3 %>%
  select(id, year, 
         how_well_tech_industry_supports_employees_with_mental_health_issues,
         to_improve_mental_health_support_for_employees
         )


covid_19 <- df3 %>% 
  select(id, year, diagnosed_with_covid_19)



#verificam ce atribute au fost folosite in DataFrame-urile specializate
used_attribs <- c(
  names(curent_workplace),
  names(mentalhealth_curent_workplace),
  names(physicalhealth_curent_workplace),
  names(cowokers_curent_workplace),
  names(previous_workplace),
  names(mentalhealth_previous_workplace),
  names(physicalhealth_previous_workplace),
  names(cowokers_previous_workplace),
  names(clients),
  names(work_and_mentalhealth),
  names(demographycs),
  names(mental_health),
  names(mental_health_multiple_answers),
  names(extern_resources),
  names(general_info),
  names(tech_industry),
  names(covid_19)
)

#verificam daca au ramas atribute neutilizate in DataFrame-urile create anterior
unused_attribs<- setdiff(names(df3), used_attribs)
