# Cleaning data for analyses (only includes participants who said that god is real)

# loading package
library(readr)
library(janitor)
library(tidyverse)

# read in data ----
d_all <- read_csv("data/data_raw/cielo_merged_raw.csv")

# snake case, removing spaces, all lowercase values
d_all <- d_all %>%
  clean_names() %>%
  mutate_all(tolower)

# changing all of the written "not answered" values to NAs
d_all <- d_all %>%
  mutate_all(~ ifelse(. == "not answered", NA, .))

# removing trailing whitespaces from dataframe
d_all <- d_all %>%
  mutate(across(where(is.character), str_trim))

# subsetting participants who need to be excluded from analyses
d_all <- d_all %>%
  filter(subject_number != "129" & subject_number != "152" & subject_number != "175a" & subject_number != "277")

# getting rid of columns we do not need
d_all <- d_all %>% select(-dogs_are_one_of_my_favorite_animals, 
                          -i_like_to_eat_vegetables_more_than_chocolate, 
                          -i_like_to_go_swimming_in_the_summer,
                          -lots_of_the_things_in_my_life_happen_on_their_own_by_accident,
                          -what_about_germs_are_there_really_germs_in_the_world_do_germs_exist,
                          -how_sure_are_you_20,
                          -what_about_vitamins_are_there_really_vitamins_in_the_world_do_vitamins_exist,
                          -how_sure_are_you_22,
                          -what_about_mermaids_are_there_really_mermaids_in_the_world_do_mermaids_exist,
                          -how_sure_are_you_24,
                          -what_about_elves_are_there_really_elves_in_the_world_do_elves_exist,
                          -how_sure_are_you_26,
                          -what_about_fairies_are_there_really_fairies_in_the_world_do_fairies_exist,
                          -how_sure_are_you_28,
                          -what_about_the_tooth_fairy_is_there_really_a_tooth_fairy_in_the_world_does_the_tooth_fairy_exist,
                          -how_sure_are_you_34,
                          -what_about_santa_claus_is_there_really_santa_claus_in_the_world_does_santa_claus_exist,
                          -how_sure_are_you_36,
                          -what_about_oxygen_is_there_really_oxygen_in_the_world_does_oxygen_exist,
                          -how_sure_are_you_38,
                          -jordan_said_he_thinks_that_god_lives_in_heaven_and_watches_over_people_on_earth_but_god_doesnt_really_make_things_happen_in_peoples_lives,
                          -jordan_said_she_thinks_that_god_lives_in_heaven_and_watches_over_people_on_earth_but_god_doesnt_really_make_things_happen_in_peoples_lives,
                          -riley_said_he_thinks_that_god_lives_in_heaven_and_watches_over_people_on_earth_and_god_makes_things_happen_in_peoples_lives_but_only_at_special_times,
                          -riley_said_she_thinks_that_god_lives_in_heaven_and_watches_over_people_on_earth_and_god_makes_things_happen_in_peoples_lives_but_only_at_special_times,
                          -sam_said_he_thinks_that_god_lives_in_heaven_and_watches_over_people_on_earth_and_god_makes_things_happen_in_peoples_lives_almost_every_day,
                          -sam_said_she_thinks_that_god_lives_in_heaven_and_watches_over_people_on_earth_and_god_makes_things_happen_in_peoples_lives_almost_every_day,
                          -now_i_want_to_know_who_do_you_agree_with_the_most_jordan_who_says_god_doesnt_make_things_happen_in_peoples_lives_riley_who_says_god_makes_things_happen_in_peoples_lives_but_only_at_special_times_or_sam_who_says_god_still_makes_things_happen_in_peoples_lives_now,
                          -now_i_want_to_know_who_do_you_agree_with_the_most_jordan_who_says_god_doesnt_make_things_happen_in_peoples_lives_riley_who_says_god_makes_things_happen_in_peoples_lives_but_only_at_special_times_or_sam_who_says_god_still_makes_things_happen_in_peoples_lives_now_2,
                          -do_you_ever_engage_in_any_behaviors_or_use_any_particular_objects_in_an_effort_to_bring_about_good_luck_or_avoid_bad_luck,
                          -do_you_ever_engage_in_any_other_superstitious_behaviors,
                          -now_we_will_have_a_debrief_of_the_study_this_study_is_investigating_how_children_view_gods_involvement_in_their_lives_as_they_develop_i_will_also_send_you_a_thank_you_email_with_a_detailed_debriefing_form_attached_as_a_thank_you_for_participating_in_this_study_we_can_enter_you_into_a_drawing_for_a_25_visa_giftcard_if_you_are_interested_what_email_would_you_like_us_to_use_for_the_drawing_do_you_have_any_other_questions,
                          -notes_ml)

# renaming columns ----
d_all <- d_all %>%
  rename(pid = subject_number, 
         childs_race_other = if_other_please_describe_12,
         edu_parent1 = highest_level_of_education_attained_by_parent_guardian_1, 
         edu_parent2 = highest_level_of_education_attained_by_parent_guardian_2, 
         exist_souls = what_about_souls_are_there_really_souls_in_the_world_do_souls_exist,
         exist_souls_sure = how_sure_are_you_30,
         exist_god = what_about_god_is_there_really_god_in_the_world_does_god_exist,
         exist_god_sure = how_sure_are_you_32,
         heard_of_god = x1_have_you_ever_heard_about_god,
         heard_of_god_where = x1a_where_have_you_heard_about_god,
         talk_about_god = x2_do_you_ever_talk_about_god,
         talk_about_god_who = x2a_who_do_you_talk_to_about_god,
         warmup_know_about_god = x3_tell_me_some_things_you_know_about_god,
         warmup_what_does_god_do = x4_what_does_god_do,
         frame_can_make_happen = x5_do_you_think_god_can_make_things_happen_here_on_earth,
         frame_on_earth = x6a_do_you_think_god_ever_makes_things_happen_here_on_earth,
         frame_on_earth_spec = if_yes_6b_what_kinds_of_things_does_god_make_happen_if_no_does_god_make_things_happen_anywhere,
         frame_peoples_lives = x7a_does_god_make_things_happen_in_peoples_lives,
         frame_peoples_lives_spec = if_yes_7b_what_kinds_of_things_does_god_make_happen_in_peoples_lives,
         domain_health_sick = x8a_ok_does_god_help_sick_people_get_better,
         domain_nature_sun = x8b_what_about_helping_the_sun_come_up,
         domain_nature_seasons = x8c_how_about_helping_make_the_seasons_change,
         domain_nature_plants = x8d_does_god_help_plants_grow,
         domain_social_love = x8e_what_about_helping_people_fall_in_love,
         domain_social_friends = x8f_how_about_helping_people_become_friends,
         domain_psych_dreams = x8g_what_about_giving_people_good_dreams,
         domain_achieve_find = x8h_ok_does_god_help_people_find_lost_things,
         domain_psych_decisions = x8i_how_about_helping_people_make_hard_decisions,
         domain_achieve_school = x8j_does_god_ever_help_people_do_well_in_school_or_at_their_job,
         domain_achieve_sports = x8k_what_about_helping_people_do_well_in_games_and_sports,
         domain_psych_happier = x8l_how_about_helping_people_feel_happier,
         domain_psych_brave = x8m_what_about_helping_people_feel_braver_about_scary_things,
         domain_health_safe = x8n_does_god_keep_people_safe_from_danger,
         frame_own_life = x9a_has_god_ever_made_something_happen_in_your_own_life,
         frame_own_life_freq = if_yes_9b_how_often_has_that_happened,
         frame_own_life_spec = if_yes_9c_can_you_tell_me_about_one_time_that_god_did_that_answer_ask_for_clarification_if_needed_e_g_if_they_say_when_my_grandma_was_sick_ask_tell_me_what_god_did_if_they_say_he_made_her_feel_better_perhaps_also_ask_how_did_he_do_that_are_there_other_times_until_they_stop,
         decide_god = x10a_does_god_decide_to_make_things_happen_on_his_own_does_he_only_make_things_happen_if_people_ask_him_to_or_does_he_do_both,
         decide_people_ask = x10b_if_say_people_ask_both_how_can_people_ask_god_to_help_make_things_happen,
         decide_god_only = x10c_if_say_god_decides_both_how_does_god_decide_what_things_to_help_make_happen,
         pray_meaning = x11_do_you_know_what_it_means_to_pray,
         pray_ever = x12_have_you_ever_prayed_to_god,
         pray_about = x12a_what_was_your_prayer_about,
         pray_for_something = x12b_if_yes_and_petitionary_jump_to_next_question_if_yes_but_not_petitionary_ask_the_following_question_have_you_ever_prayer_for_something,
         pray_got = x13_if_yes_get_what_prayed_for_did_you_get_what_you_prayed_for,
         pray_true_freq = x13_do_prayers_always_come_true_do_they_come_true_just_some_of_the_time_or_do_they_never_come_true,
         pray_how_happen = x14_does_anyone_of_anything_make_prayers_come_true_or_does_it_just_happen_on_its_own,
         pray_how_happen_spec = if_anything_what_makes_prayers_come_true_if_anyone_who_makes_prayers_come_true,
         relig_parent1 = religious_affiliation_of_parent_guardian_1,
         relig_parent2 = religious_affiliation_of_parent_guardian_2,
         relig_parent1_other = if_other_please_describe_87,
         relig_parent2_other = if_other_please_describe_89,
         tradition_attend_service_freq = how_often_does_your_child_attend_services_at_places_of_religious_worship,
         tradition_importance = how_important_is_it_for_you_to_raise_your_child_in_a_religious_tradition,
         school = what_type_of_school_does_your_child_attend,
         pray_parent_freq = how_often_do_you_pray_to_god,
         intervention_parent_god = please_select_the_description_that_most_closely_matches_your_idea_of_god,
         intervention_parent_own_life = do_you_believe_that_god_has_ever_intervened_in_your_own_life)

# exporting clean names data to csv
write.csv(d_all, file='../cielo/data/data_raw/cielo_clean2wrangle.csv', row.names=FALSE)

