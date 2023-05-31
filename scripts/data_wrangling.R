# loading packages
library(readr)
library(janitor)
library(tidyverse)
library(stringr)
library(dplyr)

# read in data ----
d_all <- read_csv("data/data_raw/cielo_clean2wrangle.csv")

# recoding demographic variables ----

# recoding control to be grouped into 2: control vs. no control
d_all <- d_all %>%
  mutate(
    control_cat = case_when(
      control %in% c("really agree", "agree", "kind of agree") ~ "no control",
      control %in% c("kind of disagree", "disagree", "really disagree") ~ "control",
      TRUE ~ control  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recoding childs ethinicity
d_all <- d_all %>%
  mutate(
    childs_ethnicity = case_when(
      childs_ethnicity %in% c("not hispanic or latino/a") ~ "not hispanic or latino",
      childs_ethnicity %in% c("hispanic or latino/a", "hispanic/latino") ~ "hispanic or latino",
      TRUE ~ childs_ethnicity  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recoding childs race
d_all <- d_all %>%
  mutate(
    childs_race = case_when(
      childs_race %in% c("white", "white ") ~ "white",
      childs_race %in% c("white/middle eastern or n. african", "white/asian", 
                         "asian,white", "native hawaiian or pacific islander,white", 
                         "black or african american,white",
                         "american indian or alaska native,white") ~ "two or more races",
      TRUE ~ childs_race  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode parent education 
d_all <- d_all %>%
  mutate(
    across(c(edu_parent1, edu_parent2), ~ case_when(
      . == "mom - bach. degree" | . == "dad - bach. degree" | 
        . == "bachelor's or 4-year degree" | . == "bachelor's of 4 year degree" 
      ~ "bachelor's or 4 year degree",
      . == "mom - associates degree" ~ "associate's degree",
      . == "dad - grad/prof. degree" | . == "mom - grad/prof. degree" | 
        . == "mom - graduate or professional degree" ~ "graduate or professional degree",
      . == "mom - some college" | . == "dad - some college" ~ "some college",
      . == "dad - high school diploma or equivalent" ~ "high school diploma or equivalent",
      TRUE ~ .
    ))
  )

# attending service
d_all <- d_all %>%
  mutate(
    tradition_attend_service_freq = case_when(
      tradition_attend_service_freq %in% c("few times a month") ~ "a few times a month",
      tradition_attend_service_freq %in% c("few times a year") ~ "a few times a year",
      TRUE ~ as.character(tradition_attend_service_freq)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recoding god variables ----

# recode heard of god
d_all <- d_all %>%
  mutate(
    heard_of_god = case_when(
      heard_of_god %in% c("not really") ~ "no",
      heard_of_god %in% c("yeah", "yes.", "many times", "i have many times", 
                          "yes, he lives in heaven and he loves everyone in the whole entire world", 
                          "yes, he is in the bible", "\"i have a bible, ya know? so yes.\"") ~ "yes",
      TRUE ~ as.character(heard_of_god)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode talk about god
d_all <- d_all %>%
  mutate(
    talk_about_god = case_when(
      talk_about_god %in% c("not really", "not normally, but if someone asked me about it",
                            "not necessarily, just to school", "not really.", "no, not really.",
                            "not too much, not really something they talk about"
      ) ~ "no",
      talk_about_god %in% c("yeah", "yes.", "yes, but not to my friends", "probably", 
                            "yes, he lives in heaven and he loves everyone in the whole entire world", 
                            "yes, he is in the bible", "\"i have a bible, ya know? so yes.\"",
                            "only if we talk about how you were born", 
                            "yes in religion class at school but not a lot in public",
                            "yes, alot, prays to god every god every night", 
                            "in scripture study but not at school",
                            "yes, not often", "sometimes", "sometimes, but not all the time",
                            "not so much, sometimes she asks questions",
                            "at summer school", "not that much.",
                            "sometimes, not too often, more in the household than anywhere else",
                            "not a ton, but a little. just sometimes. pretty rare",
                            "kind of shy to", "not a lot", "not much", "not that much",
                            "not very much, when i'm at my grandma's house we pray for the food we get",
                            "yes, brings up in conversation when it seems right",
                            "yes, sometimes", "a little bit", "yeah sometimes", "not on my own", 
                            "with dad, with mom, with sister, pray every night with mom", 
                            "yes, i talk to my dad a lot about god when we are going on walks.",
                            "yes, in front of my friend across the street",
                            "maybe", "kinda", "every once in a while.", "only at chapel",
                            "not about wat hes wearing and stuff.. not mean things about god.. we always pray before meals"
      ) ~ "yes",
      TRUE ~ as.character(talk_about_god)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode framework question 1
d_all <- d_all %>%
  mutate(
    frame_can_make_happen = case_when(
      frame_can_make_happen %in% c("not really", "no.") ~ "no",
      frame_can_make_happen %in% c("i don;t know", "not sure.", "i don't know", "#name?",
                                   "not exactly sure. in the bible yes, but not sure if god is even real, so not exactly sure if he does.") ~ "idk",
      frame_can_make_happen %in% c("yeah", "yes.", "yes, he may hold your hand if you ask him to",
                                   "sometimes", "yes, hes made it happen for me", "maybe, probably",
                                   "yes - can make people breathe so they don't die. he is the air and the whole world, god invented air conditioning", 
                                   "sort of", "maybe", "yes like tornados, hurricanes, thunder, and lightning",
                                   "i think so, yeah", "a little", "i think so", 
                                   "i mean some things are not controlled by god. the clouds in the sky make the weather. god can't cause earthquakes or hurricanes or waves to move. the moon controls the waves. i don't think he does anything that happens on earth.",
                                   "he can make anything he wants happen if he feels like its the right thing",
                                   "yes, like tornados, earthquakes, he can make you happy, he can make all osrts of natural disasters happen,",
                                   "not everything happens, only some things happen",
                                   "yeah if he wants to, but he mostly leaves it to us", 
                                   "yes, maybe not everything but most things",
                                   "i guess so, yeah", "-he brings the weather",
                                   "he could, but since he gave us free will he doesn't usually",
                                   "yes, i think so but i don't think so", "sure", "ya", "probably", "definitely.",
                                   "yes, otherwise how would he have created adam and eve",
                                   "yes, i think he made the winter storm in texas. he makes people and birds and animals and all of that.",
                                   "some things", "yea", "i do, i think so", "yes, he can make rain happen or snow.",
                                   "maybe, but more like person who created cells is out of picture, just helped make the beginning of the world and stopped"
                                   ) ~ "yes",
      TRUE ~ as.character(frame_can_make_happen)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode framework question 2
d_all <- d_all %>%
  mutate(
    frame_on_earth = case_when(
      frame_on_earth %in% c("not really", "god has made things happen but doesn't currently make things happen") ~ "no",
      frame_on_earth %in% c("i don't know", "not sure.") ~ "idk",
      frame_on_earth %in% c("yeah", "yeah but sometimes he does something by accident or he just gets mad",
                            "maybe", "yes like tornados, hurricanes, thunder, and lightning", "probably",
                            "sometimes", "i think so", "yes.", "maybe once", "sometimes.", "ya",
                            "a little sure. is god even real?", "sure", "he probably does",
                            "yes, some things", "yeah but sometims he does something by accident or he just gets mad",
                            "he sometimes makes things happen here on earth, so yes", "possibly",
                            "no, unless something crazy is happening, lie climate change"
                            ) ~ "yes",
      TRUE ~ as.character(frame_on_earth)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode framework question 3
d_all <- d_all %>%
  mutate(
    frame_peoples_lives = case_when(
      frame_peoples_lives %in% c("not really", "no, my life is totally random",
                                 "no, he doesn't like protect you from bullies, because he has to hold the earth, so he can't solve people's problems because then the air would dissapear",
                                 "no, unless something crazy is happening, lie climate change", 
                                 "not really", "not sure", "not really anything too big except life and death"
      ) ~ "no",
      frame_peoples_lives %in% c("i don't know", "not sure.", "i'm not sure", 
                                 "probably for some people if they believe in him and go to church. they talk about him and do prayers i think. if they believe in him, they will probably think he did something. i don't really know because it's up to the person what they believe.", 
                                 "\"shrugs\"",
                                 "i don't know. i don't even know if god is looking down at us every single second"
      ) ~ "idk",
      frame_peoples_lives %in% c("yes, sometimes", "sometimes", "yes probably", "think so",
                                 "yes a lot", "probably", "yeah", "thinks so", "yep", "a little bit",
                                 "mentally, yes. physically, yes", "yes.", "possible but not 100%", 
                                 "yes, i thik so", "sometimes.", "maybe", "probably.",
                                 "yes, he makes us be good, so basically yes", 
                                 "maybe- in the middle but yes **",
                                 "yes, a lot of people who are religious",
                                 "i think so, yes", "definitely", "yes, i think so"
      ) ~ "yes",
      TRUE ~ as.character(frame_peoples_lives)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode framework question 4
d_all <- d_all %>%
  mutate(
    frame_own_life = case_when(
      frame_own_life %in% c("not really", "no, my life is totally random", "no - i dont know",
                            "never", "i don't think so", "not that he can recall", "noo",
                            "no, not yet", "no.", "nope", "i don't think so.", "not yet/ no"
                            ) ~ "no",
      frame_own_life %in% c("i don't know", "idk; asked about light story and said \"idk if it was the light or...\"",
                            "does not know because she can't see him", "i'm not sure", "don't know", "if he has, not sure.",
                            "kind of, but not really sure.", "isn't sure", "can't say if they would know if it did"
                            ) ~ "idk",
      frame_own_life %in% c("kind of", "probably", "sometimes - parents or teachers usually make decisions",
                            "yes at certain times in life", "yes, like at good times", "when i was born", 
                            "you being born", "think so.", "yeah", "sometimes in school, with tests", 
                            "one time", "yes, but not sure if its a coincidence or not",
                            "sometimes.", "maybe", "yes, helped with them getting dogs", 
                            "probably.", "probably and he didnt notice it", "sometimes", "maybe? the blue plant that i saw.",
                            "i feel like yes", "he made him feel braver when he first met his friend",
                            "he has", "i think so", "yes, maybe", 
                            "probably, never really seen it but maybe made something happen"
      ) ~ "yes",
      TRUE ~ as.character(frame_own_life)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode domain plants
d_all <- d_all %>%
  mutate(domain_nature_plants = ifelse(domain_nature_plants == "no idea", NA, domain_nature_plants))

# recode domain love
d_all <- d_all %>%
  mutate(
    domain_social_love = case_when(
      domain_social_love %in% c("no idea", "doesnt know") ~ NA_character_,
      domain_social_love %in% c("never, not something he does") ~ "never",
      domain_social_love %in% c("only at special times; idk") ~ "only at special times",
      TRUE ~ as.character(domain_social_love)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode domain safe
d_all <- d_all %>%
  mutate(
    domain_health_safe = case_when(
      domain_health_safe %in% c("always, except if they are doing something bad") ~ "always",
      domain_health_safe %in% c("depends on the type of keeping people safe") ~ "only at special times",
      TRUE ~ as.character(domain_health_safe)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode intervention parent god
d_all <- d_all %>%
  mutate(
    intervention_parent_god = case_when(
      intervention_parent_god %in% c("do not believe in god", "i do not believe in god") ~ "i do not believe in god.",
      intervention_parent_god %in% c("interventionist god; sometimes involved", "interventionist god: sometimes involved"
                                     ) ~ "an interventionist god; one who is sometimes involved in the workings of modern life.",
      intervention_parent_god %in% c("non-interventionist god", "non-interventionist"
                                     ) ~ "a non-interventionist god; one who does not attempt to influence events in human life.",
      intervention_parent_god %in% c("interventionist god in biblical times"
                                     ) ~ "an interventionist god; one who is intimately involved in the workings of modern life.",
      intervention_parent_god %in% c("interventionist god; intimately involved"
                                     ) ~ "an interventionist god; one who is intimately involved in the workings of modern life.", 
      TRUE ~ as.character(intervention_parent_god)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recoding prayer variables ----
d_all <- d_all %>%
  mutate(
    pray_meaning = case_when(
      pray_meaning %in% c("talk to allah", "amen, something like that",
                          "to talk to god", "it means you love god and jesus", "to speak to god",
                          "yes - ask god for something", "thinks so.",
                          "yes, it means to take a second in your day to wish for something or be grateful for something you have",
                          "yes- it means that your full attention is on the person who you are praying for, you hope your prayer would help them. if you pray to god you pray that he helps you like get a good night's sleep or maybe you could pray to god and help you with a hard test you didn't study for or something like that.",
                          "yeah", "yes. it's like sending a message to him", "yes,", "it means kindness",
                          "yes. praying is important because when you go to sleep, you pray to god to keep you safe and for your dreams to not be scary.",
                          "yes, it's when you're telling god to watch over your family or do something that you have trouble with",
                          "talk to god, yes", "it means to tell god something",
                          "basically before you eat a meal. you thank him for food. i don't know why people do that because you bought it yourself. a prayer is just showing your appreciation.",
                          "kind of, normally means that you are thanking god/jesus, asking for something, or telling them something",
                          "it means really making a connection with god and just talking to him like he's an old friend",
                          "means that you're talking to god", "sort of", "yes.",
                          "yes, did it once before a thanksgiving dinner and did it at friend's house a couple of times",
                          "yes, where you put your hands otgether and close your eyes and pray to god or jesus for having great stuff happen and at end they say amen",
                          "yes, fairly sure", "yes, talking to god about things, or asking for him to do stuff",
                          "yes, give god a request.", "yes, it's a time when you have a conversation with god",
                          "yes- to ask god to erase your sins, help some one survive in a hard time, or to talk to god every once in a while",
                          "yes, to talk to god", "asking god for something to happen or thanking him",
                          "yes, if i pray, which is seldom, i whisper it in my head or you can do it down on bended knee, next to a window preferably, people pray in church but thats public, asking god for something you need or want to happen",
                          "kind of", "yes, but i don't do it.",
                          "praying means to talk to god about what has happened in your day and what you are thankful for in your life",
                          "to worship him and ask him for help, if your going through tough times",
                          "yes, it means to honor something", "it means that you're talking to jesus  or god",
                          "thank god, say thanks to him", "pray will be like saying things for food and living",
                          "yes, to thank god when you eat dinner and tell god how many things you've done for him and much you love and thank you",
                          "you might be praying for if you want something to happen and ask god for help.",
                          "close your eyes, clench your fists, and talk to god, just talk to him",
                          "not really, i guess it means praying for something you want because protecting something",
                          "yes it means that your thankful or that you need something. most people pray to god or other ancient gods that they celebrate sometimes like mexican and spanish day. they celebrate the day of the dead",
                          "yea, i think so", "it means you love god and jesus."
      ) ~ "yes",
      pray_meaning %in% c("not really", "not very much") ~ "no",
      pray_meaning %in% c("i don't know", "no, well i think i know. ask god to do something"
      ) ~ "idk",
      TRUE ~ as.character(pray_meaning)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# recode pray ever
d_all <- d_all %>%
  mutate(
    pray_ever = case_when(
      pray_ever %in% c("yeah", "yes - every night", "only once", "not very often, but yes",
                       "only at special times, like easter, when i sm forced to at church, when something horrible happens",
                       "yes, sometimes", "yes, we do it every night", "mmmhmm",
                       "i just like to not say anything (i think he means he closes his eyes when people pray but he doesn't pray himself)"
                       ) ~ "yes",
      pray_ever %in% c("no. idk if it counts") ~ "no",
      TRUE ~ as.character(pray_ever)  # Keep the original value if it doesn't match any of the conditions
    )
  )

# deleting new columns
d_all <- d_all %>%
  select(-x100, -x101)

# creating new variables ----

# framework intervention task score
d_all$frame_can_make_happen_num <- dplyr::recode(d_all$frame_can_make_happen, 'no' = 1, 'yes' = 2, 'idk' = 0)
d_all$frame_on_earth_num <- dplyr::recode(d_all$frame_on_earth, 'no' = 1, 'yes' = 2, 'idk' = 0)
d_all$frame_peoples_lives_num <- dplyr::recode(d_all$frame_peoples_lives, 'no' = 1, 'yes' = 2, 'idk' = 0)
d_all$frame_own_life_num <- dplyr::recode(d_all$frame_own_life, 'no' = 1, 'yes' = 2, 'idk' = 0)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    framework_score = mean(c(frame_can_make_happen_num, frame_on_earth_num, frame_peoples_lives_num, frame_own_life_num))
  )

# nature domain
d_all$domain_nature_plants_num <- dplyr::recode(d_all$domain_nature_plants, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_nature_seasons_num <- dplyr::recode(d_all$domain_nature_seasons, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_nature_sun_num <- dplyr::recode(d_all$domain_nature_sun, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    domain_nature = mean(c(domain_nature_plants_num, domain_nature_seasons_num, domain_nature_sun_num))
  )

# psych domain
d_all$domain_psych_brave_num <- dplyr::recode(d_all$domain_psych_brave, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_psych_decisions_num <- dplyr::recode(d_all$domain_psych_decisions, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_psych_dreams_num <- dplyr::recode(d_all$domain_psych_dreams, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_psych_happier_num <- dplyr::recode(d_all$domain_psych_happier, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    domain_psych = mean(c(domain_psych_brave_num, domain_psych_decisions_num, domain_psych_dreams_num, domain_psych_happier_num))
  )

# social domain
d_all$domain_social_friends_num <- dplyr::recode(d_all$domain_social_friends, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_social_love_num <- dplyr::recode(d_all$domain_social_love, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    domain_social = mean(c(domain_social_friends_num, domain_social_love_num))
  )

# achieve domain
d_all$domain_achieve_find_num <- dplyr::recode(d_all$domain_achieve_find, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_achieve_school_num <- dplyr::recode(d_all$domain_achieve_school, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_achieve_sports_num <- dplyr::recode(d_all$domain_achieve_sports, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    domain_achieve = mean(c(domain_achieve_find_num, domain_achieve_school_num,domain_achieve_sports_num))
  )

# health domain
d_all$domain_health_safe_num <- dplyr::recode(d_all$domain_health_safe, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all$domain_health_sick_num <- dplyr::recode(d_all$domain_health_sick, 'never' = 1, 'only at special times' = 2, 'pretty often' = 3, 'always' = 4)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    domain_health = mean(c(domain_health_safe_num, domain_health_sick_num))
  )

# domain intervention task score
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    domain_score = mean(c(domain_achieve, domain_health, domain_psych, domain_social, domain_nature))
  )

# family religiosity
d_all$tradition_attend_service_freq_num <- dplyr::recode(d_all$tradition_attend_service_freq, 'never' = 1, 'less than once a year' = 2, 'a few times a year' = 3, 'a few times a month' = 4,
                                                         'a few times a week' = 5, 'daily' = 6)
d_all$tradition_importance_num <- dplyr::recode(d_all$tradition_importance, 'not at all important' = 1, 'minimally important' = 2, 'moderately important' = 3, 'very important' = 4)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    fam_religiosity = mean(c(tradition_attend_service_freq_num, tradition_importance_num))
  )

# parent religious beliefs
d_all$intervention_parent_god_num <- dplyr::recode(d_all$intervention_parent_god, 
                                                   'i do not believe in god.' = 1, 
                                                   'a non-interventionist god; one who does not attempt to influence events in human life.' = 2, 
                                                   'a god who used to intervene in historical or biblical times, but is not particularly involved in the workings of modern life.' = 3, 
                                                   'an interventionist god; one who is sometimes involved in the workings of modern life.' = 4,
                                                   'an interventionist god; one who is intimately involved in the workings of modern life.' = 5)
d_all$intervention_parent_own_life_num <- dplyr::recode(d_all$intervention_parent_own_life, 
                                                   'absolutely not' = 1, 
                                                   'probably not' = 2, 
                                                   'unsure' = 0, 
                                                   'probably yes' = 3,
                                                   'absolutely yes' = 4)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    parent_beliefs = mean(c(intervention_parent_god_num, intervention_parent_own_life_num))
  )

# family influence (family religiosity and parent belief)
d_all <- d_all %>%
  rowwise() %>%
  mutate(
    fam_influence = mean(c(intervention_parent_god_num, intervention_parent_own_life_num, tradition_attend_service_freq_num, tradition_importance_num))
  )

# exporting clean names data to csv
write.csv(d_all, file='../cielo/data/data_clean/cielo_data.csv', row.names=FALSE)
