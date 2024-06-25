# santa god analyses

library(tidyverse)
library(janitor) # cleans variable names
library(readxl) # read excel file
library(vcd) # for phi test

# loading data ----
d_cielo <- read_excel("data/data_raw/cielo_qualtrics_raw.xlsx")
d_cielo2 <- read_csv("data/cielo2.csv")
d_alexa_child <- read_csv("data/alexa_child.csv")
d_alexa_par <- read_csv("data/alexa_parent.csv")

d_cielo <- d_cielo %>%
  clean_names() %>%
  select(pid, demo_age, demo_gender, rp_god, rp_god_c, rp_tooth, rp_tooth_c, rp_santa, rp_santa_c, demo_relig_p1, demo_relig_p1_other,
         famrelig_1, famrelig_2) %>%
  slice(-c(1)) %>%
  rename(age = demo_age,
         gender = demo_gender,
         real_god = rp_god,
         real_god_sure = rp_god_c,
         real_santa = rp_santa,
         real_santa_sure = rp_santa_c,
         attend_church = famrelig_1,
         relig_tradition = famrelig_2,
         real_tooth = rp_tooth,
         real_tooth_sure = rp_tooth_c) 


d_cielo2 <- d_cielo2 %>%
  clean_names() %>%
  select(pid, age, gender, sort_god, sort_god_sure, sort_tooth, sort_tooth_sure, sort_santa, sort_santa_sure, contains("demo_rel_pg1"),
         demo_attend, demo_important) %>%
  slice(-c(1:2)) %>%
  filter(pid != "Test",
         pid != "TEST",
         pid != "test") %>%
  rename(real_god = sort_god,
         real_god_sure = sort_god_sure,
         real_santa = sort_santa,
         real_santa_sure = sort_santa_sure,
         attend_church = demo_attend,
         relig_tradition = demo_important,
         real_tooth = sort_tooth,
         real_tooth_sure = sort_tooth_sure) %>%
  mutate(demo_relig_p1 = unite(across(starts_with("demo_rel_pg1")), "demo_relig_p1", sep = "", na.rm = TRUE))

d_cielo2 <- d_cielo2 %>%
  select(-starts_with("demo_rel_pg1")) %>%
  rename(`demo_relig_p1$demo_relig_p1` = demo_relig_p1)

d_alexa_child <- d_alexa_child %>% 
  clean_names() %>%
  select(pid, age, real_god, real_santa) %>%
  slice(-c(1:2)) %>%
  filter(pid != "Test",
         pid != "TEST",
         pid != "test",
         pid != "Test14",
         pid != "PRACTICE")

d_alexa_par <- d_alexa_par %>% 
  clean_names() %>%
  select(id, gender, q53, q37, q38, q41) %>%
  slice(-c(1:2)) %>%
  filter(id != "Test",
         id != "TEST",
         id != "test",
         id != "Test14",
         id != "PRACTICE",
         id != "4/28/23",
         id != "Test (12)") %>%
  rename(pid = id,
         demo_relig_p1 = q53,
         attend_church = q37,
         relig_tradition = q38,
         santa_ritual = q41) 

d_alexa <- left_join(d_alexa_par, d_alexa_child, by = "pid") #%>%
  #select(188:199, everything()) 

d_all <- bind_rows(d_cielo, d_cielo2, d_alexa)

d_all <- d_all %>%
mutate(real_god = case_when(
  real_god == "Yes" ~ "Real",
  real_god == "No" ~ "Pretend",
  TRUE ~ real_god  # Keep other values unchanged
))

d_all <- d_all %>%
  mutate(real_santa = case_when(
    real_santa == "Yes" ~ "Real",
    real_santa == "No" ~ "Pretend",
    TRUE ~ real_santa  # Keep other values unchanged
  ))

d_all <- d_all %>%
  mutate(age = case_when(
    age == "-please select-" ~ "NA",
    TRUE ~ age  # Keep other values unchanged
  ))

# ordering variables for analyses ----

d_all <- d_all %>%
  mutate(real_santa = fct_relevel(real_santa, "Pretend", "Real")) %>%
  mutate(real_god = fct_relevel(real_god, "Pretend", "Real")) %>%
  mutate(attend_church = fct_relevel(attend_church, "Never",
                                     "Less than once a year",
                                     "A few times a year", 
                                     "A few times a month",
                                     "A few times a week", 
                                     "Daily")) %>%
  mutate(relig_tradition = fct_relevel(relig_tradition, "Not at all important",
                                     "Minimally important",
                                     "Moderately important", 
                                     "Very important")) %>%
  mutate(santa_ritual = fct_relevel(santa_ritual, "Not at all",
                                     "A lot",
                                     "A little bit")) %>%
  mutate(age = fct_relevel(age, "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) %>%
  mutate(real_god_sure = fct_relevel(real_god_sure, "A little sure", "Very sure"))

# descriptives ----
# overall belief in god
table(d_all$real_god) # 74.52% god is real (351/471)
test = d_all %>%
  group_by(age) %>%
  count(d_all$real_god)

print(test, n = 50)
table(d_all$real_santa) # 57.20% santa is real (246/430)

# correlations ----

# create a table of counts for the two binary variables
table_scg <- table(d_all$real_santa, d_all$real_god)
assocstats(table_scg)
# The chi-squared test indicates a significant association between the two binary variables.
# The Phi coefficient of 0.165 suggests a weak positive association.

table_gtf <- table(d_all$real_tooth, d_all$real_god)
assocstats(table_gtf)
# The Phi coefficient of 0.158 suggests a weak positive association.

table_scg2 <- table(d_all$real_santa, d_all$real_god_sure)
assocstats(table_scg2)

d_all %>%
  group_by(real_god) %>%
  count(d_all$real_god_sure)

d_all %>%
  group_by(real_santa) %>%
  count(d_all$real_santa_sure)

table(d_all$real_santa_sure)

# correlation by santa belief and god belief by age! young, middle, old
d_young <- subset(d_all, age %in% c(4, 5, 6))
d_young <- d_young %>%
  select(real_santa, real_god) %>%
  na.omit()
table_young <- table(d_young$real_santa, d_young$real_god)
assocstats(table_young)

d_middle <- subset(d_all, age %in% c(7, 8, 9))
d_middle <- d_middle %>%
  select(real_santa, real_god) %>%
  na.omit()
table_middle <- table(d_middle$real_santa, d_middle$real_god)
assocstats(table_middle)

d_old <- subset(d_all, age %in% c(10, 11, 12, 13))
d_old <- d_old %>%
  select(real_santa, real_god) %>%
  na.omit()
table_old <- table(d_old$real_santa, d_old$real_god)
assocstats(table_old)

# regressions ----
model_age <- glm(real_santa ~ age, data = d_all, family = "binomial")
summary(model_age)

model_ritual <- glm(real_santa ~ santa_ritual, data = d_all, family = "binomial")
summary(model_ritual)

model_church <- glm(real_santa ~ attend_church, data = d_all, family = "binomial")
summary(model_church)

model_relig <- glm(real_santa ~ relig_tradition, data = d_all, family = "binomial")
summary(model_relig)

# when kids find out that santa isn't real, do they then start questioning god?
# are kids less certain about santa?

# figs ----
ggplot(d_all, aes(x = real_god, fill = real_god)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_grid(. ~ real_santa) +
  theme_minimal()

# god, santa, age
d_all %>%
  select(age, real_god, real_santa) %>%
  filter(real_santa == "Pretend",
         age != "NA") %>%
  na.omit() %>%
  ggplot(aes(x = age, fill = real_god)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#3e7951", "#c94c4c"), name = "God") +  
  labs(title = "Santa Pretend",
       x = "Age",
       y = "Count") +
  theme_minimal() 

d_all %>%
  select(age, real_god, real_santa) %>%
  filter(real_santa == "Real") %>%
  na.omit() %>%
  ggplot(aes(x = age, fill = real_god)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#3e7951", "#c94c4c"), name = "God") +  
  labs(title = "Santa Real",
       x = "Age",
       y = "Count") +
  theme_minimal() 

d_corr %>%
  select(age_group, real_god, real_santa) %>%
  filter(real_santa == "Pretend",
         age_group != "NA") %>%
  na.omit() %>%
  ggplot(aes(x = age_group, fill = real_god)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#3e7951", "#c94c4c"), name = "God") +  
  labs(title = "Santa Pretend",
       x = "Age",
       y = "Count") +
  theme_minimal() 

d_corr %>%
  select(age_group, real_god, real_santa) %>%
  filter(real_santa == "Real") %>%
  na.omit() %>%
  ggplot(aes(x = age_group, fill = real_god)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#3e7951", "#c94c4c"), name = "God") +  
  labs(title = "Santa Real",
       x = "Age",
       y = "Count") +
  theme_minimal() 

# density plots
d_all %>%
  select(age, real_god, real_santa) %>%
  filter(real_santa == "Pretend",
         age != "NA") %>%
  na.omit() %>%
  group_by(age, real_god) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = age, y = count, color = real_god, group = real_god)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#3e7951", "#c94c4c"), name = "God") +
  labs(title = "Santa is Pretend",
       x = "Age",
       y = "Count") +
  theme_minimal()

d_all %>%
  select(age, real_god, real_santa) %>%
  filter(real_santa == "Real",
         age != "NA") %>%
  na.omit() %>%
  group_by(age, real_god) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = age, y = count, color = real_god, group = real_god)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#3e7951", "#c94c4c"), name = "God") +
  labs(title = "Santa is Real",
       x = "Age",
       y = "Count") +
  theme_minimal()

d_line <- d_all %>%
  select(age, real_god, real_santa) %>%
  na.omit() %>%
  mutate(real_god = recode(real_god,
                                "Real" = "God is real",
                                "Pretend" = "God is pretend"),
         real_santa = recode(real_santa,
                             "Real" = "Santa is real",
                             "Pretend" = "Santa is pretend")) %>%
  unite(combined_belief, real_god, real_santa, sep = ", ", remove = FALSE)

d_line %>%
  group_by(age, combined_belief) %>%
  filter(age != "NA") %>%
  summarise(count = n()) %>%
  ggplot(aes(x = age, y = count, color = combined_belief, group = combined_belief)) +
  #geom_line(size = 1) +
  geom_smooth(size = 1, se = FALSE) +
  scale_color_manual(values = c("#3e7951", "#abc5b5", "#c94c4c", "#891314"), name = "Beliefs") +
  labs(title = "Santa, God, Age",
       x = "Age",
       y = "Count") +
  theme_minimal()

fig_scg <- as.data.frame(as.table(table_scg))

# heatmap
fig_scg %>%
  ggplot(aes(x = factor(Var1, levels = rev(levels(factor(Var1)))), y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = .5, size = 20, color = "white") +  # Add text labels
  labs(title = "Santa and God Heatmap",
       x = "Santa",
       y = "God") +
  scale_fill_gradient(low = "#e0e2c2", high = "#3e7951", guide = "none") +
  theme_minimal()

# HL add correlation by santa belief and god belief by age! young, middle, old
#d_corr <- d_all %>%
#  select(age, real_god, real_santa) %>%
#  na.omit() %>%
#  filter(age != "NA") %>%
#  mutate(age_group = recode(age,
#                                `4` = "Young",
#                                `5` = "Young",
#                                `6` = "Young",
#                                `7` = "Middle",
#                                `8` = "Middle",
#                                `9` = "Middle",
#                                `10` = "Old",
#                                `11` = "Old",
#                                `12` = "Old",
#                                `13` = "Old")) 
#
#d_corr %>%
#  ggplot(aes(x = real_god, y = real_santa, group = age_group, color = age_group)) +
#  geom_smooth() +
#  labs(title = "Santa and God Heatmap",
#       x = "Santa",
#       y = "God") +
#  #scale_fill_gradient(low = "#e0e2c2", high = "#3e7951", guide = "none") +
#  theme_minimal()
#