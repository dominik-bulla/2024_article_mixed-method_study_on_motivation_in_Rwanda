### environment ----------------------- ----------------------- ----------------------- -----------------------

setwd("C:/Users/domin/GitHub/2024_article_mixed-method_study_on_motivation_in_Rwanda")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



### packages ----------------------- ----------------------- ----------------------- -----------------------

library(readxl)
library(dplyr)
library(psych)
library(irr)
library(tidyr)



### local data ----------------------- ----------------------- ----------------------- -----------------------

motivation <- read.csv("01 raw data/motivation_in_Rwanda_20250124_v01.csv")



# compile stats on questionnaires/ statememts --------------------------------- --------------------------------- --------------------------------- --------------------------------- 
# Here are the total number of questionnaires, and statements before they were coded
# We also extract segments that were non-informative as well as questionnaires that contained both 
# motivation and amotivation segments.

length(unique(motivation$sno))        # the total number of statements that were analysed

noninformative <- motivation %>%
  filter(coding_round_1_final_coding == "no" | coding_round_2_final_coding == "no") %>%
  select(sno_student, 
         school,
         sno,
         age, gender, school_grade,
         statement_Kinyarwanda, statement_English)



# compile stats on questionnaires/ statememts --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

questionnaires <- motivation %>%
  filter(!duplicated(sno_student)) %>%
  group_by(questionnaire_type) %>%
  summarise(resondents = n()) %>%
  rename("questionnaire" = questionnaire_type)
total <- motivation %>%
  filter(!duplicated(sno_student)) %>%
  summarise(resondents = n()) %>%
  mutate(questionnaire = "total")
questionnaires <- rbind(questionnaires, total)

mismatch <- motivation %>%
  group_by(sno_student, coding_round_2_final_coding) %>%
  summarise(n = n()) %>%
  mutate(both = row_number()) %>%
  ungroup()
double <- c(mismatch$sno_student[mismatch$both == 2])
mismatch <- mismatch %>%
  filter(sno_student %in% double) %>%
  select(-c(n, both))
mismatch <- merge(mismatch, motivation[!duplicated(motivation$sno_student), c("sno_student", "questionnaire_type")], by = "sno_student", all.x = TRUE) 
mismatch <- mismatch %>%
  filter(questionnaire_type != coding_round_2_final_coding) %>%
  group_by(questionnaire_type) %>%
  summarise(`resondents both` = n()) %>%
  rename("questionnaire" = questionnaire_type)
total <- data.frame(questionnaire = "total", `resondents both` = sum(mismatch$`resondents both`))
colnames(total) <- colnames(mismatch)
mismatch <- rbind(mismatch, total)
questionnaires <- merge(questionnaires, mismatch, by = "questionnaire")
rm(mismatch, double)

excluded <- motivation %>%
  group_by(sno_student) %>%
  summarise(n = n())
excluded1 <- motivation %>%
  filter(coding_round_1_final_coding == "no") %>%
  group_by(sno_student) %>%
  summarise(n1 = n())
excluded <- merge(excluded, excluded1, all.x = TRUE) 
excluded <- excluded %>%
  mutate(n1 = ifelse(is.na(n1), 0, n1)) %>%
  filter(n == n1) %>%
  select(sno_student)
excluded_sno <- excluded
excluded <- merge(excluded, motivation[!duplicated(motivation$sno_student), c("sno_student", "questionnaire_type")], by = "sno_student", all.x = TRUE) 
excluded <- excluded %>%
  group_by(questionnaire_type) %>%
  summarise(`resondents excluded` = n()) %>%
  rename("questionnaire" = questionnaire_type)
total <- data.frame(questionnaire = "total", `resondents both` = sum(excluded$`resondents excluded`))
colnames(total) <- colnames(excluded)
excluded <- rbind(excluded, total)
questionnaires <- merge(questionnaires, excluded, by = "questionnaire")
rm(excluded, excluded1, excluded2)

questionnaires_final <- motivation %>%
  filter(!is.na(coding_round_2_final_coding)) %>%
  group_by(sno_student, coding_round_2_final_coding) %>%
  summarise(resondents = n()) %>%
  mutate(resondents = ifelse(resondents > 1, 1, resondents)) %>%
  rename("questionnaire" = coding_round_2_final_coding) %>%
  group_by(questionnaire) %>%
  summarise(`respondents final` = sum(resondents))
total <- data.frame(questionnaire = "total", `respondents final` = sum(questionnaires_final$`resondents final`))
colnames(total) <- colnames(questionnaires_final)
questionnaires_final <- rbind(questionnaires_final, total)
questionnaires <- merge(questionnaires, questionnaires_final, by = "questionnaire")
rm(questionnaires_final)

statements <- motivation %>%                                  ### this is the number of unique statements
  filter(coding_round_1_final_coding == "yes") %>%
  group_by(coding_round_2_final_coding) %>%
  summarise(statements = n())%>%
  rename("questionnaire" = coding_round_2_final_coding,
         "statements final" = statements)
total <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  summarise(statements = n()) %>%
  mutate(questionnaire = "total") %>%
  rename("statements final" = statements)
statements <- rbind(statements, total)

questionnaires <- merge(questionnaires, statements, by = "questionnaire")
rm(statements, total)  



# compile student stats (all) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

students_all <- motivation %>%
  filter(!duplicated(sno_student))
length(unique(students_all$sno_student))

school <- students_all %>%
  filter(!duplicated(sno_student)) %>%
  group_by(school) %>%
  summarise(all = n())
school

age <- students_all %>%
  summarize(round(mean(age), 2),
            round(sd(age), 2))

students_all %>%
  group_by(school) %>% 
  summarize(mean(age),
            sd(age))

students_all %>%
  group_by(gender) %>%
  summarise(n = round(n() / length(unique(motivation$sno_student)), 4) * 100)
  
school_gender <- students_all %>%
  group_by(school, gender) %>%
  summarise(ng = n())
school_gender <- merge(school_gender, school)
school_gender <- school_gender %>%
  mutate(proportion = round(ng / all, 4) * 100) %>%
  select(-c(all, ng))
school_gender



# coding frequencies by statements (table 2; motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 




statement_motivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno) %>%
  summarise(Expectancies = sum(expectancies),
            Value = sum(value),
            Cost = sum(costs),
            Goals = sum(goals),
            `Other factors`  = sum(other.factors),
            Consequence = sum(consequence)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Categories",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Categories) %>%
  summarise(`Statements (M; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Categories = factor(Categories, levels = c("Expectancies", "Value", "Cost", "Goals", "Other factors", "Consequence")), 
         Facet = "",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Statements (M; in %)`) %>%
  arrange(Categories) 

statement_value <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno) %>%
  summarise(intrinsic = sum(value.intrinsic),
            utility = sum(value.utility)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Statements (M; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("intrinsic", "utility")),
         Categories = "Value",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Statements (M; in %)`) %>%
  arrange(Facet) 

statement_utility <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno) %>%
  summarise('daily life' = sum(value.utility.daily.life),
            'general' = sum(value.utility.general),
            'learning' = sum(value.utility.learning.utility),
            'school' = sum(value.utility.school.utility),
            'social' = sum(value.utility.social.utility),
            'useless' = sum(value.utility.useless),
            'other' = sum(value.utility.other)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Sub facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(`Sub facet`) %>%
  summarise(`Statements (M; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(`Sub facet` = factor(`Sub facet`, levels = c("daily life", "general", "learning", "school", "social", "useless", "other")),
         Categories = "Value",
         `Facet` = "utility") %>%
  select(Categories, Facet, `Sub facet`, `Statements (M; in %)`) %>%
  arrange(`Sub facet`) 

statement_value <- rbind(statement_value,
                         statement_utility)

statement_cost <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno) %>%
  summarise(effort = sum(costs.effort),
            emotion = sum(costs.emotional),
            opportunity = sum(costs.opportunity),
            outside = sum(costs.outside)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Statements (M; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("effort", "emotion", "opportunity", "outside")),
         Categories = "Cost",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Statements (M; in %)`) %>%
  arrange(Facet) 

statement_motivation <- rbind(statement_motivation, 
                              statement_value,
                              statement_cost) %>%
  arrange(Categories) %>% 
  bind_rows(data.frame(Categories = "n",
                       Facet = "",
                       `Sub facet` = "",
                       `Statements (M; in %)` = questionnaires$`statements final`[questionnaires$questionnaire == "motivation"], check.names = FALSE))
# The facets and sub facets do not sum up to the total accociated with the correspoinding category. 
# The reason is each statement could have been coded using different facets and sub facets. 



# coding frequencies by statements (table 2; amotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

statement_amotivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno) %>%
  summarise(Expectancies = sum(expectancies),
            Value = sum(value),
            Cost = sum(costs),
            Goals = sum(goals),
            `Other factors`  = sum(other.factors),
            Consequence = sum(consequence)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Categories",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Categories) %>%
  summarise(`Statements (A; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(Categories = factor(Categories, levels = c("Expectancies", "Value", "Cost", "Goals", "Other factors", "Consequence")), 
         Facet = "",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Statements (A; in %)`) %>%
  arrange(Categories) 

statement_value <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno) %>%
  summarise(intrinsic = sum(value.intrinsic),
            utility = sum(value.utility)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Statements (A; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("intrinsic", "utility")),
         Categories = "Value",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Statements (A; in %)`) %>%
  arrange(Facet) 

statement_utility <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno) %>%
  summarise('daily life' = sum(value.utility.daily.life),
            'general' = sum(value.utility.general),
            'learning' = sum(value.utility.learning.utility),
            'school' = sum(value.utility.school.utility),
            'social' = sum(value.utility.social.utility),
            'useless' = sum(value.utility.useless),
            'other' = sum(value.utility.other)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Sub facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(`Sub facet`) %>%
  summarise(`Statements (A; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(`Sub facet` = factor(`Sub facet`, levels = c("daily life", "general", "learning", "school", "social", "useless", "other")),
         Categories = "Value",
         `Facet` = "utility") %>%
  select(Categories, Facet, `Sub facet`, `Statements (A; in %)`) %>%
  arrange(`Sub facet`) 

statement_value <- rbind(statement_value,
                         statement_utility)

statement_cost <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno) %>%
  summarise(effort = sum(costs.effort),
            emotion = sum(costs.emotional),
            opportunity = sum(costs.opportunity),
            outside = sum(costs.outside)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Statements (A; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("effort", "emotion", "opportunity", "outside")),
         Categories = "Cost",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Statements (A; in %)`) %>%
  arrange(Facet) 

statement_amotivation <- rbind(statement_amotivation, 
                              statement_value,
                              statement_cost) %>%
  arrange(Categories) %>%
  bind_rows(data.frame(Categories = "n",
                                             Facet = "",
                                             `Sub facet` = "",
                                             `Statements (A; in %)` = questionnaires$`statements final`[questionnaires$questionnaire == "demotivation"], check.names = FALSE))
# The facets and sub facets do not sum up to the total accociated with the correspoinding category. 
# The reason is each statement could have been coded using different facets and sub facets. 




# coding frequencies by students (table 2; motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

respondents_motivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno_student) %>%
  summarise(Expectancies = sum(expectancies),
            Value = sum(value),
            Cost = sum(costs),
            Goals = sum(goals),
            `Other factors`  = sum(other.factors),
            Consequence = sum(consequence)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Categories",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Categories) %>%
  summarise(`Students (M; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Categories = factor(Categories, levels = c("Expectancies", "Value", "Cost", "Goals", "Other factors", "Consequence")), 
         Facet = "",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Students (M; in %)`) %>%
  arrange(Categories)

respondents_value <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno_student) %>%
  summarise(intrinsic = sum(value.intrinsic),
            utility = sum(value.utility)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Students (M; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("intrinsic", "utility")),
         Categories = "Value",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Students (M; in %)`) %>%
  arrange(Facet)

respondents_utility <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno_student) %>%
  summarise('daily life' = sum(value.utility.daily.life),
            'general' = sum(value.utility.general),
            'learning' = sum(value.utility.learning.utility),
            'school' = sum(value.utility.school.utility),
            'social' = sum(value.utility.social.utility),
            'useless' = sum(value.utility.useless),
            'other' = sum(value.utility.other)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Sub facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(`Sub facet`) %>%
  summarise(`Students (M; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(`Sub facet` = factor(`Sub facet`, levels = c("daily life", "general", "learning", "school", "social", "useless", "other")),
         Categories = "Value",
         `Facet` = "utility") %>%
  select(Categories, Facet, `Sub facet`, `Students (M; in %)`) %>%
  arrange(Facet, `Sub facet`)

respondents_value <- rbind(respondents_value,
                           respondents_utility)

respondents_cost <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno_student) %>%
  summarise(effort = sum(costs.effort),
            emotion = sum(costs.emotional),
            opportunity = sum(costs.opportunity),
            outside = sum(costs.outside)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Students (M; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("effort", "emotion", "opportunity", "outside")),
         Categories = "Cost",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Students (M; in %)`) %>%
  arrange(Facet)

respondents_motivation <- rbind(respondents_motivation, 
                                respondents_value,
                                respondents_cost) %>%
  arrange(Categories) %>% 
  bind_rows(data.frame(Categories = "n",
                       Facet = "",
                       `Sub facet` = "",
                       `Students (M; in %)` = questionnaires$`respondents final`[questionnaires$questionnaire == "motivation"], check.names = FALSE))
# The facets and sub facets do not sum up to the total accociated with the correspoinding category. 
# The reason is each statement could have been coded using different facets and sub facets. 

coding_motivation <- merge(statement_motivation, respondents_motivation, by = c("Categories", "Facet", "Sub facet")) %>%
  mutate(Categories = factor(Categories, levels = c("Expectancies", "Value", "Cost", "Goals", "Other factors", "Consequence", "n")),
         `Sub facet` = factor(`Sub facet`, levels = c("", "daily life", "general", "learning", "school", "social", "useless", "other"))) %>%
  arrange(Categories, `Sub facet`)



# coding frequencies by students (table 2; demotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

respondents_amotivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno_student) %>%
  summarise(Expectancies = sum(expectancies),
            Value = sum(value),
            Cost = sum(costs),
            Goals = sum(goals),
            `Other factors`  = sum(other.factors),
            Consequence = sum(consequence)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Categories",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Categories) %>%
  summarise(`Students (A; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(Categories = factor(Categories, levels = c("Expectancies", "Value", "Cost", "Goals", "Other factors", "Consequence")), 
         Facet = "",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Students (A; in %)`) %>%
  arrange(Categories)

respondents_value <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno_student) %>%
  summarise(intrinsic = sum(value.intrinsic),
            utility = sum(value.utility)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Students (A; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("intrinsic", "utility")),
         Categories = "Value",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Students (A; in %)`) %>%
  arrange(Facet)

respondents_utility <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno_student) %>%
  summarise('daily life' = sum(value.utility.daily.life),
            'general' = sum(value.utility.general),
            'learning' = sum(value.utility.learning.utility),
            'school' = sum(value.utility.school.utility),
            'social' = sum(value.utility.social.utility),
            'useless' = sum(value.utility.useless),
            'other' = sum(value.utility.other)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Sub facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(`Sub facet`) %>%
  summarise(`Students (A; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(`Sub facet` = factor(`Sub facet`, levels = c("daily life", "general", "learning", "school", "social", "useless", "other")),
         Categories = "Value",
         `Facet` = "utility") %>%
  select(Categories, Facet, `Sub facet`, `Students (A; in %)`) %>%
  arrange(Facet)

respondents_value <- rbind(respondents_value,
                           respondents_utility)

respondents_cost <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno_student) %>%
  summarise(effort = sum(costs.effort),
            emotion = sum(costs.emotional),
            opportunity = sum(costs.opportunity),
            outside = sum(costs.outside)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(Facet) %>%
  summarise(`Students (A; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  mutate(Facet = factor(Facet, levels = c("effort", "emotion", "opportunity", "outside")),
         Categories = "Cost",
         `Sub facet` = "") %>%
  select(Categories, Facet, `Sub facet`, `Students (A; in %)`) %>%
  arrange(Facet)

respondents_amotivation <- rbind(respondents_amotivation, 
                                respondents_value,
                                respondents_cost) %>%
  arrange(Categories) %>% 
  bind_rows(data.frame(Categories = "n",
                       Facet = "",
                       `Sub facet` = "",
                       `Students (A; in %)` = questionnaires$`respondents final`[questionnaires$questionnaire == "demotivation"], check.names = FALSE))
# The facets and sub facets do not sum up to the total accociated with the correspoinding category. 
# The reason is each statement could have been coded using different facets and sub facets. 

coding_amotivation <- merge(statement_amotivation, respondents_amotivation, by = c("Categories", "Facet", "Sub facet")) %>%
  mutate(Categories = factor(Categories, levels = c("Expectancies", "Value", "Cost", "Goals", "Other factors", "Consequence", "n")),
         `Sub facet` = factor(`Sub facet`, levels = c("", "daily life", "general", "learning", "school", "social", "useless", "other"))) %>%
  arrange(Categories, `Sub facet`)



# merging coding results (table 2; (a)motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

coding_results <- merge(coding_motivation, coding_amotivation, by = c("Categories", "Facet", "Sub facet"), all = TRUE) %>%
  mutate(Categories = factor(Categories, levels = c("Expectancies", "Value", "Cost", "Goals", "Other factors", "Consequence", "n")),
         `Sub facet` = factor(`Sub facet`, levels = c("", "daily life", "general", "learning", "school", "social", "useless", "other"))) %>%
  mutate(across(starts_with("St"), ~ ifelse(is.na(.), 0, .))) %>%
  arrange(Categories, `Sub facet`)
rm(coding_motivation, coding_amotivation,
   statement_motivation, statement_amotivation,
   statement_value, statement_utility, statement_cost,
   respondents_motivation, respondents_amotivation,
   respondents_value, respondents_utility, respondents_cost)



# Create sub tables merging coding results (table 2; (a)motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

table_main <- coding_results %>%
  filter(Facet == "") %>%
  filter(`Sub facet` == "") %>%
  select(-c(Facet, `Sub facet`)) 

table_value <- coding_results %>%
  filter(Categories == "Value" | Categories == "n") 

table_costs <- coding_results %>%
  filter(Categories == "Cost" | Categories == "n") 



# Create sub set on goals only  --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

goals <- motivation %>%
  filter(goals == 1) %>%
  select(school, 
         sno, 
         age, gender, school_grade, 
         final_coding,
         statement_Kinyarwanda, statement_English) %>%
  arrange(final_coding) 



# Additional coding results [education-related utility facets] --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

statement_utility_education <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  mutate(utility_education = pmax(value.utility.learning.utility, value.utility.school.utility)) %>%
  group_by(sno) %>%
  summarise(utility_education = sum(utility_education)) %>%
  pivot_longer(
    cols = -sno,
    names_to = "Sub facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(`Sub facet`) %>%
  summarise(`Statements (M; in %)` = round(sum(count) / questionnaires$`statements final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Categories = "Value",
         `Facet` = "utility") %>%
  select(Categories, Facet, `Sub facet`, `Statements (M; in %)`) %>%
  arrange(`Sub facet`) 

respondents_utility_education <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  mutate(utility_education = pmax(value.utility.learning.utility, value.utility.school.utility)) %>%
  group_by(sno_student) %>%
  summarise(utility_education = sum(utility_education)) %>%
  pivot_longer(
    cols = -sno_student,
    names_to = "Sub facet",
    values_to = "count"  ) %>%
  mutate(count = ifelse(count > 1, 1, count)) %>%
  group_by(`Sub facet`) %>%
  summarise(`Students (M; in %)` = round(sum(count) / questionnaires$`respondents final`[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  mutate(Categories = "Value",
         `Facet` = "utility") %>%
  select(Categories, Facet, `Sub facet`, `Students (M; in %)`) %>%
  arrange(Facet, `Sub facet`)

utility_education <- merge(statement_utility_education, respondents_utility_education, by = c("Categories", "Facet", "Sub facet"))



# Additional coding results [expectancies across motivation and amotivation respondents] --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

respondents_expectancies <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  group_by(sno_student) %>%
  mutate(exeff = pmax(expectancies, costs.effort)) %>%
  summarise(expectancies = sum(expectancies),
            value = sum(value),
            cost = sum(costs),
            effort = sum(costs.effort),
            exeff = sum(exeff))  %>%
  mutate(expectancies = ifelse(expectancies > 1, 1, expectancies),
         value = ifelse(value > 1, 1, value),
         cost = ifelse(cost > 1, 1, cost),
         effort = ifelse(effort > 1, 1, effort),
         exeff = ifelse(exeff > 1, 1, exeff)) %>%
  summarise(expectancies = round(mean(expectancies), 4) * 100,
            value = round(mean(value), 4) * 100,
            cost = round(mean(cost), 4) * 100,
            effort = round(mean(effort), 4) * 100,
            exeff = round(mean(exeff), 4) * 100)



# Additional coding results [purpose-driven respondents] --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

respondents_reason <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  mutate(reason = pmax(value, goals)) %>%
  group_by(sno_student) %>%
  summarise(reason = sum(reason))  %>%
  mutate(reason = ifelse(reason > 1, 1, reason)) %>%
  summarise(reason = round(mean(reason), 4) * 100)

respondents_purpose <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  mutate(purpose = pmax(value.utility, goals)) %>%
  group_by(sno_student) %>%
  summarise(purpose = sum(purpose))  %>%
  mutate(purpose = ifelse(purpose > 1, 1, purpose)) %>%
  summarise(purpose = round(mean(purpose), 4) * 100)



# Additional coding results [affective states] --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

respondents_affective <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  mutate(affective = pmax(value.intrinsic, costs.emotional)) %>%
  group_by(sno_student) %>%
  summarise(affective = sum(affective))  %>%
  mutate(affective = ifelse(affective > 1, 1, affective)) %>%
  summarise(affective = round(mean(affective), 4) * 100)



# Create data to determine interrater agreement (round 1) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

ira1 <- motivation %>%
  select(coding_round_1_coder_1, coding_round_1_coder_2) %>%
  mutate(coding_round_1_coder_1 = ifelse(coding_round_1_coder_1 == "yes", as.numeric("1"), as.numeric("0")),
         coding_round_1_coder_2 = ifelse(coding_round_1_coder_2 == "yes", as.numeric("1"), as.numeric("0")))
kripp.alpha(t(ira1), method = "nominal")
# Krippendorff's alpha is .469%

ira1_agreement <- motivation %>%
  select(coding_round_1_coder_agreement) 
table(ira1_agreement)
1 - round(prop.table(table(ira1_agreement)[2:3]),4)                                                             ### Who deferred?



# Create data to determine interrater agreement (round 2) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

ira2 <- motivation %>%
  select(coding_round_2_coder_1, coding_round_2_coder_2) %>%
  mutate(coding_round_2_coder_1 = ifelse(coding_round_2_coder_1 == "motivation", as.numeric("1"), as.numeric("0")),
         coding_round_2_coder_2 = ifelse(coding_round_2_coder_2 == "motivation", as.numeric("1"), as.numeric("0")))
ira2_agreement <- motivation %>%
  select(coding_round_2_coder_agreement) 
kripp.alpha(t(ira2), method = "nominal")
# Krippendorff's alpha is .936%
table(ira2_agreement)
1 - round(prop.table(table(ira2_agreement)[2:3]),4)                                                             ### Who deferred?



# Create data to determine interrater agreement (round 3) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

categories <- data.frame(cat = unique(c(motivation$coding_round_3_coder_1_answer_1, 
                                        motivation$coding_round_3_coder_1_answer_2,
                                        motivation$coding_round_3_coder_1_answer_3,
                                        motivation$coding_round_3_coder_1_answer_4,
                                        motivation$coding_round_3_coder_2_answer_1, 
                                        motivation$coding_round_3_coder_2_answer_2,
                                        motivation$coding_round_3_coder_2_answer_3,
                                        motivation$coding_round_3_coder_2_answer_4)))
categories <- categories %>%
  filter(!is.na(cat)) %>%
  mutate(NO = 1:n())

ira3 <- motivation %>%
  select(coding_round_3_coder_1_answer_1, coding_round_3_coder_1_answer_2, coding_round_3_coder_1_answer_3, coding_round_3_coder_1_answer_4,
         coding_round_3_coder_2_answer_1, coding_round_3_coder_2_answer_2, coding_round_3_coder_2_answer_3, coding_round_3_coder_2_answer_4) %>%
  mutate_all(funs(ifelse(. == categories[ 1, 1], categories[ 1, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 2, 1], categories[ 2, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 3, 1], categories[ 3, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 4, 1], categories[ 4, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 5, 1], categories[ 5, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 6, 1], categories[ 6, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 7, 1], categories[ 7, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 8, 1], categories[ 8, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[ 9, 1], categories[ 9, 2], .))) %>%
  mutate_all(funs(ifelse(. == categories[10, 1], categories[10, 2], .))) %>%
  mutate_all(funs(as.numeric(.))) %>% 
  select(where(~ !(all(is.na(.))))) %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate_all(funs(ifelse(is.na(.), 100, .)))

ira3[, 1:3] <- t(apply(ira3[, 1:3], 1, FUN=function(x) sort(x, decreasing=FALSE)))
ira3[, 4:6] <- t(apply(ira3[, 4:6], 1, FUN=function(x) sort(x, decreasing=FALSE)))
ira3 <- ira3 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ ifelse(. == "100", " ", .))) %>%
  mutate(coding_round_3_coder_1_answer_1 = paste0(coding_round_3_coder_1_answer_1, sep = "|" , coding_round_3_coder_1_answer_2, sep = "|" , coding_round_3_coder_1_answer_3)) %>%
  mutate(coding_round_3_coder_2_answer_1 = paste0(coding_round_3_coder_2_answer_1, sep = "|" , coding_round_3_coder_2_answer_2, sep = "|" , coding_round_3_coder_2_answer_3)) %>%
  select(coding_round_3_coder_1_answer_1, coding_round_3_coder_2_answer_1) %>%
  mutate(across(everything(), ~ gsub("\\| ", "", .))) 
  
ira3 <- t(data.frame(ira3))
rownames(ira3) <- c("C1", "C2")
write.csv(ira3, "02 processed data/interrate_agreement_round3_20250119_v01.csv", row.names = FALSE)

ira3_agreement <- motivation %>%
  select(coding_round_3_coder_agreement) 
table(ira3_agreement)
round(c((1 - prop.table(table(ira3_agreement)[2:4])[3]) - prop.table(table(ira3_agreement)[2:4])[1:2],                      ### Who deffered?
        prop.table(table(ira3_agreement)[2:4])[3]),4)                             ### Who deferred?



# Create data to determine interrater agreement (round 4; utility) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

categories <- data.frame(cat = unique(c(motivation$coding_round_4.1_coder_1_answer_1,
                                        motivation$coding_round_4.1_coder_1_answer_2,
                                        motivation$coding_round_4.1_coder_1_answer_3,
                                        motivation$coding_round_4.1_coder_2_answer_1,
                                        motivation$coding_round_4.1_coder_2_answer_2,
                                        motivation$coding_round_4.1_coder_2_answer_3)))
categories <- categories %>%
  filter(!is.na(cat)) %>%
  mutate(NO = 1:n())

ira4 <- motivation %>%
  select(coding_round_4.1_coder_1_answer_1, coding_round_4.1_coder_1_answer_2, coding_round_4.1_coder_1_answer_3,
         coding_round_4.1_coder_2_answer_1, coding_round_4.1_coder_2_answer_2, coding_round_4.1_coder_2_answer_3) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 1, 1], categories[ 1, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 2, 1], categories[ 2, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 3, 1], categories[ 3, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 4, 1], categories[ 4, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 5, 1], categories[ 5, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 6, 1], categories[ 6, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 7, 1], categories[ 7, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 8, 1], categories[ 8, 2], .))) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  mutate(across(starts_with("coding_round_4.1_coder"), ~ ifelse(is.na(.), "", .))) %>%
  mutate(result1 = ifelse(coding_round_4.1_coder_1_answer_2 != "", paste0(coding_round_4.1_coder_1_answer_1,"|", coding_round_4.1_coder_1_answer_2), coding_round_4.1_coder_1_answer_1),
         result2 = ifelse(coding_round_4.1_coder_2_answer_2 != "", paste0(coding_round_4.1_coder_2_answer_1,"|", coding_round_4.1_coder_2_answer_2), coding_round_4.1_coder_2_answer_1)) %>%
  select(result1, result2)

ira4 <- t(data.frame(ira4))
rownames(ira4) <- c("C1", "C2")
write.csv(ira4, "02 processed data/interrate_agreement_round4_utility_20250119_v01.csv", row.names = FALSE)

ira4_agreement <- motivation %>%
  select(coding_round_4.1_coder_agreement) 
table(ira4_agreement)
round(c((1 - prop.table(table(ira4_agreement)[2:4])[3]) - prop.table(table(ira4_agreement)[2:4])[1:2],                      ### Who deffered?
        prop.table(table(ira4_agreement)[2:4])[3]),4)                             ### Who deferred?



# Create data to determine interrater agreement (round 5; 'other factors'/ consequence) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

categories <- data.frame(cat = unique(c(motivation$coding_round_4.2_coder_1_answer_1,
                                        motivation$coding_round_4.2_coder_1_answer_2,
                                        motivation$coding_round_4.2_coder_1_answer_3,
                                        motivation$coding_round_4.2_coder_2_answer_1,
                                        motivation$coding_round_4.2_coder_2_answer_2,
                                        motivation$coding_round_4.2_coder_2_answer_3)))
categories <- categories %>%
  filter(!is.na(cat)) %>%
  filter(cat != "[EMPTY]") %>%
  mutate(NO = 1:n())

ira5 <- motivation %>%
  select(coding_round_4.2_coder_1_answer_1, coding_round_4.2_coder_1_answer_2,
         coding_round_4.2_coder_2_answer_1, coding_round_4.2_coder_2_answer_2) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 1, 1], categories[ 1, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 2, 1], categories[ 2, 2], .))) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  mutate(across(starts_with("coding_round_4.2_coder"), ~ ifelse(is.na(.), "", .))) %>%
  mutate(result1 = ifelse(coding_round_4.2_coder_1_answer_2 != "", paste0(coding_round_4.2_coder_1_answer_1,"|", coding_round_4.2_coder_1_answer_2), coding_round_4.2_coder_1_answer_1),
         result2 = ifelse(coding_round_4.2_coder_2_answer_2 != "", paste0(coding_round_4.2_coder_2_answer_1,"|", coding_round_4.2_coder_2_answer_2), coding_round_4.2_coder_2_answer_1)) %>%
  select(result1, result2)

ira5 <- t(data.frame(ira5))
rownames(ira5) <- c("C1", "C2")
write.csv(ira5, "02 processed data/interrate_agreement_round5_others_20250119_v01.csv", row.names = FALSE)

ira5_agreement <- motivation %>%
  select(coding_round_4.2_coder_agreement) 
table(ira5_agreement)
round(c((1 - prop.table(table(ira5_agreement)[2:4])[3]) - prop.table(table(ira5_agreement)[2:4])[1:2],                      ### Who deffered?
        prop.table(table(ira5_agreement)[2:4])[3]),4)                             ### Who deferred?
rm(categories)



# Listing results (table 5) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

subjects <- c("Biology", "Chemistry", "English", "Entrepreneurship", "French", "Geography", "History",
              "ICT", "Kinyarwanda", "Kiswahili", "Literature", "Mathematics", "Physics", "Religion", "Sports")
for(subject in subjects) {
  subject <- tolower(subject)
  data <- motivation %>%
    select(all_of(grep(subject, colnames(motivation)))) 
  print(subject)
}

listing_motivation <- motivation %>%
  filter(!sno_student %in% excluded_sno$sno_student) %>%
  filter(!duplicated(sno_student)) %>%
  select(all_of(grep("_mot", colnames(motivation)))) %>% 
  pivot_longer(cols = everything(), names_to = "subject", values_to = "count") %>%
  mutate(subject = gsub("_mot", "", subject)) %>%
  group_by(subject) %>%
  summarise(Motivation = sum(count, na.rm = TRUE)) %>%
  mutate(Motivation = round(Motivation / questionnaires$resondents[questionnaires$questionnaire == "motivation"], 4) * 100) 

listing_amotivation <- motivation %>%
  filter(!sno_student %in% excluded_sno$sno_student) %>%
  filter(!duplicated(sno_student)) %>%
  select(all_of(grep("_demot", colnames(motivation)))) %>% 
  pivot_longer(cols = everything(), names_to = "subject", values_to = "count") %>%
  mutate(subject = gsub("_demot", "", subject)) %>%
  group_by(subject) %>%
  summarise(Amotivation = sum(count, na.rm = TRUE)) %>%
  mutate(Amotivation = round(Amotivation / questionnaires$resondents[questionnaires$questionnaire == "demotivation"], 4) * 100) 

listing_grades <- motivation %>%
  filter(!sno_student %in% excluded_sno$sno_student) %>%
  filter(!duplicated(sno_student)) %>%
  select(all_of(grep("_grades", colnames(motivation)))) %>% 
  pivot_longer(cols = everything(), names_to = "subject", values_to = "grades") %>%
  mutate(subject = gsub("_grades", "", subject)) %>%
  group_by(subject) %>%
  summarise(Grades = round(mean(grades, na.rm = TRUE), 4) * 100,
            sd = round(sd(grades, na.rm = TRUE), 4) * 100)  
listing <- merge(listing_motivation, listing_amotivation, by = "subject")
listing <- merge(listing, listing_grades, by = "subject")

cor.test(listing$Motivation, listing$Amotivation)
cor.test(listing$Motivation, listing$Grades)
cor.test(listing$Amotivation, listing$Grades)







# Biserial correlations between school grades and perceived (a)motivation  (table 6) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

biserial <- data.frame(Subject = as.character(), Motivation = as.character(), Amotivation = as.character())


for(subject in subjects) {
  subject <- tolower(subject)
  data <- motivation %>%
    filter(!sno_student %in% excluded_sno$sno_student) %>%
    filter(!duplicated(sno_student)) %>%
    select(all_of(grep(subject, colnames(motivation)))) 
  colnames(data) <- c("amotivation", "motivation", "grades")
  
  e <- round(cor.test(data[, 1], data[, 3])$estimate, 2)
  p <- round(cor.test(data[, 1], data[, 3])$p.value, 2)
  if (!is.na(e)) {
    if (p <= 0.05)  {p2 <- as.character("*")} 
    if (p <= 0.01)  {p2 <- as.character("**") }
    if (p <= 0.001) {p2 <- as.character("***") }
    if (p >  0.05)   {p2 <- as.character(" ") }
    f1 <- paste0(e, p2)
  } else {
    f1 <- "NA"
  }
  
  e <- round(cor.test(data[, 2], data[, 3])$estimate, 2)
  p <- round(cor.test(data[, 2], data[, 3])$p.value, 2)
  if (!is.na(e)) {
    if (p <= 0.05)  {p2 <- as.character("*")} 
    if (p <= 0.01)  {p2 <- as.character("**") }
    if (p <= 0.001) {p2 <- as.character("***") }
    if (p >  0.05)   {p2 <- as.character(" ") }
    f2 <- paste0(e, p2)
  } else {
    f2 <- "NA"
  }
  
  new <- data.frame(Subject = subject, Motivation = f2, Amotivation = f1)
  biserial <- rbind(biserial, new)
}
rm(new, e, p, f1, f2, data, subjects, subject, p2)



# save data --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

write.csv(noninformative, "02 processed data/noninformative statements_20250119_v01.csv", row.names = FALSE)
write.csv(coding_results, "04 results/04.1 Tables/table2_coding_results_full_20250107_v01.csv", row.names = FALSE)
write.csv(table_main, "04 results/04.1 Tables/table2_coding_results_main_20250107_v01.csv", row.names = FALSE)
write.csv(table_value, "04 results/04.1 Tables/table3_coding_results_value_20250107_v01.csv", row.names = FALSE)
write.csv(table_costs, "04 results/04.1 Tables/table4_coding_results_costs_20250107_v01.csv", row.names = FALSE)
write.csv(goals, "02 processed data/goals_20250119_v01.csv", row.names = FALSE)
write.csv(listing, "04 results/04.1 Tables/table5_listings_20250126_v01.csv", row.names = FALSE)
write.csv(biserial, "04 results/04.1 Tables/table6_biserial_20250126_v01.csv", row.names = FALSE)










