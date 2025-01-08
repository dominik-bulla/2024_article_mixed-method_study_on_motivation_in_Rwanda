### environment ----------------------- ----------------------- ----------------------- -----------------------

setwd("C:/Users/domin/GitHub/2024_article_mixed-method_study_on_motivation_in_Rwanda")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



### packages ----------------------- ----------------------- ----------------------- -----------------------

library(readxl)
library(dplyr)
library(psych)



### local data ----------------------- ----------------------- ----------------------- -----------------------

motivation <- read.csv("C:/Users/domin/GitHub/2019_motivation_in_Rwanda/02 processed data/motivation_in_Rwanda_20250103_v01.csv")



# compile stats on questionnaires/ statememts/ segments --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

questionnaires <- motivation %>%
  filter(!duplicated(sno_student)) %>%
  group_by(questionnaire_type_initial) %>%
  summarise(resondents = n()) %>%
  rename("questionnaire" = questionnaire_type_initial)
total <- motivation %>%
  filter(!duplicated(sno_student)) %>%
  summarise(resondents = n()) %>%
  mutate(questionnaire = "total")
questionnaires <- rbind(questionnaires, total)

mismatch <- motivation %>%
  filter(coding_round_2_final_coding != "no") %>%
  group_by(sno_student, coding_round_2_final_coding) %>%
  summarise(n = n()) %>%
  mutate(both = row_number()) %>%
  ungroup()
double <- c(mismatch$sno_student[mismatch$both == 2])
mismatch <- mismatch %>%
  filter(sno_student %in% double) %>%
  select(-c(n, both))
mismatch <- merge(mismatch, motivation[!duplicated(motivation$sno_student), c("sno_student", "questionnaire_type_initial")], by = "sno_student", all.x = TRUE) 
mismatch <- mismatch %>%
  filter(questionnaire_type_initial != coding_round_2_final_coding) %>%
  group_by(questionnaire_type_initial) %>%
  summarise(`resondents both` = n()) %>%
  rename("questionnaire" = questionnaire_type_initial)
total <- data.frame(questionnaire = "total", `resondents both` = sum(mismatch$`resondents both`))
colnames(total) <- colnames(mismatch)
mismatch <- rbind(mismatch, total)
questionnaires <- merge(questionnaires, mismatch, by = "questionnaire")
rm(mismatch, double)

exlcuded <- motivation %>%
  group_by(sno_student) %>%
  summarise(n = n())
exlcuded1 <- motivation %>%
  filter(coding_round_1_final_coding == "no") %>%
  group_by(sno_student) %>%
  summarise(n1 = n())
exlcuded2 <- motivation %>%
  filter(coding_round_2_final_coding == "no") %>%
  group_by(sno_student) %>%
  summarise(n2 = n())
exlcuded <- merge(exlcuded, exlcuded1, all.x = TRUE) 
exlcuded <- merge(exlcuded, exlcuded2, all.x = TRUE) 
exlcuded <- exlcuded %>%
  mutate(n1 = ifelse(is.na(n1), 0, n1),
         n2 = ifelse(is.na(n2), 0, n2),
         nt = n1 + n2) %>%
  filter(n == nt) %>%
  select(sno_student)
exlcuded <- merge(exlcuded, motivation[!duplicated(motivation$sno_student), c("sno_student", "questionnaire_type_initial")], by = "sno_student", all.x = TRUE) 
exlcuded <- exlcuded %>%
  group_by(questionnaire_type_initial) %>%
  summarise(`resondents excluded` = n()) %>%
  rename("questionnaire" = questionnaire_type_initial)
total <- data.frame(questionnaire = "total", `resondents both` = sum(exlcuded$`resondents excluded`))
colnames(total) <- colnames(exlcuded)
exlcuded <- rbind(exlcuded, total)
questionnaires <- merge(questionnaires, exlcuded, by = "questionnaire")
rm(exlcuded, exlcuded1, exlcuded2)

questionnaires_final <- motivation %>%
  filter(coding_round_2_final_coding != "no" & !is.na(coding_round_2_final_coding)) %>%
  group_by(sno_student, coding_round_2_final_coding) %>%
  summarise(resondents = n()) %>%
  mutate(resondents = ifelse(resondents > 1, 1, resondents)) %>%
  rename("questionnaire" = coding_round_2_final_coding) %>%
  group_by(questionnaire) %>%
  summarise(`resondents final` = sum(resondents))
total <- data.frame(questionnaire = "total", `resondents final` = sum(questionnaires_final$`resondents final`))
colnames(total) <- colnames(questionnaires_final)
questionnaires_final <- rbind(questionnaires_final, total)
questionnaires <- merge(questionnaires, questionnaires_final, by = "questionnaire")
rm(questionnaires_final)

statements <- motivation %>%                                  ### this is the number of unique statements
  filter(statement_unique == "yes") %>%
  group_by(coding_round_2_final_coding) %>%
  summarise(statements = n())%>%
  rename("questionnaire" = coding_round_2_final_coding)
total <- motivation %>%
  filter(statement_unique == "yes") %>%
  summarise(statements = n()) %>%
  mutate(questionnaire = "total")
statements <- rbind(statements, total)

segmemts <- motivation %>%                                    ### this is the number of meanoing-bearing data segments (recall: some statements consist of more than just one segment)
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  group_by(coding_round_2_final_coding) %>%
  summarise(segmemts = n()) %>%
  rename("questionnaire" = coding_round_2_final_coding)
total <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  summarise(segmemts = n()) %>%
  mutate(questionnaire = "total")
segmemts <- rbind(segmemts, total)

questionnaires <- merge(questionnaires, statements, by = "questionnaire")
questionnaires <- merge(questionnaires, segmemts, by = "questionnaire")
rm(segmemts, statements, total)  



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




# coding frequencies within motivation by data segments (table 2) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation" & motivation$coding_round_1_final_coding == "yes" & motivation$coding_round_2_final_coding != "no"])
sum(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation" & motivation$coding_round_1_final_coding == "yes" & motivation$coding_round_2_final_coding != "no" & !is.na(motivation$coding_round_2_final_coding)]))  
table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "demotivation" & motivation$coding_round_1_final_coding == "yes" & motivation$coding_round_2_final_coding != "no"])
sum(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "demotivation" & motivation$coding_round_1_final_coding == "yes" & motivation$coding_round_2_final_coding != "no"]))  



# coding frequencies by segments (table 2; motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

segmemts_motivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (M)`) %>%
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "source"))) %>%
  arrange(Category, Sub_category1, Sub_category2) %>%
  mutate(Sub_category2 = ifelse(Sub_category2 == Sub_category1, "", Sub_category2), 
         Sub_category1 = ifelse(Sub_category1 == Category, "", Sub_category1)) 

total  <- segmemts_motivation %>%
  filter(Category == "value" | Category == "costs" | Category == "goals") %>%
  group_by(Category) %>%
  summarize(`Segments (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category1 = "total",
         Sub_category2 = "") 
segmemts_motivation <- rbind(segmemts_motivation, total)

total  <- segmemts_motivation %>%
  filter(Sub_category1 == "utility") %>%
  group_by(Sub_category1) %>%
  summarize(`Segments (M)` = sum(`Segments (M)`)) %>%
  mutate(Category = "value", 
         Sub_category2 = "total",
         Sub_category1 = "utility") 
segmemts_motivation <- rbind(segmemts_motivation, total) %>%
  arrange(Category, 
          if_else(Sub_category1 == "total", "aaaa", Sub_category1), 
          if_else(Sub_category2 == "total", "aaaa", Sub_category2))



# coding frequencies by segments (table 2; demotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

segmemts_demotivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (D)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (D)`) %>%
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "source"))) %>%
  arrange(Category, Sub_category1, Sub_category2) %>%
  mutate(Sub_category2 = ifelse(Sub_category2 == Sub_category1, "", Sub_category2), 
         Sub_category1 = ifelse(Sub_category1 == Category, "", Sub_category1))

total  <- segmemts_demotivation %>%
  filter(Category == "value" | Category == "costs" | Category == "goals") %>%
  group_by(Category) %>%
  summarize(`Segments (D)` = sum(`Segments (D)`)) %>%
  mutate(Sub_category1 = "total",
         Sub_category2 = "") 
segmemts_demotivation <- rbind(segmemts_demotivation, total)

total  <- segmemts_demotivation %>%
  filter(Sub_category1 == "utility") %>%
  group_by(Sub_category1) %>%
  summarize(`Segments (D)` = sum(`Segments (D)`)) %>%
  mutate(Category = "value", 
         Sub_category2 = "total",
         Sub_category1 = "utility") 
segmemts_demotivation <- rbind(segmemts_demotivation, total) %>%
  arrange(Category, 
          if_else(Sub_category1 == "total", "aaaa", Sub_category1), 
          if_else(Sub_category2 == "total", "aaaa", Sub_category2))



# coding frequencies by students (table 2; motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

students_motivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (M)`) %>%
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "source"))) %>%
  arrange(sno_student, Category, Sub_category1, Sub_category2) %>%
  mutate(`Segments (M)` = ifelse(`Segments (M)` > 1, 1, `Segments (M)`)) %>%
  group_by(Category, Sub_category1, Sub_category2) %>%
  summarise(`Students (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category2 = ifelse(Sub_category2 == Sub_category1, "", Sub_category2), 
         Sub_category1 = ifelse(Sub_category1 == Category, "", Sub_category1))

total <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (M)`) %>%
  mutate(`Segments (M)` = ifelse(`Segments (M)` > 1, 1, `Segments (M)`)) %>%
  filter(Sub_category1 == "utility") %>%
  filter(!duplicated(sno_student)) %>%
  group_by(Category, Sub_category1) %>%
  summarise(`Students (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category2 = "total")
students_motivation <- rbind(students_motivation, total)

total <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (M)`) %>%
  mutate(`Segments (M)` = ifelse(`Segments (M)` > 1, 1, `Segments (M)`)) %>%
  filter(Category == "value" | Category == "costs" | Category == "goals") %>%
  filter(!duplicated(paste0(sno_student, Category))) %>%
  group_by(Category) %>%
  summarise(`Students (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category1 = "total",
         Sub_category2 = "")
students_motivation <- rbind(students_motivation, total) %>%
  arrange(Category, 
          if_else(Sub_category1 == "total", "aaaa", Sub_category1), 
          if_else(Sub_category2 == "total", "aaaa", Sub_category2))



# coding frequencies by students (table 2; demotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

students_demotivation <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (D)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (D)`) %>%
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "source"))) %>%
  arrange(sno_student, Category, Sub_category1, Sub_category2) %>%
  mutate(`Segments (D)` = ifelse(`Segments (D)` > 1, 1, `Segments (D)`)) %>%
  group_by(Category, Sub_category1, Sub_category2) %>%
  summarise(`Students (D)` = sum(`Segments (D)`)) %>%
  mutate(Sub_category2 = ifelse(Sub_category2 == Sub_category1, "", Sub_category2), 
         Sub_category1 = ifelse(Sub_category1 == Category, "", Sub_category1))

total <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (D)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (D)`) %>%
  mutate(`Segments (D)` = ifelse(`Segments (D)` > 1, 1, `Segments (D)`)) %>%
  filter(Sub_category1 == "utility") %>%
  filter(!duplicated(sno_student)) %>%
  group_by(Category, Sub_category1) %>%
  summarise(`Students (D)` = sum(`Segments (D)`)) %>%
  mutate(Sub_category2 = "total")
students_demotivation <- rbind(students_demotivation, total)

total <- motivation %>%
  filter(coding_round_1_final_coding == "yes") %>%
  filter(coding_round_2_final_coding != "no") %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (D)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (D)`) %>%
  mutate(`Segments (D)` = ifelse(`Segments (D)` > 1, 1, `Segments (D)`)) %>%
  filter(Category == "value" | Category == "costs" | Category == "goals") %>%
  filter(!duplicated(paste0(sno_student, Category))) %>%
  group_by(Category) %>%
  summarise(`Students (D)` = sum(`Segments (D)`)) %>%
  mutate(Sub_category1 = "total",
         Sub_category2 = "")
students_demotivation <- rbind(students_demotivation, total) %>%
  arrange(Category, 
          if_else(Sub_category1 == "total", "aaaa", Sub_category1), 
          if_else(Sub_category2 == "total", "aaaa", Sub_category2))



# merging coding results (table 2; (de)motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

coding_segments <- merge(segmemts_motivation, segmemts_demotivation, by = c("Category", "Sub_category1", "Sub_category2"), all = TRUE)
coding_segments <- coding_segments %>%
  mutate(across(starts_with("Seg"), ~ ifelse(is.na(.), 0, .))) %>%
  arrange(Category, 
          if_else(Sub_category1 == "total", "aaaa", Sub_category1),  
          if_else(Sub_category2 == "total", "aaaa", Sub_category2)) 
rm(segmemts_motivation, segmemts_demotivation)

coding_students <- merge(students_motivation, students_demotivation, by = c("Category", "Sub_category1", "Sub_category2"), all = TRUE)
coding_students <- coding_students %>%
  mutate(across(starts_with("Stu"), ~ ifelse(is.na(.), 0, .)))  
rm(students_motivation, students_demotivation)

coding_results  <- merge(coding_segments, coding_students, by = c("Category", "Sub_category1", "Sub_category2"), all = TRUE)
coding_results <- coding_results %>%
  select(Category, Sub_category1, Sub_category2,
         `Segments (M)`, `Students (M)`, 
         `Segments (D)`, `Students (D)`) %>%
  mutate(`Segments (M)` = round(`Segments (M)` / questionnaires$segmemts[questionnaires$questionnaire == "motivation"] , 4) * 100, 
         `Students (M)` = round(`Students (M)` / questionnaires$`resondents final`[questionnaires$questionnaire == "motivation"] , 4) * 100, 
         `Segments (D)` = round(`Segments (D)` / questionnaires$segmemts[questionnaires$questionnaire == "demotivation"] , 4) * 100, 
         `Students (D)` = round(`Students (D)` / questionnaires$`resondents final`[questionnaires$questionnaire == "demotivation"] , 4) * 100)
total <- data.frame(Category = "total", 
                    Sub_category1 = "", 
                    Sub_category2 = "",
                    `Segments (M)` = questionnaires$segmemts[questionnaires$questionnaire == "motivation"], 
                    `Students (M)` = questionnaires$`resondents final`[questionnaires$questionnaire == "motivation"], 
                    `Segments (D)` = questionnaires$segmemts[questionnaires$questionnaire == "demotivation"], 
                    `Students (D)` = questionnaires$`resondents final`[questionnaires$questionnaire == "demotivation"])
colnames(total) <- colnames(coding_results)
coding_results <- rbind(coding_results, total) %>%
  arrange(Category, 
          if_else(Sub_category1 == "total", "aaaa", Sub_category1),  
          if_else(Sub_category2 == "total", "aaaa", if_else(Sub_category2 == "other", "xxxx", Sub_category2))) %>%
  mutate(Sub_category1 = ifelse(Sub_category1 == "total", "", Sub_category1),
         Sub_category2 = ifelse(Sub_category2 == "total", "", Sub_category2))
rm(total)



# Create sub tables merging coding results (table 2; (de)motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

table_main <- coding_results %>%
  filter(Sub_category2 == "") %>%
  filter(Sub_category1 == "" | Sub_category1 == "total") %>%
  select(-c(Sub_category1, Sub_category2))

table_value <- coding_results %>%
  filter(Category == "value" | Category == "total") %>%
  filter(Sub_category1 != "total") 

table_costs <- coding_results %>%
  filter(Category == "costs" | Category == "total") %>%
  filter(Sub_category1 != "total") %>%
  select(-c(Sub_category2))

table_goals <- coding_results %>%
  filter(Category == "goals" | Category == "total") %>%
  filter(Sub_category1 != "total") %>%
  select(-c(Sub_category2))



# save data --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

write.csv(table_main, "04 results/04.1 Tables/table2_coding_results_main_20250107_v01.csv")
write.csv(table_value, "04 results/04.1 Tables/table2.1_coding_results_value_20250107_v01.csv")
write.csv(table_costs, "04 results/04.1 Tables/table2.2_coding_results_costs_20250107_v01.csv")
write.csv(table_goals, "04 results/04.1 Tables/table2.3_coding_results_goals_20250107_v01.csv")







