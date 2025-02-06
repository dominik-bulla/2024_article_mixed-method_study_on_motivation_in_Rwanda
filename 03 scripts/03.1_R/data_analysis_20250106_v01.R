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



### local data ----------------------- ----------------------- ----------------------- -----------------------

motivation <- read.csv("01 raw data/motivation_in_Rwanda_20250119_v01.csv")



# compile stats on questionnaires/ statememts/ segments --------------------------------- --------------------------------- --------------------------------- --------------------------------- 
# Here are the total number of questionnaires, statements, and segments before they were coded
# We also extract segments that were non-informative as well as questionnaires that contained both 
# motivation and amotivation segments.

length(unique(motivation$sno))
length(unique(motivation$sno_segment))
length(unique(motivation$sno_segment[motivation$coding_round_1_final_coding == "no"]))

noninformative <- motivation %>%
  filter(coding_round_1_final_coding == "no" | coding_round_2_final_coding == "no") %>%
  select(sno_student, 
         school,
         sno_questionnaire, sno_statement, sno_segment, sno,
         age, gender, school_grade,
         statement_Kinyarwanda, statement_English)

both <- motivation %>%
  select(sno_questionnaire, questionnaire_type_initial, coding_round_2_final_coding) %>%
  mutate(mismatch = ifelse(questionnaire_type_initial != coding_round_2_final_coding, 1, 0)) %>%
  filter(mismatch == 1 & coding_round_2_final_coding != "no") %>%
  select(sno_questionnaire)
both <- merge(both, motivation, by = "sno_questionnaire")%>%
  select(sno_student, 
         school,
         questionnaire_type_initial, coding_round_2_final_coding, 
         sno_questionnaire, sno_statement, sno_segment, sno,
         age, gender, school_grade,
         statement_Kinyarwanda, statement_English) %>%
  filter(coding_round_2_final_coding != "no")



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
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "other factors"))) %>%
  arrange(Category, Sub_category1, Sub_category2) %>%
  mutate(Sub_category2 = ifelse(Sub_category2 == Sub_category1, "", Sub_category2), 
         Sub_category1 = ifelse(Sub_category1 == Category, "", Sub_category1)) 

total  <- segmemts_motivation %>%
  filter(Category == "value" | Category == "costs") %>%
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
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "other factors"))) %>%
  arrange(Category, Sub_category1, Sub_category2) %>%
  mutate(Sub_category2 = ifelse(Sub_category2 == Sub_category1, "", Sub_category2), 
         Sub_category1 = ifelse(Sub_category1 == Category, "", Sub_category1))

total  <- segmemts_demotivation %>%
  filter(Category == "value" | Category == "costs") %>%
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
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "other factors"))) %>%
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
  filter(Category == "value" | Category == "costs") %>%
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
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "other factors"))) %>%
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
  filter(Category == "value" | Category == "costs") %>%
  filter(!duplicated(paste0(sno_student, Category))) %>%
  group_by(Category) %>%
  summarise(`Students (D)` = sum(`Segments (D)`)) %>%
  mutate(Sub_category1 = "total",
         Sub_category2 = "")
students_demotivation <- rbind(students_demotivation, total) %>%
  arrange(Category, 
          if_else(Sub_category1 == "total", "aaaa", Sub_category1), 
          if_else(Sub_category2 == "total", "aaaa", Sub_category2))



# merging coding results (table 2; (a)motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

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



# Create sub tables merging coding results (table 2; (a)motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

table_main <- coding_results %>%
  filter(Sub_category2 == "") %>%
  filter(Sub_category1 == "" | Sub_category1 == "total") %>%
  select(-c(Sub_category1, Sub_category2)) %>%
  mutate(Category = paste0(toupper(substr(Category, 1,1)), substr(Category, 2,nchar(as.character(Category)))))

table_value <- coding_results %>%
  filter(Category == "value" | Category == "total") %>%
  filter(Sub_category1 != "total") %>%
  mutate(Category = paste0(toupper(substr(Category, 1,1)), substr(Category, 2,nchar(as.character(Category)))))

table_costs <- coding_results %>%
  filter(Category == "costs" | Category == "total") %>%
  filter(Sub_category1 != "total") %>%
  select(-c(Sub_category2)) %>%
  mutate(Category = paste0(toupper(substr(Category, 1,1)), substr(Category, 2,nchar(as.character(Category)))))



# Create sub set on goals only  --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

goals <- motivation %>%
  filter(grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  select(school, 
         sno_statement, sno_segment, sno, 
         age, gender, school_grade, 
         coding_round_4.2_final_coding_separate,
         statement_Kinyarwanda, statement_English) %>%
  arrange(coding_round_4.2_final_coding_separate) 
goals$coding_round_4.2_final_coding_separate <- gsub("goals/", "", goals$coding_round_4.2_final_coding_separate)



# Create data to determine interrater agreement (round 1) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

ira1 <- motivation %>%
  select(coding_round_1_coder_1, coding_round_1_coder_2) %>%
  mutate(coding_round_1_coder_1 = ifelse(coding_round_1_coder_1 == "yes", as.numeric("1"), as.numeric("0")),
         coding_round_1_coder_2 = ifelse(coding_round_1_coder_2 == "yes", as.numeric("1"), as.numeric("0")))
kripp.alpha(t(ira1), method = "nominal")
# Krippendorff's alpha is .466%

ira1_agreement <- motivation %>%
  select(coding_round_1_coder_agreement) 
table(ira1_agreement)
1 - round(prop.table(table(ira1_agreement)[2:3]),3)                                                             ### Who deferred?



# Create data to determine interrater agreement (round 2) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

ira2 <- motivation %>%
  select(coding_round_2_coder_1, coding_round_2_coder_2) %>%
  mutate(coding_round_2_coder_1 = ifelse(coding_round_2_coder_1 == "motivation", "2", coding_round_2_coder_1),
         coding_round_2_coder_1 = ifelse(coding_round_2_coder_1 == "demotivation", "1", coding_round_2_coder_1),
         coding_round_2_coder_1 = ifelse(coding_round_2_coder_1 == "no", "0", coding_round_2_coder_1),
         coding_round_2_coder_2 = ifelse(coding_round_2_coder_2 == "motivation", "2", coding_round_2_coder_2),
         coding_round_2_coder_2 = ifelse(coding_round_2_coder_2 == "demotivation", "1", coding_round_2_coder_2),
         coding_round_2_coder_2 = ifelse(coding_round_2_coder_2 == "no", "0", coding_round_2_coder_2))

table(ira2$coding_round_2_coder_2)

ira2_agreement <- motivation %>%
  select(coding_round_2_coder_agreement) 
table(ira2_agreement)
1 - round(prop.table(table(ira2_agreement)[2:3]),3)                                                             ### Who deferred?



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
  mutate_all(funs(ifelse(. == categories[11, 1], categories[11, 2], .))) %>%
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



# Create data to determine interrater agreement (round 4.1) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

categories <- data.frame(cat = unique(c(motivation$coding_round_4.1_coder_1_answer_1,
                                        motivation$coding_round_4.1_coder_1_answer_2,
                                        motivation$coding_round_4.1_coder_1_answer_3,
                                        motivation$coding_round_4.1_coder_2_answer_1,
                                        motivation$coding_round_4.1_coder_2_answer_2,
                                        motivation$coding_round_4.1_coder_2_answer_3)))
categories <- categories %>%
  filter(!is.na(cat)) %>%
  mutate(NO = 1:n())

motivation$coding_round_4.2_final_coding_separate

table(ira4.1$coding_round_2_final_coding,
        ira4.1$coding_round_3_final_coding_separate)

ira4.1 <- motivation %>%
  select(coding_round_2_final_coding, 
         coding_round_3_final_coding_separate, 
         coding_round_4.1_coder_1_answer_1, coding_round_4.1_coder_1_answer_2, coding_round_4.1_coder_1_answer_3,
         coding_round_4.1_coder_2_answer_1, coding_round_4.1_coder_2_answer_2, coding_round_4.1_coder_2_answer_3,
         coding_round_4.2_final_coding_separate) 

write.csv(ira4.1, "aaa.csv")


%>%
  mutate(across(everything(), ~ ifelse(. == categories[ 1, 1], categories[ 1, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 2, 1], categories[ 2, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 3, 1], categories[ 3, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 4, 1], categories[ 4, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 5, 1], categories[ 5, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 6, 1], categories[ 6, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 7, 1], categories[ 7, 2], .))) %>%
  mutate(across(everything(), ~ ifelse(. == categories[ 8, 1], categories[ 8, 2], .))) %>%
  mutate(across(everything(), as.numeric)) 


%>%
  filter(rowSums(is.na(.)) < ncol(.))

ira4.1 <- ira4.1 %>%
  mutate(across(starts_with("coding_round_4.1_coder"), ~ ifelse(is.na(.), "", .))) %>%
  mutate(result1 = ifelse(coding_round_4.1_coder_1_answer_2 != "", paste0(coding_round_4.1_coder_1_answer_1,"|", coding_round_4.1_coder_1_answer_2), coding_round_4.1_coder_1_answer_1),
         result2 = ifelse(coding_round_4.1_coder_2_answer_2 != "", paste0(coding_round_4.1_coder_2_answer_1,"|", coding_round_4.1_coder_2_answer_2), coding_round_4.1_coder_2_answer_1)) %>%
  select(result1, result2)

ira4.1 <- t(data.frame(ira4.1))
rownames(ira4.1) <- c("C1", "C2")
write.csv(ira4.1, "02 processed data/interrate_agreement_round4.1_20250119_v01.csv", row.names = FALSE)

ira4.1_agreement <- motivation %>%
  select(coding_round_4.1_coder_agreement) 
table(ira4.1_agreement)
round(c((1 - prop.table(table(ira4.1_agreement)[2:4])[3]) - prop.table(table(ira4.1_agreement)[2:4])[1:2],                      ### Who deffered?
        prop.table(table(ira4.1_agreement)[2:4])[3]),4)                             ### Who deferred?




# Create data to determine interrater agreement (round 4.2) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

categories <- data.frame(cat = unique(c(qualdata_segment_level$R4.2C1.1, 
                                        qualdata_segment_level$R4.2C1.2,
                                        qualdata_segment_level$R4.2C2.1,
                                        qualdata_segment_level$R4.2C2.2)))
categories <- categories %>%
  filter(!is.na(cat)) %>%
  filter(cat != "[EMPTY]") %>%
  mutate(NO = 1:n())

motR4.2 <- qualdata_segment_level %>%
  filter(Unique     == "Yes") %>%
  select(starts_with("R4.2C")) %>%
  mutate(across(starts_with("R4.2C"), ~ ifelse(. == categories[ 1, 1], categories[ 1, 2], .))) %>%
  mutate(across(starts_with("R4.2C"), ~ ifelse(. == categories[ 2, 1], categories[ 2, 2], .))) %>%
  filter_all(any_vars(!is.na(.))) 

motR4.2 <- motR4.2 %>%
  mutate(across(starts_with("R4.2C"), ~ ifelse(is.na(.), "", .))) %>%
  mutate(R4.2C1.3 = ifelse(R4.2C1.2 != "", paste0(R4.2C1.1,"|", R4.2C1.2), R4.2C1.1),
         R4.2C2.3 = ifelse(R4.2C2.2 != "", paste0(R4.2C2.1,"|", R4.2C2.2), R4.2C2.1)) %>%
  select(R4.2C1.3, R4.2C2.3)

motR4.2 <- t(data.frame(motR4.2))
# write.csv(motR4.2, "Quant/MVNA_M4.2_20240326.csv")  
# motivation: krippendorff's alpha is .548

motR4.2Com <- qualdata_segment_level %>%
  filter(TYPE_FINAL == "MOTIVATION") %>%
  filter(Unique     == "Yes") %>%
  select(starts_with("R4.2remark"))
table(motR4.2Com)
round(c((1 - prop.table(table(motR4.2Com)[2:4])[3]) - prop.table(table(motR4.2Com)[2:4])[1:2],                      ### Who deffered?
        prop.table(table(motR4.2Com)[2:4])[3]),3)

rm(motR1, motR1Com,
   motR2, motR2Com,
   motR3, motR3Com,
   motR4.1, motR4.1Com,
   motR4.2, motR4.2Com)














# Motivational correlations (table 2) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

MOTVARS <- c("EXPECTANCY", 
             "COSTS (EFFORT)", "COSTS (EMOTIONAL)", "COSTS (OPPORTUNITY)", "COSTS (OUTSIDE)", 
             "DAILY UTILITY", "GENERAL UTILITY", "INTRINISC VALUE", "LEARNING UTILITY", 
             "SCHOOL UTILITY", "SOCIAL UTILITY", "USELESSNESS")

Motivational_correlations <- matrix(rep(0, length(MOTVARS)^2), ncol = length(MOTVARS), nrow = length(MOTVARS)) 
colnames(Motivational_correlations) <- MOTVARS
rownames(Motivational_correlations) <- MOTVARS
Motivational_correlations2 <- Motivational_correlations
Demotivational_correlations <- Motivational_correlations
Demotivational_correlations2 <- Motivational_correlations

for (VAR in MOTVARS) {
  for (VAR2 in MOTVARS) {
    Result <- cor.test(qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "MOTIVATION", VAR], qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "MOTIVATION", VAR2])$estimate
    Motivational_correlations[rownames(Motivational_correlations) == VAR, colnames(Motivational_correlations) == VAR2] <- format(round(Result, 2), nsmall = 2)
    Result <- cor.test(qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "MOTIVATION", VAR], qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "MOTIVATION", VAR2])$p.value
    Motivational_correlations2[rownames(Motivational_correlations2) == VAR, colnames(Motivational_correlations2) == VAR2] <- format(round(Result, 2), nsmall = 2)
    
    Result <- cor.test(qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "DEMOTIVATION", VAR], qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "DEMOTIVATION", VAR2])$estimate
    Demotivational_correlations[rownames(Demotivational_correlations) == VAR, colnames(Demotivational_correlations) == VAR2] <- format(round(Result, 2), nsmall = 2)
    Result <- cor.test(qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "DEMOTIVATION", VAR], qualdata_segment_level_wide[qualdata_segment_level_wide$Questionnaire == "DEMOTIVATION", VAR2])$p.value
    Demotivational_correlations2[rownames(Demotivational_correlations2) == VAR, colnames(Demotivational_correlations2) == VAR2] <- format(round(Result, 2), nsmall = 2)
  }
}
rm(VAR, VAR2)

Motivational_correlations2 <- ifelse(Motivational_correlations2 < 0.001, 4, Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 < 0.01, 3, Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 < 0.05, 2, Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 < 0.1, 1, Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 > 0.1 & Motivational_correlations2 < 1, 0, Motivational_correlations2)
Motivational_correlations2 <- matrix(as.character(Motivational_correlations2), ncol = length(MOTVARS))
Motivational_correlations2 <- ifelse(Motivational_correlations2 == "4", "***", Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 == "3", "**", Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 == "2", "*", Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 == "1", "+", Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 == "0", "", Motivational_correlations2)
Motivational_correlations2 <- ifelse(Motivational_correlations2 == "NA", "", Motivational_correlations2)
Motivational_correlations2 <- matrix(paste0(Motivational_correlations, Motivational_correlations2), ncol = length(MOTVARS))
Motivational_correlations2[upper.tri(Motivational_correlations2, diag = TRUE)] <- ""
colnames(Motivational_correlations2) <- MOTVARS
rownames(Motivational_correlations2) <- MOTVARS

Demotivational_correlations2 <- ifelse(Demotivational_correlations2 < 0.001, 4, Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 < 0.01, 3, Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 < 0.05, 2, Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 < 0.1, 1, Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 > 0.1 & Demotivational_correlations2 < 1, 0, Demotivational_correlations2)
Demotivational_correlations2 <- matrix(as.character(Demotivational_correlations2), ncol = length(MOTVARS))
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 == "4", "***", Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 == "3", "**", Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 == "2", "*", Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 == "1", "+", Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 == "0", "", Demotivational_correlations2)
Demotivational_correlations2 <- ifelse(Demotivational_correlations2 == "NA", "", Demotivational_correlations2)
Demotivational_correlations2 <- matrix(paste0(Demotivational_correlations, Demotivational_correlations2), ncol = length(MOTVARS))
Demotivational_correlations2[upper.tri(Demotivational_correlations2, diag = TRUE)] <- ""
colnames(Demotivational_correlations2) <- MOTVARS
rownames(Demotivational_correlations2) <- MOTVARS

#Correlations <- matrix(paste0(Demotivational_correlations2, Motivational_correlations2), ncol = length(MOTVARS))
#colnames(Correlations) <- MOTVARS
#rownames(Correlations) <- MOTVARS

#rm(Demotivational_correlations, Demotivational_correlations2, Motivational_correlations, Motivational_correlations2, Result, MOTVARS)
write.csv(Motivational_correlations2, "Article_1_Motivational Correlations_20240417.csv")
write.csv(Demotivational_correlations2, "Article_1_Demotivational Correlations_20240417.csv")






# save data --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

write.csv(noninformative, "02 processed data/noninformative statements_20250119_v01.csv", row.names = FALSE)
write.csv(both, "02 processed data/questionnaires containing both_20250119_v01.csv", row.names = FALSE)
write.csv(table_main, "04 results/04.1 Tables/table2_coding_results_main_20250107_v01.csv", row.names = FALSE)
write.csv(table_value, "04 results/04.1 Tables/table2.1_coding_results_value_20250107_v01.csv", row.names = FALSE)
write.csv(table_costs, "04 results/04.1 Tables/table2.2_coding_results_costs_20250107_v01.csv", row.names = FALSE)
write.csv(goals, "02 processed data/goals_20250119_v01.csv", row.names = FALSE)










