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



# compile student stats --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

students <- motivation %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(!duplicated(sno_student))

school <- students %>%
  filter(!duplicated(sno_student)) %>%
  group_by(school) %>%
  summarise(n = n())
school

students %>%
  summarize(mean(age),
            sd(age))

students %>%
  group_by(school) %>% 
  summarize(mean(age),
            sd(age))

students %>%
  group_by(gender) %>%
  summarise(n = round(n() / length(unique(motivation$sno_student)), 4) * 100)
  
school_gender <- students %>%
  group_by(school, gender) %>%
  summarise(ng = n())
school_gender <- merge(school_gender, school)
school_gender <- school_gender %>%
  mutate(ng = round(ng / n, 4) * 100) %>%
  select(-c(n))
school_gender
rm(school, school_gender)



# compile stats on questionnaires --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

questionnaires <- motivation %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(!duplicated(sno_student)) %>%
  group_by(questionnaire) %>%
  summarise(n = n())
  


# compile stats on data segments --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

motivation %>% 
  filter(statement_unique == "yes") %>%
  summarise(n = n())

segments <- motivation %>%
  filter(!is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_2_final_coding != "no") %>%
  group_by(coding_round_2_final_coding) %>%
  summarise(n = n())



# coding frequencies within motivation by data segments (table 2) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation"])
sum(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation"]))  
table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "demotivation"])
sum(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "demotivation"]))  

round(prop.table(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation"])),4) * 100
sum(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation"])) 

round(prop.table(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "demotivation"])),4) * 100
sum(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "demotivation"]))

round(prop.table(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation" | motivation$coding_round_2_final_coding == "demotivation"])),3)*100
sum(table(motivation$coding_round_4.2_final_coding_separate[motivation$coding_round_2_final_coding == "motivation" | motivation$coding_round_2_final_coding == "demotivation"]))



# coding frequencies by segments (table 2; motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

segmemts_motivation <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (M)`) %>%
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "source"))) %>%
  arrange(Category, Sub_category1, Sub_category2)

segmemts_motivation_total_sub  <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
         grepl("costs", coding_round_4.2_final_coding_separate) | 
         grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  group_by(Category, Sub_category1) %>%
  summarize(n = n(),
            `Segments (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category2 = "total") %>%
  filter(n > 1) %>%
  select(-c(n))
       
segmemts_motivation <- rbind(segmemts_motivation, segmemts_motivation_total_sub) %>%
  arrange(Category, Sub_category1, desc(Sub_category2))

segmemts_motivation_total  <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
           grepl("costs", coding_round_4.2_final_coding_separate) | 
           grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate)) %>%
  group_by(Category) %>%
  summarize(n = n(),
            `Segments (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category1 = "total", 
         Sub_category2 = "") %>%
  filter(n > 1) %>%
  select(-c(n)) 

segmemts_motivation <- rbind(segmemts_motivation, segmemts_motivation_total) %>%
  arrange(Category, Sub_category1, desc(Sub_category2)) %>%
  mutate(`Segments (M; in %)` = round(`Segments (M)`/segments$n[segments$coding_round_2_final_coding == "motivation"], 4) * 100) %>%
  arrange(Category, if_else(Sub_category1 == "total", "aaaa", Sub_category1), if_else(Sub_category2 == "total", "aaaa", Sub_category2))
rm(segmemts_motivation_total, segmemts_motivation_total_sub)  



# coding frequencies by segments (table 2; demotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

segmemts_demotivation <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (A)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate)),
         Sub_category2 = gsub("^[^/]+/", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  select(Category, Sub_category1, Sub_category2, `Segments (A)`) %>%
  mutate(Category = factor(Category, levels = c("ability", "value", "costs", "goals", "consequence", "source"))) %>%
  arrange(Category, Sub_category1, Sub_category2)

segmemts_demotivation_total_sub  <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
           grepl("costs", coding_round_4.2_final_coding_separate) | 
           grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (A)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  group_by(Category, Sub_category1) %>%
  summarize(n = n(),
            `Segments (A)` = sum(`Segments (A)`)) %>%
  mutate(Sub_category2 = "total") %>%
  filter(n > 1) %>%
  select(-c(n))

segmemts_demotivation <- rbind(segmemts_demotivation, segmemts_demotivation_total_sub) %>%
  arrange(Category, Sub_category1, desc(Sub_category2))

segmemts_demotivation_total  <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
           grepl("costs", coding_round_4.2_final_coding_separate) | 
           grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (A)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate)) %>%
  group_by(Category) %>%
  summarize(n = n(),
            `Segments (A)` = sum(`Segments (A)`)) %>%
  mutate(Sub_category1 = "total", 
         Sub_category2 = "") %>%
  filter(n > 1) %>%
  select(-c(n)) 

segmemts_demotivation <- rbind(segmemts_demotivation, segmemts_demotivation_total) %>%
  arrange(Category, Sub_category1, desc(Sub_category2)) %>%
  mutate(`Segments (A; in %)` = round(`Segments (A)`/segments$n[segments$coding_round_2_final_coding == "demotivation"], 4) * 100) %>%
  arrange(Category, if_else(Sub_category1 == "total", "aaaa", Sub_category1), if_else(Sub_category2 == "total", "aaaa", Sub_category2))
rm(segmemts_demotivation_total, segmemts_demotivation_total_sub)  



# coding frequencies by students (table 2; motivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

students_motivation <- motivation %>%
  filter(coding_round_2_final_coding == "motivation" & !is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_1_final_coding != "No") %>%
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
  summarise(`Students (M)` = sum(`Segments (M)`))

students_motivation_total_sub  <- motivation %>%
  filter(coding_round_2_final_coding == "motivation" & !is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
           grepl("costs", coding_round_4.2_final_coding_separate) | 
           grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  group_by(sno_student, Category, Sub_category1) %>%
  summarize(n = n(),
            `Segments (M)` = sum(`Segments (M)`)) %>%
  filter(n > 1) %>%
  mutate(`Segments (M)` = ifelse(`Segments (M)` > 1, 1, `Segments (M)`)) %>%
  group_by(Category, Sub_category1) %>%
  summarise(`Students (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category2 = "total")
students_motivation <- rbind(students_motivation, students_motivation_total_sub)

students_motivation_total  <- motivation %>%
  filter(coding_round_2_final_coding == "motivation" & !is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
           grepl("costs", coding_round_4.2_final_coding_separate) | 
           grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (M)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student, Category) %>%
  summarize(`Segments (M)` = sum(`Segments (M)`)) %>%
  mutate(`Segments (M)` = ifelse(`Segments (M)` > 1, 1, `Segments (M)`)) %>%
  group_by(Category) %>%
  summarise(`Students (M)` = sum(`Segments (M)`)) %>%
  mutate(Sub_category1 = "total", 
         Sub_category2 = "")
students_motivation <- rbind(students_motivation, students_motivation_total) %>% 
  arrange(Category, Sub_category1, desc(Sub_category2)) %>%
  mutate(`Students (M; in %)` = round(`Students (M)`/questionnaires$n[questionnaires$questionnaire == "motivation"], 4) * 100) %>%
  arrange(Category, if_else(Sub_category1 == "total", "aaaa", Sub_category1), if_else(Sub_category2 == "total", "aaaa", Sub_category2))
rm(students_motivation_total, students_motivation_total_sub)  
motivation_results <- merge(segmemts_motivation, students_motivation, by = c("Category", "Sub_category1", "Sub_category2"))
motivation_results <- motivation_results %>%  
    arrange(Category, if_else(Sub_category1 == "total", "aaaa", Sub_category1), if_else(Sub_category2 == "total", "aaaa", Sub_category2))



# coding frequencies by students (table 2; demotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

students_demotivation <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" & !is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_1_final_coding != "No") %>%
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
  summarise(`Students (D)` = sum(`Segments (D)`))

students_demotivation_total_sub  <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" & !is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
           grepl("costs", coding_round_4.2_final_coding_separate) | 
           grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (D)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate),
         Sub_category1 = gsub("/.*", "", gsub("^[^/]+/", "", coding_round_4.2_final_coding_separate))) %>%
  group_by(sno_student, Category, Sub_category1) %>%
  summarize(n = n(),
            `Segments (D)` = sum(`Segments (D)`)) %>%
  filter(n > 1) %>%
  mutate(`Segments (D)` = ifelse(`Segments (D)` > 1, 1, `Segments (D)`)) %>%
  group_by(Category, Sub_category1) %>%
  summarise(`Students (D)` = sum(`Segments (D)`)) %>%
  mutate(Sub_category2 = "total")
students_demotivation <- rbind(students_demotivation, students_demotivation_total_sub)

students_demotivation_total  <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" & !is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate) | 
           grepl("costs", coding_round_4.2_final_coding_separate) | 
           grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student, coding_round_4.2_final_coding_separate) %>%
  summarize(`Segments (D)` = n()) %>%
  mutate(Category = gsub("/.*", "", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student, Category) %>%
  summarize(`Segments (D)` = sum(`Segments (D)`)) %>%
  mutate(`Segments (D)` = ifelse(`Segments (D)` > 1, 1, `Segments (D)`)) %>%
  group_by(Category) %>%
  summarise(`Students (D)` = sum(`Segments (D)`)) %>%
  mutate(Sub_category1 = "total", 
         Sub_category2 = "")
students_demotivation <- rbind(students_demotivation, students_demotivation_total) %>% 
  arrange(Category, Sub_category1, desc(Sub_category2)) %>%
  mutate(`Students (D; in %)` = round(`Students (D)`/questionnaires$n[questionnaires$questionnaire == "demotivation"], 4) * 100) %>%
  arrange(Category, if_else(Sub_category1 == "total", "aaaa", Sub_category1), if_else(Sub_category2 == "total", "aaaa", Sub_category2))
rm(students_demotivation_total, students_demotivation_total_sub)  
demotivation_results <- merge(segmemts_demotivation, students_demotivation, by = c("Category", "Sub_category1", "Sub_category2"))
demotivation_results <- demotivation_results %>%  
  arrange(Category, if_else(Sub_category1 == "total", "aaaa", Sub_category1), if_else(Sub_category2 == "total", "aaaa", Sub_category2))

coding_results <- merge(motivation_results, demotivation_results, by = c("Category", "Sub_category1", "Sub_category2"), all = TRUE)






Studentfreq1 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "ability") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "ability")

Studentfreq2 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "costs (effort)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "costs (effort)")

Studentfreq3 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation" & !is.na(coding_round_2_final_coding)) %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value")

motivation2 <- motivation %>%
  select(sno_student, coding_round_1_final_coding, coding_round_2_final_coding)



motivation[, c("sno_student", )]


Studentfreq4 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("(positive)", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value (positive)")

Studentfreq5 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("value/utility", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility")

Studentfreq6 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (daily life)")

Studentfreq7 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (general)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (general)")

Studentfreq8 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (learning utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (learning utility)")

Studentfreq9 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (other)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (other)")

Studentfreq10 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (school utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (school utility)")

Studentfreq10.1 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (school utility)" | coding_round_4.2_final_coding_separate == "value/utility (learning utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (education utility)")

Studentfreq11 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (social utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (social utility)")

Studentfreq11.1 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)" | coding_round_4.2_final_coding_separate == "value/utility (social utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (non-education utility)")

Studentfreq11.2 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (daily/general utility)")

Studentfreq12 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "goals")

Studentfreq13 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "consequence") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "consequence")

Studentfreq14 <- motivation %>%
  filter(coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "source") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "source")

motivation_results <- rbind(Studentfreq1, Studentfreq2, Studentfreq3, Studentfreq4, Studentfreq5,
                            Studentfreq6, Studentfreq7, Studentfreq8, Studentfreq9, Studentfreq10,
                            Studentfreq10.1, Studentfreq11, Studentfreq11.1, Studentfreq11.2, 
                            Studentfreq12, Studentfreq13, Studentfreq14)
motivation_results <- motivation_results %>%
  arrange(sno_student) 

motivation_segmemts <- motivation_results %>%
  group_by(Category) %>%
  summarise(Segments = sum(total)) %>%
  mutate(`Segments (%)` = round(Segments / segments$n[segments$coding_round_2_final_coding == "motivation"], 4) * 100)
Total <- data.frame(Category = "Total", Segments = segments$n[segments$coding_round_2_final_coding == "motivation"])
Total <- Total %>%
  mutate(`Segments (%)` = 100)
motivation_segmemts <- rbind(motivation_segmemts, Total)

motivation_students <- motivation_results %>%
  arrange(sno_student) %>%
  mutate(total = ifelse(total > 0 ,1, 0)) %>%
  group_by(Category) %>%
  summarise(Students = n()) %>%
  mutate(`Students (%)` = round(Students / questionnaires$n[questionnaires$questionnaire == "motivation"], 4) * 100)
Total <- data.frame(Category = "Total", Students = questionnaires$n[questionnaires$questionnaire == "motivation"])
Total <- Total %>%
  mutate(`Students (%)` = 100)
motivation_segmemts <- rbind(motivation_students, Total)




rm(Studentfreq1, Studentfreq2, Studentfreq3, Studentfreq4, Studentfreq5, Studentfreq6, Studentfreq7,
   Studentfreq8, Studentfreq9, Studentfreq10, Studentfreq10.1, Studentfreq11, Studentfreq11.1,
   Studentfreq11.2, Studentfreq12, Studentfreq13, Studentfreq14)






# coding frequencies within motivation by student (table 2; demotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

Studentfreq1 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "ability") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "ability")

Studentfreq2 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(grepl("COSTS", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS")

Studentfreq3 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "cost (effort)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "cost (effort)")

Studentfreq4 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "COSTS (EMOTIONAL)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (EMOTIONAL)")

Studentfreq5 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "COSTS (OPPORTUNITY)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (OPPORTUNITY)")

Studentfreq6 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "COSTS (OUTSIDE)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (OUTSIDE)")

Studentfreq6.1 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "cost (effort)" | coding_round_4.2_final_coding_separate == "COSTS (OPPORTUNITY)" | coding_round_4.2_final_coding_separate == "COSTS (OUTSIDE)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS w/o EMO")

Studentfreq6.2 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "cost (effort)" | coding_round_4.2_final_coding_separate == "COSTS (EMOTIONAL)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (EFFORT | EMO")

Studentfreq6.3 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "COSTS (OPPORTUNITY)" | coding_round_4.2_final_coding_separate == "COSTS (OUTSIDE)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (OPPORTUNUTY | OUTSIDE")

Studentfreq7 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value")

Studentfreq8 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(grepl("(positive)", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value (positive)")

Studentfreq9 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(grepl("value/utility", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility")

Studentfreq10 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (daily life)")

Studentfreq11 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (general)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (general)")

Studentfreq12 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (other)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (other)")

Studentfreq13 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (school utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (school utility)")

Studentfreq14 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "VALUE/UTILITY (USELESS)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "VALUE/UTILITY (USELESS)")

Studentfreq14.1 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (school utility)" | coding_round_4.2_final_coding_separate == "VALUE/UTILITY (USELESS)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (education utility)")

Studentfreq14.2 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)" | coding_round_4.2_final_coding_separate == "value/utility (social utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (non-education utility)")

Studentfreq14.3 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (daily/general utility)")

Studentfreq14.4 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)" | coding_round_4.2_final_coding_separate == "VALUE/UTILITY (USELESS)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "VALUE/UTILITY (DAILY/GENERAL UTILITY/ USELESSNESS)")

Studentfreq15 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "goals")

Studentfreq16 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "consequence") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "consequence")

Studentfreq17 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation") %>%
  filter(coding_round_4.2_final_coding_separate == "source") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "source")

StudentfreqD <- rbind(Studentfreq1,
                      Studentfreq2,
                      Studentfreq3,
                      Studentfreq4,
                      Studentfreq5,
                      Studentfreq6,
                      Studentfreq6.1,
                      Studentfreq6.2,
                      Studentfreq6.3,
                      Studentfreq7,
                      Studentfreq8,
                      Studentfreq9,
                      Studentfreq10,
                      Studentfreq11,
                      Studentfreq12,
                      Studentfreq13,
                      Studentfreq14,
                      Studentfreq14.1,
                      Studentfreq14.2,
                      Studentfreq14.3,
                      Studentfreq14.4,                      
                      Studentfreq15,
                      Studentfreq16,
                      Studentfreq17)
StudentfreqD <- StudentfreqD %>%
  filter(sno_student %in% motivation$sno_student[motivation$questionnaire == "demotivation"]) %>%
  arrange(sno_student) 

StudentfreqD_statements <- StudentfreqD %>%
  group_by(Category) %>%
  summarise(Total = mean(total)) 

StudentfreqD_freq <- StudentfreqD %>%
  filter(sno_student %in% motivation$sno_student[motivation$questionnaire == "demotivation"]) %>%
  arrange(sno_student) %>%
  mutate(total = ifelse(total > 0 ,1, 0)) %>%
  group_by(Category) %>%
  summarise(Demotivation = n()) 

StudentfreqD_perc <- StudentfreqD_freq %>%
  mutate(Demotivation = round(Demotivation/length(unique(motivation$sno_student[motivation$questionnaire == "demotivation" & motivation$coding_round_1_final_coding != "No"])), 4) * 100)

rm(Studentfreq1, Studentfreq2, Studentfreq3, Studentfreq4, Studentfreq5, Studentfreq6, Studentfreq6.1,
   Studentfreq6.2, Studentfreq6.3, Studentfreq7, Studentfreq8, Studentfreq9, Studentfreq10,
   Studentfreq11, Studentfreq12, Studentfreq13, Studentfreq14, Studentfreq14.1, Studentfreq14.2,
   Studentfreq14.3, Studentfreq14.4, Studentfreq15, Studentfreq16, Studentfreq17)



# coding frequencies within motivation by student (table 2; motivation/ demotivation) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

Studentfreq1 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "ability") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "ability")

Studentfreq2 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(grepl("COSTS", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS")

Studentfreq3 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "cost (effort)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "cost (effort)")

Studentfreq4 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "COSTS (EMOTIONAL)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (EMOTIONAL)")

Studentfreq5 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "COSTS (OPPORTUNITY)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (OPPORTUNITY)")

Studentfreq6 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "COSTS (OUTSIDE)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS (OUTSIDE)")

Studentfreq7 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(grepl("value", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value")

Studentfreq8 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(grepl("(positive)", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value (positive)")

Studentfreq9 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(grepl("value/utility", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility")

Studentfreq10 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (daily life)")

Studentfreq11 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (general)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (general)")

Studentfreq12 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (learning utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (learning utility)")

Studentfreq13 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (other)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (other)")

Studentfreq14 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (school utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (school utility)")

Studentfreq15 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (social utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (social utility)")

Studentfreq16 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "VALUE/UTILITY (USELESS)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "VALUE/UTILITY (USELESS)")

Studentfreq16.1 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (school utility)" | coding_round_4.2_final_coding_separate == "VALUE/UTILITY (USELESS)" | coding_round_4.2_final_coding_separate == "value/utility (learning utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (education utility)")

Studentfreq16.2 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (school utility)" | coding_round_4.2_final_coding_separate == "value/utility (learning utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "VALUE/UTILITY (EDUCATION UTILITY 2)")

Studentfreq16.3 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)" | coding_round_4.2_final_coding_separate == "value/utility (social utility)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (non-education utility)")

Studentfreq16.4 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "value/utility (daily/general utility)")

Studentfreq17 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(grepl("goals", coding_round_4.2_final_coding_separate)) %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "goals")

Studentfreq18 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "consequence") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "consequence")

Studentfreq19 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "source") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "source")

Studentfreq20 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(grepl("(positive)", coding_round_4.2_final_coding_separate) | coding_round_4.2_final_coding_separate == "COSTS (EMOTIONAL)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "EMOTIONS")

Studentfreq21 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_4.2_final_coding_separate == "cost (effort)" | coding_round_4.2_final_coding_separate == "COSTS (OPPORTUNITY)" | coding_round_4.2_final_coding_separate == "COSTS (OUTSIDE)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "COSTS w/o EMO")

Studentfreq22 <- motivation %>%
  filter(coding_round_2_final_coding == "demotivation" | coding_round_2_final_coding == "motivation") %>%
  filter(coding_round_1_final_coding != "No") %>%
  filter(coding_round_4.2_final_coding_separate == "value/utility (daily life)" | coding_round_4.2_final_coding_separate == "value/utility (general)" | coding_round_4.2_final_coding_separate == "VALUE/UTILITY (USELESS)") %>%
  group_by(sno_student) %>%
  summarize(total = n()) %>%
  mutate(Category = "VALUE/UTILITY (DAILY/GENERAL UTILITY/ USELESSNESS)")

StudentfreqDM <- rbind(Studentfreq1,
                       Studentfreq2,
                       Studentfreq3,
                       Studentfreq4,
                       Studentfreq5,
                       Studentfreq6,
                       Studentfreq7,
                       Studentfreq8,
                       Studentfreq9,
                       Studentfreq10,
                       Studentfreq11,
                       Studentfreq12,
                       Studentfreq13,
                       Studentfreq14,
                       Studentfreq15,
                       Studentfreq16,
                       Studentfreq16.1,
                       Studentfreq16.2,  
                       Studentfreq16.3,
                       Studentfreq16.4,                       
                       Studentfreq17,
                       Studentfreq18,
                       Studentfreq19,
                       Studentfreq20,
                       Studentfreq21,
                       Studentfreq22)
StudentfreqDM <- StudentfreqDM %>%
  filter(sno_student %in% motivation$sno_student[motivation$questionnaire == "motivation" | motivation$questionnaire == "demotivation"]) %>%
  arrange(sno_student) 

StudentfreqDM_statements <- StudentfreqDM %>%
  group_by(Category) %>%
  summarise(Total = mean(total)) 

StudentfreqDM_freq <- StudentfreqDM %>%
  filter(sno_student %in% motivation$sno_student[motivation$questionnaire == "motivation" | motivation$questionnaire == "demotivation"]) %>%
  arrange(sno_student) %>%
  mutate(total = ifelse(total > 0 ,1, 0)) %>%
  group_by(Category) %>%
  summarise(`(DE)motivation` = n()) 

StudentfreqDM_perc <- StudentfreqDM_freq %>%
  mutate(`(DE)motivation` = round(`(DE)motivation`/length(unique(motivation$sno_student[(motivation$questionnaire == "motivation" | motivation$questionnaire == "demotivation") & motivation$coding_round_1_final_coding != "No"])), 4) * 100)

rm(Studentfreq1, Studentfreq2, Studentfreq3, Studentfreq4, Studentfreq5, Studentfreq6, Studentfreq7, Studentfreq8, Studentfreq9,
   Studentfreq10, Studentfreq11, Studentfreq12, Studentfreq13, Studentfreq14, Studentfreq15, Studentfreq16, Studentfreq16.1,
   Studentfreq16.2, Studentfreq16.3, Studentfreq16.4, Studentfreq17, Studentfreq18, Studentfreq19, Studentfreq20,
   Studentfreq21, Studentfreq22)



# coding frequencies within motivation by student (table 2; merging) --------------------------------- --------------------------------- --------------------------------- --------------------------------- 

Studentfreq_perc <- merge(StudentfreqM_perc, StudentfreqD_perc, by = "Category", all = TRUE)








