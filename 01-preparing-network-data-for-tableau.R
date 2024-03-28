# Preparing Network data in Tableau for Salesforce
# Jung Mee Park
# jmpark@arizona.edu
# 2024-03-28

# load libraries
library(tidyverse)
library(sentimentr)
library(readr)
library(knitr)

# read in data
initial_report_SF <- read_csv("~/Documents/Trellis/Network_Analysis_Tableau/data/initial-report-SF.csv")

# masking the emplid
all_possible_ids <- c(initial_report_SF$`Contact EmplID`) %>% unique() %>% na.omit()

random_ids <- sample(100:12067, length(all_possible_ids), replace = F) %>% as.character()

id_dictionary <- cbind(all_possible_ids, random_ids) %>% `colnames<-`(c("old", "new")) %>% as.data.frame()

id_dictionary %>% knitr::kable(format = "html") %>% kableExtra::kable_styling(full_width = T)

# adding the new ID
id_dictionary <- id_dictionary %>% rename(student_id = old)

initial_report_SF <- initial_report_SF %>% 
                        rename(student_id = `Contact EmplID`) 
initial_report_SF$student_id <- as.character(initial_report_SF$student_id)
d <- initial_report_SF %>% left_join(id_dictionary, by = "student_id") 

# d %>% knitr::kable(format = "html") %>% kableExtra::kable_styling(full_width = T)

# filter out cases missing a student ID
d2 <- d %>% 
  filter(!is.na(student_id) | student_id != "")
