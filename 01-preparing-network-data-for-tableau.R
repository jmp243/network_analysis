# Preparing Network data in Tableau for Salesforce
# Jung Mee Park
# jmpark@arizona.edu
# 2024-03-28

# load libraries
library(tidyverse)
library(sentimentr)
library(readr)
library(knitr)
library(stringr)
library(lubridate)

# read in data
initial_report_SF <- read_csv("~/Documents/Trellis/Network_Analysis_Tableau/data/initial-report-SF2.csv")

initial_report_SF <- initial_report_SF %>% 
  filter(!is.na(`Case ID`) | `Case ID` != "") %>% 
  filter(!is.na(`Contact EmplID`) | `Contact EmplID` != "")
  
# masking the emplid
all_possible_ids <- c(initial_report_SF$`Contact EmplID`) %>% unique() %>% na.omit()

random_ids <- sample(1:53013, length(all_possible_ids), replace = F) %>% as.character()

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

# rename owner id
d2 <- d2 %>% 
  rename(owner_id = `Owner: Owner ID`) %>% 
  rename(new_id = new)

# create a new column with student name
d2$student_name <-str_replace_all(d2$`Appointment Name`, '(.*?)- (.*?)', '')

# add an index column
d2$Index <- 1:nrow(d2)

# create a date
d2$AppointmentDate = mdy(d2$`Appointment Date`)

# filter the data based on date
d3 <- d2 %>% 
  filter(AppointmentDate >= "2024-03-01")

# random sample of a few cases

# write csv
write.csv(d3,"~/Documents/Trellis/Network_Analysis_Tableau/data/output-report-SF.csv")

# more than one Edge
# create an edges csv
edges <- d3 %>% 
  select(`Advisor Name`, new_id) %>% 
  rename(Source = "Advisor Name") %>% 
  rename(Target = new_id)

edges$Weight <- "1"
edges$Type <- "Directed" 

# write csv
write.csv(edges,"~/Documents/Trellis/Network_Analysis_Tableau/data/edges.csv")

# create nodes sheet
nodes <- d3 %>% 
  select(owner_id, `Advisor Name`) %>% 
  unique()
# write nodes csv
write.csv(nodes,"~/Documents/Trellis/Network_Analysis_Tableau/data/nodes.csv")
