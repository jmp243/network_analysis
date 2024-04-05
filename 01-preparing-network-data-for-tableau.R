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
# library(rgexf)

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
# adding the new ID for source/ advisor name

Source_col <- d3 %>% 
  select(`Advisor Name`) %>% 
  distinct() %>% 
  mutate(Source = rownames(.))

d3 <- d3 %>% 
  left_join(Source_col)

edges <- d3 %>% 
  select(Source, new_id) %>% 
  # mutate(Source = rownames("Advisor Name")) %>% 
  rename(Target = new_id) %>% 
  distinct()
 
edges$Weight <- "1"
edges$Type <- "Directed" 

# write csv
write.csv(edges,"~/Documents/Trellis/Network_Analysis_Tableau/data/edges.csv")

# create nodes sheet
nodes <- d3 %>% 
  select(`Advisor Name`, owner_id, Source) %>% 
  unique()

nodes_gephi <- d3 %>% 
  select(owner_id, Source) %>% 
  rename(Id = Source) %>% 
  unique()

# write nodes csv
write.csv(nodes,"~/Documents/Trellis/Network_Analysis_Tableau/data/nodes.csv")
# write.csv(nodes2,"~/Documents/Trellis/Network_Analysis_Tableau/data/nodes2.csv")
write.csv(nodes_gephi,"~/Documents/Trellis/Network_Analysis_Tableau/data/nodes_gephi.csv")



# # reading in a GEPHI file
# # Accessing the path of the file
# fn    <- system.file(
#   "../gephi2.gexf", package = "rgexf"
# )
# gephi <- read.gexf(fn)


# bringing in edges from Gephi/xml/excel
gephi_edges <- read_csv("~/Documents/Trellis/Network_Analysis_Tableau/data/edges_from_xml.csv")
gephi_edges <- gephi_edges %>% 
  rename(Source = `Attribute:source`) %>% 
  rename(Index = `Attribute:id`) %>% 
  rename(Target = `Attribute:target`)
gephi_edges$Source <- as.character(gephi_edges$Source)


# merge tableau table to edges
student_name <- d3 %>% 
  select(new_id, student_name) %>% 
  rename(Target = new_id) 
student_name$Target <- as.numeric(student_name$Target)

# bringing in the X Target and Y Target csv
tableau_xy <- read_csv("~/Documents/Trellis/Network_Analysis_Tableau/data/gephi_output_from_xml.csv")
tableau_xy$Source <- as.character(tableau_xy$Source)

tableau_xy1 <- tableau_xy %>% 
  left_join(nodes) %>% 
  # select(-`Attribute:for`) %>% 
  mutate(base = 1) %>% 
  left_join(gephi_edges) %>% 
  left_join(student_name)

tableau_xy1$direction <- paste(tableau_xy1$Source, "->", tableau_xy1$Target)


# create a column named direction
# network_tableau2$direction <- paste(network_tableau2$Source, "->", network_tableau2$Target)
# network_tableau2 <- network_tableau2 %>% 
#   select(-`Attribute:value.1`) %>% 
#   distinct()

tableau_xy2 <- read_csv("~/Documents/Trellis/Network_Analysis_Tableau/data/gephi_output_from_xml.csv")
tableau_xy2$Source <- as.character(tableau_xy2$Source)

tableau_xy2 <- tableau_xy2 %>% 
  left_join(nodes) %>% 
  # select(-`Attribute:for`) %>% 
  mutate(base = 2) %>% 
  left_join(gephi_edges) %>% 
  left_join(student_name)

tableau_xy2$direction <- paste(tableau_xy2$Target, "->", tableau_xy2$Source)
tableau_table <- rbind(tableau_xy1, tableau_xy2)

# reduce the data size
network_tableau2 <- tableau_table %>% 
  filter(!is.na(Target) | Target != "") %>% 
  distinct()



# write name csv for the tableau table
write.csv(network_tableau,"~/Documents/Trellis/Network_Analysis_Tableau/data/network_tableau.csv")

write.csv(network_tableau2,"~/Documents/Trellis/Network_Analysis_Tableau/data/network_tableau2.csv")
# write.csv(nodes2,"~/Documents/Trellis/Network_Analysis_Tableau/data/nodes2.csv")
