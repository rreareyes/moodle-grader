# Load packages -----------------------------------------------------------
library(tidyverse)

# Define base directories -------------------------------------------------
folder_root <- dirname(rstudioapi::getActiveDocumentContext()$path)
folder_data <- file.path(folder_root, "Data")

# Load the student list ---------------------------------------------------
lab_list <- read.csv(file = file.path(dirname(folder_root), 
                                      "biology153_lab_roster.csv"))

lab_names <- c("Full name", "Section")
colnames(lab_list) <- lab_names  

hw_list <-  read.csv(file = file.path(folder_data, "ws01_grading.csv"))

hw_names <- c("Identifier", "Full name", "Email address", "Status", 
              #"Allow submissions from", "Due date", "Cut-off date", 
              "Grade", "Maximum Grade", "Grade can be changed", 
              "Last modified (submission)", "Last modified (grade)",
              "Feedback comments")

colnames(hw_list) <- hw_names


# Clean dataset and export ------------------------------------------------
identifier_list <- hw_list %>% 
  select(1:2) %>% 
  mutate(Identifier = str_remove(Identifier, "Participant")) %>% 
  full_join(lab_list) %>% 
  na.omit() %>% 
  filter(Section %in% c(28, 41)) %>% 
  arrange(Section, "Full Name")

write.csv(identifier_list,
          file = file.path(folder_root, "ws01_identifiers.csv"),
          row.names = F)
