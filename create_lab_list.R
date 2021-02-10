# This script assigns each student in the class with their respective
# lab section, and adds their unique Moodle ID for the particular assignment.
# 
# You need to first download the general roster of the course (under the 
# "Grades" section in Moodle) and then run the script "extract_student_roster.R" 
# to format the list so we can use it here. You only need to run this first
# step one time. Also, you need to download the grading worksheet for the 
# specific assignment you are grading.
# 
# You will need to run this script for every assignment you want to grade, 
# since the Moodle ID is unique for each assignment (don't ask me why, 
# Moodle moves in mysterious ways).
# 
# Be sure to change the name of the source files (the general roster and the
# grading worksheet) as well as the output (the identifiers for each student).
# Also, change the lab sections to filter the final list, unless you want to
# have the whole class in your list (I can feel the rebel in you, but I don't
# recommend this)

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
  filter(Section %in% c(28, 41)) %>% #THESE ARE YOUR LABS!
  arrange(Section, "Full Name")

write.csv(identifier_list,
          file = file.path(folder_root, "ws01_identifiers.csv"),
          row.names = F)
