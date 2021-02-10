
# Load packages -----------------------------------------------------------
library(tidyverse)

# Define base directories -------------------------------------------------
folder_root   <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Load the student list ---------------------------------------------------
raw_list <- read.csv(file = file.path(folder_root, "biology153_roster.csv"))

# Clean dataset and export ------------------------------------------------
student_list <- raw_list %>% 
  select("first" = 1, "last" = 2, "section" = 9) %>% 
  unite("name", 1:2, sep = " ") %>% 
  mutate(section = str_extract(section, "(?<=Lab)(.*)(?=\\sBIOLOGY)")) %>% 
  arrange(section)

write.csv(student_list, 
          file = file.path(folder_root, "biology153_lab_roster.csv"),
          row.names = F)