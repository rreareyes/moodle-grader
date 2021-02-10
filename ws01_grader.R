# This script creates a pdf report based on a qualtrics rubric. It has to be
# tailored for the specific assignment and the number and name of the questions
# on it. The naming for the feedback files follows the guidelines from Moodle
# that allow to add them to a zip file and then upload in one go. It needs this
# naming convention so Moodle can recognize to which student it belongs, since
# it has embeded the student ID for the assignment.
# 
# Additionally, it exports a .csv file with the grades from each student in the
# correct format to upload as the grading worksheet in Moodle. The naming 
# convention should be:
# First Last_assignmentID_assignsubmission_file_whatevernameyouwantforthefile.pdf
# Ramiro Rea_8818222_assignsubmission_file_HW1_grade.pdf (or docx, xlsx)

# Get the base files ####
library(tidyverse)
library(knitr)
library(kableExtra)

# Base directories --------------------------------------------------------
folder_root   <- dirname(rstudioapi::getActiveDocumentContext()$path)
folder_data   <- file.path(folder_root, "Data")
folder_grades <- file.path(folder_root, "Feedback")

# Load the survey data ----------------------------------------------------
raw_survey <- read.csv(file      = file.path(folder_data, "ws01_rubric.csv"), 
                       header    = T, 
                       skip      = 2,
                       row.names = NULL, 
                       colClasses = c("NULL", "NULL", "NULL", "NULL", "NULL",
                                      "NULL", "NULL", "NULL", "NULL", "NULL",
                                      "NULL", "NULL", "NULL", "NULL", "NULL",
                                      "NULL", "NULL", #columns that we don't want
                                      "factor", 
                                      "factor", "factor", #one factor per lab
                                      "character", "factor",
                                      "factor", "character", "character", "numeric", #one row per question
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "factor", "character", "character", "numeric",
                                      "character")) #final comments 


col_names <- c("lab", 
               "id_28", "id_41", #add the ids for your labs here
               "initials", "grader",
               "q02a", "q02a_part", "q02a_inc", "q02a_points", "q02b", "q02b_part", "q02b_inc", "q02b_points",
               "q03a", "q03a_part", "q03a_inc", "q03a_points", "q03b", "q03b_part", "q03b_inc", "q03b_points",
               "q04a", "q04a_part", "q04a_inc", "q04a_points", "q04b", "q04b_part", "q04b_inc", "q04b_points",
               
               "q05a", "q05a_part", "q05a_inc", "q05a_points", "q05b", "q05b_part", "q05b_inc", "q05b_points",
               "q05c", "q05c_part", "q05c_inc", "q05c_points", "q05d", "q05d_part", "q05d_inc", "q05d_points",
               "q05e", "q05e_part", "q05e_inc", "q05e_points", "q05f", "q05f_part", "q05f_inc", "q05f_points",
               
               "q06a", "q06a_part", "q06a_inc", "q06a_points", 
              
               "Comments")

colnames(raw_survey) <- col_names


# Get IDs assigned by Moodle for this quiz --------------------------------
grading_sheet <- read.csv(file = file.path(folder_data, "ws01_grading.csv"))

grading_names <- c("Identifier", "Full name", "Email address", "Status", 
                   #"Allow submissions from", "Due date", "Cut-off date", 
                   "Grade", "Maximum Grade", "Grade can be changed", 
                   "Last modified (submission)", "Last modified (grade)",
                   "Feedback comments")

colnames(grading_sheet) <- grading_names


identifiers <- grading_sheet %>% 
  select("id" = 1, "full_name" = 2) %>% 
  mutate(id = str_remove(id, "Participant "))

# Create reference for question names and values --------------------------
point_key <- tibble("question" = c("q02a", "q02b", "q03a", "q03b", "q04a", "q04b",
                                   "q05a", "q05b", "q05c", "q05d", "q05e", "q05f",
                                   "q06a"),
                    "total" = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                                1, 1, 1, 1, 1, 1, 
                                1)
                    )

# Create final comments for students with little feedback -----------------
good_comments <- c("Great job!", "Everything looks fine, good job!", "Nice work!",
                   "Nothing to correct, good work!", "Your assignment has no issues, good job!",
                   "All good, no need to correct anything", "Excellent job!",
                   "Everything runs without issues, good work!", "Nice job! Everything looks good.",
                   "Great work! Nothing to correct here.", "Good work! Evertything runs as expected.",
                   "Your homework looks good, nothing to correct here. Good job!")

decent_comments <- c("Great job!", "Only minor changes needed, good job!", "Nice work!",
                     "Your assignment has no major issues, good job!",
                     "All good, just small things to fix. Good job!", 
                     "Everything runs without major issues, good work!", 
                     "Nice job! Everything looks good.",
                     "Great work! Nothing mejor to correct here.", "Good work! Evertything runs as expected, only minor issues in some areas.",
                     "Your homework looks good, just fix the small issues pointed out. Good job!")

# Extract feedback and points ---------------------------------------------
valence_data <- raw_survey %>% 
  pivot_longer(cols = c("q02a", "q02b", "q03a", "q03b", "q04a", "q04b",
                        "q05a", "q05b", "q05c", "q05d", "q05e", "q05f",
                        "q06a"), 
               names_to = "question",
               values_to = "valence") %>% 
  select(c(lab:grader, question, valence, Comments)) %>% 
  unite("id", id_28:id_41, na.rm = T, sep = "")

partial_feedback_data <- raw_survey %>% 
  pivot_longer(cols = c("q02a_part", "q02b_part", "q03a_part", "q03b_part", "q04a_part", "q04b_part",
                        "q05a_part", "q05b_part", "q05c_part", "q05d_part", "q05e_part", "q05f_part",
                        "q06a_part"), 
               names_to = "question",
               values_to = "partial") %>% 
  select(c(lab:grader, question, partial, Comments)) %>% 
  mutate(question = str_remove(question, "_part")) %>% 
  unite("id", id_28:id_41, na.rm = T, sep = "")

incorrect_feedback_data <- raw_survey %>% 
  pivot_longer(cols = c("q02a_inc", "q02b_inc", "q03a_inc", "q03b_inc", "q04a_inc", "q04b_inc",
                        "q05a_inc", "q05b_inc", "q05c_inc", "q05d_inc", "q05e_inc", "q05f_inc",
                        "q06a_inc"), 
               names_to = "question",
               values_to = "incorrect") %>% 
  select(c(lab:grader, question, incorrect, Comments)) %>% 
  mutate(question = str_remove(question, "_inc")) %>% 
  unite("id", id_28:id_41, na.rm = T, sep = "")

points_data <- raw_survey %>% 
  pivot_longer(cols = c("q02a_points", "q02b_points", "q03a_points", "q03b_points", "q04a_points", "q04b_points",
                        "q05a_points", "q05b_points", "q05c_points", "q05d_points", "q05e_points", "q05f_points",
                        "q06a_points"), 
               names_to = "question",
               values_to = "points") %>% 
  select(c(lab:grader, question, points, Comments)) %>% 
  mutate(question = str_remove(question, "_points")) %>% 
  unite("id", id_28:id_41, na.rm = T, sep = "")


# Create final dataset ----------------------------------------------------
grade_data <- full_join(valence_data, partial_feedback_data) %>% 
  full_join(incorrect_feedback_data) %>% 
  full_join(points_data) %>% 
  full_join(identifiers) %>% 
  full_join(point_key) %>% 
  unite("feedback", c(valence, partial, incorrect), na.rm = T, sep = " ") %>% 
  mutate(question = str_remove(question, "q")) %>% 
  select(full_name, initials, id, lab, grader, question, feedback, points, total, Comments) %>% 
  filter(!is.na(question))

export_data <- grade_data %>% 
  mutate(x = "Participant") %>% 
  unite("Identifier", c(x, id), sep = " ") %>% 
  group_by(Identifier, full_name) %>% 
  summarise(Grade = sum(points)) %>% 
  select(-full_name)

students_graded <- unique(export_data$Identifier)

grading_export <- grading_sheet %>% 
  filter(Identifier %in% students_graded) %>% 
  select(-Grade) %>% 
  full_join(export_data, by = "Identifier") %>% 
  select(Identifier:Status, Grade, everything()) %>% 
  mutate(`Feedback comments` = "")

write.csv(grading_export, 
          file = file.path(folder_root, "ws01_grades.csv"), 
          row.names = F)

# Create feedback files ---------------------------------------------------
student_IDs   <- unique(grade_data$id)
nStudents     <- length(student_IDs)
student_names <- unique(grade_data$full_name)

for (iStudent in 1:nStudents) {

  scores <- filter(grade_data, id == student_IDs[iStudent]) %>%
    select(-c(id, initials, lab, grader))

  
  detailed_grade <- scores %>% 
    select(-c(full_name, Comments)) %>% 
    rename("Question" = question,
           "Feedback" = feedback,
           "Points Obtained" = points,
           "Points Possible" = total)
  
  points_obtained <- scores %>% 
    group_by(full_name) %>% 
    summarise("Points Obtained" = sum(points),
              "Total Possible" = sum(total)) %>% 
    mutate("Grade" = `Points Obtained`*100/`Total Possible`) %>% 
    rename("Name" = "full_name")
  
  additional_comments <- scores %>% 
    select("Final comments" = Comments) %>% 
    unique()
  
  if (additional_comments$`Final comments`[1] == "" & points_obtained$Grade == 100) {
    
    additional_comments$`Final comments`[1] <- sample(good_comments, 1)
    
    } else if (additional_comments$`Final comments`[1] == "" & points_obtained$Grade >= 80) {
    
    additional_comments$`Final comments`[1] <- sample(decent_comments, 1)
    
    } else if (additional_comments$`Final comments`[1] == "" & points_obtained$Grade < 80) {
    
    additional_comments$`Final comments`[1] <- "Don't hessitate in contacting me if you are struggling with any of the topics from the class or programming in R"
    
  }
  
  # Use the naming scheme needed to upload in moodle
  rmarkdown::render(envir = new.env(),
                    input         = file.path(dirname(folder_root), 
                                              "feedback_template.Rmd"),
                    output_format = "pdf_document",
                    output_file   = paste(student_names[iStudent], 
                                          student_IDs[iStudent], 
                                          "assignsubmission_file", 
                                          student_names[iStudent], 
                                          "ws01_grade_",
                                          sep = "_"),
                    output_dir    = folder_grades)
}



