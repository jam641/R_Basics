# Cog Psych Lab: Paper 2 Demographics
  # data as of 10/29/24

# Adding a "#" puts in a comment - comments are not actual code. You can use
  # comments to write text to explain what you are doing.
# I am typing in the main script. The panel below is your "Console" that will 
  # show output of what you write here. 

# LOAD PACKAGES (these allow you to perform certain actions in R)
# install.packages("tidyverse")
# install.packages("psych")
library(tidyverse) # call packages using "library()"
library(psych)
# the console below spits out the output of the code you write here

# SET YOUR WORKING DIRECTORY (Where is your data located?)
# go to "Session"
# go to "Set Working Directory"
# go to "Choose Directory"
# setwd("~/Desktop")

# READ IN DATA
demographics <- read.csv("Paper2_Demographics.csv")
head(demographics) # view the first six lines of your data
nrow(demographics) # number of rows in your data, 50 rows

# DEMOGRAPHICS
  # Let's look at demographics, one column at a time

# Eighteen or Older
demographics %>% 
  group_by(Eighteen_or_Older) %>% # group participants by Eighteen_or_Older column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option

# Fluent in English
demographics %>% 
  group_by(Fluent_in_English) %>% # group participants by Fluent_in_English column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option

# Cognitive Psychology Student
demographics %>% 
  group_by(CogPsych_Student) %>% # group participants by CogPsych_Student column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option

# Age
demographics %>% 
  group_by(Age) %>% # group participants by Age column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option

# Gender
demographics %>% 
  group_by(Gender) %>% # group participants by Gender column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option

# Race and Ethnicity
demographics %>% 
  group_by(Race_Ethnicity) %>% # group participants by Race_Ethnicity column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option

# Academic Year
demographics %>% 
  group_by(Academic_Year) %>% # group participants by Academic_Year column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option

# Psychology Major
demographics %>% 
  group_by(Psych_Major) %>% # group participants by Psych_Major column
  summarise(percent = 100 * n() / nrow(demographics)) # % each answer option
