# Cog Psych Lab: Paper 2 Statistics
  # data as of 10/29/24

# Adding a "#" puts in a comment - comments are not actual code. You can use
# comments to write text to explain what you are doing.
# I am typing in the main script. The panel below is your "Console" that will 
# show output of what you write here.

# LOAD PACKAGES (these allow you to perform certain actions in R)
# install.packages("tidyverse")
# install.packages("psych)
library(tidyverse) # call packages using "library()"
library(psych)
# the console below spits out the output of the code you write here

# SET YOUR WORKING DIRECTORY (Where is your data located?)
# go to "Session"
# go to "Set Working Directory"
# go to "Choose Directory"
# setwd("~/Desktop")

# READ IN DATA
final_test <- read.csv("Paper2_FinalTest.csv")
head(final_test) # view the first six lines of your data
nrow(final_test) # number of rows in your data, 50 rows

# PIVOT TO LONG FORMAT FOR DATA ANALYSIS
final_test %>% pivot_longer(cols=c(Test1:Test10),
                            names_to="Question",
                            values_to="Response") -> final_test_long
head(final_test_long)
nrow(final_test_long) # 500
# we can now see the responses for each of the ten test questions
  # 1 indicates the correct answer (aka answer option "a")
  # 2, 3, and 4 are wrong answers (aka answer options "b", "c", and "d")
# let's code that (what is right and what is wrong)

# RIGHT OR WRONG IFELSE
final_test_long %>% mutate(Correct = ifelse(Response == 1, 1, 0)) -> final_test_long
# create a new column called Correct based off of the Response column
# if the response is "1" give them "1" (correct)
# if the response is anything other than 1, give them a "0" (incorrect)
head(final_test_long)

# DESCRIPTIVE STATISTICS (Means and Standard Deviations)
  # Mean and SD by Group
final_test_long %>% group_by(Group) %>% summarise(accuracy = mean(Correct),
                                                  SD = sd(Correct))
# INFERENTIAL STATISTICS  (ANOVA)
model <- aov(Correct ~ # DV: performance on final test questions
             Group, # IV: Group
             data = final_test_long) # data frame
summary(model)

