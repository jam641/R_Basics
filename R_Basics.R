## R BASICS

## PACKAGES
library(psych)
library(tidyverse)
library(nycflights13)

## DATA
glimpse(flights)
head(flights) # first 6 rows
head(flights, 20) # first 20 rows 

class(flights) # the "kind" of variable: character, factor, numeric, integer 

view(flights) # full data frame
view(cbind(flights$dep_time, flights$dep_delay)) #if only interested in a few variable 

## FILTER
flights %>%
  select(-carrier) %>% # select all variables except carrier THEN
  filter(dest == "ORD") # only include observations whose destination is ORD

flights %>% filter(month == 1, day == 1) # all flights on Jan 1st 

jan1 <- flights %>% filter(month == 1, day == 1) # save in new data frame

(dec25 <- flights %>% filter(month == 12, day == 25)) # save & print 

flights %>% filter(month == 11 | month == 12) # either Nov or Dec

flights %>% filter(!(arr_delay > 120 | dep_delay > 120)) # flights NOT delayed by >2 hr

flights %>% filter(month >= 7 & month <= 9) # departed in summer (July, August, and September)

flights %>% filter((dest == 'IAH' | dest == 'HOU')) # flew to Houston (IAH or HOU)

## ARRANGE
flights %>% arrange(desc(dep_delay)) # arrange by descending order

## SELECT
flights %>% select(year, month, day)

flights %>% select(year:day) # select all columns between year and day (inclusive) 

flights %>% select(-(year:day)) # select all columns except those from year to day (inclusive) 

## SUMMARIZE
flights %>% summarise(delay = mean(dep_delay, na.rm = TRUE)) # removes NA prior to computation

## MUTATE
flights_sml %>% mutate(gain = dep_delay - arr_delay, hours = air_time / 60, 
         gain_per_hour = gain / hours) # create new variables
flights_sml %>%
  mutate(gain = dep_delay - arr_delay, 
         hours = air_time / 60, 
         gain_per_hour = gain / hours, 
         fast_plane = ifelse(gain_per_hour > 1, 1, 0)) # >1 code as 1, <1 code as 0

## DESCRIPTIVE STATISTICS
# Introduction to  dplyr for Faster Data Manipulation in R
  # Five basic verbs: `filter`, `select`, `arrange`, `summarise` (plus `group_by`),
    # `mutate`
  # Pick observations by their values (filter()).
  # Reorder the rows (arrange()).
  # Pick variables by their names (select()).
  # Create new variables with functions of existing variables (mutate()).
  # Collapse many values down to a single summary (summarise()).
  # These can all be used in conjunction with group_by() which changes the scope 
    # of each function 
  # from operating on the entire dataset to operating on it group-by-group.

describe(flights$dep_time)
mean(flights$dep_time, na.rm = TRUE)
median(flights$dep_time, na.rm = TRUE)
sd(flights$dep_time, na.rm = TRUE)