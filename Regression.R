## REGRESSION

## PACKAGES
library(MASS)
library(dplyr)
library(ggplot2)
library(psych)
library(ppcor)

## DISABLE SCIENTIFIC NOTATION
options(scipen = 999)

# A researcher is interested in examining whether reading skills (READING)
  # at kindergarten entry is predicted by cognitive stimulation (COGSTIM) in the 
  # home environment and is concerned about the confounding influence of children’s
  # age when they start kindergarten (AGEENTRY).

#### Read in data
data_1 <- read.csv("ECLS-K.csv") 
head(data_1)

#### Variables
# READING = reading skills (DV)
# COGSTIM = cognitive stimulation (IV)
# AGEENTRY = are when children start kindergarten (IV)

# Calculate the regression equation predicting reading skills with cognitive
  # stimulation and age at kindergarten entry. Present the standardized and 
  # unstandardized regression equations.

# UNSTANDARDIZED
model_1 <-  lm(READING ~ COGSTIM + AGEENTRY, data= data_1) 
summary(model_1)
anova(model_1)
confint(model_1,level=0.95)
coefficients(model_1)
# The intercept is -3.59 and is the predicted level of reading skills (READING) when
  # cognitive stimulation (COGSTIM) and age at kindergarten entry (AGEENTRY) are zero. 
# A one unit increase in reading skills (READING) is associated with a 6.73 unit increase
  # in cognitive stimulation (COGSTIM) and a 0.29 unit increase in age at kindergarten 
  # entry (AGEENTRY).
# Y = -3.59 + 6.73X1 + 0.29X2
# For COGSTIM, t-value = 35.10 with a significant p-value of 0.0000000000000002.
# For AGEENTRY, t-value = 14.81 with a significant p-value of 0.0000000000000002. 

# STANDARDIZED
data_1 <- data_1 %>%
  mutate(zREADING = scale(READING))
data_1 <- data_1 %>%
  mutate(zCOGSTIM = scale(COGSTIM))
data_1 <- data_1 %>%
  mutate(zAGEENTRY = scale(AGEENTRY))
head(data_1)

model_1_z <- lm(zREADING ~ zCOGSTIM + zAGEENTRY, data= data_1) 
summary(model_1_z) 
anova(model_1_z)
confint(model_1_z, level=0.95) 
coefficients(model_1_z)
# The intercept is 4.961146e-16 and is the predicted level of reading skills when
  # cognitive stimulation (COGSTIM) and age at kindergarten entry (AGEENTRY) are zero. 
# A one standard deviation unit increase in reading skills is associated with a 
  # 0.40 unit increase in cognitive stimulation and a 0.17 unit increase in age at 
  # kindergarten entry.
# Y = 0.40X1 + 0.17X2
# For COGSTIM, t-value = 35.10 with a significant p-value of 0.0000000000000002.
# For AGEENTRY, t-value = 14.81 with a significant p-value of 0.0000000000000002.

# What proportion of the total variance in reading skills is explained by 
  # cognitive stimulation AND age at kindergarten entry?
  # adjusted R^2 so either is fine
m1 <- lm(READING ~ COGSTIM + AGEENTRY, data= data_1) 
summary(m1)
m1_r<-summary(m1) 
m1_r$r.squared
m1_r$r.squared - summary(lm(READING~ READING, data=data_1))$r.squared
# 0.18 (18%) represents the total variance in reading skills (READING) that is 
  # explained by both cognitive stimulation (COGSTIM) and age at kindergarten
  # entry (AGEENTRY).

# Is the proportion in c significantly different from zero?
resid(model_1) 
model_1.res <- resid(model_1) 
plot(data_1$READING, model_1.res)

m1 <- lm(READING ~ COGSTIM + AGEENTRY, data= data_1) 
summary(m1)
m1_r<-summary(m1) 
m1_r$r.squared
m1_r$r.squared - summary(lm(READING~ READING, data=data_1))$r.squared 
# R^2 = variance explained by the model/total variance
# Yes 0.18 is significantly different from zero because our F statistic is above
# zero [F(2,6396)=720.7] and our p-value is significant (p=0.00000000000000022).