## TWO WAY ANOVA

# DISABLE SCIENTIFIC NOTATION
options(scipen = 999) 

# PACKAGES
library(tidyverse)
library(DescTools)
library(effectsize)
library(emmeans)
library(car)
library(reghelper)

# VARIABLES & STUDY PREMISE
# 120 participants, 40/group, between-subjects design
# DV - performance on final retrieval quiz
# VariableA (IV/PV) - level of trained expertise (categorical, binomial)
  # A1 - Novice (lower mean)
  # A2 - Expert (higher mean)
# VariableB (IV/PV) - study strategy intervention (categorical, binomial)
  # B1 - Restudy Intervention (lower mean)
  # B2 - Retrieval Intervention (higher mean)

# READ IN DATA
final <- read.csv("finalexam-2way.csv")
head (final)
view(final)

# TREAT AS FACTORS
final <- read.csv('finalexam-2way.csv', stringsAsFactors = TRUE)
head(final)

### MODEL BUILDING ONE-WAY ANOVAS FOR SANITY CHECK ###
# ONE-WAY ANOVA VARIABLE_A
# BUILD MODEL
model_a <- lm(DV~ 1 + VariableA, data=final)
# ANOVA OUTPUT
anova(model_a)
# No, there is not significant difference between groups.
# Same performance regardless of assigned expertise.

# ONE-WAY ANOVA VARIABLE_B
# BUILD MODEL
model_b <- lm(DV~ 1 + VariableB, data=final)
# ANOVA OUTPUT
anova(model_b)
# Yes, there is a significant difference between groups.
# Different performance regarding study strategy intervention.

### MODELBUILDING TWO-WAY ANOVA OUTPUT, GRAPH TO VISUALIZE, AND SS###
model_1 <- lm(DV ~ VariableA*VariableB , data=final)

# GRAPH
final %>% group_by(VariableA, VariableB) %>% 
  summarize(DV_M=mean(DV)) %>%
  ggplot(aes(x=VariableB, y=DV_M, color=VariableA, group=VariableA)) +   
  geom_point() + geom_line()
# PERFORMANCE ON FINAL QUIZ Z-SCORES
# A2B2 best (Expert, Retrieval)
# A1B2 second (Novice, Retrieval)
# A1B1 third (Novice, Restudy)
# A2B1 last (Expert, Restudy)

# Type III Sums of Squares:
joint_tests(model_1)
# Type III Sums of Squares is most appropriate to use because Type III Sums of 
# Squares weights all cells equally regardless of sample size.

# SIMPLE EFFECTS FOR TWO-WAY ANOVA
joint_tests(model_1, by='VariableA')
# Simple effect of level of trained expertise: "novice." restudy versus retrieval
# A1 across B1 and B2
# F(1,116) = 0.95, p > .05 # among novices, effect of variable B
# Simple effect bv level of trained expertise: "expert." restudy versus retrieval
# A2 across B1 and B2
# F(1,116) = 32.72, p < .001
joint_tests(model_1, by='VariableB')
# Simple effect of study strategy intervention: restudy "novice" versus "expert"
# B1 across A1 and A2
# F(1,116) = 1.91, p > .05
# Simple effect of study strategy intervention: retrieval "novice" versus "expert"
# B2 across A1 and A2
# F(1,116) = 11.30, p < .01

# CONTRASTS VARIABLE A
contrasts(final$VariableA) # 0 = novice,1 = expert)
contrasts(final$VariableA) <- cbind(c(.5, -.5))
colnames(contrasts(final$VariableA)) <- c('Restudy versus Retrieval on Expertise')
contrasts(final$VariableA) # 0.5 = novice, -0.5 = expert
model_x <- lm(DV~ 1 + VariableA, data=final)
summary(model_x) # first A1 across B, then A2 across B

# CONTRASTS VARIABLE B
contrasts(final$VariableB) # 0 = restudy,1 = retrieval)
contrasts(final$VariableB) <- cbind(c(.5, -.5))
colnames(contrasts(final$VariableB)) <- c('Novice versus Expert on Study Strategy')
contrasts(final$VariableB) # 0.5 = restudy, -0.5 = retrieval
model_y <- lm(DV~ 1 + VariableB, data=final)
summary(model_y) # first B1 across A, then B2 across A

# TUKEY HSD POST HOC TEST
TukeyHSD(aov(model_1))
# ANCOVA
model_c <- lm(DV ~ VariableA + VariableB, data=final) 
# RESULTS
Anova(model_c, type='III')
# ESTIMATED MARGINAL MEANS
emmeans(model_c, ~VariableA)
emmeans(model_c, ~VariableB)
# MODERATION
model_d <- lm(DV ~ VariableA*VariableB, data=final)
# RESULTS
Anova(model_d, type='III')
# SIMPLE SLOPES
simple_slopes(model_d)
