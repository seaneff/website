#####################################################################
## load required libraries ##########################################
#####################################################################

#install.packages("ggplot2")
#install.packages("lme4")
#install.packages("lmerTest")
library(ggplot2)
library(lme4)
library(lmerTest)

#####################################################################
## set working directory and load data ##############################
#####################################################################

## http://web.stanford.edu/class/psych252/section_2013/Mixed_models_tutorial.html

## Methodological Reference: Steph Gagnon, Lauren Howe, Micael Waskom 
## Stanford Pysch 252 materials available online: http://web.stanford.edu/class/psych252/section_2013/Mixed_models_tutorial.html

## Data Reference:
## Winter and Gawunder (2012): The phonetic profile of Korean formal and informal speech registers
## http://www.bodowinter.com/papers/winter_grawunder_formality.pdf

setwd("~/Documents/Website/politeness")
polite <- read.csv("polite.csv")

#####################################################################
## Format Data ######################################################
#####################################################################

## Frequency should be a numeric variable
polite$frequency <- as.numeric(as.character(polite$frequency))

## Attitute: polite or informal
polite$attitude <- as.character(polite$attitude)
polite$attitude[polite$attitude == " pol"] <- "formal"
polite$attitude[polite$attitude == " inf"] <- "informal"
polite$attitude <- factor(polite$attitude, levels = c("formal", "informal"))

## Gender: male or female
polite$gender <- as.character(polite$gender)
polite$gender[polite$gender == " F"] <- "female"
polite$gender[polite$gender == " M"] <- "male"
polite$gender <- factor(polite$gender, levels = c("male", "female"))

#####################################################################
## Exploratory Data Analysis ########################################
#####################################################################

ggplot(data = polite, aes(attitude, frequency)) +
  geom_boxplot(col = "#084081", fill = "#4eb3d3") +
  facet_grid(~subject) +
  xlab ("Attitude") +
  ylab("Pitch") +
  ggtitle("Pitch vs. Attitute in Korean Language Speakers")

#####################################################################
## Fit a Random Intercept Model (Subject) ###########################
#####################################################################

ri_subject <-  lmer(frequency ~ attitude + (1 | subject), data = polite)
anova(ri_subject)

#####################################################################
## Fit a Random Intercept Model (Subject and Scenario) ##############
#####################################################################

ri_subject_scenario <-  lmer(frequency ~ attitude + (1 | subject) + (1 | scenario), data = polite)
anova(ri_subject_scenario)

anova(ri_subject, ri_subject_scenario)
## adding scenario as a random intercept improves the model
## current best model: attitude (fixed effect), subject (random intercept), 
## scenario (random intercept)

#################################################################################################
## Fit a Random Intercept Model (Subject and Scenario), add gender as fixed effect ##############
#################################################################################################

gender_ri_subject_scenario <-  lmer(frequency ~ attitude + gender + (1 | subject) + (1 | scenario), data = polite)
anova(gender_ri_subject_scenario)

anova(ri_subject_scenario, gender_ri_subject_scenario)

## adding gender as a fixed effect improves the model
## current best model: attitude (fixed effect), gender (fixed effect), 
## subject (random intercept), scenario (random intercept)

#################################################################################################
## Fit a Random Intercept (Subject and Scenario) , Random Slope (Subject) Model #################
#################################################################################################

gender_ri_subject_scenario_rs_subject <-  lmer(frequency ~ attitude + gender + (1 + attitude | subject) + (1 | scenario), data = polite)
anova(gender_ri_subject_scenario_rs_subject)

anova(gender_ri_subject_scenario, gender_ri_subject_scenario_rs_subject)

## adding a random slope for each subject does not improve the model 
## current best model: attitude (fixed effect), gender (fixed effect), 
## subject (random intercept), scenario (random intercept)

#####################################################################
## Explore Best Fit Model Results ###################################
#####################################################################

final_model <- gender_ri_subject_scenario

jpeg(filename = "pitch_formality_gender.jpeg", width = 850, height = 400, type = "quartz")
ggplot(data = polite, aes(attitude, frequency)) +
  geom_boxplot(col = "#084081", fill = "#4eb3d3") +
  facet_grid(~ gender + subject) +
  xlab ("Scenario Type") +
  ylab("Vocal Pitch (frequency in Hz)") +
  ggtitle("Pitch vs. Attitute in Korean Language Speakers")
dev.off()