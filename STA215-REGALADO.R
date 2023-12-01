## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
# EXAMINE QUANT_VAR1
table(data$People)
mean(data$People)
sd(data$People)
summary(data$People)

# EXAMINE QUANT_VAR2
table(data$Men)
mean(data$Men)
sd(data$Men)
summary(data$Men)

#EXAMINE QUANT_VAR3
table(data$Emotional)
mean(data$Emotional)
sd(data$Emotional)
summary(data$Emotional)

#EXAMINE QUAL_VAR1
table(data$Environment)
describe(data$Environment)

#EXAMINE QUAL_VAR2
table(data$Name)
describe(data$Name) 

##################################################################################
####################  CREATE TABLE 2: Step  2             ####################   
##################################################################################
table(data$Environment,data$Name)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(data$Environment,data$Name)

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
# Perform ANOVA
anova_adapted <- aov(Environment ~ Emotional, data=data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
plot(data$Men, data$Emotional)
cor(data$Men, data$Emotional)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Emotional ~ Men, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(data$Men,data$Emotional)
abline(linear_relationship, col="navy") 
mean(data$Men)
mean(data$Emotional)
abline(a=NULL, b=NULL, h=2.983333 , v=0.8166667, col="lightblue")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Men, residuals(linear_relationship))
abline(h = 0, col = "grey")