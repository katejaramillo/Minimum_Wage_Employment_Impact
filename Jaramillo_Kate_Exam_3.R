##################################################
# ECON 418-518 Exam 3
# Kate Jaramillo
# The University of Arizona
# kmjaramillo@arizona.edu 
# 15 December 2024
###################################################


#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)
library(data.table)

# Set working directory
setwd("/Users/katejaramillo/Desktop")

# Loading data in & check
data <- fread("418Exam3Data.csv")
head(data)

#################
# Part (ii)
#################

# Indicator vars for post-treatment time period & treatment group
  ## November = 1, 0 otherwise; New Jersey = 1, 0 otherwise (Pennsylvania)
data$is_nov <- ifelse(data$time_period == "Nov", 1, 0)
data$is_nj <- ifelse(data$state == 1, 1, 0)

# Calculating mean total employment in each state & time period
mean_emp <- data[, .(mean_total_emp = mean(total_emp, na.rm = TRUE)), 
                 by = .(state, time_period)]
mean_emp_pa_feb <- mean_emp[state == 0 & time_period == "Feb", mean_total_emp]
mean_emp_pa_nov <- mean_emp[state == 0 & time_period == "Nov", mean_total_emp]
mean_emp_nj_feb <- mean_emp[state == 1 & time_period == "Feb", mean_total_emp]
mean_emp_nj_nov <- mean_emp[state == 1 & time_period == "Nov", mean_total_emp]

# DiD estimate manually
DiD <- (mean_emp_nj_nov - mean_emp_nj_feb) - (mean_emp_pa_nov - mean_emp_pa_feb)

# DiD estimate using lm & checking out summary
model <- lm(total_emp ~ is_nov * is_nj, data = data)
summary(model)

# Extracting coef of interaction term
post_treat_b <- coef(model)["is_nov:is_nj"]

# Extracting std error of interaction term
post_treat_se <- summary(model)$coefficients["is_nov:is_nj", "Std. Error"]

# Constructing confidence interval
lower_bound <- post_treat_b - 1.96 * post_treat_se
upper_bound <- post_treat_b + 1.96 * post_treat_se

# Adding restaurant fixed effects to the model & checking out summary
model_rfe <- lm(total_emp ~ is_nov * is_nj + factor(restaurant_id), data = data)
summary(model_rfe)

