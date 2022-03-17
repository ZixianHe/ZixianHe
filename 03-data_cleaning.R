#### Preamble ####
# Purpose: Clean the survey data downloaded from US Genaral Social Survey
# Author: Zixian He
# Data: 16 March 2022
# Contact: zixian.he@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)

# Read in the raw data. 
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(haven)
getOption("repos")
raw_data <- read_dta("/Users/eileen/Downloads/2021_stata (1)/gss2021.dta")

# create figure 1
fig.cap="Distribution of the survey respondents' age (in years)"
library(ggplot2)
library(dplyr)
labels <- c("18-29 years", "30-39 years", "40-49 years", "50-64 years", "65-89 years")
age_group <- cut(raw_data$age, 5, labels = labels)
raw_data[, "age_group"] <- age_group
ggplot(data = raw_data, aes(x = age_group)) +
  ggtitle("Survey respondents' age") +
  geom_bar(alpha = 0.8, color="orange", fill="steelblue")
         

# create figure 2
fig.cap="Distribution of the survey respondents' gender"
labels <- c("Female", "Male")
sex_group <- cut(raw_data$sex, 2, labels = labels)
raw_data[, "sex_group"] <- sex_group
ggplot(data = raw_data, aes(x = sex_group)) +
  ggtitle("Survey respondents's gender") +
  geom_bar(alpha = 0.8, color="snow1", fill="plum4")

# create figure 3
fig.cap="Distribution of the survey respondents' degree"
labels <- c("Less than high school",
            "High school",
            "Associate/Junior college",
            "Bachelors",
            "Graduate")
deg_group <- cut(raw_data$degree, 5, labels = labels)
raw_data[, "deg_group"] <- deg_group
ggplot(data = raw_data, aes(x = deg_group)) +
  ggtitle("Survey respondents's degree") +
  geom_bar(alpha = 0.8, color="lightsalmon2", fill="olivedrab4") +
  coord_flip()

# create figure 4
fig.cap="Distribution of the decision on suicide because of incurable disease"
labels <- c("Yes","No")
s1_group <- cut(raw_data$suicide1, 2, labels = labels)
raw_data[, "s1_group"] <- s1_group
ggplot(data = raw_data, aes(x = s1_group)) +
  ggtitle("Do you think a person has the right to end his or her own life if 
          this person Has an incurable disease?") +
  geom_bar(alpha = 0.8, color="red4", fill="lightskyblue2")

# create figure 5
fig.cap="Distribution of the decision on suicide because of bankrupt"
labels <- c("Yes","No")
s2_group <- cut(raw_data$suicide2, 2, labels = labels)
raw_data[, "s2_group"] <- s2_group
ggplot(data = raw_data, aes(x = s2_group)) +
  ggtitle("Do you think a person has the right to end his or her own life if 
          this person Has gone bankrupt?") +
  geom_bar(alpha = 0.8, color="red4", fill="rosybrown2")

# create figure 6
fig.cap="Distribution of the decision on suicide because of dishonored"
labels <- c("Yes","No")
s3_group <- cut(raw_data$suicide3, 2, labels = labels)
raw_data[, "s3_group"] <- s3_group
ggplot(data = raw_data, aes(x = s3_group)) +
  ggtitle("Do you think a person has the right to end his or her own life if 
          this person Has dishonored his or her family?") +
  geom_bar(alpha = 0.8, color="steelblue2", fill="thistle")

# create figure 7
fig.cap="Distribution of the decision on suicide because of tired of living"
labels <- c("Yes","No")
s4_group <- cut(raw_data$suicide4, 2, labels = labels)
raw_data[, "s4_group"] <- s4_group
ggplot(data = raw_data, aes(x = s4_group)) +
  ggtitle("Do you think a person has the right to end his or her own life if 
          this person Is tired of living and ready to die?") +
  geom_bar(alpha = 0.8, color="olivedrab3", fill="tan1")

# create figure 8
fig.cap="Distribution of the decision of suicide1 by gender"
ggplot(data=raw_data, aes(x=s1_group, fill=factor(sex_group))) + 
  geom_bar(position = "stack", alpha=0.3) +
  ggtitle("Decision of suicide1 by gender")

# create figure 9
fig.cap="Distribution of the decision of suicide1 by degree"
ggplot(data=raw_data, aes(x=deg_group, fill=factor(s1_group))) + 
  geom_bar(position = "identity",alpha=0.4) +
  ggtitle("Decision of suicide1 by different degree") +
  coord_flip()

# create figure 10
fig.cap="Distribution of the decision of suicide1 by age"
ggplot(data=raw_data, aes(x=age_group, fill=factor(s1_group))) + 
  geom_bar(position = "identity",alpha=0.6) +
  ggtitle("Decision of suicide1 by different age") +
  coord_flip()

# appendix of screenshot of survey questions
fig.align="left", out.width = '70%'
knitr::include_graphics("screenshot 1.jpeg")
knitr::include_graphics("screenshot 2.jpeg")
knitr::include_graphics("3.jpeg")
knitr::include_graphics("4.jpeg")
knitr::include_graphics("5.jpeg")
