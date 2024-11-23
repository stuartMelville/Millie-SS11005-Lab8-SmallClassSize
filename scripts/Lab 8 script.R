#### -- SSI1005: Intro to Social Data -- ####
### Week 8 - Analysing experiment data
## Dr Hannah Bunting
# 20.11.23


# This script is accompanied by a PDF of prompts to prepare you for the take-home
# exam that forms the second assessment for this module.
# The content in this script and the PDF will be spread over 2 weeks of labs if
# it is too much to complete in one session.
# We will use the STAR dataset to analyse the results of the experiment on 
# classroom sizes and pupil outcomes. The dataset is described in the PDF and in
# the Llaudet & Imai textbook.
# We will transform some of the variables into a more useable format and analyse
# a set of average treatment effects, as explained in the Week 8 lecture.

getwd()
options(max.print=999999)

#### Load in the tidyverse
library(tidyverse)


#### Data 

# Remember to check your working directory if needed using getwd() and setwd()

# Read in the data
star <- read.csv("./data/raw/STAR.csv")

# check it
glimpse(star)
colnames(star)
nrow(star)
library(dplyr)
star %>% 
  arrange(desc(star)) %>% 
  slice(1:5)
star %>% 
  arrange(star) %>% 
  slice(1:5)
top_n(star,-5,pupilID)


#### Question 1

# The 3 groups pupils were assigned in kindergarten are identified in the classtype variable.
# We want to look at just the differences between small classrooms and regular,
# i.e. Treatment group 1 and the control group
# Create a new dataframe that contains just these two groups.
# The new dataframe should be called star_2

# create new df uding tidyverse code, excluding the 3rd group
star_2 <- star %>%
  filter(classtype != 3)

# check it
glimpse(star_2)

table(star_2$classtype)


#### Question 2

# The number of years spent in a small classroom is recorded in the yearssmall variable.
# Some pupils may have spent time in a small class after their initial assignment
# to either the treatment or control group.
# We want those in the treatment category to have only been in regular classes.
# Remove any pupils who are in the regular category and have been in a small class.

# check the number of years in small classes for each group
table(star_2$yearssmall, star_2$classtype)

# make any cases where yearssmall is more than 0 and classtype_fac is regular = NA
# if both conditions are not TRUE, return the original classtype_fac variable
star_2 <- star_2 %>%
  mutate(classtype = ifelse(yearssmall > 0 & classtype == 2, NA, classtype))

# check it
table(star_2$yearssmall, star_2$classtype)



#### Question 3

# Create a new factor variable called classtype_fac that gives each group a label.
# This will help us identify the treatment and control groups accurately.

# create the variable with tidyverse code
star_2 <- star_2 %>% mutate(classtype_fac = factor(classtype, levels = c(1, 2),
                                               labels = c("Small", "Regular")))

# check it has coded correctly
table(star_2$classtype, star_2$classtype_fac)

# check its type
class(star_2$classtype_fac)



#### Question 4

# The experiment also recorded the ethnicity of each pupil using the race variable.
# Create a new character variable that writes the categories in text and
# combines the 'Asian', 'Hispanic' and 'Native American' categories with the 'others' category, 
# meaning the new variable has a total of 3 categories.
# Name the new variable race_3cats

# create it using the case_when tidyverse code
star_2 <- star_2 %>% mutate(race_3cats = case_when(
  race == 1 ~ "White",
  race == 2 ~ "Black",
  race == 3 | race == 4 | race == 5 | race == 6 ~ "Other race"))

# check it
table(star_2$race, star_2$race_3cats)

class(star_2$race_3cats)

# details of how to do this in base R are in lab 7's script



#### Question 5

# Create a crosstab of proportions showing each new race category
# variable by the classroom types factor variable.
# What proportion of pupils are black in the treatment group?
# What proportion of pupils are black in the control group?

# create crosstab
prop.table(table(star_2$race_3cats, star_2$classtype_fac), 1)

# Record the answers here
# Proportion black in the treatment group:
# Proportion black in the control group:



#### Question 6

# The hsgrad variable shows whether the pupil graduated high school many
# years after the class size experiment was conducted.
# Missing values indicate the study could not record whether they graduated
# because the pupil had dropped out of the study. 
# Quantify the attrition rate of the study in two ways:
# 1. What number of pupils dropped out of the study before their graduation year?
# 2. What proportion of pupils remained in the study until graduation?

# 1. create table of the hsgrad variable
star_2 %>%
  count(hsgrad)

# note: to do this in base R, you must specify an extra argument in the table function
table(star_2$hsgrad, useNA = "always")

# Record the number of pupils that dropped out here


# 2. Find the proportions
star_2 %>%
  count(hsgrad) %>%
  mutate(prop = n/sum(n))

# or
prop.table(table(star_2$hsgrad, useNA = "always"))

# final step: calculate the proportion that remained in the study
100 - 51.56



#### Question 7

# Find the average treatment effect of being in a small classroom on
# Grade 4 reading scores, recorded in the g4reading variable.

# find the group averages: tidyverse way
star_2 %>%
  drop_na(g4reading, classtype_fac) %>%
  group_by(classtype_fac) %>%
  summarise(mean = mean(g4reading))

# find the group averages: base R way
mean(star_2$g4reading[star_2$classtype_fac == "Small"], na.rm = T)
mean(star_2$g4reading[star_2$classtype_fac == "Regular"], na.rm = T)

# calculate the average treatment effect
723 - 719

# How would you interpret this?
# The answer is that there's no substantive increase in reading
# score when a pupil was in a small class, i.e. we don't observe
# much of a causal effect.



#### Question 8

# Create a boxplot to visualise the differences in reading scores
# across the treatment and control groups.

# if you prefer, you can build the plot one line at a time like in previous labs
star_2 %>% drop_na(classtype_fac) %>%
  ggplot(aes(x = classtype_fac, y = g4reading)) +
  geom_boxplot(aes(fill = classtype_fac)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Classroom type", y = "Grade 4 reading score",
       title = "The effect of classroom size on reading score",
       caption = "Source: STAR Experiment data")



#### Question 9

# Find the average treatment effect of being in a small classroom on
# Grade 4 maths scores, recorded in the g4math variable. 

# find the group averages: tidyverse way
star_2 %>%
  drop_na(g4math, classtype_fac) %>%
  group_by(classtype_fac) %>%
  summarise(mean = mean(g4math))

# find the group averages: base R way
mean(star_2$g4math[star_2$classtype_fac == "Small"], na.rm = T)
mean(star_2$g4math[star_2$classtype_fac == "Regular"], na.rm = T)

# calculate the average treatment effect
709 - 709

# There are no differences in maths score across treatment groups.
# meaning there is no causal effect of classroom size on maths test scores.


#### Question 10

# Create a boxplot to visualise the differences in maths scores
# across the treatment and control groups.

# if you prefer, you can build the plot one line at a time like in previous labs
star_2 %>% drop_na(classtype_fac) %>%
  ggplot(aes(x = classtype_fac, y = g4math)) +
  geom_boxplot(aes(fill = classtype_fac)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Classroom type", y = "Grade 4 maths score",
       title = "The effect of classroom size on maths score",
       caption = "Source: STAR Experiment data")



