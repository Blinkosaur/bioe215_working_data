
library(tidyverse)

# Read and inspect 

surveys <- read_csv("data/portal_data_joined.csv")
head(surveys)
summary(surveys)


# Q1: What’s the type of column species_id? Of hindfoot_length?
  # Character & double

# Q2: How many rows and columns are in surveys?
# 13 columns, 32786 rows...? 


# Select is for coloumns; first argument = data, everything else is column names without quotes
select(surveys, plot_id, species_id, weight)
select(surveys, plot_id, weight_g=weight) # changes the name of weight to weight_g 

#select to remove rows
select(surveys, -record_id, -species_id)


#filters are used for conditions within rows

filter(surveys, year == 1995) # condition 1995 
filter(surveys, year == 1995, plot_id == 7) #conditions 1995 AND plot ID 7
filter(surveys, month ==2 | day == 20) # the little line thing is for OR

# Q3: filter() surveys to records collected in November where hindfoot_length is greater than 36.0
filter(surveys, hindfoot_length > 36.0, month == 11)

# Q4: Fix these errors
filter(surveys, year == 1995)
filter(surveys, plot_id == 2)


# Pipeline filter and select together... %>% = "then"

surveys_psw <- surveys %>% 
  filter(year == 1995) %>% 
  select(plot_id, species_id, weight)

# Q5: Use pipes to subset surveys to animals collected before 1995 retaining just 
# the columns year, sex, and weight

surveys_heck <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)

surveys %>% 
  mutate(weight_kg = weight/1000) %>% 
  view()

surveys %>% 
  filter(!is.na(weight)) %>% #filter to remove na's from weight column
  mutate(weight_kg = weight/1000, 
         weight_lb = weight * 2.2) %>% 
  view()

# Q6: Create a new data frame from the surveys data that meets the following criteria: 
# contains only the species_id column and a new column called hindfoot_cm containing 
# the hindfoot_length values (currently in mm) converted to centimeters. 
# In this hindfoot_cm column, there are no NAs and all values are less than 3.

# Hint: think about how the commands should be ordered to produce this data frame!

fuck <- surveys %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  select(species_id, hindfoot_cm) %>% 
  filter(hindfoot_cm < 3) %>% 
  filter(!is.na(hindfoot_cm)) %>% 
           view()


surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))


surveys %>% 
  drop_na(weight) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            .groups = "drop") %>%  
  arrange(mean_weight)

head(surveys)

# Q7: How many animals were caught in each plot_type surveyed?

surveys %>% 
  group_by(plot_type) %>% 
  count()

  
# Q8: Use group_by() and summarize() to find the mean, min, and max hindfoot length 
# for each species (using species_id). Also add the number of observations (hint: see ?n).

surveys %>% 
  drop_na(hindfoot_length) %>% 
  summarize(mean_hindfoot_length = mean(hindfoot_length),
            min_hindfoot_length = min(hindfoot_length),
            max_hindfoot_length = max(hindfoot_length),
            .groups = "drop") 

# Q9: What was the heaviest animal measured in each year? Return the columns year, 
# genus, species_id, and weight.     


