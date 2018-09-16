# In this exercise, you'll work with one of the most popular starter data sets 
# in data science, the Titanic data set. This is a data set that records 
# various attributes of passengers on the Titanic, including who survived and who didn’t.

library(tidyverse)

titanic_original <- read_csv("C:\\Users\\bliss\\Desktop\\Practice\\titanic_original.csv")

# 1: Port of embarkation

# The embarked column has some missing values, which are known to correspond 
# to passengers who actually embarked at Southampton. 
# Find the missing values and replace them with S. 
# (Caution: Sometimes a missing value might be read into R as a blank or empty string.)

View(titanic_original)

glimpse(titanic_original)

colnames(titanic_original)

# Looks like there are 4 unique values, but am I missing blanks/MT strings?

titanic_original$embarked %>% unique()

sum(is.na(titanic_original$embarked)) # There are 3 NA's

sum(is_empty(titanic_original$embarked)) # Not a dplyr function, but said there were 0 

titanic_original %>% filter(embarked == "") %>% select(embarked)

# So now I will replace the 3 NA's with "S" and double check

titanic_1 <- titanic_original %>% mutate(embarked = replace_na(embarked, "S"))

sum(is.na(titanic_1$embarked))

titanic_original %>% group_by(embarked) %>% summarise(n()) # originally had 914 "S"

titanic_1 %>% group_by(embarked) %>% summarise(n())  # now have 917 "S"

# 2: Age

# You’ll notice that a lot of the values in the Age column are missing. 
# While there are many ways to fill these missing values, 
# using the mean or median of the rest of the values is quite common in such cases.

# Calculate the mean of the Age column and use that value to populate the missing values

# Think about other ways you could have populated the missing values in the age column
# Why would you pick any of those over the mean (or not)?

sum(is.na(titanic_1$age)) # There are 264 NA's

titanic_1$age %>% mean(na.rm = TRUE) # average age = 29.9

titanic_1$age %>% median(na.rm = TRUE) #median age is 28

hist(titanic_1$age)

range(titanic_1$age, na.rm = TRUE) # 0.17 - 80 

titanic_1 <- titanic_1 %>% mutate(age = replace_na(age, 29.9))

sum(is.na(titanic_1$age)) # Confirm that there are no NA's left

titanic_1$age %>% mean() # Average age is still 29.9

# 3: Lifeboat

# You’re interested in looking at the distribution of passengers in different lifeboats,
# but as we know, many passengers did not make it to a boat :-( 
# This means that there are a lot of missing values in the boat column. 
# Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'

colnames(titanic_1)

unique(titanic_1$boat)

n_distinct(titanic_1$boat) # 28 different boats

sum(is.na(titanic_1$boat)) # 824 missing values, but does it include MT strings?

sum(is_empty(titanic_1$boat)) # Does this catch MT strings?

titanic_1$boat

titanic_1 <- titanic_1 %>% mutate(boat = replace_na(boat, "None"))

sum(is.na(titanic_1$boat)) # Confirmed that all NA were replaced

# 4: Cabin

# You notice that many passengers don’t have a cabin number associated with them.
# Does it make sense to fill missing cabin numbers with a value?
# What does a missing value here mean?
# You have a hunch that the fact that the cabin number is missing 
# might be a useful indicator of survival. 
# Create a new column has_cabin_number which has 1 if there is a cabin number, 
# and 0 otherwise.

colnames(titanic_1)

n_distinct(titanic_1$name) # There are 1308 unique passanger names, but there are 1310 observations

# How do I find duplicate values?

titanic_1 %>% count(name) %>% filter(n>1)  # There are 2 "Connolly, Miss.Kate" and
# 2 "Kelly, Mr. James"

sum(is.na(titanic_1$cabin)) # There are 1015 missing cabins

titanic_1 <- titanic_1 %>% mutate(has_cabin_number = if_else(!is.na(cabin), 1, 0))

View(titanic_1) 

# Double check that NA cabin values are associated with a 0 in "has_cabin_number"
# and cabins with a value are associated with a 1 in "has_cabin_number"

titanic_1 %>% select(cabin, has_cabin_number)

sum(is.na(titanic_1$cabin)) # There are 1015 missing cabins

1310 - 1015 # The number of observations minus the number of NA = 295 observations
# with cabin numbers

sum(titanic_1$has_cabin_number) # The sum of "yes" cabin (1) = 295

write_csv(x = titanic_1, path = "C:\\Users\\bliss\\Desktop\\Practice\\Done\\titanic_1.csv",
          col_names = TRUE)
