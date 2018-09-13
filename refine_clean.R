# This is the first data wrangling exercise in the Springboard Curriculum

# Test 1,2,3

# Test 4,5,6

# Test 7,8,9

# Test 10

library(tidyverse)

refine_original <- read_csv("C:\\Users\\bliss\\Desktop\\Practice\\refine_original.csv")

View(refine_original)

# 1. Clean up brand names so they are all the same and in lowercase:
# philips, akzo, van houten and unilever

# How many different values for company?

# I am going to use mutate to rename my observations vs. transmute because
# I want to keep the original column for now

# There are 19 unique company names.  Ultimately want to have 4

unique(refine_original$company)

length(unique(refine_original$company))

# 1. Recode all observations to a single, consistent name

refine_original_1 <- refine_original %>% mutate(company = 
                                                  recode(company, "Akzo" = "akzo",
                                                         "AKZO" = "akzo",
                                                         "akz0" = "akzo",
                                                         "ak zo" = "akzo",
                                                         "Phillips" = "philips",
                                                         "phillips" = "philips",
                                                         "phllips" = "philips",
                                                         "phillps" = "philips",
                                                         "phillipS" = "philips",
                                                         "fillips" = "philips",
                                                         "phlips" = "philips", 
                                                         "Van Houten" = "van houten",
                                                         "van Houten" = "van houten",
                                                         "unilver" = "unilever",
                                                         "Unilever" = "unilever"))               

View(refine_original_1)

# Double check that we have 4 company names

unique(refine_original_1$company)

# 2. Separate product code and number
# Create separate columns 
# i.e. add two new columns called product_code and product_number
# containing the product code and number respectively
# Will keep default remove = TRUE so that the original column is removed

refine_original_1 <- refine_original_1 %>% separate(col = `Product code / number`, 
                                                    into = c('product_code', 'product_number'), 
                                                    sep = "-")

View(refine_original_1)

# 3: Add product categories
# p = Smartphone
# v = TV
# x = Laptop
# q = Tablet

refine_original_1 <- refine_original_1 %>% mutate(product_code = 
                                                    recode(product_code,
                                                           "p" = "Smartphone",
                                                           "v" = "TV",
                                                           "x" = "Laptop",
                                                           "q" = "Tablet"))

View(refine_original_1)

# 4: Add full address for geocoding
# You'd like to view the customer information on a map. 
# Thus, the addresses need to be in a form that can be easily geocoded. 
# Create a new column full_address that concatenates the three address fields:
# (address, city, country), separated by commas.

refine_original_1 <- refine_original_1 %>%
  unite(col = full_address, c(address, city, country), sep = ", ", remove = FALSE)

View(refine_original_1)











