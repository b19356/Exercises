# This is the first data wrangling exercise in the Springboard Curriculum
# refine_original / refine_clean

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
# I had some problems with this step.  At first, it seemed that I had to
# recode all at the same time.  When I tried it stepwise, it would "undo" the last mutate
# But now it seems to be working in steps

# refine_original_1 <- refine_original %>% mutate(company =
#                                                   recode(company, "Akzo" = "akzo",
#                                                          "AKZO" = "akzo",
#                                                          "akz0" = "akzo",
#                                                          "ak zo" = "akzo",
#                                                          "Phillips" = "philips",
#                                                          "phillips" = "philips",
#                                                          "phllips" = "philips",
#                                                          "phillps" = "philips",
#                                                          "phillipS" = "philips",
#                                                          "fillips" = "philips",
#                                                          "phlips" = "philips",
#                                                          "Van Houten" = "van houten",
#                                                          "van Houten" = "van houten",
#                                                          "unilver" = "unilever",
#                                                          "Unilever" = "unilever"))

refine_original_1 <- refine_original %>% mutate(company = 
                                                    recode(company,
                                                           "Akzo" = "akzo",
                                                           "AKZO" = "akzo",
                                                           "akz0" = "akzo",
                                                           "ak zo" = "akzo"))

refine_original_1 <- refine_original_1 %>% mutate(company = 
                                                    recode(company, "Phillips" = "philips",
                                                           "phillips" = "philips",
                                                           "phllips" = "philips",
                                                           "phillps" = "philips",
                                                           "phillipS" = "philips",
                                                           "fillips" = "philips",
                                                           "phlips" = "philips"))

refine_original_1 <- refine_original_1 %>% mutate(company = 
                                                    recode(company, "Van Houten" = "van houten",
                                                           "van Houten" = "van houten"))


refine_original_1 <- refine_original_1 %>% mutate(company = 
                                                    recode(company,"unilver" = "unilever",
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


# 5: Create dummy variables for company and product category

# Both the company name and product category are categorical variables 
# i.e. they take only a fixed set of values. 
# In order to use them in further analysis you need to create dummy variables. 
# Create dummy binary variables for each of them with the prefix company_ and product_ i.e.,
# Add four binary (1 or 0) columns for company: 
# company_philips, company_akzo, company_van_houten and company_unilever.
# Add four binary (1 or 0) columns for product category: 
# product_smartphone, product_tv, product_laptop and product_tablet.
# I have to admit, I find this exercise strange

refine_original_1 <- refine_original_1 %>% 
  mutate(company_philips = recode(company, "philips" = 1,
                                  "akzo" = 0,
                                  "van houten" = 0,
                                  "unilever" =0))


refine_original_1 <- refine_original_1 %>%
  mutate(company_akzo = recode(company, "akzo" = 1,
                               "philips" = 0,
                               "van houten" = 0,
                               "unilever" = 0))


refine_original_1 <- refine_original_1 %>%
  mutate(company_van_houten = recode(company, "van houten" = 1,
                                     "akzo" = 0,
                                     "philips" = 0,
                                     "unilever" = 0))


refine_original_1 <- refine_original_1 %>%
  mutate(company_unilever = recode(company, "unilever" = 1,
                                   "akzo" = 0,
                                   "philips" = 0,
                                   "van houten" = 0))

View(refine_original_1)

# Add four binary (1 or 0) columns for product category: 
# product_smartphone, product_tv, product_laptop and product_tablet.
# I have to admit, I find this exercise strange

refine_original_1 <- refine_original_1 %>%
  mutate(product_smartphone = recode(product_code, "Smartphone" = 1,
                                     "Laptop" = 0,
                                     "TV" = 0,
                                     "Tablet" = 0))


refine_original_1 <- refine_original_1 %>%
  mutate(product_tv = recode(product_code, "TV" = 1,
                             "Smartphone" = 0,
                             "Laptop" = 0,
                             "Tablet" = 0))

refine_original_1 <- refine_original_1 %>%
  mutate(product_laptop = recode(product_code, "Laptop" = 1,
                                 "TV" = 0,
                                 "Smartphone" = 0,
                                 "Tablet" = 0))

refine_original_1 <- refine_original_1 %>%
  mutate(product_tablet = recode(product_code, "Tablet" = 1,
                                 "TV" = 0,
                                 "Laptop" = 0,
                                 "Smartphone" = 0))

View(refine_original_1)

# Double check my binary assignments for company

refine_original_1 %>% group_by(company) %>% summarise(no_companies = n())

refine_original_1 %>% select(company_philips, company_akzo,
                             company_unilever, company_van_houten) %>%
  summarise_each(funs(sum))

# Double check my binary assignments for product_code

refine_original_1 %>% group_by(product_code) %>% summarise(no_codes = n())

refine_original_1 %>% select(product_smartphone:product_tablet) %>%
  summarise_each(funs(sum))

