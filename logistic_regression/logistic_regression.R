## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

# BC

library(tidyverse)

NH11 <- readRDS("dataSets\\NatHealth2011.rds")

View(NH11)

glimpse(NH11)

labs <- attributes(NH11)$labels

View(labs)

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev)
# check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

# BC

glimpse(NH11$hypev)

unique(NH11$hypev)

levels(NH11$hypev)

table(NH11$hypev)

# No NA's in hypertension, but lots in df

sum(is.na(NH11$hypev))

sum(is.na(NH11))

# collapse all missing values to NA

NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))

table(NH11$hypev)

levels(NH11$hypev)

View(NH11)

hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")

summary(hyp.out)

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

# BC

hyp.out.tab <- coef(summary(hyp.out))

View(hyp.out.tab)

colnames(hyp.out.tab)

# add a var by rewriting the "log odds" as an exponent

hyp.out.tab <- data.frame(hyp.out.tab)

hyp.out.tab <- hyp.out.tab %>% mutate("Estimate2" = exp(Estimate))

View(hyp.out.tab)

View(hyp.out.tab)

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))

View(predDat)

# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

# BC

predDat2 <- data_frame(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(NH11$bmi, na.rm = TRUE),
                            sleep = mean(NH11$sleep, na.rm = TRUE))

View(predDat2)

cbind(predDat2, predict(hyp.out, type = "response", se.fit = TRUE,
                       interval = "confidence", newdata = predDat2))

View(predDat2)

predictions2 <- predict(object = hyp.out, type = "response", newdata = predDat2,
                        interval = "confidence", se.fit = TRUE)

View(predictions2)

summary(hyp.out)


## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

# Deal with NA's - looks like everwrk was the only var with NA's
# So refactored NA's into just Yes and No

glimpse(NH11$everwrk)

sum(is.na(NH11$everwrk))

levels(NH11$everwrk)

table(NH11$everwrk)

NH11$everwrk <- factor(NH11$everwrk, levels = c("1 Yes", "2 No"))

levels(NH11$everwrk)

sum(is.na(NH11$age_p))

glimpse(NH11$age_p)

unique(NH11$age_p)

sum(is.na(NH11$r_maritl))

glimpse(NH11$r_maritl)

levels(NH11$r_maritl)

# Develop model

glm_model <- glm(formula = everwrk ~ age_p + r_maritl, family = "binomial", 
                 data = NH11)

View(glm_model)

summary(glm_model)

plot(allEffects(glm_model))

# as age increases, the log odds of working decrease - makes sense as people
# shift into retirement
# The following marital statuses were associated with an increased log odds of working:
# Widowed
# Never married

# The following marital statuses were associated with a decreased log odds of working:
# Divorced
# Living with partner

# And these marital statuses did not affect the probability of working
# Married - spouse not in household
# Separated
# Unknown marital status

# Make predictions for each level of marital status
# I will keep age constant (use avg)

# Note - there are 10 levels of marital status, but only 8 are associated
# with having Yes/No for work.  So I need to just use these 8 levels

levels(NH11$r_maritl)

table(NH11$r_maritl, NH11$everwrk)

make_predictions <- data_frame("age_p" = mean(NH11$age_p, na.rm = TRUE),
                               "r_maritl" = c("1 Married - spouse in household",
                                              "2 Married - spouse not in household",
                                              "4 Widowed", "5 Divorced",
                                              "6 Separated","7 Never married",
                                              "8 Living with partner", 
                                              "9 Unknown marital status"))
View(make_predictions)

predictions <- predict(object = glm_model, 
                       newdata = make_predictions, type = "response")

View(predictions)

# Merge probabilities with original df to have everything in one place

# First convert predictions from a named vector into a df, then bind_cols

predictions <- data_frame(predictions)

make_predictions <- bind_cols(make_predictions, predictions)

View(make_predictions)

# There is another way to make predictions of marital status - 
# just put effects into a table...admittedly, I saw this after I looked at the solutions
# this approach makes more sense vs. making up an age like I did above.
# the "fit" terms shows the effect for each marital status

data.frame(Effect(focal.predictors = "r_maritl", mod = glm_model))




