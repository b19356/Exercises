#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
setwd("C:/Users/bliss/Documents/R/Exercises/linear_regression")
getwd()
list.files("dataSets") # files in the dataSets folder
library(tidyverse)
library(GGally) # will make correlation comparisons easier via ggpairs

## Load the states data
## ────────────────────────

# read the states data

states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

glimpse(states.data)
View(states.data)
glimpse(states.info)
View(states.info)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
View(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)
sum(is.na(sts.ex.sat$expense))
sum(is.na(sts.ex.sat$csat))

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

# BC

sts.ex.sat %>% ggplot(aes(x = expense, y = csat)) +
  geom_point()

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

# BC

SSE <- sum(sat.mod$residuals^2)
SSE

RMSE <- sqrt(SSE/nrow(sts.ex.sat))
RMSE

SST <- sum(((mean(sts.ex.sat$csat)) - sts.ex.sat$csat)^2)
SST

R2 <- 1-SSE/SST
R2

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

#BC
# OK, I think I see what is going on using the ggpairs()
# it is counterintuitive to think that increasing expense results in
# poorer csat scores.  So this says we've got some multi-colinearity going on
# ggpairs shows that expense and percent (%kids taking sat) are positively correlated
# in other words, as expense goes up, then the % kids taking the sat goes up
# But then why does increasing % result in lower SAT scores?
# Only the rich kids take the SAT, and they skew to high scores
# When we open it up to more kids, you democratize it???

cor(states.data$expense, states.data$percent)

summary(lm(csat ~ percent, data = states.data))

states.data %>% ggplot(aes(x = expense, y = percent)) +
  geom_point()

states.data %>% ggplot(aes(x = percent, y = csat)) +
  geom_point()

sts.ex.sat2 <- states.data %>% select(csat, expense, percent)

View(sts.ex.sat2)

cor(sts.ex.sat2)

ggpairs(sts.ex.sat2)

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

summary(sat.mod)
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

# BC

residuals <- data_frame("Residuals" = sat.mod$residuals)

residuals %>% ggplot(aes(x = Residuals)) +
  geom_histogram()

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic (similar amounts of variance), the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

# BC - Assessing the distribution of the residuals

View(sat.mod)

Res_Fit <- data_frame("Residuals" = sat.mod$residuals, 
                      "Fitted_Values" = sat.mod$fitted.values)

View(Res_Fit)

p1 <- Res_Fit %>% ggplot(aes(x = Fitted_Values, y = Residuals)) +
  geom_point()

p1

p2 <- qqnorm(Res_Fit$Residuals)

p2

# I am struggling with grid.arrange(p1, p2, nrow = 1) I think because the above commands
# are forcing a certain plot layout and I can't overide it

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
summary(sat.voting.mod)
sat.mod <- update(sat.mod, data=na.omit(states.data))
summary(sat.mod)
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

# BC - compare model with house, senate and expense
# technically the model is better, but house & senate are correlated with expense
# Expense increases as more house and senate vote, especially senate

sum(is.na(states.data$house))
sum(is.na(states.data$senate))

sts.ex.sat2 <- states.data %>% select(csat, expense, percent, house, senate)

View(sts.ex.sat2)

sts.ex.sat2 <- sts.ex.sat2 %>% filter(!is.na(house) & !is.na(senate))

View(sts.ex.sat2)

ggpairs(sts.ex.sat2)

## Exercise_1: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

# Develop new df with desired predictors and remove NA's

energy_predict <- data_frame("Energy" = states.data$energy,
                             "Metro_Pop" = states.data$metro,
                             "House_Pct_Vote" = states.data$house,
                             "Senate_Pct_Vote" = states.data$senate)

View(energy_predict)

energy_predict <- energy_predict %>% filter_all(any_vars(!is.na(.)))

# Explore association between energy and metro (pop)
# 
# Weak negative correlation between % people in metro pop and energy consumed 
# per person (-0.34).  In other words, as the % metro pop increases, 
# the energy capita decreases slightly
# Perhaps as population gets denser, energy resources are more shared?

# look at correlation between energy (explanatory) and metro (predictor)

cor(energy_predict$Energy, energy_predict$Metro_Pop)

# Make all correlation plots visible using entire df

ggpairs(energy_predict)

# run linear model.  Although Metro_Pop significantly affects the energy,
# on its own, it yields a poor fit (R2 = 0.1)

lm_energy_metro <- lm(Energy ~ Metro_Pop, data = energy_predict)

summary(lm_energy_metro)

View(lm_energy_metro)

# Plot residuals and assess their distribution via a histogram and qqplot.
# First make data frame from model
# by pulling in the right vectors from lm_energy_metro.
# The residual plot has a strong right tail skew, as evident in 4 of the values

energy_lm_df <- data_frame("Residuals" = lm_energy_metro$residuals,
                           "Fitted_Energy_Values" = lm_energy_metro$fitted.values)

View(energy_lm_df)

glimpse(energy_lm_df)

resid_dist <- energy_lm_df %>% ggplot(aes(x = Residuals)) +
  geom_histogram() # can see the 4 data point outliers

resid_qqplot <- energy_lm_df %>% ggplot(aes(sample = Residuals)) +
  geom_qq() +
  geom_qq_line()

grid.arrange(resid_dist, resid_qqplot, nrow = 1)

# The poor fit of the 4 values can be seen in a scatter plot of the Residuals vs. Predicted

energy_lm_df %>% ggplot(aes(x = Fitted_Energy_Values, y = Residuals)) +
  geom_point()

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# Create new model adding in house and senate % votes
# All vars are negatively correlated with energy, as seen in the ggpairs
# But now the House % Vote is most strongly associated with energy consumption
# The model fit improved from R2 = 0.1 with just metro to 0.4 with house and senate vars

ggpairs(energy_predict)

lm_energy_metro_2 <- lm(Energy ~ Metro_Pop + House_Pct_Vote +
                     Senate_Pct_Vote, data = energy_predict)

summary(lm_energy_metro_2)

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 

summary(sat.expense.by.percent)

#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 

summary(sat.region)
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

# Develop new df with desired predictors and remove NA's.

energy_predict_2 <- data_frame("Energy" = states.data$energy,
                             "Metro_Pop" = states.data$metro,
                             "House_Pct_Vote" = states.data$house,
                             "Senate_Pct_Vote" = states.data$senate, 
                             "Region" = states.data$region)

View(energy_predict_2)

energy_predict_2 <- energy_predict_2 %>% filter_all(any_vars(!is.na(.)))

# Create interaction term house*senate.  By adding the House and Senate interaction,
# the model fit has increased from R2 = 0.4 to R2 = 0.51

lm_energy_2 <- lm(Energy ~ Metro_Pop + House_Pct_Vote +
                                         Senate_Pct_Vote +
                    House_Pct_Vote*Senate_Pct_Vote, data = energy_predict_2)

summary(lm_energy_2)

# Do another model to predict energy but this time use region (categorical var)
# Double check to see if region is a factor
# Predictive power is very week with R2 = 0.08
# Northeast was the only region with a signficant effect vs. West (-156)
# Makes sense from what we learned earlier...region will correlate with population
# the East has a larger metro pop, which tends to use less energy per capita since
# people share energy resources

glimpse(energy_predict_2)

# check the reference level - it is West

contrasts(energy_predict_2$Region)

lm_energy_3 <- lm(Energy ~  Region, data = energy_predict_2)

summary(lm_energy_3)

# Find out if there are significant differences across 4 regions by using anova
# on the lm object.
# Yes, the regions have different energy means.
# the F-value was 2.4.  This means that the variance of the grouped means
# is larger than the variance within the means.
# The F-value is the same as that obtained in the lm...not sure anova provides
# additional value beyond the lm

anova(lm_energy_3)
