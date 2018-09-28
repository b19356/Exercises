# This exercise is from the Data Camp ggplot2 (Part 1) module.
# It supplments the Titanic data from the Data Wranlging module with
# ggplot2 visuals.
# The goal was to assess survival by a number of factors
# Note: although the idea was to simply copy and paste the Data Camp code into a .R
# file, I had to modify the code a bit so it jived with the titanic_1 Data Wranlging
# output file

# 1 - read in Data Wrangling Titanic file

titanic_2 <- read_csv("C:\\Users\\bliss\\Desktop\\Practice\\Done\\titanic_1.csv")

# 2 - load tidyverse and check structure of titanic_2

library(tidyverse)

glimpse(titanic_2)

# 2 - Use ggplot() for the first instruction.

# In the code below, sometimes the first code is from Data Camp while 
# the second code is what worked better since I had to first remove the NA
# values from the sex variable in my data file

# Data Camp: ggplot(titanic_2, aes(x = pclass, fill = sex)) +
  # geom_bar(position = "dodge")

titanic_2 %>% filter(!is.na(sex)) %>% ggplot(aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge") 

# 3 - Plot 2, add facet_grid() layer
# Data Camp: ggplot(titanic_2, aes(x = pclass, fill = sex)) +
  # geom_bar(position = "dodge") + facet_grid(. ~ survived)

titanic_2 %>% filter(!is.na(sex)) %>% ggplot(aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge") + facet_grid(. ~ survived)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <-  position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
# Data Camp: ggplot(titanic, aes(x = Pclass, y = Age, col = Sex)) +
  # geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  # facet_grid(. ~ Survived)

titanic_2 %>% filter(!is.na(sex)) %>% ggplot(aes(x = pclass, y = age, col = sex)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + facet_grid(. ~ survived)

# Additional analysis:

# Find % Women / men by pclass and re-plot

w_m_pclass <- titanic_2 %>% select(pclass, sex) %>% group_by(pclass, sex) %>% 
  summarise("tally" = n()) %>% group_by(pclass) %>% mutate("sum" = sum(tally),
                                                           "Pct" = round(tally/sum*100,1))
View(w_m_pclass)

w_m_pclass_plot <- w_m_pclass %>% filter(!is.na(sex)) %>% ggplot(aes(x = pclass, y = Pct, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") + geom_text(aes(y = Pct + 1.5, 
                                               label = paste0(round(Pct,0), " %")), 
                                               position = position_dodge(width = 0.9), size = 3)
                                            

w_m_pclass_plot

# Find % women / men by pclass by survived (yes/no) and plot

w_m_pclass_s <- titanic_2 %>% select(pclass, sex, survived) %>% group_by(pclass, survived, sex) %>%
  summarise("N" = n()) %>% group_by(pclass, survived) %>% 
  mutate("People_Class_Y_N_Survived" = sum(N), "Pct_M_W" = 
           round(N/People_Class_Y_N_Survived *100,0))

View(w_m_pclass_s)

w_m_pclass_s_p <- w_m_pclass_s %>% filter(!is.na(sex)) %>% ggplot(aes(x = pclass, y = `Pct_M_W`, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(. ~ survived) +
  geom_text(aes(y = Pct_M_W + 2, label = paste0(round(Pct_M_W, 0), " %")),
            position = position_dodge(width = 0.9), size = 3)

w_m_pclass_s_p

View(w_m_pclass_s)

# Find and plot the mean age in each class by survival


w_m_pclass_s <- titanic_2 %>% select(pclass, sex, survived, age) 

View(w_m_pclass_s)


age <- data.frame(w_m_pclass_s %>% group_by(pclass, survived) %>% 
                    summarise("Avg_Age" = round(mean(age),1)))

View(age)

# First view average age by class

age_plot_1 <- age %>% filter(!is.na(survived)) %>% ggplot(aes(x = pclass, y = Avg_Age)) +
  geom_bar(stat = "identity", position = "dodge")

age_plot_1

age_plot_2 <- age %>% filter(!is.na(survived)) %>% ggplot(aes(x = pclass, y = Avg_Age)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(. ~ survived) +
  geom_text(aes(y = Avg_Age + 1.0, label = round(Avg_Age,0)), 
            position = position_dodge(width = 0.9), size =3)

age_plot_2

# Insights

# Despite having more men than women in each class, survivers skewed to women across all classes

# The age amoung survivors trended lower than those who didn't survive across classes, probably because
# of the effort to save children




