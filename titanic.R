# Libraries

library(tidyverse)
library(tidymodels)
library(ggtea)
library(GGally)


# Data
titanic <- read_excel("C:/Users/Pushkar/Downloads/titanic.xls")
titanic_raw <- as_tibble(titanic)
titanic_raw

# Cleaning
titanic_raw %>% 
  count(is.na(Cabin))

titanic_raw %>% 
  count(is.na(Age))

titanic <- titanic_raw %>% 
  filter(!is.na(Age), !is.na(Embarked)) %>% 
  select(-Cabin, -PassengerId) %>% 
  mutate(Sex = as.factor(Sex), 
         Embarked = as.factor(Embarked), 
         Survived = as.factor(
           if_else(Survived==1,"Yes","No")),
         Pclass = as.factor(Pclass)
         )

titanic

# EDA
# Survival rates by passenger class

titanic %>% 
  ggplot(aes(Pclass)) + 
  geom_bar(aes(fill = Survived), position = "dodge") + 
  apricot_d()

# To nobody's surprise, 3rd class passengers are at the highest risk

# Passenger class by point of departure

titanic %>% 
  ggplot(aes(Embarked)) +
  geom_bar(aes(fill = Pclass), position = "dodge") + 
  apricot_d()

# Cherbourg picked up by far the most passengers and also a huge proportion of 3rd class passengers
# Dropping this variable for modeling as its not balanced + better represented in the passenger class

# Survival rates by passenger class & Sex

titanic %>%  
  ggplot(aes(Sex)) + 
  geom_bar(aes(fill = Survived), position = "dodge") + 
  facet_wrap(~Pclass) + 
  apricot_d()

# The men were more likely to die

# Overall age distribution
titanic %>% 
  ggplot(aes(Age)) + 
  geom_density(fill = "pink", alpha = 0.5) + 
  apricot_d()

# Age distribution by sex   
titanic %>% 
  ggplot(aes(Age)) + 
  geom_density(aes(fill = Sex), alpha = 0.5) + 
  apricot_d()  

# Age distribution by sex & class  
titanic %>% 
  ggplot(aes(Age)) + 
  geom_density(aes(fill = Sex), alpha = 0.5) + 
  facet_wrap(~Pclass) +
  apricot_d()  

# Nothing interesting here so far

# Age distribution by survival rate

titanic %>% 
  ggplot(aes(Age)) + 
  geom_density(aes(fill = Survived), alpha = 0.5) + 
  facet_wrap(~Pclass) + 
  apricot_d()

# Nope. Everybody died.

# Survival rates by family count | switching from visualisations to summary statistics
titanic %>% count(SibSp)

titanic_family <- titanic %>% 
  mutate(Family = as.character(if_else(SibSp > 2, "3+", as.character(SibSp))),
         Survived = as.numeric(if_else(Survived == "Yes",1,0)))

titanic_family %>% 
  group_by(Family) %>% 
  summarise(survived = sum(Survived), 
            perc = sum(survived)/n())

# Large families didn't survive - presumably 3rd class passengers 
# Single people also didn't survive - presumably males

# Checking the above
titanic_family %>% 
  group_by(Family, Pclass) %>% 
  count()
# Large families were pretty much only 3rd class passengers 

titanic_family %>% 
  group_by(Family, Sex) %>% 
  count()
# Single passengers were mostly males

# Checking the fares by passenger class - we might not need one of the two variables
titanic %>% 
  filter(Fare < 200) %>% 
  ggplot(aes(Fare)) + 
  geom_density(aes(fill = Pclass), alpha = 0.5) +
  apricot_d()

# Checking correlations
titanic_cor <- titanic %>% 
  mutate(Pclass = as.numeric(Pclass))

ggcorr(titanic_cor, label = TRUE)







