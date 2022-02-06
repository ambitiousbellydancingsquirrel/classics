# Libraries

library(tidyverse)
library(tidymodels)
library(ggtea)

# Data

abalone_uci <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"))
abalone_uci_names <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names"))

abalone <- abalone_uci %>% 
  as_tibble() %>% 
  rename(sex = M,
         length = X0.455,
         diameter = X0.365,
         height = X0.095,
         whole_weight = X0.514,
         shucked_weight = X0.2245,
         viscera_weight = X0.101,
         shell_weight = X0.15,
         rings = X15)

# EDA

abalone %>% 
  group_by(sex) %>% 
  summarise(n = n(),
            avglength = mean(length),
            avgdia = mean(diameter),
            avgww = mean(whole_weight),
            avgsw = mean(shucked_weight),
            avgvw = mean(viscera_weight),
            avgshw = mean(shell_weight),
            medr = median(rings),
            meanr = mean(rings),
            sdr = sd(rings))

# Infant/Adult Logistic Regression
# Wrangling 

abalone_log <- abalone %>% 
  mutate(sex = as.factor(if_else(sex %in% c("F","M"),"A","I")))
# (sex is now not sex, but adult/infant)

# Data Split
set.seed(1234)

ab_split <- initial_split(abalone_log, strata = sex)
ab_split

ab_train <- training(ab_split)
ab_test <- testing(ab_split)
ab_train

# Bootstrap resamples

set.seed(5678)
ab_boot <- bootstraps(ab_train)
ab_boot


# Model 1 - GLM 

ab_glm <- logistic_reg() %>% 
  set_engine("glm")

ab_glm

ab_glm_workflow <- workflow() %>% 
  add_formula(sex~.) %>% 
  add_model(ab_glm) %>% 
  fit_resamples(ab_boot, 
                control = control_resamples(save_pred = TRUE,
                                            verbose = TRUE))


# Model 2 - RF

ab_rf <- rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

ab_rf

ab_rf_workflow <- workflow() %>% 
  add_formula(sex~.) %>% 
  add_model(ab_rf) %>% 
  fit_resamples(ab_boot, 
                control = control_resamples(save_pred = TRUE,
                                            verbose = TRUE))

# Evaluation

collect_metrics(ab_glm_workflow)
# Mean 0.822 SE 0.00164, AUC Mean 0.884 SE 0.00151
collect_metrics(ab_rf_workflow)
# Mean 0.831 SE 0.00158, AUC Mean 0.885 SE 0.00154

# Confusion Matrix GLM
ab_glm_workflow %>% 
  conf_mat_resampled()
# Confusion Matrix RF
ab_rf_workflow %>% 
  conf_mat_resampled()

# ROC Curves
ab_glm_workflow %>% 
  collect_predictions() %>% 
  group_by(id) %>% 
  roc_curve(sex, .pred_A) %>% 
  autoplot()

ab_rf_workflow %>% 
  collect_predictions() %>% 
  group_by(id) %>% 
  roc_curve(sex, .pred_A) %>% 
  autoplot() + theme_apricot() + theme(legend.position = "none")

# Testing

ab_final <- workflow() %>% 
  add_formula(sex~.) %>% 
  add_model(ab_rf) %>% 
  last_fit(ab_split)

# Results

ab_final %>% collect_metrics()
ab_final %>% collect_predictions() %>% conf_mat(sex, .pred_class)

# Errors
# Type I - 101/741
# Type II - 69/709

ab_final$.workflow[[1]]


