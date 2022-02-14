# Libraries

library(tidyverse)
library(tidymodels)

# Data

heart <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv"))

heart <- heart %>%
  as_tibble() %>%
  mutate(creatinine_phosphokinase = as.double(creatinine_phosphokinase),
         ejection_fraction = as.double(ejection_fraction),
         DEATH_EVENT = as.factor(DEATH_EVENT)) %>% glimpse()


# Test/train Sets

set.seed(1234)

heart_split <- initial_split(heart, strata = DEATH_EVENT)
heart_train <- training(heart_split)
heart_test <- testing(heart_split)

# Logistic Regression

heart_glm <- logistic_reg() %>%
  set_engine("glm")


heart_wf <- workflow() %>%
  add_formula(DEATH_EVENT ~.)

heart_model <- heart_wf %>%
  add_model(heart_glm) %>%
  fit(heart_train)

# Evaluation
heart_model %>% tidy() %>%
  filter(p.value < 0.05)

# Test
heart_final <- heart_model %>%
  last_fit(heart_split)

heart_final %>%
  collect_metrics()

heart_final %>%
  collect_predictions() %>%
  conf_mat(DEATH_EVENT, .pred_class)

heart_final %>%
  collect_predictions() %>%
  roc_curve(DEATH_EVENT, .pred_0) %>%
  autoplot()


