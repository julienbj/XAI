# Load libraries
library(ranger)
library(shapr)
library(ggplot2)
library(data.table)
library(tidyverse)   
library(parsnip)

# Load COMPAS dataset
url <- "https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv"
compas <- fread(url)

# Filter and preprocess
# Removing outliers and identifying values as described in https://www.rdocumentation.org/packages/mlr3fairness/versions/0.3.2/topics/compas; 
compas <- compas[days_b_screening_arrest <= 30 & days_b_screening_arrest >= -30 &
                 is_recid != -1 & c_charge_degree != "O" & score_text != "N/A"]
compas[, high_risk := ifelse(score_text == "High", 1, 0)]

# Select features (might include others as well)
features <- c("age", "sex", "race", "priors_count", "juv_fel_count", "juv_misd_count")
compas <- compas[, c(features, "high_risk"), with = FALSE]
compas[, sex := as.factor(sex)]
compas[, race := as.factor(race)]
compas[, high_risk := factor(high_risk, levels = c(0, 1))]

# Can now divide into train/test data to train black box model (e.g., random forest?)
set.seed(2025)
train_idx <- sample(nrow(compas), 0.8 * nrow(compas))
train   <- compas[train_idx]
test    <- compas[-train_idx]

rf_fit <- rand_forest() %>% set_engine("ranger") %>% set_mode("classification") %>% fit(high_risk ~ ., data=train)

# Use shapr to explain predictions for test data. 
explain_set <- train$high_risk[1:10]
  
train_prob <- predict(rf_fit, new_data=train, type="prob")$".pred_1"
p0 <- mean(train_prob)

explaination_independence <- explain(model = rf_fit$fit, x_explain=explain_set, x_train = train[, !"high_risk"], approach="independence", phi0=p0)

# Might try a small number of test samples to make it less time-consuming
