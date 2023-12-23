
# Setup

library(dplyr)
library(readr)
library(lubridate)
library(tidymodels)
library(ggplot2)
library(modelsummary) 

filepath <- "data/UK-HPI-full-file-2023-06.csv"
uk_hpi <- read_csv(filepath)

selected_cols <- c("Date", "RegionName", "SalesVolume")

df_uk <-
    uk_hpi %>%
    filter(RegionName == "United Kingdom") %>%
    select(all_of(selected_cols)) %>%
    rename(date = Date, region = RegionName, sales_volume = SalesVolume) %>%
    mutate(date = dmy(date)) # to make sure that date is a date object

df_uk_train <- 
    df_uk %>% 
    filter(date <= ymd("2019-12-31"))

df_uk_test <- 
    df_uk %>% 
    filter(date >= ymd("2020-01-01"))

# Creation of a linear model

## For df_uk_train :

df_uk_train <-
    df_uk_train %>%
    drop_na(sales_volume)

my_recip <- 
    recipe(sales_volume ~ ., data = df_uk_train) %>%  
    step_arrange(date) %>% # I sort the data by date before using step_lag()
    step_lag(sales_volume, lag=1:12) %>%
    update_role(date, region, new_role = "id") %>% #`Date` and `RegionName` have the role of a 'predictor', but I want to use them as 'identifiers' for that row
    step_naomit(all_predictors(), skip=FALSE) %>% # I remove any rows with missing values
    prep() 

my_recip %>%
  summary() %>%
  knitr::kable()

my_recip %>% 
    bake(df_uk_train) %>% 
    arrange(date)

lm_model <- 
    linear_reg() %>%
    set_engine("lm")

wflow <- 
    workflow() %>% 
    add_recipe(my_recip) %>% 
    add_model(lm_model)

model1 <- 
    wflow %>% 
    fit(data = df_uk_train)

msummary(model1)

model1 %>% 
  glance() %>%
  knitr::kable()

## For df_uk_test :

df_uk_test <-
    df_uk_test %>%
    drop_na(sales_volume)

my_recip %>% 
    bake(df_uk_test) %>% 
    arrange(date)

model2 <- 
    wflow %>% 
    fit(data = df_uk_test)

msummary(model2)

model2 %>% 
  glance() %>%
  knitr::kable()

# How well my model fits the data

fitted_model1 <- model1 %>% extract_fit_parsnip()

df_uk_train_baked <- my_recip %>% bake(df_uk_train)

plot_df_uk_train_bake_fitted <- fitted_model1 %>% augment(new_data = df_uk_train_baked) 

g1 <- ggplot(plot_df_uk_train_bake_fitted, aes(x = .pred, y = .resid)) +
    geom_point(alpha=0.2, size=3, color="red", stroke=1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted values", y = "Residuals", title="Model1 : df_uk_train: Residuals vs Fitted") + 
    theme_bw() + 
    theme(axis.title.x = element_text(size = rel(1.2)), 
          axis.text.x = element_text(size = rel(1.2)),
          axis.title.y = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(size = rel(1.5), face = "bold"))
g1

mae_df_uk_train <- 
    plot_df_uk_train_bake_fitted %>% 
    mae(truth = sales_volume, estimate = .pred)

mae_df_uk_train %>%
  knitr::kable()

plot_df_uk_train_bake_fitted %>% 
  summary() %>%
  knitr::kable()

# How well my model predicts the future? 

fitted_model2 <- model2 %>% extract_fit_parsnip()

df_uk_test_baked <- my_recip %>% bake(df_uk_test)

plot_df_uk_test_bake_fitted <- fitted_model2 %>% augment(new_data = df_uk_test_baked) 

g2 <- ggplot(plot_df_uk_test_bake_fitted, aes(x = .pred, y = .resid)) +
    geom_point(alpha=0.2, size=3, color="red", stroke=1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted values", y = "Residuals", title="Model2 : df_uk_test: Residuals vs Fitted") + 
    theme_bw() + 
    theme(axis.title.x = element_text(size = rel(1.2)), 
          axis.text.x = element_text(size = rel(1.2)),
          axis.title.y = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(size = rel(1.5), face = "bold"))
g2

mae_df_uk_test <- 
    plot_df_uk_test_bake_fitted %>% 
    mae(truth = sales_volume, estimate = .pred)

mae_df_uk_test %>%
  knitr::kable()

plot_df_uk_test_bake_fitted %>% 
  summary() %>%
  knitr::kable()

# Explanation of my choices

# For df_uk_train :
  
df_uk_train_1_lag <- 
  df_uk_train %>%
  arrange(date) %>% # Sort in ascending order so we can use lag 
  mutate(sales_volume_lag1 = lag(sales_volume, 1)) %>%
  drop_na() %>%
  arrange(desc(date)) # sort by date in descending order just to make it easier to read

# For df_uk_test :
  
df_uk_test_1_lag <- 
  df_uk_test %>%
  arrange(date) %>% # Sort in ascending order so we can use lag 
  mutate(sales_volume_lag1 = lag(sales_volume, 1)) %>%
  drop_na() %>%
  arrange(desc(date)) # sort by date in descending order just to make it easier to read

model1_1_lag <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(sales_volume ~ sales_volume_lag1, data = df_uk_test_1_lag)

model2_1_lag <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(sales_volume ~ sales_volume_lag1, data = df_uk_train_1_lag)

model1_1_lag %>% 
  tidy() %>%
  knitr::kable()

model1_1_lag %>% 
  glance() %>%
  knitr::kable()

model2_1_lag %>% 
  tidy() %>%
  knitr::kable()

model2_1_lag %>% 
  glance() %>%
  knitr::kable()
