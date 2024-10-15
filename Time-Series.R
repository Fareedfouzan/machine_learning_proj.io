library(readxl)
library(neuralnet)
library(dplyr)
library(caret)
library(Metrics)
library(ggplot2)


# read in the dataset
uow_consumption <- read_excel("Downloads/uow_consumption.xlsx")
str(uow_consumption)
summary(uow_consumption)
colnames(uow_consumption)[colnames(uow_consumption)=="0.75"] <- "18:00"
colnames(uow_consumption)[colnames(uow_consumption)=="0.79166666666666663"] <- "19:00"
colnames(uow_consumption)[colnames(uow_consumption)=="0.83333333333333337"] <- "20:00"
summary(uow_consumption$"20:00")
uow_consumption_updated <- subset(uow_consumption, select = -c(date))

#AR approach

uow_consumption_new <- c(uow_consumption_updated$"20:00") #accessing the 20:00 column 

input_data_ts <- bind_cols(
  G_prev7 = lag(uow_consumption_new, 8),
  G_prev4 = lag(uow_consumption_new, 5),
  G_prev3 = lag(uow_consumption_new, 4),
  G_prev2 = lag(uow_consumption_new, 3),
  G_prev1 = lag(uow_consumption_new, 2),
  G_current = lag(uow_consumption_new, 1),
  G_pred = uow_consumption_new)

input_data_ts

input_data_ts <- input_data_ts[complete.cases(input_data_ts),]

head(input_data_ts)

str(input_data_ts)


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

norm <- as.data.frame(lapply(input_data_ts, normalize))
summary(norm)


training_data <- norm[1:380,]
test_data <- norm[381:462,]
colnames(training_data)
colnames(test_data)

consumption_model <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = 10, data = training_data, linear.output=TRUE)

consumption_model_2 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = 12, data = training_data, linear.output=TRUE)

consumption_model_3 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(4,5), data = training_data, linear.output=TRUE)

consumption_model_4 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(1,2), data = training_data, linear.output=TRUE)

consumption_model_5 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(6,7), data = training_data, linear.output=TRUE)

consumption_model_6 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(9,10), data = training_data, linear.output=TRUE)

consumption_model_7 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(2,3), data = training_data, linear.output=TRUE)

consumption_model_8 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(5,6), data = training_data, linear.output=TRUE)

consumption_model_9 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(15,16), data = training_data, linear.output=TRUE)

consumption_model_10 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = 9, data = training_data, linear.output=TRUE)

consumption_model_11 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(12,13), data = training_data, linear.output=TRUE)

consumption_model_12 <- neuralnet("G_pred ~ G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(13,14), data = training_data, linear.output=TRUE)

model_results <- predict(consumption_model, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_2 <- predict(consumption_model_2, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_3 <- predict(consumption_model_3, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_4 <- predict(consumption_model_4, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_5 <- predict(consumption_model_5, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_6 <- predict(consumption_model_6, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_7 <- predict(consumption_model_7, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_8 <- predict(consumption_model_8, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_9 <- predict(consumption_model_9, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_10 <- predict(consumption_model_10, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_11 <- predict(consumption_model_11, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_12 <- predict(consumption_model_12, test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
str(model_results_12)
str(test_data[, c("G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])


predicted_strength <- model_results
predicted_strength_2 <- model_results_2
predicted_strength_3 <- model_results_3
predicted_strength_4 <- model_results_4
predicted_strength_5 <- model_results_5
predicted_strength_6 <- model_results_6
predicted_strength_7 <- model_results_7
predicted_strength_8 <- model_results_8
predicted_strength_9 <- model_results_9
predicted_strength_10 <- model_results_10
predicted_strength_11 <- model_results_11
predicted_strength_12 <- model_results_12

print(predicted_strength)
print(predicted_strength_2)
print(predicted_strength_3)
print(predicted_strength_4)
print(predicted_strength_5)
print(predicted_strength_6)
print(predicted_strength_7)
print(predicted_strength_8)
print(predicted_strength_9)
print(predicted_strength_10)
print(predicted_strength_11)
print(predicted_strength_12)

consumption_train_original_strength <- input_data_ts[1:380,"G_pred"]  
consumption_test_original_strength <- input_data_ts[381:462,"G_pred"] 

strength_min <- min(consumption_train_original_strength)
strength_max <- max(consumption_train_original_strength)

print(strength_min)
print(strength_max)

head(consumption_test_original_strength)
print(consumption_test_original_strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

strength_pred <- unnormalize(predicted_strength, strength_min, strength_max)
head(strength_pred)
strength_pred_2 <- unnormalize(predicted_strength_2, strength_min,strength_max)
head(strength_pred_2)
strength_pred_3 <- unnormalize(predicted_strength_3, strength_min,strength_max)
head(strength_pred_3)
strength_pred_4 <- unnormalize(predicted_strength_4, strength_min,strength_max)
head(strength_pred_4)
strength_pred_5 <- unnormalize(predicted_strength_5, strength_min,strength_max)
head(strength_pred_5)
strength_pred_6 <- unnormalize(predicted_strength_6, strength_min,strength_max)
head(strength_pred_6)
strength_pred_7 <- unnormalize(predicted_strength_7, strength_min,strength_max)
head(strength_pred_7)
strength_pred_8 <- unnormalize(predicted_strength_8, strength_min,strength_max)
head(strength_pred_8)
strength_pred_9 <- unnormalize(predicted_strength_9, strength_min,strength_max)
head(strength_pred_9)
strength_pred_10 <- unnormalize(predicted_strength_10, strength_min,strength_max)
head(strength_pred_10)
strength_pred_11 <- unnormalize(predicted_strength_11, strength_min,strength_max)
head(strength_pred_11)
strength_pred_12 <- unnormalize(predicted_strength_12, strength_min,strength_max)
head(strength_pred_12)



rmse <- RMSE(strength_pred, test_data$G_pred)
print(paste0("RMSE: ", rmse))
rmse_2 <- RMSE(strength_pred_2, test_data$G_pred)
print(paste0("RMSE: ", rmse_2))
rmse_3 <- RMSE(strength_pred_3, test_data$G_pred)
print(paste0("RMSE: ", rmse_3))
rmse_4 <- RMSE(strength_pred_4, test_data$G_pred)
print(paste0("RMSE: ", rmse_4))
rmse_5 <- RMSE(strength_pred_5, test_data$G_pred)
print(paste0("RMSE: ", rmse_5))
rmse_6 <- RMSE(strength_pred_6, test_data$G_pred)
print(paste0("RMSE: ", rmse_6))
rmse_7 <- RMSE(strength_pred_7, test_data$G_pred)
print(paste0("RMSE: ", rmse_7))
rmse_8 <- RMSE(strength_pred_8, test_data$G_pred)
print(paste0("RMSE: ", rmse_8))
rmse_9 <- RMSE(strength_pred_9, test_data$G_pred)
print(paste0("RMSE: ", rmse_9))
rmse_10 <- RMSE(strength_pred_10, test_data$G_pred)
print(paste0("RMSE: ", rmse_10))
rmse_11 <- RMSE(strength_pred_11, test_data$G_pred)
print(paste0("RMSE: ", rmse_11))
rmse_12 <- RMSE(strength_pred_12, test_data$G_pred)
print(paste0("RMSE: ", rmse_12))


mae_consumption_model <- MAE(strength_pred, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model))
mae_consumption_model_2 <- MAE(strength_pred_2, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_2))
mae_consumption_model_3 <- MAE(strength_pred_3, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_3))
mae_consumption_model_4 <- MAE(strength_pred_4, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_4))
mae_consumption_model_5 <- MAE(strength_pred_5, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_5))
mae_consumption_model_6 <- MAE(strength_pred_6, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_6))
mae_consumption_model_7 <- MAE(strength_pred_7, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_7))
mae_consumption_model_8 <- MAE(strength_pred_8, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_8))
mae_consumption_model_9 <- MAE(strength_pred_9, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_9))
mae_consumption_model_10 <- MAE(strength_pred_10, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_10))
mae_consumption_model_11 <- MAE(strength_pred_11, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_11))
mae_consumption_model_12 <- MAE(strength_pred_12, test_data$G_pred)
print(paste0("MAE: ", mae_consumption_model_12))

mape_consumption_model <- mape(strength_pred_2, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model))
mape_consumption_model_2 <- mape(strength_pred_2, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_2))
mape_consumption_model_3 <- mape(strength_pred_3, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_3))
mape_consumption_model_4 <- mape(strength_pred_4, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_4))
mape_consumption_model_5 <- mape(strength_pred_5, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_5))
mape_consumption_model_6 <- mape(strength_pred_6, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_6))
mape_consumption_model_7 <- mape(strength_pred_7, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_7))
mape_consumption_model_8 <- mape(strength_pred_8, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_8))
mape_consumption_model_9 <- mape(strength_pred_9, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_9))
mape_consumption_model_10 <- mape(strength_pred_10, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_10))
mape_consumption_model_11 <- mape(strength_pred_11, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_11))
mape_consumption_model_12 <- mape(strength_pred_12, test_data$G_pred)
print(paste0("MAPE: ", mape_consumption_model_12))


smape <- function(actual, predicted) {
  mean(2 * abs(predicted - actual) / (abs(actual) + abs(predicted)))
}


smape_consumption_model <- smape(test_data$G_pred, strength_pred)
print(paste0("sMAPE: ", smape_consumption_model))
smape_consumption_model_2 <- smape(test_data$G_pred, strength_pred_2)
print(paste0("sMAPE: ", smape_consumption_model_2))
smape_consumption_model_3 <- smape(test_data$G_pred, strength_pred_3)
print(paste0("sMAPE: ", smape_consumption_model_3))
smape_consumption_model_4 <- smape(test_data$G_pred, strength_pred_4)
print(paste0("sMAPE: ", smape_consumption_model_4))
smape_consumption_model_5 <- smape(test_data$G_pred, strength_pred_5)
print(paste0("sMAPE: ", smape_consumption_model_5))
smape_consumption_model_6 <- smape(test_data$G_pred, strength_pred_6)
print(paste0("sMAPE: ", smape_consumption_model_6))
smape_consumption_model_7 <- smape(test_data$G_pred, strength_pred_7)
print(paste0("sMAPE: ", smape_consumption_model_7))
smape_consumption_model_8 <- smape(test_data$G_pred, strength_pred_8)
print(paste0("sMAPE: ", smape_consumption_model_8))
smape_consumption_model_9 <- smape(test_data$G_pred, strength_pred_9)
print(paste0("sMAPE: ", smape_consumption_model_9))
smape_consumption_model_10 <- smape(test_data$G_pred, strength_pred_10)
print(paste0("sMAPE: ", smape_consumption_model_10))
smape_consumption_model_11 <- smape(test_data$G_pred, strength_pred_11)
print(paste0("sMAPE: ", smape_consumption_model_11))
smape_consumption_model_12 <- smape(test_data$G_pred, strength_pred_12)
print(paste0("sMAPE: ", smape_consumption_model_12))


results_df <- data.frame(
  Model = c(
    "Consumption Model",
    "Consumption Model 2",
    "Consumption Model 3",
    "Consumption Model 4",
    "Consumption Model 5",
    "Consumption Model 6",
    "Consumption Model 7",
    "Consumption Model 8",
    "Consumption Model 9",
    "Consumption Model 10",
    "Consumption Model 11",
    "Consumption Model 12"
  ),
  RMSE = c(
    rmse,
    rmse_2,
    rmse_3,
    rmse_4,
    rmse_5,
    rmse_6,
    rmse_7,
    rmse_8,
    rmse_9,
    rmse_10,
    rmse_11,
    rmse_12
  ),
  MAE = c(
    mae_consumption_model,
    mae_consumption_model_2,
    mae_consumption_model_3,
    mae_consumption_model_4,
    mae_consumption_model_5,
    mae_consumption_model_6,
    mae_consumption_model_7,
    mae_consumption_model_8,
    mae_consumption_model_9,
    mae_consumption_model_10,
    mae_consumption_model_11,
    mae_consumption_model_12
  ),
  MAPE = c(
    mape_consumption_model,
    mape_consumption_model_2,
    mape_consumption_model_3,
    mape_consumption_model_4,
    mape_consumption_model_5,
    mape_consumption_model_6,
    mape_consumption_model_7,
    mape_consumption_model_8,
    mape_consumption_model_9,
    mape_consumption_model_10,
    mape_consumption_model_11,
    mape_consumption_model_12
  ),
  sMAPE = c(
    smape_consumption_model,
    smape_consumption_model_2,
    smape_consumption_model_3,
    smape_consumption_model_4,
    smape_consumption_model_5,
    smape_consumption_model_6,
    smape_consumption_model_7,
    smape_consumption_model_8,
    smape_consumption_model_9,
    smape_consumption_model_10,
    smape_consumption_model_11,
    smape_consumption_model_12
  ),
  Parameters = NA
)

# calculate the number of parameters for each model and update the data frame
for (i in 1:nrow(results_df)) {
  model_formula <- as.formula(paste("G_pred ~", paste0(colnames(training_data)[-1], collapse = " + ")))
  if (grepl("Model 1$", results_df$Model[i])) {
    nn <- neuralnet(model_formula, data = training_data, hidden = 1)
  } else if (grepl("Model 2$", results_df$Model[i])) {
    nn <- neuralnet(model_formula, data = training_data, hidden = c(5, 2))
  } else {
    nn <- neuralnet(model_formula, data = training_data, hidden = 1)
  }
  num_params <- sum(sapply(nn$weights, length))
  results_df$Parameters[i] <- num_params
}


# format data frame into table using kable
kable(results_df, caption = "Comparison of RMSE, MAE, MAPE, sMAPE, and number of parameters for twelve neural network models")

#NARX approach

uow_consumption_new_2 <- c(uow_consumption_updated$"18:00") #accessing the 18:00 column 
uow_consumption_new_3 <- c(uow_consumption_updated$"19:00") #accessing the 19:00 column 


# create exogenous variables
exog_vars <- bind_cols(uow_consumption_new_2, uow_consumption_new_3)

# rename columns
exog_vars <- exog_vars %>% rename(exog_var_1 = ...1, exog_var_2 = ...2)

summary(exog_vars$exog_var_1)
summary(exog_vars$exog_var_2)
# combine lagged variables, exogenous variables, and target variable
input_data_ts_2 <- bind_cols(
  exog_vars,
  G_prev7 = lag(uow_consumption_new, 8),
  G_prev4 = lag(uow_consumption_new, 5),
  G_prev3 = lag(uow_consumption_new, 4),
  G_prev2 = lag(uow_consumption_new, 3),
  G_prev1 = lag(uow_consumption_new, 2),
  G_current = lag(uow_consumption_new, 1),
  G_pred = uow_consumption_new
)

input_data_ts_2

# create a logical vector indicating which rows of input_data_ts_2 contain complete cases
complete_rows <- complete.cases(input_data_ts_2)

# subset input_data_ts based on the row indices of complete cases in input_data_ts_2
input_data_ts_2 <- input_data_ts_2[complete_rows, ]


head(input_data_ts_2)



normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

norm_2 <- as.data.frame(lapply(input_data_ts_2, normalize))
summary(norm_2)   

training_data_2 <- norm_2[1:380,]
test_data_2 <- norm_2[381:462,]

colnames(training_data_2)
colnames(test_data_2)
names(training_data_2)

consumption_model_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = 10, data = training_data_2, linear.output=TRUE)

consumption_model_2_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = 12, data = training_data_2, linear.output=TRUE)

consumption_model_3_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(4,5), data = training_data_2, linear.output=TRUE)

consumption_model_4_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(1,2), data = training_data_2, linear.output=TRUE)

consumption_model_5_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(6,7), data = training_data_2, linear.output=TRUE)

consumption_model_6_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(9,10), data = training_data_2, linear.output=TRUE)

consumption_model_7_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(2,3), data = training_data_2, linear.output=TRUE)

consumption_model_8_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(5,6), data = training_data_2, linear.output=TRUE)

consumption_model_9_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(15,16), data = training_data_2, linear.output=TRUE)

consumption_model_10_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = 9, data = training_data_2, linear.output=TRUE)

consumption_model_11_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(12,13), data = training_data_2, linear.output=TRUE)

consumption_model_12_narx <- neuralnet("G_pred ~ exog_var_1 + exog_var_2 + G_prev7 + G_prev4 + G_prev3 + G_prev2 + G_prev1 + G_current", hidden = c(13,14), data = training_data_2, linear.output=TRUE)

model_results_narx <- predict(consumption_model_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_2_narx <- predict(consumption_model_2_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_3_narx <- predict(consumption_model_3_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_4_narx <- predict(consumption_model_4_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_5_narx <- predict(consumption_model_5_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_6_narx <- predict(consumption_model_6_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_7_narx <- predict(consumption_model_7_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_8_narx <- predict(consumption_model_8_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_9_narx <- predict(consumption_model_9_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_10_narx <- predict(consumption_model_10_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_11_narx <- predict(consumption_model_11_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])
model_results_12_narx <- predict(consumption_model_12_narx, test_data_2[, c("exog_var_1","exog_var_2","G_prev7", "G_prev4", "G_prev3", "G_prev2", "G_prev1", "G_current")])


predicted_strength <- model_results_narx
predicted_strength_2 <- model_results_2_narx
predicted_strength_3 <- model_results_3_narx
predicted_strength_4 <- model_results_4_narx
predicted_strength_5 <- model_results_5_narx
predicted_strength_6 <- model_results_6_narx
predicted_strength_7 <- model_results_7_narx
predicted_strength_8 <- model_results_8_narx
predicted_strength_9 <- model_results_9_narx
predicted_strength_10 <- model_results_10_narx
predicted_strength_11 <- model_results_11_narx
predicted_strength_12 <- model_results_12_narx

consumption_train_original_strength <- input_data_ts_2[1:380,"G_pred"]
consumption_test_original_strength <- input_data_ts_2[381:462,"G_pred"]

strength_min <- min(consumption_train_original_strength)
strength_max <- max(consumption_train_original_strength)

print(strength_min)
print(strength_max)

head(consumption_test_original_strength)
print(consumption_test_original_strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

strength_pred <- unnormalize(predicted_strength, strength_min, strength_max)
head(strength_pred)
strength_pred_2 <- unnormalize(predicted_strength_2, strength_min,strength_max)
head(strength_pred_2)
strength_pred_3 <- unnormalize(predicted_strength_3, strength_min,strength_max)
head(strength_pred_3)
strength_pred_4 <- unnormalize(predicted_strength_4, strength_min,strength_max)
head(strength_pred_4)
strength_pred_5 <- unnormalize(predicted_strength_5, strength_min,strength_max)
head(strength_pred_5)
strength_pred_6 <- unnormalize(predicted_strength_6, strength_min,strength_max)
head(strength_pred_6)
strength_pred_7 <- unnormalize(predicted_strength_7, strength_min,strength_max)
head(strength_pred_7)
strength_pred_8 <- unnormalize(predicted_strength_8, strength_min,strength_max)
head(strength_pred_8)
strength_pred_9 <- unnormalize(predicted_strength_9, strength_min,strength_max)
head(strength_pred_9)
strength_pred_10 <- unnormalize(predicted_strength_10, strength_min,strength_max)
head(strength_pred_10)
strength_pred_11 <- unnormalize(predicted_strength_11, strength_min,strength_max)
head(strength_pred_11)
strength_pred_12 <- unnormalize(predicted_strength_12, strength_min,strength_max)
head(strength_pred_12)


rmse_narx <- RMSE(strength_pred, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_narx))
rmse_2_narx <- RMSE(strength_pred_2, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_2_narx))
rmse_3_narx <- RMSE(strength_pred_3, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_3_narx))
rmse_4_narx <- RMSE(strength_pred_4, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_4_narx))
rmse_5_narx <- RMSE(strength_pred_5, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_5_narx))
rmse_6_narx <- RMSE(strength_pred_6, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_6_narx))
rmse_7_narx <- RMSE(strength_pred_7, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_7_narx))
rmse_8_narx <- RMSE(strength_pred_8, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_8_narx))
rmse_9_narx <- RMSE(strength_pred_9, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_9_narx))
rmse_10_narx <- RMSE(strength_pred_10, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_10_narx))
rmse_11_narx <- RMSE(strength_pred_11, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_11_narx))
rmse_12_narx <- RMSE(strength_pred_12, test_data_2$G_pred)
print(paste0("RMSE: ", rmse_12_narx))


mae_consumption_model_narx <- MAE(strength_pred, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_narx))
mae_consumption_model_2_narx <- MAE(strength_pred_2, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_2_narx))
mae_consumption_model_3_narx <- MAE(strength_pred_3, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_3_narx))
mae_consumption_model_4_narx <- MAE(strength_pred_4, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_4_narx))
mae_consumption_model_5_narx <- MAE(strength_pred_5, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_5_narx))
mae_consumption_model_6_narx <- MAE(strength_pred_6, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_6_narx))
mae_consumption_model_7_narx <- MAE(strength_pred_7, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_7_narx))
mae_consumption_model_8_narx <- MAE(strength_pred_8, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_8_narx))
mae_consumption_model_9_narx <- MAE(strength_pred_9, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_9_narx))
mae_consumption_model_10_narx <- MAE(strength_pred_10, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_10_narx))
mae_consumption_model_11_narx <- MAE(strength_pred_11, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_11_narx))
mae_consumption_model_12_narx <- MAE(strength_pred_12, test_data_2$G_pred)
print(paste0("MAE: ", mae_consumption_model_12_narx))

mape_consumption_model_narx <- mape(strength_pred_2, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_narx))
mape_consumption_model_2_narx <- mape(strength_pred_2, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_2_narx))
mape_consumption_model_3_narx <- mape(strength_pred_3, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_3_narx))
mape_consumption_model_4_narx <- mape(strength_pred_4, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_4_narx))
mape_consumption_model_5_narx <- mape(strength_pred_5, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_5_narx))
mape_consumption_model_6_narx <- mape(strength_pred_6, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_6_narx))
mape_consumption_model_7_narx <- mape(strength_pred_7, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_7_narx))
mape_consumption_model_8_narx <- mape(strength_pred_8, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_8_narx))
mape_consumption_model_9_narx <- mape(strength_pred_9, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_9_narx))
mape_consumption_model_10_narx <- mape(strength_pred_10, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_10_narx))
mape_consumption_model_11_narx <- mape(strength_pred_11, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_11_narx))
mape_consumption_model_12_narx <- mape(strength_pred_12, test_data_2$G_pred)
print(paste0("MAPE: ", mape_consumption_model_12_narx))


smape <- function(actual, predicted) {
  mean(2 * abs(predicted - actual) / (abs(actual) + abs(predicted)))
}


smape_consumption_model_narx <- smape(test_data_2$G_pred, strength_pred)
print(paste0("sMAPE: ", smape_consumption_model_narx))
smape_consumption_model_2_narx <- smape(test_data_2$G_pred, strength_pred_2)
print(paste0("sMAPE: ", smape_consumption_model_2_narx))
smape_consumption_model_3_narx <- smape(test_data_2$G_pred, strength_pred_3)
print(paste0("sMAPE: ", smape_consumption_model_3_narx))
smape_consumption_model_4_narx <- smape(test_data_2$G_pred, strength_pred_4)
print(paste0("sMAPE: ", smape_consumption_model_4_narx))
smape_consumption_model_5_narx <- smape(test_data_2$G_pred, strength_pred_5)
print(paste0("sMAPE: ", smape_consumption_model_5_narx))
smape_consumption_model_6_narx <- smape(test_data_2$G_pred, strength_pred_6)
print(paste0("sMAPE: ", smape_consumption_model_6_narx))
smape_consumption_model_7_narx <- smape(test_data_2$G_pred, strength_pred_7)
print(paste0("sMAPE: ", smape_consumption_model_7_narx))
smape_consumption_model_8_narx <- smape(test_data_2$G_pred, strength_pred_8)
print(paste0("sMAPE: ", smape_consumption_model_8_narx))
smape_consumption_model_9_narx <- smape(test_data_2$G_pred, strength_pred_9)
print(paste0("sMAPE: ", smape_consumption_model_9_narx))
smape_consumption_model_10_narx <- smape(test_data_2$G_pred, strength_pred_10)
print(paste0("sMAPE: ", smape_consumption_model_10_narx))
smape_consumption_model_11_narx <- smape(test_data_2$G_pred, strength_pred_11)
print(paste0("sMAPE: ", smape_consumption_model_11_narx))
smape_consumption_model_12_narx <- smape(test_data_2$G_pred, strength_pred_12)
print(paste0("sMAPE: ", smape_consumption_model_12_narx))

results_df_2 <- data.frame(
  Model = c(
    "Consumption Model",
    "Consumption Model 2",
    "Consumption Model 3",
    "Consumption Model 4",
    "Consumption Model 5",
    "Consumption Model 6",
    "Consumption Model 7",
    "Consumption Model 8",
    "Consumption Model 9",
    "Consumption Model 10",
    "Consumption Model 11",
    "Consumption Model 12"
  ),
  RMSE = c(
    rmse_narx,
    rmse_2_narx,
    rmse_3_narx,
    rmse_4_narx,
    rmse_5_narx,
    rmse_6_narx,
    rmse_7_narx,
    rmse_8_narx,
    rmse_9_narx,
    rmse_10_narx,
    rmse_11_narx,
    rmse_12_narx
  ),
  MAE = c(
    mae_consumption_model_narx,
    mae_consumption_model_2_narx,
    mae_consumption_model_3_narx,
    mae_consumption_model_4_narx,
    mae_consumption_model_5_narx,
    mae_consumption_model_6_narx,
    mae_consumption_model_7_narx,
    mae_consumption_model_8_narx,
    mae_consumption_model_9_narx,
    mae_consumption_model_10_narx,
    mae_consumption_model_11_narx,
    mae_consumption_model_12_narx
  ),
  MAPE = c(
    mape_consumption_model_narx,
    mape_consumption_model_2_narx,
    mape_consumption_model_3_narx,
    mape_consumption_model_4_narx,
    mape_consumption_model_5_narx,
    mape_consumption_model_6_narx,
    mape_consumption_model_7_narx,
    mape_consumption_model_8_narx,
    mape_consumption_model_9_narx,
    mape_consumption_model_10_narx,
    mape_consumption_model_11_narx,
    mape_consumption_model_12_narx
  ),
  sMAPE = c(
    smape_consumption_model_narx,
    smape_consumption_model_2_narx,
    smape_consumption_model_3_narx,
    smape_consumption_model_4_narx,
    smape_consumption_model_5_narx,
    smape_consumption_model_6_narx,
    smape_consumption_model_7_narx,
    smape_consumption_model_8_narx,
    smape_consumption_model_9_narx,
    smape_consumption_model_10_narx,
    smape_consumption_model_11_narx,
    smape_consumption_model_12_narx
  ),
  Parameters = NA
)

# calculate the number of parameters for each model and update the data frame
for (i in 1:nrow(results_df_2)) {
  model_formula_2 <- as.formula(paste("G_pred ~", paste0(colnames(training_data_2)[-1], collapse = " + ")))
  if (grepl("Model 1$", results_df_2$Model[i])) {
    nn <- neuralnet(model_formula_2, data = training_data_2, hidden = 3)
  } else if (grepl("Model 2$", results_df_2$Model[i])) {
    nn <- neuralnet(model_formula_2, data = training_data_2, hidden = c(5, 2))
  } else {
    nn <- neuralnet(model_formula_2, data = training_data_2, hidden = 3)
  }
  num_params <- sum(sapply(nn$weights, length))
  results_df_2$Parameters[i] <- num_params
}


# format data frame into table using kable

kable(results_df_2, caption = "Comparison of RMSE, MAE, MAPE, sMAPE, and number of parameters for twelve neural network models")


#AR scatter plot graph for two models to compare and check

# create a dataframe with the actual and predicted values
results_df <- data.frame(actual = test_data$G_pred, predicted = model_results)

# create the plot
ggplot(data = results_df, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  xlab("Actual") +
  ylab("Predicted") +
  ggtitle("Actual vs. Predicted") 

ggplot(data = results_df, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  xlab("Actual") +
  ylab("Predicted") +
  ggtitle("Actual vs. Predicted") +
  geom_text(x = max(results_df$actual) - 0.1 * diff(range(results_df$actual)),
            y = max(results_df$predicted) - 0.1 * diff(range(results_df$predicted)),
            label = paste0("RMSE: ", round(rmse, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue") +
  geom_text(x = max(results_df$actual) - 0.1 * diff(range(results_df$actual)),
            y = max(results_df$predicted) - 0.2 * diff(range(results_df$predicted)),
            label = paste0("MAE: ", round(mae_consumption_model, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue") +
  geom_text(x = max(results_df$actual) - 0.1 * diff(range(results_df$actual)),
            y = max(results_df$predicted) - 0.3 * diff(range(results_df$predicted)),
            label = paste0("MAPE: ", round(mape_consumption_model, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue") +
  geom_text(x = max(results_df$actual) - 0.1 * diff(range(results_df$actual)),
            y = max(results_df$predicted) - 0.4 * diff(range(results_df$predicted)),
            label = paste0("sMAPE: ", round(smape_consumption_model, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue")
 

# create a dataframe with the actual and predicted values
results_df_4 <- data.frame(actual = test_data$G_pred, predicted = model_results_2)

# create the plot
ggplot(data = results_df_4, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  xlab("Actual") +
  ylab("Predicted") +
  ggtitle("Actual vs. Predicted") 




#NARX scatter plot graph for two models to compare and check

# create a dataframe with the actual and predicted values
results_df_2 <- data.frame(actual = test_data_2$G_pred, predicted = model_results_narx)

# create the plot
ggplot(data = results_df_2, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  xlab("Actual") +
  ylab("Predicted") +
  ggtitle("Actual vs. Predicted")

# create a dataframe with the actual and predicted values
results_df_3 <- data.frame(actual = test_data_2$G_pred, predicted = model_results_2_narx)

# create the plot
ggplot(data = results_df_3, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  xlab("Actual") +
  ylab("Predicted") +
  ggtitle("Actual vs. Predicted")

ggplot(data = results_df_3, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  xlab("Actual") +
  ylab("Predicted") +
  ggtitle("Actual vs. Predicted") +
  geom_text(x = max(results_df_3$actual) - 0.1 * diff(range(results_df_3$actual)),
            y = max(results_df_3$predicted) - 0.1 * diff(range(results_df_3$predicted)),
            label = paste0("RMSE: ", round(rmse_narx, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue") +
  geom_text(x = max(results_df_3$actual) - 0.1 * diff(range(results_df_3$actual)),
            y = max(results_df_3$predicted) - 0.2 * diff(range(results_df_3$predicted)),
            label = paste0("MAE: ", round(mae_consumption_model_narx, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue") +
  geom_text(x = max(results_df_3$actual) - 0.1 * diff(range(results_df_3$actual)),
            y = max(results_df_3$predicted) - 0.3 * diff(range(results_df_3$predicted)),
            label = paste0("MAPE: ", round(mape_consumption_model_narx, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue") +
  geom_text(x = max(results_df_3$actual) - 0.1 * diff(range(results_df_3$actual)),
            y = max(results_df_3$predicted) - 0.4 * diff(range(results_df_3$predicted)),
            label = paste0("sMAPE: ", round(smape_consumption_model_narx, 2), "%"), 
            hjust = 1, vjust = 1, color = "blue")

