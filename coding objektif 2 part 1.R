# Objektif 2: Pemodelan Ramalan Siri Masa
# Kedatangan Pelancong Antarabangsa ke Malaysia
# Model: SARIMAX dan LSTM dengan Pembolehubah Intervensi COVID-19

rm(list = ls())
options(warn = -1, scipen = 999)

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tsibble)
  library(fable)
  library(fabletools)
  library(feasts)
  library(scales)
  library(lubridate)
  library(knitr)
  library(kableExtra)
  library(forecast)
})

options(tibble.width = Inf, tibble.print_max = Inf)

# Tetapan tema plot
plot_theme <- theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9)
  )

theme_set(plot_theme)
y_axis_format <- label_number(big.mark = ",", accuracy = 1)

# Import data
data <- read.csv(file.choose(), header = TRUE)

ts_tbl <- data %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Feature engineering
ts_tbl_interv <- ts_tbl %>%
  mutate(
    pandemic_dummy = ifelse(date >= yearmonth("2020 Mar") & 
                            date <= yearmonth("2022 Mar"), 1, 0),
    month_num = month(date),
    month_sin = sin(2 * pi * month_num / 12),
    month_cos = cos(2 * pi * month_num / 12)
  )

# Pembahagian data
train_set <- ts_tbl_interv %>% filter(year(date) <= 2022)
test_set <- ts_tbl_interv %>% filter(year(date) >= 2023)

cat("\nData latihan:", nrow(train_set), "baris (2013-2022)\n")
cat("Data ujian:", nrow(test_set), "baris (2023-2024)\n\n")

# ============================================================================
# MODEL SARIMAX
# ============================================================================

cat("\n--- MODEL SARIMAX ---\n\n")

# Build model
sarimax_fits <- train_set %>%
  model(SARIMAX = ARIMA(y ~ pandemic_dummy))

cat("Spesifikasi model:\n")
print(report(sarimax_fits))

# Diagnostik sisaan
cat("\nDiagnostik sisaan:\n")
p_residuals_sarimax <- sarimax_fits %>%
  select(SARIMAX) %>%
  gg_tsresiduals() +
  labs(title = "Diagnostik Sisaan Model SARIMAX")

print(p_residuals_sarimax)

# Ljung-Box test
lb_test <- sarimax_fits %>%
  select(SARIMAX) %>%
  residuals() %>%
  features(.resid, ljung_box, lag = 24)

cat("\nUjian Ljung-Box:\n")
print(lb_test)

# Ramalan
sarimax_forecast <- sarimax_fits %>%
  select(SARIMAX) %>%
  forecast(new_data = test_set)

# Metrik kejituan
cat("\nMetrik kejituan SARIMAX:\n")
acc_sarimax <- accuracy(sarimax_forecast, test_set) %>%
  select(.model, .type, RMSE, MAE, MAPE, MASE) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print(acc_sarimax, width = Inf)

# Plot ramalan
p_forecast_sarimax <- sarimax_forecast %>%
  autoplot(data = ts_tbl_interv, level = 95, alpha = 0.5) +
  labs(
    title = "Perbandingan Ramalan SARIMAX vs Data Sebenar",
    subtitle = "Model dengan Intervensi COVID-19 (2023-2024)",
    x = "Tahun",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_forecast_sarimax)

# ============================================================================
# MODEL LSTM
# ============================================================================

cat("\n--- MODEL LSTM ---\n\n")

# Load Keras
suppressPackageStartupMessages({
  library(keras)
  library(tensorflow)
})

# Set seed
set.seed(42)
tensorflow::tf$random$set_seed(42)
reticulate::py_set_seed(42)

# Penskalaan data
y_train_vec <- train_set$y
min_val <- min(y_train_vec)
max_val <- max(y_train_vec)

scale_data <- function(x) { (x - min_val) / (max_val - min_val) }
unscale_data <- function(x) { (x * (max_val - min_val)) + min_val }

train_scaled_tbl <- train_set %>% mutate(y_scaled = scale_data(y))
test_scaled_tbl <- test_set %>% mutate(y_scaled = scale_data(y))

cat("Data berjaya diskalakan (0-1)\n")

# Hiperparameter
n_steps <- 12
n_features <- 4

cat("\nHiperparameter:\n")
cat("- Timesteps:", n_steps, "bulan\n")
cat("- Features:", n_features, "\n")
cat("- LSTM units: 100\n")
cat("- Epochs: 100\n")
cat("- Batch size: 16\n\n")

# Fungsi create sequences
create_sequences <- function(data_matrix, n_steps) {
  X_list <- list()
  y_list <- list()
  
  for (i in 1:(nrow(data_matrix) - n_steps)) {
    X_list[[i]] <- data_matrix[i:(i + n_steps - 1), ]
    y_list[[i]] <- data_matrix[i + n_steps, 1]
  }
  
  X_arr <- array(unlist(X_list), dim = c(n_steps, n_features, length(X_list)))
  X_arr <- aperm(X_arr, c(3, 1, 2))
  y_vec <- unlist(y_list)
  
  list(X = X_arr, y = y_vec)
}

# Prepare training sequences
feature_cols <- c("y_scaled", "pandemic_dummy", "month_sin", "month_cos")
train_matrix <- as.matrix(train_scaled_tbl[, feature_cols])

train_seq <- create_sequences(train_matrix, n_steps)
X_train <- train_seq$X
y_train <- train_seq$y

cat("Bentuk X_train:", paste(dim(X_train), collapse = " x "), "\n")
cat("Panjang y_train:", length(y_train), "\n\n")

# Build LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 100, activation = "relu", 
             input_shape = c(n_steps, n_features)) %>%
  layer_dense(units = 1)

model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')

cat("Ringkasan model:\n")
summary(model)

# Train model
cat("\nMemulakanlatihan model...\n")
history <- model %>% fit(
  X_train, y_train,
  epochs = 100,
  batch_size = 16,
  validation_split = 0.1,
  verbose = 1
)

cat("\nLatihan selesai\n")

# Plot training history
p_history <- plot(history) +
  labs(title = "Sejarah Latihan Model LSTM")
print(p_history)

# Generate forecasts
cat("\nMenghasilkan ramalan...\n")

forecast_scaled_vec <- numeric(nrow(test_set))
current_batch_data <- train_matrix[(nrow(train_matrix) - n_steps + 1):nrow(train_matrix), ]
future_features <- test_set %>% select(pandemic_dummy, month_sin, month_cos)

for (i in 1:nrow(test_set)) {
  current_batch_arr <- array_reshape(current_batch_data, c(1, n_steps, n_features))
  pred_scaled <- model %>% predict(current_batch_arr, verbose = 0)
  forecast_scaled_vec[i] <- pred_scaled
  
  new_row <- c(pred_scaled, future_features$pandemic_dummy[i],
               future_features$month_sin[i], future_features$month_cos[i])
  current_batch_data <- rbind(current_batch_data[-1, ], new_row)
}

forecast_unscaled_vec <- unscale_data(forecast_scaled_vec)
results_tbl <- test_set %>% mutate(forecast_lstm = forecast_unscaled_vec)

# Metrik kejituan
cat("\nMetrik kejituan LSTM:\n")

errors <- results_tbl$y - results_tbl$forecast_lstm
rmse_val <- sqrt(mean(errors^2))
mae_val <- mean(abs(errors))
mape_val <- mean(abs(errors / results_tbl$y)) * 100

y_train_vec_mase <- train_set$y
naive_errors <- abs(y_train_vec_mase[(12 + 1):length(y_train_vec_mase)] -
                    y_train_vec_mase[1:(length(y_train_vec_mase) - 12)])
mae_naive <- mean(naive_errors)
mase_val <- mae_val / mae_naive

acc_lstm <- tibble(
  .model = "LSTM",
  .type = "Test",
  RMSE = rmse_val,
  MAE = mae_val,
  MAPE = mape_val,
  MASE = mase_val
) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print(acc_lstm, width = Inf)

# Plot ramalan LSTM
plot_data_lstm <- ts_tbl %>%
  left_join(results_tbl %>% select(date, forecast_lstm), by = "date")

p_forecast_lstm <- ggplot(plot_data_lstm, aes(x = date)) +
  geom_line(aes(y = y, color = "Data Sebenar"), linewidth = 1) +
  geom_line(aes(y = forecast_lstm, color = "Ramalan LSTM"), linewidth = 1) +
  labs(
    title = "Perbandingan Ramalan LSTM vs Data Sebenar",
    subtitle = "Model dengan Intervensi COVID-19 (2023-2024)",
    x = "Tahun",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(name = "Data",
                     values = c("Data Sebenar" = "black", "Ramalan LSTM" = "red"))

print(p_forecast_lstm)

# ============================================================================
# PERBANDINGAN MODEL
# ============================================================================

cat("\n--- PERBANDINGAN MODEL ---\n\n")

comparison_table <- bind_rows(
  acc_sarimax %>% select(.model, RMSE, MAE, MAPE, MASE),
  acc_lstm %>% select(.model, RMSE, MAE, MAPE, MASE)
) %>%
  mutate(
    Rank_RMSE = rank(RMSE),
    Rank_MAE = rank(MAE),
    Rank_MAPE = rank(MAPE),
    Rank_MASE = rank(MASE)
  )

cat("Perbandingan metrik:\n")
print(comparison_table %>% select(.model, RMSE, MAE, MAPE, MASE), width = Inf)

best_model <- comparison_table %>%
  mutate(Total_Rank = Rank_RMSE + Rank_MAE + Rank_MAPE + Rank_MASE) %>%
  arrange(Total_Rank) %>%
  slice(1) %>%
  pull(.model)

cat("\nModel terbaik:", best_model, "\n")

# Diebold-Mariano test
cat("\nUjian Diebold-Mariano:\n")

e_sarimax <- test_set$y - sarimax_forecast$.mean
e_lstm <- test_set$y - results_tbl$forecast_lstm

dm_test_result <- dm.test(e_sarimax, e_lstm, h = 1, alternative = "two.sided")

cat("DM Statistic:", round(dm_test_result$statistic, 4), "\n")
cat("p-value:", format.pval(dm_test_result$p.value, digits = 4), "\n")

if(dm_test_result$p.value < 0.05) {
  cat("\nKesimpulan: Terdapat perbezaan signifikan antara model (p < 0.05)\n")
} else {
  cat("\nKesimpulan: Tiada perbezaan signifikan antara model (p >= 0.05)\n")
}
