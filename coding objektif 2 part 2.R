# Ramalan 2025 (SARIMAX & LSTM)
# Run this after completing main modeling script

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
})

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
    axis.title = element_text(face = "bold", size = 10)
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

# Prepare future data (Jan-Jun 2025)
cat("\n--- Data Masa Hadapan (Jan-Jun 2025) ---\n\n")

future_data_2025 <- new_data(ts_tbl_interv, n = 6) %>%
  mutate(
    pandemic_dummy = 0,
    month_num = month(date),
    month_sin = sin(2 * pi * month_num / 12),
    month_cos = cos(2 * pi * month_num / 12)
  )

cat("Tempoh ramalan: Januari - Jun 2025 (6 bulan)\n\n")

# ============================================================================
# RAMALAN SARIMAX 2025
# ============================================================================

cat("--- Ramalan SARIMAX 2025 ---\n\n")

# Build model on full data (2013-2024)
refit_sarimax <- ts_tbl_interv %>%
  model(sarimax_interv = ARIMA(y ~ pandemic_dummy))

cat("Model SARIMAX trained on full data\n\n")

# Generate forecasts
sarimax_fc_2025 <- forecast(refit_sarimax, new_data = future_data_2025)

cat("Ramalan SARIMAX (Jan-Jun 2025):\n")

sarimax_fc_2025_display <- sarimax_fc_2025 %>%
  as_tibble() %>%
  select(date, .mean) %>%
  mutate(
    Bulan = format(as.Date(date), "%B %Y"),
    Ramalan = format(round(.mean, 0), big.mark = ",")
  ) %>%
  select(Bulan, Ramalan)

print(sarimax_fc_2025_display, n = Inf)
cat("\n")

# ============================================================================
# RAMALAN LSTM 2025
# ============================================================================

cat("--- Ramalan LSTM 2025 ---\n\n")

# Load Keras
suppressPackageStartupMessages({
  library(keras)
  library(tensorflow)
})

# Set seeds
set.seed(42)
tensorflow::tf$random$set_seed(42)
reticulate::py_set_seed(42)

# Define scaling functions
y_train_vec <- ts_tbl_interv$y
min_val <- min(y_train_vec)
max_val <- max(y_train_vec)

scale_data <- function(x) { (x - min_val) / (max_val - min_val) }
unscale_data <- function(x) { (x * (max_val - min_val)) + min_val }

# Scale data
full_scaled_tbl <- ts_tbl_interv %>%
  mutate(y_scaled = scale_data(y))

# Hyperparameters
n_steps <- 12
n_features <- 4

# Create sequences function
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

# Prepare training data
feature_cols <- c("y_scaled", "pandemic_dummy", "month_sin", "month_cos")
full_matrix <- as.matrix(full_scaled_tbl[, feature_cols])

full_seq <- create_sequences(full_matrix, n_steps)
X_full_train <- full_seq$X
y_full_train <- full_seq$y

# Build and train LSTM
model_final <- keras_model_sequential() %>%
  layer_lstm(units = 100, activation = "relu", 
             input_shape = c(n_steps, n_features)) %>%
  layer_dense(units = 1)

model_final %>% compile(loss = 'mean_squared_error', optimizer = 'adam')

history_final <- model_final %>% fit(
  X_full_train, y_full_train,
  epochs = 100,
  batch_size = 16,
  verbose = 0
)

cat("Model LSTM trained on full data\n\n")

# Generate forecasts
forecast_scaled_2025_vec <- numeric(6)
current_batch_data_2025 <- full_matrix[(nrow(full_matrix) - n_steps + 1):nrow(full_matrix), ]
future_features_2025 <- future_data_2025 %>%
  as.data.frame() %>%
  select(pandemic_dummy, month_sin, month_cos)

for (i in 1:6) {
  current_batch_arr <- array_reshape(current_batch_data_2025, c(1, n_steps, n_features))
  pred_scaled <- model_final %>% predict(current_batch_arr, verbose = 0)
  forecast_scaled_2025_vec[i] <- pred_scaled
  
  new_row <- c(pred_scaled, future_features_2025$pandemic_dummy[i],
               future_features_2025$month_sin[i], future_features_2025$month_cos[i])
  current_batch_data_2025 <- rbind(current_batch_data_2025[-1, ], new_row)
}

# Unscale forecasts
forecast_unscaled_2025_vec <- unscale_data(forecast_scaled_2025_vec)
lstm_fc_2025 <- future_data_2025 %>%
  mutate(forecast_lstm = forecast_unscaled_2025_vec) %>%
  select(date, forecast_lstm)

cat("Ramalan LSTM (Jan-Jun 2025):\n")

lstm_fc_2025_display <- lstm_fc_2025 %>%
  mutate(
    Bulan = format(as.Date(date), "%B %Y"),
    Ramalan = format(round(forecast_lstm, 0), big.mark = ",")
  ) %>%
  select(Bulan, Ramalan)

print(lstm_fc_2025_display, n = Inf)
cat("\n")

# ============================================================================
# PLOT PERBANDINGAN
# ============================================================================

cat("--- Visualisasi Perbandingan ---\n\n")

history_plot_data <- ts_tbl_interv %>% filter(year(date) >= 2020)
vline_date <- yearmonth("2025 Jan")

p_comparison_2025 <- ggplot(history_plot_data, aes(x = date, y = y)) +
  geom_line(aes(color = "Data Sebenar"), linewidth = 1.2) +
  geom_line(data = sarimax_fc_2025, aes(y = .mean, color = "Ramalan SARIMAX"),
            linewidth = 1.1, linetype = "dashed") +
  geom_line(data = lstm_fc_2025, aes(y = forecast_lstm, color = "Ramalan LSTM"),
            linewidth = 1.1, linetype = "dashed") +
  geom_vline(xintercept = vline_date, linetype = "dotted", 
             color = "grey20", linewidth = 0.8) +
  annotate("text", x = vline_date, y = max(history_plot_data$y) * 0.95,
           label = "Permulaan Ramalan  ", hjust = 1.1, vjust = 1,
           color = "grey20", size = 3.5, fontface = "bold") +
  labs(
    title = "Perbandingan Ramalan Model SARIMAX & LSTM (2025)",
    subtitle = "Data Sebenar (2020-2024) vs Ramalan 6-Bulan (Jan-Jun 2025)",
    x = "Tahun",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(name = "Siri / Model",
                     values = c("Data Sebenar" = "black",
                               "Ramalan SARIMAX" = "red",
                               "Ramalan LSTM" = "darkgreen"))

print(p_comparison_2025)

# Comparison table
comparison_2025 <- bind_cols(
  sarimax_fc_2025_display,
  lstm_fc_2025_display %>% select(Ramalan) %>% rename(`Ramalan LSTM` = Ramalan)
) %>%
  rename(`Ramalan SARIMAX` = Ramalan)

cat("\nPerbandingan SARIMAX vs LSTM (2025):\n")
print(comparison_2025, n = Inf)
cat("\n")

# ============================================================================
# VALIDASI DATA SEBENAR (JAN-JUN 2025)
# ============================================================================

cat("\n--- Validasi: Data Sebenar vs Ramalan (Jan-Jun 2025) ---\n\n")

# Input actual data
actual_values_2025 <- c(2458711, 1853122, 2054968, 1990120, 2131096, 2363204)

# Combine actual vs predicted
validation_df <- tibble(
  date = lstm_fc_2025$date,
  Actual = actual_values_2025,
  SARIMAX_Pred = sarimax_fc_2025$.mean,
  LSTM_Pred = lstm_fc_2025$forecast_lstm
)

cat("Perbandingan terperinci:\n")

validation_display <- validation_df %>%
  mutate(
    Bulan = format(as.Date(date), "%B %Y"),
    Actual_Fmt = format(Actual, big.mark = ","),
    SARIMAX_Fmt = format(round(SARIMAX_Pred), big.mark = ","),
    LSTM_Fmt = format(round(LSTM_Pred), big.mark = ","),
    `Ralat SARIMAX (%)` = sprintf("%.2f%%", abs((Actual - SARIMAX_Pred)/Actual)*100),
    `Ralat LSTM (%)` = sprintf("%.2f%%", abs((Actual - LSTM_Pred)/Actual)*100)
  ) %>%
  select(Bulan, Actual_Fmt, SARIMAX_Fmt, `Ralat SARIMAX (%)`, 
         LSTM_Fmt, `Ralat LSTM (%)`) %>%
  rename(`Data Sebenar` = Actual_Fmt,
         `Ramalan SARIMAX` = SARIMAX_Fmt,
         `Ramalan LSTM` = LSTM_Fmt)

print(validation_display, n = Inf)
cat("\n")

# Calculate accuracy metrics
calc_accuracy_manual <- function(actual, predicted, model_name) {
  errors <- actual - predicted
  mape <- mean(abs(errors / actual)) * 100
  rmse <- sqrt(mean(errors^2))
  mae <- mean(abs(errors))
  
  data.frame(
    Model = model_name,
    MAPE = sprintf("%.2f%%", mape),
    RMSE = format(round(rmse, 2), big.mark = ",", scientific = FALSE),
    MAE = format(round(mae, 2), big.mark = ",", scientific = FALSE)
  )
}

acc_sarimax_2025 <- calc_accuracy_manual(validation_df$Actual, 
                                         validation_df$SARIMAX_Pred, "SARIMAX")
acc_lstm_2025 <- calc_accuracy_manual(validation_df$Actual, 
                                      validation_df$LSTM_Pred, "LSTM")

final_accuracy_table <- bind_rows(acc_sarimax_2025, acc_lstm_2025)

cat("Prestasi model (validasi 6 bulan):\n")
print(final_accuracy_table)
cat("\n")

# Validation plot
plot_data_final <- validation_df %>%
  select(date, Actual, SARIMAX_Pred, LSTM_Pred) %>%
  pivot_longer(cols = c(Actual, SARIMAX_Pred, LSTM_Pred),
               names_to = "Jenis", values_to = "Nilai")

p_validasi <- ggplot(plot_data_final, aes(x = date, y = Nilai, 
                                           color = Jenis, linetype = Jenis)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Actual" = "black", 
                                "SARIMAX_Pred" = "red", 
                                "LSTM_Pred" = "blue"),
                     labels = c("Data Sebenar", "Ramalan LSTM", "Ramalan SARIMAX")) +
  scale_linetype_manual(values = c("Actual" = "solid", 
                                   "SARIMAX_Pred" = "dashed", 
                                   "LSTM_Pred" = "dashed"),
                        labels = c("Data Sebenar", "Ramalan LSTM", "Ramalan SARIMAX")) +
  labs(title = "Validasi Akhir: Data Sebenar vs Ramalan (Jan-Jun 2025)",
       y = "Jumlah Pelancong", x = "Bulan") +
  theme_classic() +
  scale_y_continuous(labels = label_number(big.mark = ","))

print(p_validasi)

options(warn = 0)
