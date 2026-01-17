rm(list = ls())
options(warn = -1, scipen = 999)

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

options(tibble.width = Inf)
options(tibble.print_max = Inf)

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

data <- read.csv(file.choose(), header = TRUE)

# Tukar kepada format tsibble
ts_tbl <- data %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Mencipta pembolehubah luaran dan ciri kemusiman
ts_tbl_interv <- ts_tbl %>%
  mutate(
    # Pembolehubah dummy untuk intervensi pandemik COVID-19
    # 1 = Semasa pandemik (Mac 2020 - Mac 2022)
    # 0 = Sebaliknya
    pandemic_dummy = ifelse(
      date >= yearmonth("2020 Mar") & date <= yearmonth("2022 Mar"), 1, 0
    ),
    # Ciri-ciri kemusiman (untuk model LSTM)
    month_num = month(date),
    month_sin = sin(2 * pi * month_num / 12),
    month_cos = cos(2 * pi * month_num / 12)
  )

# Data latihan: 2013-2022
train_set <- ts_tbl_interv %>%
  filter(year(date) <= 2022)

# Data ujian: 2023-2024
test_set <- ts_tbl_interv %>%
  filter(year(date) >= 2023)

# Pembinaan model SARIMAX dengan automatic order selection
sarimax_fits <- train_set %>%
  model(
    SARIMAX = ARIMA(y ~ pandemic_dummy)
  )

# Laporan model
print(report(sarimax_fits))

# Plot diagnostik sisaan
p_residuals_sarimax <- sarimax_fits %>%
  select(SARIMAX) %>%
  gg_tsresiduals() +
  labs(title = "Diagnostik Sisaan Model SARIMAX",
       subtitle = "Semakan Andaian: Sisaan Rawak, Normal, dan Tidak Berkorelasi")

print(p_residuals_sarimax)


# Ujian Ljung-Box untuk semak autokorelasi sisaan
lb_test <- sarimax_fits %>%
  select(SARIMAX) %>%
  residuals() %>%
  features(.resid, ljung_box, lag = 24)

print(lb_test)

# Hasilkan ramalan pada data ujian
sarimax_forecast <- sarimax_fits %>%
  select(SARIMAX) %>%
  forecast(new_data = test_set)

# Kira metrik kejituan
acc_sarimax <- accuracy(sarimax_forecast, test_set) %>%
  select(.model, .type, RMSE, MAE, MAPE, MASE) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print(acc_sarimax, width = Inf)

p_forecast_sarimax <- sarimax_forecast %>%
  autoplot(data = ts_tbl_interv, level = 95, alpha = 0.5) +
  labs(
    title = "Perbandingan Ramalan SARIMAX vs Data Sebenar",
    subtitle = "Model dengan Intervensi COVID-19 (Diuji pada 2023-2024)",
    x = "Tahun",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_forecast_sarimax)

suppressPackageStartupMessages({
  library(keras)
  library(tensorflow)
})


# Menetapkan seed untuk kebolehulangan
set.seed(42)
tensorflow::tf$random$set_seed(42)
reticulate::py_set_seed(42)

# Tentukan parameter penskalaan berdasarkan DATA LATIHAN 
y_train_vec <- train_set$y
min_val <- min(y_train_vec)
max_val <- max(y_train_vec)

cat(sprintf("Min (dari data latihan): %s\n", format(min_val, big.mark = ",")))
cat(sprintf("Max (dari data latihan): %s\n", format(max_val, big.mark = ",")))

# Fungsi penskalaan dan nyahskalaan
scale_data <- function(x) { (x - min_val) / (max_val - min_val) }
unscale_data <- function(x) { (x * (max_val - min_val)) + min_val }

# Skalakan data latihan dan ujian
train_scaled_tbl <- train_set %>% mutate(y_scaled = scale_data(y))
test_scaled_tbl <- test_set %>% mutate(y_scaled = scale_data(y))

n_steps <- 12  # Look-back window (12 bulan)
n_features <- 4  # (y_scaled, pandemic_dummy, month_sin, month_cos)

cat(sprintf("- Timesteps (n_steps)  : %d bulan\n", n_steps))
cat(sprintf("- Bilangan Ciri        : %d (y_scaled, pandemic_dummy, month_sin, month_cos)\n", n_features))
cat(sprintf("- LSTM Units           : 100\n"))
cat(sprintf("- Activation           : ReLU\n"))
cat(sprintf("- Optimizer            : Adam\n"))
cat(sprintf("- Loss Function        : MSE\n"))
cat(sprintf("- Epochs               : 100\n"))
cat(sprintf("- Batch Size           : 16\n"))
cat(sprintf("- Validation Split     : 10%%\n"))

create_sequences <- function(data_matrix, n_steps) {
  X_list <- list()
  y_list <- list()
  
  for (i in 1:(nrow(data_matrix) - n_steps)) {
    X_list[[i]] <- data_matrix[i:(i + n_steps - 1), ]
    y_list[[i]] <- data_matrix[i + n_steps, 1]  # Target: y_scaled
  }
  
  # Convert to 3D array: (samples, timesteps, features)
  X_arr <- array(
    unlist(X_list),
    dim = c(n_steps, n_features, length(X_list))
  )
  X_arr <- aperm(X_arr, c(3, 1, 2))
  y_vec <- unlist(y_list)
  
  list(X = X_arr, y = y_vec)
}

feature_cols <- c("y_scaled", "pandemic_dummy", "month_sin", "month_cos")
train_matrix <- as.matrix(train_scaled_tbl[, feature_cols])

train_seq <- create_sequences(train_matrix, n_steps)
X_train <- train_seq$X
y_train <- train_seq$y

cat(sprintf("  - Bentuk X_train: (%d, %d, %d)\n", 
            dim(X_train)[1], dim(X_train)[2], dim(X_train)[3]))
cat(sprintf("  - Panjang y_train: %d\n", length(y_train)))

model <- keras_model_sequential() %>%
  layer_lstm(
    units = 100,
    activation = "relu",
    input_shape = c(n_steps, n_features)
  ) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

summary(model)

history <- model %>% fit(
  X_train, y_train,
  epochs = 100,
  batch_size = 16,
  validation_split = 0.1,
  verbose = 1
)

# Plot sejarah latihan
p_history <- plot(history) +
  labs(title = "Sejarah Latihan Model LSTM",
       subtitle = "Loss vs Epochs (Training & Validation)")

print(p_history)

# Ramalan iteratif
forecast_scaled_vec <- numeric(nrow(test_set))
current_batch_data <- train_matrix[(nrow(train_matrix) - n_steps + 1):nrow(train_matrix), ]
future_features <- test_set %>% select(pandemic_dummy, month_sin, month_cos)

for (i in 1:nrow(test_set)) {
  current_batch_arr <- array_reshape(
    current_batch_data, c(1, n_steps, n_features)
  )
  pred_scaled <- model %>% predict(current_batch_arr, verbose = 0)
  forecast_scaled_vec[i] <- pred_scaled
  
  new_row <- c(
    pred_scaled,
    future_features$pandemic_dummy[i],
    future_features$month_sin[i],
    future_features$month_cos[i]
  )
  current_batch_data <- rbind(current_batch_data[-1, ], new_row)
}

# Nyahskalakan ramalan
forecast_unscaled_vec <- unscale_data(forecast_scaled_vec)
results_tbl <- test_set %>%
  mutate(forecast_lstm = forecast_unscaled_vec)

# Kira metrik kejituan
errors <- results_tbl$y - results_tbl$forecast_lstm
rmse_val <- sqrt(mean(errors^2))
mae_val <- mean(abs(errors))
mape_val <- mean(abs(errors / results_tbl$y)) * 100

# Kira MASE (Mean Absolute Scaled Error)
y_train_vec_mase <- train_set$y
naive_errors <- abs(
  y_train_vec_mase[(12 + 1):length(y_train_vec_mase)] -
    y_train_vec_mase[1:(length(y_train_vec_mase) - 12)]
)
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

plot_data_lstm <- ts_tbl %>%
  left_join(
    results_tbl %>% select(date, forecast_lstm),
    by = "date"
  )

p_forecast_lstm <- ggplot(plot_data_lstm, aes(x = date)) +
  geom_line(aes(y = y, color = "Data Sebenar"), linewidth = 1) +
  geom_line(
    aes(y = forecast_lstm, color = "Ramalan LSTM"),
    linewidth = 1,
    linetype = "solid"
  ) +
  labs(
    title = "Perbandingan Ramalan LSTM vs Data Sebenar",
    subtitle = "Model dengan Intervensi COVID-19 (Diuji pada 2023-2024)",
    x = "Tahun",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    name = "Data",
    values = c("Data Sebenar" = "black", "Ramalan LSTM" = "red")
  )

print(p_forecast_lstm)

# Gabungkan metrik kejituan
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

print(comparison_table %>% select(.model, RMSE, MAE, MAPE, MASE), width = Inf)

# Dapatkan vektor ralat untuk kedua-dua model
e_sarimax <- test_set$y - sarimax_forecast$.mean
e_lstm <- test_set$y - results_tbl$forecast_lstm

# Ujian Diebold-Mariano
dm_test_result <- dm.test(e_sarimax, e_lstm, h = 1, alternative = "two.sided")

cat(sprintf("DM Statistic: %.4f\n", dm_test_result$statistic))
cat(sprintf("p-value: %s\n", format.pval(dm_test_result$p.value, digits = 4)))