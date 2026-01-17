rm(list = ls())
options(warn = -1, scipen = 999)

# 1. PEMUATAN PAKEJ 
suppressPackageStartupMessages({
  library(tidyverse)
  library(tsibble)
  library(feasts)
  library(scales)
  library(lubridate)
  library(knitr)
  library(kableExtra)
  library(lmtest)      
  library(vars)        
  library(tseries)     
  library(patchwork)   
  library(urca)        
})

# 2. TETAPAN TEMA PLOT 
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

# 3. IMPORT DATA 
data <- read.csv(file.choose(), header = TRUE)

# Tukar kepada format tsibble
ts_tbl <- data %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

print("Ringkasan Data:")
print(glimpse(ts_tbl))

# 4. PEMERIKSAAN KEPEGUNAN DATA (ADF TEST) 
# --- Ujian ADF: Kedatangan Pelancong (Y) ---
y_series <- ts_tbl$y
adf_y_original <- adf.test(y_series, alternative = "stationary")
print(adf_y_original)

# Check p-value Y
if(adf_y_original$p.value < 0.05) {
  y_stationary <- TRUE
} else {
  y_stationary <- FALSE
  # Jika tak pegun, test difference
  y_diff <- diff(y_series)
  print(adf.test(y_diff, alternative = "stationary"))
}

# --- Ujian ADF: Kadar Pertukaran (X) ---
x_series <- ts_tbl$sgd
adf_x_original <- adf.test(x_series, alternative = "stationary")
print(adf_x_original)

# Check p-value X
if(adf_x_original$p.value < 0.05) {
  x_stationary <- TRUE
} else {
  x_stationary <- FALSE
  # Jika tak pegun, test difference
  x_diff <- diff(x_series)
  print(adf.test(x_diff, alternative = "stationary"))
}

# Penentuan Data untuk Granger (Level vs Difference)
if(y_stationary && x_stationary) {
  use_diff <- FALSE
  y_for_granger <- y_series
  x_for_granger <- x_series
  print("NOTE: Analisis dijalankan pada data tahap (Level).")
} else {
  use_diff <- TRUE
  y_for_granger <- diff(y_series)
  x_for_granger <- diff(x_series)
  print("NOTE: Analisis dijalankan pada data pembezaan pertama (First Difference).")
}

# 5. UJIAN KOINTEGRASI (JIKA PERLU)
if(use_diff) {
  # Prepare data
  coint_data <- data.frame(y = y_series, x = x_series)
  
  # Lag selection for cointegration
  lag_select_coint <- VARselect(coint_data, lag.max = 12, type = "const")
  optimal_lag_coint <- lag_select_coint$selection["AIC(n)"]
  
  # Johansen Test (Trace & Eigen)
  johansen_trace <- ca.jo(coint_data, type = "trace", ecdet = "const", K = optimal_lag_coint)
  johansen_eigen <- ca.jo(coint_data, type = "eigen", ecdet = "const", K = optimal_lag_coint)
  
  print("--- Johansen Trace Test ---")
  print(summary(johansen_trace))
  
  print("--- Johansen Eigen Test ---")
  print(summary(johansen_eigen))
  
  # Logic check for conclusion variable
  trace_stats <- johansen_trace@teststat
  trace_cval <- johansen_trace@cval
  
  if(trace_stats[1] > trace_cval[1,2]) {
    cointegrated <- TRUE
    num_coint <- 1
    if(trace_stats[2] > trace_cval[2,2]) num_coint <- 2
  } else {
    cointegrated <- FALSE
    num_coint <- 0
  }
} else {
  cointegrated <- FALSE
}

# 6. PEMILIHAN LAG OPTIMUM & DIAGNOSTIK VAR
# Prepare VAR data
var_data <- data.frame(y = y_for_granger, x = x_for_granger)

# Lag selection
lag_select <- VARselect(var_data, lag.max = 12, type = "const")
print(lag_select$selection)

optimal_lag <- lag_select$selection["AIC(n)"]

# Model VAR untuk diagnostik
var_model <- VAR(var_data, p = optimal_lag, type = "const")

# Diagnostik
serial_test <- serial.test(var_model, lags.pt = 16, type = "PT.asymptotic")
norm_test <- normality.test(var_model, multivariate.only = TRUE)
stability_roots <- roots(var_model)

print(serial_test)
print(norm_test)
print(stability_roots)

# Boolean checks for summary logic
serial_ok <- serial_test$serial$p.value > 0.05
stability_ok <- all(stability_roots < 1)

# 7. UJIAN KAUSALITI GRANGER
# Ujian 1: X -> Y
granger_x_to_y <- grangertest(y ~ x, order = optimal_lag, data = var_data)
print(granger_x_to_y)
p_value_x_to_y <- granger_x_to_y$`Pr(>F)`[2]
f_stat_x_to_y <- granger_x_to_y$F[2]
x_causes_y <- p_value_x_to_y < 0.05

# Ujian 2: Y -> X
granger_y_to_x <- grangertest(x ~ y, order = optimal_lag, data = var_data)
print(granger_y_to_x)
p_value_y_to_x <- granger_y_to_x$`Pr(>F)`[2]
f_stat_y_to_x <- granger_y_to_x$F[2]
y_causes_x <- p_value_y_to_x < 0.05

# 8. RINGKASAN DATA UNTUK PLOT 
# Tentukan jenis kausaliti untuk tajuk plot
if(x_causes_y && y_causes_x) {
  causality_type <- "Dua Hala (Bidirectional)"
} else if(x_causes_y) {
  causality_type <- "Sehala (X -> Y)"
} else if(y_causes_x) {
  causality_type <- "Sehala (Y -> X)"
} else {
  causality_type <- "Tiada Kausaliti"
}

granger_summary <- tibble(
  Arah = c("X -> Y", "Y -> X"),
  `F-statistic` = c(f_stat_x_to_y, f_stat_y_to_x),
  `p-value` = c(p_value_x_to_y, p_value_y_to_x),
  Lag = optimal_lag,
  Signifikan = c(x_causes_y, y_causes_x)
)

print(granger_summary)

# 9. VISUALISASI 
# Plot 1: Diagram Kausaliti
causality_data <- data.frame(
  x = c(1, 3),
  y = c(2, 2),
  label = c("Kadar Pertukaran\nMYR/SGD", "Kedatangan\nPelancong")
)

p_causality <- ggplot(causality_data, aes(x = x, y = y)) +
  geom_point(size = 40, shape = 21, fill = c("#E3F2FD", "#E8F5E9"), 
             color = "black", stroke = 2) +
  geom_text(aes(label = label), size = 4, fontface = "bold") +
  {if(x_causes_y) 
    geom_segment(aes(x = 1.4, y = 2, xend = 2.6, yend = 2),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "darkgreen", linewidth = 1.5)
  } +
  {if(y_causes_x)
    geom_segment(aes(x = 2.6, y = 1.85, xend = 1.4, yend = 1.85),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "darkblue", linewidth = 1.5)
  } +
  labs(
    title = "Diagram Kausaliti Granger",
    subtitle = sprintf("Jenis Kausaliti: %s (Lag = %d)", causality_type, optimal_lag)
  ) +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(1.5, 2.5)) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p_causality)

# Plot 2: Bar Plot Hasil Ujian
plot_data <- granger_summary %>%
  mutate(
    Direction = c("X -> Y", "Y -> X"),
    Significant = Signifikan,
    F_value = `F-statistic`
  )

p_results <- ggplot(plot_data, aes(x = Direction, y = F_value, fill = Significant)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = qf(0.95, optimal_lag, nrow(var_data) - 2*optimal_lag - 1),
             linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = sprintf("F = %.2f\np = %s", 
                                F_value, 
                                format.pval(`p-value`, digits = 3))),
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(
    title = "Hasil Ujian Kausaliti Granger",
    subtitle = sprintf("Lag = %d, Aras Keertian = 5%%", optimal_lag),
    x = "Arah Kausaliti",
    y = "F-statistic",
    fill = "Signifikan?"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#4CAF50", "FALSE" = "#E57373"),
    labels = c("Tidak", "Ya")
  ) +
  theme(legend.position = "bottom")

print(p_results)

# Plot 3: Time Series dengan Lag Visualization
p_ts <- ggplot(ts_tbl, aes(x = date)) +
  geom_line(aes(y = scale(y), color = "Kedatangan Pelancong (Y)"), 
            linewidth = 0.8) +
  geom_line(aes(y = scale(sgd), color = "Kadar Pertukaran (X)"), 
            linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Siri Masa Dinormalisasi: Kedatangan Pelancong vs Kadar Pertukaran",
    subtitle = "Data distandardkan untuk perbandingan visual",
    x = "Tahun",
    y = "Nilai Dinormalisasi (Z-score)",
    color = "Pembolehubah"
  ) +
  scale_color_manual(
    values = c("Kedatangan Pelancong (Y)" = "#2196F3", 
               "Kadar Pertukaran (X)" = "#FF5722")
  ) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_ts)