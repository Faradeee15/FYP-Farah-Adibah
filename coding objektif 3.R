# Objektif 3: Ujian Kausaliti Granger
# Hubungan Kausaliti antara Kadar Pertukaran MYR/SGD dan
# Kedatangan Pelancong Antarabangsa ke Malaysia

rm(list = ls())
options(warn = -1, scipen = 999)

# Load packages
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

cat(sprintf("\nData: %d pemerhatian\n", nrow(ts_tbl)))
cat(sprintf("Tempoh: %s hingga %s\n\n", 
            format(min(as.Date(ts_tbl$date)), "%B %Y"),
            format(max(as.Date(ts_tbl$date)), "%B %Y")))

# ============================================================================
# PEMERIKSAAN KEPEGUNAN DATA
# ============================================================================

cat("--- Ujian Kepegunan ---\n\n")

# Ujian ADF untuk Y (Kedatangan Pelancong)
cat("Kedatangan Pelancong (Y):\n")
y_series <- ts_tbl$y
adf_y_original <- adf.test(y_series, alternative = "stationary")

cat(sprintf("ADF Statistic: %.4f, p-value: %s\n", 
            adf_y_original$statistic,
            format.pval(adf_y_original$p.value, digits = 4)))

if(adf_y_original$p.value < 0.05) {
  cat("Data pegun (stationary)\n\n")
  y_stationary <- TRUE
} else {
  cat("Data tidak pegun (non-stationary)\n")
  y_diff <- diff(y_series)
  adf_y_diff <- adf.test(y_diff, alternative = "stationary")
  cat(sprintf("Pembezaan 1 - ADF: %.4f, p-value: %s\n", 
              adf_y_diff$statistic,
              format.pval(adf_y_diff$p.value, digits = 4)))
  if(adf_y_diff$p.value < 0.05) cat("Pegun selepas pembezaan\n")
  cat("\n")
  y_stationary <- FALSE
}

# Ujian ADF untuk X (Kadar Pertukaran)
cat("Kadar Pertukaran MYR/SGD (X):\n")
x_series <- ts_tbl$sgd
adf_x_original <- adf.test(x_series, alternative = "stationary")

cat(sprintf("ADF Statistic: %.4f, p-value: %s\n", 
            adf_x_original$statistic,
            format.pval(adf_x_original$p.value, digits = 4)))

if(adf_x_original$p.value < 0.05) {
  cat("Data pegun (stationary)\n\n")
  x_stationary <- TRUE
} else {
  cat("Data tidak pegun (non-stationary)\n")
  x_diff <- diff(x_series)
  adf_x_diff <- adf.test(x_diff, alternative = "stationary")
  cat(sprintf("Pembezaan 1 - ADF: %.4f, p-value: %s\n", 
              adf_x_diff$statistic,
              format.pval(adf_x_diff$p.value, digits = 4)))
  if(adf_x_diff$p.value < 0.05) cat("Pegun selepas pembezaan\n")
  cat("\n")
  x_stationary <- FALSE
}

# Keputusan
if(y_stationary && x_stationary) {
  cat("Keputusan: Kedua-dua pembolehubah pegun\n")
  cat("Ujian Granger pada data asal\n\n")
  use_diff <- FALSE
  y_for_granger <- y_series
  x_for_granger <- x_series
} else {
  cat("Keputusan: Sekurang-kurangnya satu pembolehubah tidak pegun\n")
  cat("Ujian Granger pada data pembezaan pertama\n\n")
  use_diff <- TRUE
  y_for_granger <- diff(y_series)
  x_for_granger <- diff(x_series)
}

# ============================================================================
# UJIAN KOINTEGRASI
# ============================================================================

if(use_diff) {
  cat("\n--- Ujian Kointegrasi Johansen ---\n\n")
  
  coint_data <- data.frame(y = y_series, x = x_series)
  
  # Lag selection
  lag_select_coint <- VARselect(coint_data, lag.max = 12, type = "const")
  optimal_lag_coint <- lag_select_coint$selection["AIC(n)"]
  cat(sprintf("Lag optimum (AIC): %d\n\n", optimal_lag_coint))
  
  # Johansen test
  johansen_trace <- ca.jo(coint_data, type = "trace", ecdet = "const", 
                          K = optimal_lag_coint)
  
  cat("Hasil Ujian Trace:\n")
  print(summary(johansen_trace))
  
  # Extract results
  trace_stats <- johansen_trace@teststat
  trace_cval <- johansen_trace@cval
  
  cat(sprintf("\nr = 0: Test Stat = %.4f, Critical (5%%) = %.4f\n", 
              trace_stats[1], trace_cval[1,2]))
  
  if(trace_stats[1] > trace_cval[1,2]) {
    cat("Keputusan: Terdapat kointegrasi\n")
    cointegrated <- TRUE
    
    # Extract cointegrating equation
    coint_vector <- johansen_trace@V[,1]
    cat(sprintf("\nPersamaan kointegrasi:\n"))
    cat(sprintf("y = %.4f × x + %.4f\n", 
                -coint_vector[2]/coint_vector[1], 
                -coint_vector[3]/coint_vector[1]))
    cat("\nInterpretasi: Wujud hubungan keseimbangan jangka panjang\n\n")
  } else {
    cat("Keputusan: Tiada kointegrasi\n")
    cat("Interpretasi: Tiada hubungan jangka panjang\n\n")
    cointegrated <- FALSE
  }
} else {
  cat("\n--- Ujian Kointegrasi ---\n")
  cat("Tidak diperlukan (kedua-dua pembolehubah sudah pegun)\n\n")
  cointegrated <- FALSE
}

# ============================================================================
# PEMILIHAN LAG OPTIMUM
# ============================================================================

cat("--- Pemilihan Lag Optimum ---\n\n")

if(use_diff) {
  var_data <- data.frame(y = y_for_granger, x = x_for_granger)
} else {
  var_data <- data.frame(y = y_for_granger, x = x_for_granger)
}

lag_select <- VARselect(var_data, lag.max = 12, type = "const")
cat("Hasil pemilihan lag:\n")
print(lag_select$selection)

optimal_lag <- lag_select$selection["AIC(n)"]
cat(sprintf("\nLag optimum (AIC): %d\n\n", optimal_lag))

# ============================================================================
# DIAGNOSTIK MODEL VAR
# ============================================================================

cat("--- Diagnostik Model VAR ---\n\n")

var_model <- VAR(var_data, p = optimal_lag, type = "const")

# Autokorelasi
cat("1. Ujian Autokorelasi (Portmanteau):\n")
serial_test_adj <- serial.test(var_model, lags.pt = 16, type = "PT.adjusted")
print(serial_test_adj)

if(serial_test_adj$serial$p.value > 0.05) {
  cat("\nTiada autokorelasi signifikan (p > 0.05)\n")
  serial_ok <- TRUE
} else {
  cat("\nWujud autokorelasi (p < 0.05)\n")
  serial_ok <- FALSE
}

# Kenormalan
cat("\n2. Ujian Kenormalan (Jarque-Bera):\n")
norm_test <- normality.test(var_model, multivariate.only = TRUE)
print(norm_test)

if(norm_test$jb.mul$JB$p.value > 0.05) {
  cat("\nSisaan bertaburan normal (p > 0.05)\n")
  normality_ok <- TRUE
} else {
  cat("\nSisaan tidak normal (p < 0.05)\n")
  cat("Nota: Ujian Granger tetap sah untuk sampel besar\n")
  normality_ok <- FALSE
}

# Kestabilan
cat("\n3. Ujian Kestabilan:\n")
roots <- roots(var_model)
cat("Eigenvalues:\n")
print(roots)

all_stable <- all(roots < 1)
if(all_stable) {
  cat("\nModel stabil (semua eigenvalues < 1)\n")
  stability_ok <- TRUE
} else {
  cat("\nModel tidak stabil\n")
  stability_ok <- FALSE
}

# Ringkasan diagnostik
diagnostics_summary <- tibble(
  Ujian = c("Autokorelasi", "Kenormalan", "Kestabilan"),
  Keputusan = c(
    ifelse(serial_ok, "Lulus", "Gagal"),
    ifelse(normality_ok, "Lulus", "Gagal"),
    ifelse(stability_ok, "Lulus", "Gagal")
  )
)

cat("\nRingkasan diagnostik:\n")
print(diagnostics_summary, n = Inf)
cat("\n")

# ============================================================================
# UJIAN KAUSALITI GRANGER
# ============================================================================

cat("\n--- Ujian Kausaliti Granger ---\n\n")

# Test 1: X → Y (Kadar Pertukaran → Kedatangan)
cat("1. Kadar Pertukaran → Kedatangan Pelancong:\n")
granger_x_to_y <- grangertest(y ~ x, order = optimal_lag, data = var_data)
print(granger_x_to_y)

p_value_x_to_y <- granger_x_to_y$`Pr(>F)`[2]
f_stat_x_to_y <- granger_x_to_y$F[2]

cat(sprintf("\nF-statistic: %.4f, p-value: %s\n", 
            f_stat_x_to_y, format.pval(p_value_x_to_y, digits = 4)))

if(p_value_x_to_y < 0.05) {
  cat("Keputusan: Kadar pertukaran GRANGER-CAUSE kedatangan pelancong\n\n")
  x_causes_y <- TRUE
} else {
  cat("Keputusan: Kadar pertukaran TIDAK Granger-cause kedatangan pelancong\n\n")
  x_causes_y <- FALSE
}

# Test 2: Y → X (Kedatangan → Kadar Pertukaran)
cat("2. Kedatangan Pelancong → Kadar Pertukaran:\n")
granger_y_to_x <- grangertest(x ~ y, order = optimal_lag, data = var_data)
print(granger_y_to_x)

p_value_y_to_x <- granger_y_to_x$`Pr(>F)`[2]
f_stat_y_to_x <- granger_y_to_x$F[2]

cat(sprintf("\nF-statistic: %.4f, p-value: %s\n", 
            f_stat_y_to_x, format.pval(p_value_y_to_x, digits = 4)))

if(p_value_y_to_x < 0.05) {
  cat("Keputusan: Kedatangan pelancong GRANGER-CAUSE kadar pertukaran\n\n")
  y_causes_x <- TRUE
} else {
  cat("Keputusan: Kedatangan pelancong TIDAK Granger-cause kadar pertukaran\n\n")
  y_causes_x <- FALSE
}

# ============================================================================
# RINGKASAN DAN INTERPRETASI
# ============================================================================

cat("\n--- Ringkasan Hasil ---\n\n")

# Summary table
granger_summary <- tibble(
  Arah = c("X → Y (SGD → Kedatangan)", "Y → X (Kedatangan → SGD)"),
  `F-statistic` = c(f_stat_x_to_y, f_stat_y_to_x),
  `p-value` = c(p_value_x_to_y, p_value_y_to_x),
  Lag = c(optimal_lag, optimal_lag),
  Keputusan = c(
    ifelse(x_causes_y, "Signifikan", "Tidak Signifikan"),
    ifelse(y_causes_x, "Signifikan", "Tidak Signifikan")
  ),
  Kausaliti = c(
    ifelse(x_causes_y, "Ya", "Tidak"),
    ifelse(y_causes_x, "Ya", "Tidak")
  )
)

print(granger_summary %>%
        mutate(`F-statistic` = round(`F-statistic`, 4),
               `p-value` = format.pval(`p-value`, digits = 4)), 
      n = Inf, width = Inf)
cat("\n")

# Interpretasi
cat("Interpretasi:\n")

if(x_causes_y && y_causes_x) {
  cat("- Jenis kausaliti: DUA HALA (Bidirectional)\n")
  cat("- Kedua-dua pembolehubah saling mempengaruhi\n")
  causality_type <- "Dua Hala"
} else if(x_causes_y && !y_causes_x) {
  cat("- Jenis kausaliti: SEHALA (X → Y)\n")
  cat("- Kadar pertukaran mempengaruhi kedatangan pelancong\n")
  causality_type <- "Sehala (X → Y)"
} else if(!x_causes_y && y_causes_x) {
  cat("- Jenis kausaliti: SEHALA (Y → X)\n")
  cat("- Kedatangan pelancong mempengaruhi kadar pertukaran\n")
  causality_type <- "Sehala (Y → X)"
} else {
  cat("- Jenis kausaliti: TIADA\n")
  cat("- Tiada hubungan kausaliti Granger\n")
  causality_type <- "Tiada"
}

if(exists("cointegrated") && cointegrated) {
  cat("\nNota: Wujud kointegrasi (hubungan jangka panjang)\n")
}

# ============================================================================
# VISUALISASI
# ============================================================================

cat("\n--- Visualisasi ---\n\n")

# Diagram kausaliti
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
    subtitle = sprintf("Jenis: %s (Lag = %d)", causality_type, optimal_lag)
  ) +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(1.5, 2.5)) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p_causality)

# Bar plot hasil ujian
plot_data <- granger_summary %>%
  mutate(
    Direction = c("X → Y", "Y → X"),
    Significant = Keputusan == "Signifikan",
    F_value = `F-statistic`
  )

p_results <- ggplot(plot_data, aes(x = Direction, y = F_value, fill = Significant)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = qf(0.95, optimal_lag, nrow(var_data) - 2*optimal_lag - 1),
             linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = sprintf("F = %.2f\np = %s", 
                                `F-statistic`, 
                                format.pval(`p-value`, digits = 3))),
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(
    title = "Hasil Ujian Kausaliti Granger",
    subtitle = sprintf("Lag = %d, α = 5%%", optimal_lag),
    x = "Arah Kausaliti",
    y = "F-statistic",
    fill = "Signifikan?"
  ) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#E57373"),
                    labels = c("Tidak", "Ya")) +
  theme(legend.position = "bottom")

print(p_results)

# Time series plot
p_ts <- ggplot(ts_tbl, aes(x = date)) +
  geom_line(aes(y = scale(y), color = "Kedatangan Pelancong (Y)"), linewidth = 0.8) +
  geom_line(aes(y = scale(sgd), color = "Kadar Pertukaran (X)"), 
            linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Siri Masa Dinormalisasi",
    x = "Tahun",
    y = "Nilai Dinormalisasi (Z-score)",
    color = "Pembolehubah"
  ) +
  scale_color_manual(values = c("Kedatangan Pelancong (Y)" = "#2196F3", 
                                 "Kadar Pertukaran (X)" = "#FF5722")) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_ts)

# Keputusan utama
cat("\n--- Keputusan Utama ---\n")
if(exists("cointegrated")) {
  cat(sprintf("Kointegrasi: %s\n", ifelse(cointegrated, "Ya", "Tidak")))
}
cat(sprintf("Jenis Kausaliti: %s\n", causality_type))
cat(sprintf("Lag Optimum: %d\n", optimal_lag))
cat(sprintf("X → Y: %s\n", ifelse(x_causes_y, "Ya", "Tidak")))
cat(sprintf("Y → X: %s\n", ifelse(y_causes_x, "Ya", "Tidak")))

options(warn = 0)
