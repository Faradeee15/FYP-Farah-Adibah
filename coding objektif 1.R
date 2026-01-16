# Analisis Penerokaan Data (EDA)
# Kedatangan Pelancong Antarabangsa ke Malaysia (2013-2024)

rm(list = ls())
options(warn = -1, scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(tsibble)      
  library(feasts)       
  library(scales)       
  library(lubridate)    
  library(patchwork)    
  library(knitr)
  library(kableExtra)
  library(zoo)
  library(tseries)
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

# Semak data hilang
cat("\nPemeriksaan Data Hilang:\n")
print(colSums(is.na(data)))

# Tukar kepada format tsibble
ts_tbl <- data %>%
  mutate(date = yearmonth(date)) %>%    
  as_tsibble(index = date)

print(ts_tbl)

# Definisi fasa COVID-19
ts_tbl <- ts_tbl %>%
  mutate(
    covid_phase = case_when(
      date < yearmonth("2020-03") ~ "Pra-COVID",
      date >= yearmonth("2020-03") & date < yearmonth("2022-04") ~ "Semasa COVID",
      date >= yearmonth("2022-04") ~ "Pasca-COVID"
    ),
    covid_phase = factor(covid_phase, 
                        levels = c("Pra-COVID", "Semasa COVID", "Pasca-COVID"))
  )

mco_start <- yearmonth("2020-03")
border_reopen <- yearmonth("2022-04")

# Statistik Deskriptif - Kedatangan Pelancong
cat("\nStatistik Deskriptif: Kedatangan Pelancong\n")

summary_overall <- ts_tbl %>%
  as_tibble() %>%
  summarise(
    Fasa = "Keseluruhan",
    Bilangan = n(),
    Min = min(y, na.rm = TRUE),
    Q1 = quantile(y, 0.25, na.rm = TRUE),
    Median = median(y, na.rm = TRUE),
    Purata = mean(y, na.rm = TRUE),
    Q3 = quantile(y, 0.75, na.rm = TRUE),
    Maksimum = max(y, na.rm = TRUE),
    Sisihan_Piawai = sd(y, na.rm = TRUE)
  )

summary_by_phase <- ts_tbl %>%
  as_tibble() %>%
  group_by(covid_phase) %>%
  summarise(
    Bilangan = n(),
    Min = min(y, na.rm = TRUE),
    Q1 = quantile(y, 0.25, na.rm = TRUE),
    Median = median(y, na.rm = TRUE),
    Purata = mean(y, na.rm = TRUE),
    Q3 = quantile(y, 0.75, na.rm = TRUE),
    Maksimum = max(y, na.rm = TRUE),
    Sisihan_Piawai = sd(y, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Fasa = covid_phase)

summary_tourist <- bind_rows(summary_overall, summary_by_phase)
options(width = 150, tibble.width = Inf, pillar.sigfig = 7)

print(summary_tourist %>% mutate(across(where(is.numeric) & !Bilangan, ~round(., 2))), 
      n = Inf, width = Inf)

# Statistik Deskriptif - Kadar Pertukaran MYR/SGD
cat("\nStatistik Deskriptif: Kadar Pertukaran MYR/SGD\n")

summary_sgd_overall <- ts_tbl %>%
  as_tibble() %>%
  summarise(
    Fasa = "Keseluruhan",
    Bilangan = n(),
    Min = min(sgd, na.rm = TRUE),
    Q1 = quantile(sgd, 0.25, na.rm = TRUE),
    Median = median(sgd, na.rm = TRUE),
    Purata = mean(sgd, na.rm = TRUE),
    Q3 = quantile(sgd, 0.75, na.rm = TRUE),
    Maksimum = max(sgd, na.rm = TRUE),
    Sisihan_Piawai = sd(sgd, na.rm = TRUE)
  )

summary_sgd_by_phase <- ts_tbl %>%
  as_tibble() %>%
  group_by(covid_phase) %>%
  summarise(
    Bilangan = n(),
    Min = min(sgd, na.rm = TRUE),
    Q1 = quantile(sgd, 0.25, na.rm = TRUE),
    Median = median(sgd, na.rm = TRUE),
    Purata = mean(sgd, na.rm = TRUE),
    Q3 = quantile(sgd, 0.75, na.rm = TRUE),
    Maksimum = max(sgd, na.rm = TRUE),
    Sisihan_Piawai = sd(sgd, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Fasa = covid_phase)

summary_sgd <- bind_rows(summary_sgd_overall, summary_sgd_by_phase)

print(summary_sgd %>% mutate(across(where(is.numeric) & !Bilangan, ~round(., 4))), 
      n = Inf, width = Inf)

# Plot Siri Masa - Kedatangan Pelancong
p_timeseries <- ggplot(ts_tbl, aes(x = date, y = y)) +
  annotate("rect", xmin = as.Date(mco_start), xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "coral") +
  geom_line(color = "darkblue", linewidth = 0.8) +
  geom_vline(xintercept = as.Date(mco_start), linetype = "dashed", 
             color = "red", linewidth = 0.7) +
  geom_vline(xintercept = as.Date(border_reopen), linetype = "dashed", 
             color = "darkgreen", linewidth = 0.7) +
  annotate("text", x = as.Date(mco_start), y = max(ts_tbl$y) * 0.95,
           label = "PKP Bermula\n(18 Mac 2020)", hjust = 1.1, vjust = 1, 
           size = 3, color = "red", fontface = "bold") +
  annotate("text", x = as.Date(border_reopen), y = max(ts_tbl$y) * 0.95,
           label = "Pembukaan Sempadan\n(1 April 2022)", hjust = -0.1, vjust = 1, 
           size = 3, color = "darkgreen", fontface = "bold") +
  labs(
    title = "Siri Masa Kedatangan Pelancong Antarabangsa ke Malaysia",
    subtitle = "Januari 2013 - Disember 2024 (dengan Tempoh Intervensi COVID-19)",
    x = "Tahun",
    y = "Bilangan Kedatangan",
    caption = "Nota: Kawasan berlorek menunjukkan tempoh intervensi COVID-19"
  ) +
  scale_y_continuous(labels = y_axis_format, expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_timeseries)

# Plot Siri Masa - Kadar Pertukaran MYR/SGD
p_exchange <- ggplot(ts_tbl, aes(x = date, y = sgd)) +
  annotate("rect", xmin = as.Date(mco_start), xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "coral") +
  geom_line(color = "darkred", linewidth = 0.8) +
  geom_vline(xintercept = as.Date(mco_start), linetype = "dashed", 
             color = "red", linewidth = 0.7) +
  geom_vline(xintercept = as.Date(border_reopen), linetype = "dashed", 
             color = "darkgreen", linewidth = 0.7) +
  labs(
    title = "Siri Masa Kadar Pertukaran MYR/SGD",
    subtitle = "Januari 2013 - Disember 2024 (dengan Tempoh Intervensi COVID-19)",
    x = "Tahun",
    y = "Kadar Pertukaran (MYR/SGD)",
    caption = "Nota: Kawasan berlorek menunjukkan tempoh intervensi COVID-19"
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01),
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_exchange)

# Penguraian Siri Masa (STL Decomposition)
cat("\nPenguraian Siri Masa (STL Decomposition)\n")

dcmp_tourist <- ts_tbl %>%
  model(stl = STL(y ~ trend(window = 13) + season(window = "periodic"), robust = TRUE)) %>%
  components()

print(dcmp_tourist, n = Inf)

dcmp_data <- dcmp_tourist %>%
  as_tibble() %>%
  select(date, y, trend, season_year, remainder)

# Plot komponen
p1 <- ggplot(dcmp_data, aes(x = date, y = y)) +
  geom_line(color = "darkblue", linewidth = 0.7) +
  labs(title = NULL, y = "Data Asal") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

p2 <- ggplot(dcmp_data, aes(x = date, y = trend)) +
  geom_line(color = "darkgreen", linewidth = 0.7) +
  labs(title = NULL, y = "Trend") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

p3 <- ggplot(dcmp_data, aes(x = date, y = season_year)) +
  geom_line(color = "darkorange", linewidth = 0.7) +
  labs(title = NULL, y = "Bermusim") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

p4 <- ggplot(dcmp_data, aes(x = date, y = remainder)) +
  geom_line(color = "darkred", linewidth = 0.7) +
  labs(title = NULL, y = "Rawak", x = "Tahun") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

p_decomposition <- (p1 / p2 / p3 / p4) +
  plot_annotation(
    title = "Penguraian Siri Masa Kedatangan Pelancong (STL Decomposition)",
    subtitle = "Januari 2013 - Disember 2024",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                  plot.subtitle = element_text(hjust = 0.5, size = 10))
  )

print(p_decomposition)

# Statistik komponen
seasonal_stats <- dcmp_data %>%
  summarise(
    Komponen = "Bermusim",
    Min = min(season_year, na.rm = TRUE),
    Purata = mean(season_year, na.rm = TRUE),
    Median = median(season_year, na.rm = TRUE),
    Maksimum = max(season_year, na.rm = TRUE),
    Sisihan_Piawai = sd(season_year, na.rm = TRUE)
  )

remainder_stats <- dcmp_data %>%
  summarise(
    Komponen = "Rawak",
    Min = min(remainder, na.rm = TRUE),
    Purata = mean(remainder, na.rm = TRUE),
    Median = median(remainder, na.rm = TRUE),
    Maksimum = max(remainder, na.rm = TRUE),
    Sisihan_Piawai = sd(remainder, na.rm = TRUE)
  )

component_summary <- bind_rows(seasonal_stats, remainder_stats) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(component_summary)

# Plot Subsiri Bermusim
cat("\nPlot Subsiri Bermusim\n")

bulan_labels <- c("Jan", "Feb", "Mac", "Apr", "Mei", "Jun", 
                  "Jul", "Ogos", "Sep", "Okt", "Nov", "Dis")

ts_tbl_monthly <- ts_tbl %>%
  mutate(
    month_num = month(date),
    month_label = factor(month_num, levels = 1:12, labels = bulan_labels),
    year = year(date)
  )

p_subseries <- ts_tbl %>%
  gg_subseries(y) +
  labs(
    title = "Plot Subsiri Bermusim Kedatangan Pelancong",
    subtitle = "Purata Bulanan Merentas Tahun (2013-2024)",
    x = "Tahun",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format)

print(p_subseries)

# Boxplot mengikut bulan
ts_tbl_boxplot <- ts_tbl_monthly %>%
  select(date, y, month_num, month_label)

p_boxplot <- ggplot(ts_tbl_boxplot, aes(x = month_label, y = y, fill = month_label)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "yellow", color = "black") +
  labs(
    title = "Plot Kotak Kedatangan Pelancong Mengikut Bulan",
    subtitle = "Januari 2013 - Disember 2024 (◆ = Purata)",
    x = "Bulan",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_fill_viridis_d(option = "turbo") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(p_boxplot)

# Statistik mengikut bulan
monthly_stats <- ts_tbl_boxplot %>%
  as_tibble() %>%
  group_by(month_label) %>%
  summarise(
    Bilangan = n(),
    Min = min(y, na.rm = TRUE),
    Q1 = quantile(y, 0.25, na.rm = TRUE),
    Median = median(y, na.rm = TRUE),
    Purata = mean(y, na.rm = TRUE),
    Q3 = quantile(y, 0.75, na.rm = TRUE),
    Maksimum = max(y, na.rm = TRUE),
    Sisihan_Piawai = sd(y, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Bulan = month_label)

print(monthly_stats %>% mutate(across(where(is.numeric) & !Bilangan, ~round(., 2))), 
      n = Inf, width = Inf)

bulan_tertinggi <- monthly_stats %>% filter(Purata == max(Purata)) %>% pull(Bulan)
bulan_terendah <- monthly_stats %>% filter(Purata == min(Purata)) %>% pull(Bulan)

cat("\nBulan tertinggi:", as.character(bulan_tertinggi), "\n")
cat("Bulan terendah:", as.character(bulan_terendah), "\n")

# Rolling Statistics
cat("\nRolling Mean dan Rolling Standard Deviation\n")

window_size <- 12

ts_tbl_rolling <- ts_tbl %>%
  as_tibble() %>%
  mutate(
    rolling_mean = rollmean(y, k = window_size, fill = NA, align = "right"),
    rolling_sd = rollapply(y, width = window_size, FUN = sd, fill = NA, align = "right")
  ) %>%
  as_tsibble(index = date)

p_rolling_mean <- ggplot(ts_tbl_rolling) +
  geom_line(aes(x = date, y = y), color = "lightblue", linewidth = 0.5, alpha = 0.6) +
  geom_line(aes(x = date, y = rolling_mean), color = "darkblue", linewidth = 1.2) +
  annotate("rect", xmin = as.Date(mco_start), xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "coral") +
  labs(
    title = "Purata Bergerak (Rolling Mean) - 12 Bulan",
    subtitle = "Kedatangan Pelancong Antarabangsa ke Malaysia",
    x = "Tahun",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_rolling_mean)

p_rolling_sd <- ggplot(ts_tbl_rolling, aes(x = date, y = rolling_sd)) +
  geom_line(color = "darkred", linewidth = 1) +
  annotate("rect", xmin = as.Date(mco_start), xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "coral") +
  labs(
    title = "Sisihan Piawai Bergerak (Rolling Standard Deviation) - 12 Bulan",
    subtitle = "Kedatangan Pelancong Antarabangsa ke Malaysia",
    x = "Tahun",
    y = "Sisihan Piawai"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_rolling_sd)

# Ujian Kepegunan
cat("\nUjian Kepegunan (Stationarity Tests)\n")

y_original <- ts_tbl$y
y_diff <- diff(y_original)

# ADF Test
cat("\nAugmented Dickey-Fuller Test:\n")
adf_original <- adf.test(y_original, alternative = "stationary")
cat("Data Asal - ADF:", round(adf_original$statistic, 4), 
    ", p-value:", format.pval(adf_original$p.value, digits = 4), "\n")

adf_diff <- adf.test(y_diff, alternative = "stationary")
cat("Pembezaan 1 - ADF:", round(adf_diff$statistic, 4), 
    ", p-value:", format.pval(adf_diff$p.value, digits = 4), "\n")

# KPSS Test
cat("\nKPSS Test:\n")
kpss_original <- kpss.test(y_original, null = "Trend")
cat("Data Asal - KPSS:", round(kpss_original$statistic, 4), 
    ", p-value:", format.pval(kpss_original$p.value, digits = 4), "\n")

kpss_diff <- kpss.test(y_diff, null = "Level")
cat("Pembezaan 1 - KPSS:", round(kpss_diff$statistic, 4), 
    ", p-value:", format.pval(kpss_diff$p.value, digits = 4), "\n")

# Ujian Kepegunan untuk MYR/SGD
cat("\nUjian Kepegunan: MYR/SGD\n")

sgd_original <- ts_tbl$sgd
sgd_diff <- diff(sgd_original)

adf_sgd_original <- adf.test(sgd_original, alternative = "stationary")
cat("Data Asal - ADF:", round(adf_sgd_original$statistic, 4), 
    ", p-value:", format.pval(adf_sgd_original$p.value, digits = 4), "\n")

adf_sgd_diff <- adf.test(sgd_diff, alternative = "stationary")
cat("Pembezaan 1 - ADF:", round(adf_sgd_diff$statistic, 4), 
    ", p-value:", format.pval(adf_sgd_diff$p.value, digits = 4), "\n")

kpss_sgd_original <- kpss.test(sgd_original, null = "Trend")
cat("Data Asal - KPSS:", round(kpss_sgd_original$statistic, 4), 
    ", p-value:", format.pval(kpss_sgd_original$p.value, digits = 4), "\n")

kpss_sgd_diff <- kpss.test(sgd_diff, null = "Level")
cat("Pembezaan 1 - KPSS:", round(kpss_sgd_diff$statistic, 4), 
    ", p-value:", format.pval(kpss_sgd_diff$p.value, digits = 4), "\n")

# ACF dan PACF
cat("\nPlot ACF dan PACF\n")

acf_original <- acf(y_original, lag.max = 36, plot = FALSE)
acf_original_df <- data.frame(lag = acf_original$lag, acf = acf_original$acf)
ci <- qnorm((1 + 0.95)/2) / sqrt(length(y_original))

p_acf_original <- ggplot(acf_original_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci, ci), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkblue", linewidth = 0.8) +
  geom_point(color = "darkblue", size = 1.5) +
  labs(title = "ACF: Data Asal", x = "Lag", y = "ACF") +
  theme(plot.title = element_text(size = 10, face = "bold"))

pacf_original <- pacf(y_original, lag.max = 36, plot = FALSE)
pacf_original_df <- data.frame(lag = pacf_original$lag, pacf = pacf_original$acf)

p_pacf_original <- ggplot(pacf_original_df, aes(x = lag, y = pacf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci, ci), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkgreen", linewidth = 0.8) +
  geom_point(color = "darkgreen", size = 1.5) +
  labs(title = "PACF: Data Asal", x = "Lag", y = "PACF") +
  theme(plot.title = element_text(size = 10, face = "bold"))

y_diff_clean <- na.omit(y_diff)
acf_diff <- acf(y_diff_clean, lag.max = 36, plot = FALSE)
acf_diff_df <- data.frame(lag = acf_diff$lag, acf = acf_diff$acf)
ci_diff <- qnorm((1 + 0.95)/2) / sqrt(length(y_diff_clean))

p_acf_diff <- ggplot(acf_diff_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci_diff, ci_diff), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkred", linewidth = 0.8) +
  geom_point(color = "darkred", size = 1.5) +
  labs(title = "ACF: Data Pembezaan Pertama", x = "Lag", y = "ACF") +
  theme(plot.title = element_text(size = 10, face = "bold"))

pacf_diff <- pacf(y_diff_clean, lag.max = 36, plot = FALSE)
pacf_diff_df <- data.frame(lag = pacf_diff$lag, pacf = pacf_diff$acf)

p_pacf_diff <- ggplot(pacf_diff_df, aes(x = lag, y = pacf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci_diff, ci_diff), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkorange", linewidth = 0.8) +
  geom_point(color = "darkorange", size = 1.5) +
  labs(title = "PACF: Data Pembezaan Pertama", x = "Lag", y = "PACF") +
  theme(plot.title = element_text(size = 10, face = "bold"))

p_acf_pacf_combined <- (p_acf_original | p_pacf_original) / (p_acf_diff | p_pacf_diff) +
  plot_annotation(
    title = "Fungsi Autokorelasi (ACF) dan Autokorelasi Separa (PACF)",
    subtitle = "Kedatangan Pelancong: Data Asal vs Pembezaan Pertama",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                  plot.subtitle = element_text(hjust = 0.5, size = 10))
  )

print(p_acf_pacf_combined)

# Boxplot perbandingan fasa COVID
p_boxplot_covid <- ggplot(ts_tbl, aes(x = covid_phase, y = y, fill = covid_phase)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2.5, 
               linewidth = 0.8, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "yellow", color = "black", stroke = 1.5) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5, color = "black") +
  labs(
    title = "Perbandingan Kedatangan Pelancong Mengikut Fasa COVID-19",
    x = "Fasa COVID-19",
    y = "Bilangan Kedatangan"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_fill_manual(values = c("Pra-COVID" = "#2E7D32", 
                                "Semasa COVID" = "#C62828", 
                                "Pasca-COVID" = "#1565C0")) +
  theme(legend.position = "none")

print(p_boxplot_covid)

# Analisis perbandingan fasa
cat("\nAnalisis Perbandingan Fasa COVID-19\n")

pra_mean <- summary_tourist %>% filter(Fasa == "Pra-COVID") %>% pull(Purata)
during_mean <- summary_tourist %>% filter(Fasa == "Semasa COVID") %>% pull(Purata)
pasca_mean <- summary_tourist %>% filter(Fasa == "Pasca-COVID") %>% pull(Purata)

pct_decline <- ((during_mean - pra_mean) / pra_mean) * 100
pct_recovery <- ((pasca_mean - during_mean) / during_mean) * 100

cat(sprintf("Penurunan (Pra → Semasa): %.2f%%\n", pct_decline))
cat(sprintf("Pemulihan (Semasa → Pasca): %.2f%%\n", pct_recovery))

# ANOVA
anova_result <- aov(y ~ covid_phase, data = ts_tbl)
print(summary(anova_result))

if(summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

# Analisis hubungan dengan kadar pertukaran
cat("\nAnalisis Hubungan: Kedatangan Pelancong vs MYR/SGD\n")

scaling_factor <- max(ts_tbl$y, na.rm = TRUE) / max(ts_tbl$sgd, na.rm = TRUE)

p_dual_axis <- ggplot(ts_tbl, aes(x = date)) +
  annotate("rect", xmin = as.Date(mco_start), xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "coral") +
  geom_line(aes(y = y, color = "Kedatangan Pelancong"), linewidth = 1) +
  geom_line(aes(y = sgd * scaling_factor, color = "Kadar Pertukaran MYR/SGD"), 
            linewidth = 1, linetype = "dashed") +
  labs(
    title = "Hubungan Kedatangan Pelancong dengan Kadar Pertukaran MYR/SGD",
    x = "Tahun",
    y = "Bilangan Kedatangan Pelancong",
    color = "Pembolehubah"
  ) +
  scale_y_continuous(
    labels = y_axis_format,
    sec.axis = sec_axis(trans = ~./scaling_factor, name = "Kadar Pertukaran (MYR/SGD)",
                        labels = number_format(accuracy = 0.1))
  ) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Kedatangan Pelancong" = "#1565C0", 
                                 "Kadar Pertukaran MYR/SGD" = "#D32F2F"))

print(p_dual_axis)

# Scatter plot
p_scatter <- ggplot(ts_tbl, aes(x = sgd, y = y, color = covid_phase)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, 
              color = "black", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Hubungan antara Kadar Pertukaran MYR/SGD dengan Kedatangan Pelancong",
    x = "Kadar Pertukaran MYR/SGD",
    y = "Bilangan Kedatangan Pelancong",
    color = "Fasa COVID-19"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_color_manual(values = c("Pra-COVID" = "#2E7D32", 
                                 "Semasa COVID" = "#C62828", 
                                 "Pasca-COVID" = "#1565C0"))

print(p_scatter)

# Analisis korelasi
cat("\nAnalisis Korelasi Pearson\n")

cor_overall <- cor.test(ts_tbl$y, ts_tbl$sgd, method = "pearson")
cat(sprintf("Keseluruhan - r: %.4f, p-value: %s\n", 
            cor_overall$estimate, format.pval(cor_overall$p.value, digits = 4)))

ts_pre <- ts_tbl %>% filter(covid_phase == "Pra-COVID")
cor_pre <- cor.test(ts_pre$y, ts_pre$sgd, method = "pearson")
cat(sprintf("Pra-COVID - r: %.4f, p-value: %s\n", 
            cor_pre$estimate, format.pval(cor_pre$p.value, digits = 4)))

ts_during <- ts_tbl %>% filter(covid_phase == "Semasa COVID")
cor_during <- cor.test(ts_during$y, ts_during$sgd, method = "pearson")
cat(sprintf("Semasa COVID - r: %.4f, p-value: %s\n", 
            cor_during$estimate, format.pval(cor_during$p.value, digits = 4)))

ts_post <- ts_tbl %>% filter(covid_phase == "Pasca-COVID")
cor_post <- cor.test(ts_post$y, ts_post$sgd, method = "pearson")
cat(sprintf("Pasca-COVID - r: %.4f, p-value: %s\n", 
            cor_post$estimate, format.pval(cor_post$p.value, digits = 4)))

# Regresi linear
cat("\nAnalisis Regresi Linear\n")

lm_model <- lm(y ~ sgd, data = ts_tbl)
lm_summary <- summary(lm_model)

cat(sprintf("Intercept: %s\n", format(round(coef(lm_model)[1], 2), big.mark = ",")))
cat(sprintf("Slope: %s\n", format(round(coef(lm_model)[2], 2), big.mark = ",")))
cat(sprintf("R²: %.4f\n", lm_summary$r.squared))
cat(sprintf("Adjusted R²: %.4f\n", lm_summary$adj.r.squared))
cat(sprintf("F-statistic: %.4f\n", lm_summary$fstatistic[1]))
