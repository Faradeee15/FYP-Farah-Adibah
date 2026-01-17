# Membersihkan persekitaran R
rm(list = ls())
options(warn = -1, scipen = 999)

# Memuatkan pakej yang diperlukan
suppressPackageStartupMessages({
  library(tidyverse)
  library(tsibble)      
  library(feasts)       
  library(scales)       
  library(lubridate)    
  library(patchwork)    
  library(knitr)
  library(kableExtra)
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
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9)
  )

theme_set(plot_theme)

# Format paksi-y untuk nombor besar
y_axis_format <- label_number(big.mark = ",", accuracy = 1)

# Membaca data
data <- read.csv(file.choose(), header = TRUE)

# Semak data hilang
kiraan_missing <- colSums(is.na(data))
print(kiraan_missing)

# Tukar kepada format tsibble
ts_tbl <- data %>%
  mutate(date = yearmonth(date)) %>%    
  as_tsibble(index = date)

print(ts_tbl)

# Menentukan fasa COVID-19
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

# Tarikh penting untuk anotasi
mco_start <- yearmonth("2020-03")        # 18 Mac 2020
border_reopen <- yearmonth("2022-04")    # 1 April 2022

# Statistik keseluruhan
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

# Statistik mengikut fasa COVID-19
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

# Gabungkan statistik
summary_tourist <- bind_rows(summary_overall, summary_by_phase)

# Set options untuk paparan penuh
options(width = 150, tibble.width = Inf, pillar.sigfig = 7)


# Round untuk display tapi kekalkan sebagai numeric
summary_tourist_display <- summary_tourist %>%
  mutate(across(where(is.numeric) & !Bilangan, ~round(., 2)))

print(summary_tourist_display, n = Inf, width = Inf)

# Statistik keseluruhan
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

# Statistik mengikut fasa COVID-19
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

# Gabungkan statistik
summary_sgd <- bind_rows(summary_sgd_overall, summary_sgd_by_phase)

# Round untuk display tapi kekalkan sebagai numeric
summary_sgd_display <- summary_sgd %>%
  mutate(across(where(is.numeric) & !Bilangan, ~round(., 4)))

print(summary_sgd_display, n = Inf, width = Inf)

# Plot dengan shaded region dan anotasi
p_timeseries <- ggplot(ts_tbl, aes(x = date, y = y)) +
  # Shaded region untuk tempoh COVID-19
  annotate("rect", 
           xmin = as.Date(mco_start), 
           xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "coral") +
  # Garis siri masa
  geom_line(color = "darkblue", linewidth = 0.8) +
  # Garis menegak untuk PKP bermula
  geom_vline(xintercept = as.Date(mco_start), 
             linetype = "dashed", color = "red", linewidth = 0.7) +
  # Garis menegak untuk pembukaan sempadan
  geom_vline(xintercept = as.Date(border_reopen), 
             linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
  # Anotasi PKP
  annotate("text", 
           x = as.Date(mco_start), 
           y = max(ts_tbl$y) * 0.95,
           label = "PKP Bermula\n(18 Mac 2020)", 
           hjust = 1.1, vjust = 1, 
           size = 3, color = "red", fontface = "bold") +
  # Anotasi pembukaan sempadan
  annotate("text", 
           x = as.Date(border_reopen), 
           y = max(ts_tbl$y) * 0.95,
           label = "Pembukaan Sempadan\n(1 April 2022)", 
           hjust = -0.1, vjust = 1, 
           size = 3, color = "darkgreen", fontface = "bold") +
  # Label dan format
  labs(
    title = "Siri Masa Kedatangan Pelancong Antarabangsa ke Malaysia",
    subtitle = "Januari 2013 - Disember 2024 (dengan Tempoh Intervensi COVID-19)",
    x = "Tahun",
    y = "Bilangan Kedatangan",
    caption = "Nota: Kawasan berlorek menunjukkan tempoh intervensi COVID-19\n(Mac 2020 - Mac 2022)"
  ) +
  scale_y_continuous(labels = y_axis_format,
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_timeseries)

p_exchange <- ggplot(ts_tbl, aes(x = date, y = sgd)) +
  # Shaded region untuk tempoh COVID-19
  annotate("rect", 
           xmin = as.Date(mco_start), 
           xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "coral") +
  # Garis siri masa
  geom_line(color = "darkred", linewidth = 0.8) +
  # Garis menegak untuk PKP bermula
  geom_vline(xintercept = as.Date(mco_start), 
             linetype = "dashed", color = "red", linewidth = 0.7) +
  # Garis menegak untuk pembukaan sempadan
  geom_vline(xintercept = as.Date(border_reopen), 
             linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
  # Label dan format
  labs(
    title = "Siri Masa Kadar Pertukaran MYR/SGD",
    subtitle = "Januari 2013 - Disember 2024 (dengan Tempoh Intervensi COVID-19)",
    x = "Tahun",
    y = "Kadar Pertukaran (MYR/SGD)",
    caption = "Nota: Kawasan berlorek menunjukkan tempoh intervensi COVID-19\n(Mac 2020 - Mac 2022)"
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01),
                     expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_exchange)

# STL Decomposition untuk kedatangan pelancong
dcmp_tourist <- ts_tbl %>%
  model(
    stl = STL(y ~ trend(window = 13) + season(window = "periodic"),
              robust = TRUE)
  ) %>%
  components()

print(dcmp_tourist, n = Inf)

# Extract komponen untuk plotting manual
dcmp_data <- dcmp_tourist %>%
  as_tibble() %>%
  select(date, y, trend, season_year, remainder)

# Plot 4 panels dengan format manual untuk kawalan penuh
p1 <- ggplot(dcmp_data, aes(x = date, y = y)) +
  geom_line(color = "darkblue", linewidth = 0.7) +
  labs(title = NULL,
       y = "Data Asal") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- ggplot(dcmp_data, aes(x = date, y = trend)) +
  geom_line(color = "darkgreen", linewidth = 0.7) +
  labs(title = NULL,
       y = "Trend") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p3 <- ggplot(dcmp_data, aes(x = date, y = season_year)) +
  geom_line(color = "darkorange", linewidth = 0.7) +
  labs(title = NULL,
       y = "Bermusim") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- ggplot(dcmp_data, aes(x = date, y = remainder)) +
  geom_line(color = "darkred", linewidth = 0.7) +
  labs(title = NULL,
       y = "Rawak",
       x = "Tahun") +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

# Gabungkan 4 panels
p_decomposition <- (p1 / p2 / p3 / p4) +
  plot_annotation(
    title = "Penguraian Siri Masa Kedatangan Pelancong (STL Decomposition)",
    subtitle = "Januari 2013 - Disember 2024",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    )
  )

print(p_decomposition)

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

# Nama bulan
bulan_labels <- c("Jan", "Feb", "Mac", "Apr", "Mei", "Jun", 
                  "Jul", "Ogos", "Sep", "Okt", "Nov", "Dis")

# Prepare data dengan bulan
ts_tbl_monthly <- ts_tbl %>%
  mutate(
    month_num = month(date),
    month_label = factor(month_num, levels = 1:12, labels = bulan_labels),
    year = year(date)
  )

# Seasonal subseries plot 
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

# Prepare data untuk boxplot
ts_tbl_boxplot <- ts_tbl_monthly %>%
  select(date, y, month_num, month_label)

# Boxplot by month
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
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

print(p_boxplot)

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

# Display dengan rounding
monthly_stats_display <- monthly_stats %>%
  mutate(across(where(is.numeric) & !Bilangan, ~round(., 2)))

print(monthly_stats_display, n = Inf, width = Inf)


# Kenal pasti bulan dengan kedatangan tertinggi dan terendah
bulan_tertinggi <- monthly_stats %>%
  filter(Purata == max(Purata)) %>%
  pull(Bulan)

bulan_terendah <- monthly_stats %>%
  filter(Purata == min(Purata)) %>%
  pull(Bulan)

cat("RINGKASAN:\n")
cat("- Bulan dengan purata kedatangan TERTINGGI: ", as.character(bulan_tertinggi), "\n", sep = "")
cat("- Bulan dengan purata kedatangan TERENDAH: ", as.character(bulan_terendah), "\n", sep = "")

# Load additional package
suppressPackageStartupMessages({
  library(zoo)
  library(tseries)
})

# Calculate rolling statistics (window = 12 months)
window_size <- 12

ts_tbl_rolling <- ts_tbl %>%
  as_tibble() %>%
  mutate(
    rolling_mean = rollmean(y, k = window_size, fill = NA, align = "right"),
    rolling_sd = rollapply(y, width = window_size, FUN = sd, fill = NA, align = "right")
  ) %>%
  as_tsibble(index = date)

# Plot Rolling Mean
p_rolling_mean <- ggplot(ts_tbl_rolling) +
  # Data asal
  geom_line(aes(x = date, y = y), color = "lightblue", linewidth = 0.5, alpha = 0.6) +
  # Rolling mean
  geom_line(aes(x = date, y = rolling_mean), color = "darkblue", linewidth = 1.2) +
  # COVID period shading
  annotate("rect", 
           xmin = as.Date(mco_start), 
           xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "coral") +
  labs(
    title = "Purata Bergerak (Rolling Mean) - 12 Bulan",
    subtitle = "Kedatangan Pelancong Antarabangsa ke Malaysia",
    x = "Tahun",
    y = "Bilangan Kedatangan",
    caption = "Nota: Garis biru gelap = Purata bergerak 12 bulan; Garis biru muda = Data asal"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_rolling_mean)


# Plot Rolling Standard Deviation
p_rolling_sd <- ggplot(ts_tbl_rolling, aes(x = date, y = rolling_sd)) +
  geom_line(color = "darkred", linewidth = 1) +
  # COVID period shading
  annotate("rect", 
           xmin = as.Date(mco_start), 
           xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "coral") +
  labs(
    title = "Sisihan Piawai Bergerak (Rolling Standard Deviation) - 12 Bulan",
    subtitle = "Kedatangan Pelancong Antarabangsa ke Malaysia",
    x = "Tahun",
    y = "Sisihan Piawai",
    caption = "Nota: Nilai tinggi menunjukkan ketidaktentuan/volatiliti tinggi"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

print(p_rolling_sd)

# Prepare data
y_original <- ts_tbl$y
y_diff <- diff(y_original)


# ADF test untuk data asal
adf_original <- adf.test(y_original, alternative = "stationary")
cat("   ADF Statistic: ", round(adf_original$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(adf_original$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(adf_original$p.value < 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# ADF test untuk data difference
adf_diff <- adf.test(y_diff, alternative = "stationary")
cat("   ADF Statistic: ", round(adf_diff$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(adf_diff$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(adf_diff$p.value < 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# KPSS test untuk data asal
kpss_original <- kpss.test(y_original, null = "Trend")
cat("   KPSS Statistic: ", round(kpss_original$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(kpss_original$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(kpss_original$p.value > 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# KPSS test untuk data difference
kpss_diff <- kpss.test(y_diff, null = "Level")
cat("   KPSS Statistic: ", round(kpss_diff$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(kpss_diff$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(kpss_diff$p.value > 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# Prepare data
sgd_original <- ts_tbl$sgd
sgd_diff <- diff(sgd_original)

# ADF test untuk data asal
adf_sgd_original <- adf.test(sgd_original, alternative = "stationary")
cat("   ADF Statistic: ", round(adf_sgd_original$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(adf_sgd_original$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(adf_sgd_original$p.value < 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# ADF test untuk data difference
adf_sgd_diff <- adf.test(sgd_diff, alternative = "stationary")
cat("   ADF Statistic: ", round(adf_sgd_diff$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(adf_sgd_diff$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(adf_sgd_diff$p.value < 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# KPSS test untuk data asal
kpss_sgd_original <- kpss.test(sgd_original, null = "Trend")
cat("   KPSS Statistic: ", round(kpss_sgd_original$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(kpss_sgd_original$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(kpss_sgd_original$p.value > 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# KPSS test untuk data difference
kpss_sgd_diff <- kpss.test(sgd_diff, null = "Level")
cat("   KPSS Statistic: ", round(kpss_sgd_diff$statistic, 4), "\n", sep = "")
cat("   p-value: ", format.pval(kpss_sgd_diff$p.value, digits = 4), "\n", sep = "")
cat("   Interpretasi: ")
if(kpss_sgd_diff$p.value > 0.05) {
  cat("Data adalah PEGUN (stationary) pada aras keertian 5%\n")
} else {
  cat("Data adalah TIDAK PEGUN (non-stationary) pada aras keertian 5%\n")
}

# Prepare data untuk ACF/PACF
y_original <- ts_tbl$y
y_diff <- c(NA, diff(y_original))  # Add NA for alignment

# Create data frames for ggplot
ts_tbl_with_diff <- ts_tbl %>%
  mutate(y_diff = y_diff)

# ACF untuk data asal
acf_original <- acf(y_original, lag.max = 36, plot = FALSE)
acf_original_df <- data.frame(
  lag = acf_original$lag,
  acf = acf_original$acf
)

# Confidence intervals (95%)
ci <- qnorm((1 + 0.95)/2) / sqrt(length(y_original))

p_acf_original <- ggplot(acf_original_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci, ci), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkblue", linewidth = 0.8) +
  geom_point(color = "darkblue", size = 1.5) +
  labs(
    title = "ACF: Data Asal",
    x = "Lag",
    y = "ACF"
  ) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# PACF untuk data asal
pacf_original <- pacf(y_original, lag.max = 36, plot = FALSE)
pacf_original_df <- data.frame(
  lag = pacf_original$lag,
  pacf = pacf_original$acf
)

p_pacf_original <- ggplot(pacf_original_df, aes(x = lag, y = pacf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci, ci), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkgreen", linewidth = 0.8) +
  geom_point(color = "darkgreen", size = 1.5) +
  labs(
    title = "PACF: Data Asal",
    x = "Lag",
    y = "PACF"
  ) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Remove NA from differenced data
y_diff_clean <- na.omit(y_diff)

# ACF untuk data difference
acf_diff <- acf(y_diff_clean, lag.max = 36, plot = FALSE)
acf_diff_df <- data.frame(
  lag = acf_diff$lag,
  acf = acf_diff$acf
)

ci_diff <- qnorm((1 + 0.95)/2) / sqrt(length(y_diff_clean))

p_acf_diff <- ggplot(acf_diff_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci_diff, ci_diff), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkred", linewidth = 0.8) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "ACF: Data Pembezaan Pertama",
    x = "Lag",
    y = "ACF"
  ) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# PACF untuk data difference
pacf_diff <- pacf(y_diff_clean, lag.max = 36, plot = FALSE)
pacf_diff_df <- data.frame(
  lag = pacf_diff$lag,
  pacf = pacf_diff$acf
)

p_pacf_diff <- ggplot(pacf_diff_df, aes(x = lag, y = pacf)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci_diff, ci_diff), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = lag, yend = 0), color = "darkorange", linewidth = 0.8) +
  geom_point(color = "darkorange", size = 1.5) +
  labs(
    title = "PACF: Data Pembezaan Pertama",
    x = "Lag",
    y = "PACF"
  ) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Combine plots dalam 2x2 grid
p_acf_pacf_combined <- (p_acf_original | p_pacf_original) /
                       (p_acf_diff | p_pacf_diff) +
  plot_annotation(
    title = "Fungsi Autokorelasi (ACF) dan Autokorelasi Separa (PACF)",
    subtitle = "Kedatangan Pelancong: Data Asal vs Pembezaan Pertama",
    caption = "Nota: Garis putus-putus biru menunjukkan selang keyakinan 95%",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.caption = element_text(hjust = 0.5, size = 8)
    )
  )

print(p_acf_pacf_combined)

# Boxplot comparing three COVID phases
p_boxplot_covid <- ggplot(ts_tbl, aes(x = covid_phase, y = y, fill = covid_phase)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2.5, 
               linewidth = 0.8, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "yellow", color = "black", stroke = 1.5) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5, color = "black") +
  labs(
    title = "Perbandingan Kedatangan Pelancong Mengikut Fasa COVID-19",
    subtitle = "Pra-COVID (Jan 2013 - Feb 2020) | Semasa COVID (Mac 2020 - Mac 2022) | Pasca-COVID (Apr 2022 - Dis 2024)",
    x = "Fasa COVID-19",
    y = "Bilangan Kedatangan",
    caption = "Nota: ◆ = Purata; • = Data titik individu; □ = Kotak (Q1 ke Q3); Garisan dalam kotak = Median"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_fill_manual(
    values = c("Pra-COVID" = "#2E7D32", 
               "Semasa COVID" = "#C62828", 
               "Pasca-COVID" = "#1565C0"),
    name = "Fasa"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, face = "bold"))

print(p_boxplot_covid)

# Calculate percentage changes
pra_mean <- summary_tourist %>% filter(Fasa == "Pra-COVID") %>% pull(Purata)
during_mean <- summary_tourist %>% filter(Fasa == "Semasa COVID") %>% pull(Purata)
pasca_mean <- summary_tourist %>% filter(Fasa == "Pasca-COVID") %>% pull(Purata)

pct_decline <- ((during_mean - pra_mean) / pra_mean) * 100
pct_recovery <- ((pasca_mean - during_mean) / during_mean) * 100
pct_vs_pre <- ((pasca_mean - pra_mean) / pra_mean) * 100

cat(sprintf("Pra-COVID        : %s\n", format(round(pra_mean, 0), big.mark = ",")))
cat(sprintf("Semasa COVID     : %s\n", format(round(during_mean, 0), big.mark = ",")))
cat(sprintf("Pasca-COVID      : %s\n", format(round(pasca_mean, 0), big.mark = ",")))

cat(sprintf("Penurunan (Pra → Semasa)  : %.2f%%\n", pct_decline))
cat(sprintf("Pemulihan (Semasa → Pasca): %.2f%%\n", pct_recovery))
cat(sprintf("Berbanding Pra-COVID      : %.2f%%\n", pct_vs_pre))

# Statistical test - ANOVA
cat("H0: Tidak ada perbezaan signifikan antara purata tiga fasa\n")
cat("H1: Terdapat perbezaan signifikan antara purata tiga fasa\n\n")

anova_result <- aov(y ~ covid_phase, data = ts_tbl)
anova_summary <- summary(anova_result)

print(anova_summary)

f_stat <- anova_summary[[1]]$`F value`[1]
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

cat(sprintf("F-statistic: %.4f\n", f_stat))
cat(sprintf("p-value: %s\n", format.pval(p_value, digits = 4)))
cat("\n")

if(p_value < 0.05) {
  cat("KEPUTUSAN: Tolak H0 pada aras keertian 5%\n")
  cat("INTERPRETASI: Terdapat perbezaan SIGNIFIKAN antara purata kedatangan\n")
  cat("              pelancong merentas tiga fasa COVID-19.\n")
} else {
  cat("KEPUTUSAN: Gagal tolak H0 pada aras keertian 5%\n")
  cat("INTERPRETASI: Tidak ada perbezaan signifikan antara purata kedatangan\n")
  cat("              pelancong merentas tiga fasa COVID-19.\n")
}


# Post-hoc test (Tukey HSD) if ANOVA is significant
if(p_value < 0.05) {
  cat("UJIAN POST-HOC (Tukey's HSD):\n")
  cat("-" , rep("-", 80), "\n", sep = "")
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  cat("\n")
  
  cat("INTERPRETASI POST-HOC:\n")
  cat("- p-value < 0.05: Perbezaan antara dua fasa adalah SIGNIFIKAN\n")
  cat("- p-value >= 0.05: Perbezaan antara dua fasa adalah TIDAK SIGNIFIKAN\n")
  cat("\n")
}

# Variability comparison
pra_sd <- summary_tourist %>% filter(Fasa == "Pra-COVID") %>% pull(Sisihan_Piawai)
during_sd <- summary_tourist %>% filter(Fasa == "Semasa COVID") %>% pull(Sisihan_Piawai)
pasca_sd <- summary_tourist %>% filter(Fasa == "Pasca-COVID") %>% pull(Sisihan_Piawai)

pra_cv <- (pra_sd / pra_mean) * 100
during_cv <- (during_sd / during_mean) * 100
pasca_cv <- (pasca_sd / pasca_mean) * 100

cat(sprintf("Pra-COVID        : %.2f%% (SD = %s)\n", pra_cv, format(round(pra_sd, 0), big.mark = ",")))
cat(sprintf("Semasa COVID     : %.2f%% (SD = %s)\n", during_cv, format(round(during_sd, 0), big.mark = ",")))
cat(sprintf("Pasca-COVID      : %.2f%% (SD = %s)\n", pasca_cv, format(round(pasca_sd, 0), big.mark = ",")))

# Scaling factor untuk dual axis
scaling_factor <- max(ts_tbl$y, na.rm = TRUE) / max(ts_tbl$sgd, na.rm = TRUE)

p_dual_axis <- ggplot(ts_tbl, aes(x = date)) +
  # Shaded region untuk COVID
  annotate("rect", 
           xmin = as.Date(mco_start), 
           xmax = as.Date(border_reopen),
           ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "coral") +
  # Tourist arrivals (primary axis)
  geom_line(aes(y = y, color = "Kedatangan Pelancong"), linewidth = 1) +
  # Exchange rate (secondary axis - scaled)
  geom_line(aes(y = sgd * scaling_factor, color = "Kadar Pertukaran MYR/SGD"), 
            linewidth = 1, linetype = "dashed") +
  # Labels
  labs(
    title = "Hubungan Kedatangan Pelancong dengan Kadar Pertukaran MYR/SGD",
    subtitle = "Januari 2013 - Disember 2024 (Plot Paksi Berkembar)",
    x = "Tahun",
    y = "Bilangan Kedatangan Pelancong",
    color = "Pembolehubah"
  ) +
  # Primary y-axis (left)
  scale_y_continuous(
    labels = y_axis_format,
    # Secondary y-axis (right)
    sec.axis = sec_axis(
      trans = ~./scaling_factor,
      name = "Kadar Pertukaran (MYR/SGD)",
      labels = number_format(accuracy = 0.1)
    )
  ) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    values = c("Kedatangan Pelancong" = "#1565C0", 
               "Kadar Pertukaran MYR/SGD" = "#D32F2F")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title.y.right = element_text(color = "#D32F2F", face = "bold"),
    axis.text.y.right = element_text(color = "#D32F2F"),
    axis.title.y.left = element_text(color = "#1565C0", face = "bold"),
    axis.text.y.left = element_text(color = "#1565C0")
  )

print(p_dual_axis)

# Separate by COVID phase for color coding
p_scatter <- ggplot(ts_tbl, aes(x = sgd, y = y, color = covid_phase)) +
  geom_point(size = 3, alpha = 0.7) +
  # Overall trend line
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, 
              color = "black", linetype = "dashed", linewidth = 1.2,
              fill = "gray80", alpha = 0.3) +
  labs(
    title = "Hubungan antara Kadar Pertukaran MYR/SGD dengan Kedatangan Pelancong",
    subtitle = "Scatter Plot dengan Garis Trend Linear",
    x = "Kadar Pertukaran MYR/SGD",
    y = "Bilangan Kedatangan Pelancong",
    color = "Fasa COVID-19"
  ) +
  scale_y_continuous(labels = y_axis_format) +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_manual(
    values = c("Pra-COVID" = "#2E7D32", 
               "Semasa COVID" = "#C62828", 
               "Pasca-COVID" = "#1565C0")
  ) +
  theme(legend.position = "bottom")

print(p_scatter)

# Overall correlation
cor_overall <- cor.test(ts_tbl$y, ts_tbl$sgd, method = "pearson")
cat(sprintf("Pekali Korelasi Pearson (r): %.4f\n", cor_overall$estimate))
cat(sprintf("t-statistic: %.4f\n", cor_overall$statistic))
cat(sprintf("p-value: %s\n", format.pval(cor_overall$p.value, digits = 4)))
cat(sprintf("95%% Selang Keyakinan: [%.4f, %.4f]\n", 
            cor_overall$conf.int[1], cor_overall$conf.int[2]))

# Correlation by COVID phase
# Pre-COVID
ts_pre <- ts_tbl %>% filter(covid_phase == "Pra-COVID")
cor_pre <- cor.test(ts_pre$y, ts_pre$sgd, method = "pearson")
cat(sprintf("Pra-COVID:    r = %.4f, p-value = %s\n", 
            cor_pre$estimate, format.pval(cor_pre$p.value, digits = 4)))

# During COVID
ts_during <- ts_tbl %>% filter(covid_phase == "Semasa COVID")
cor_during <- cor.test(ts_during$y, ts_during$sgd, method = "pearson")
cat(sprintf("Semasa COVID: r = %.4f, p-value = %s\n", 
            cor_during$estimate, format.pval(cor_during$p.value, digits = 4)))

# Post-COVID
ts_post <- ts_tbl %>% filter(covid_phase == "Pasca-COVID")
cor_post <- cor.test(ts_post$y, ts_post$sgd, method = "pearson")
cat(sprintf("Pasca-COVID:  r = %.4f, p-value = %s\n", 
            cor_post$estimate, format.pval(cor_post$p.value, digits = 4)))

# Simple linear regression: y ~ sgd
lm_model <- lm(y ~ sgd, data = ts_tbl)
lm_summary <- summary(lm_model)

cat(sprintf("Intercept (β₀)     : %s\n", 
            format(round(coef(lm_model)[1], 2), big.mark = ",")))
cat(sprintf("Slope (β₁)         : %s\n", 
            format(round(coef(lm_model)[2], 2), big.mark = ",")))
cat(sprintf("R-squared (R²)     : %.4f\n", lm_summary$r.squared))
cat(sprintf("Adjusted R²        : %.4f\n", lm_summary$adj.r.squared))
cat(sprintf("F-statistic        : %.4f\n", lm_summary$fstatistic[1]))
cat(sprintf("p-value            : %s\n", 
            format.pval(pf(lm_summary$fstatistic[1], 
                          lm_summary$fstatistic[2], 
                          lm_summary$fstatistic[3], 
                          lower.tail = FALSE), digits = 4)))

