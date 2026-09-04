library(tidyverse)
library(lubridate)
library(slider)
library(hexbin)
library(usethis)
library(gitcreds)

reed <- read.csv(file = "DATALOG.CSV", header = TRUE)
press <- read.csv(file = "PRESSLOG.CSV", header = TRUE)

data <- cbind(reed, press[3:5])

data <- data %>%
  mutate(Timestamp = ymd_hms(Timestamp) + hours(5) + minutes(14) + seconds(52))

data <- data[-c(1:151, 1021:1024), ]

data$Speed <- data$Interval_Pulses*pi*24/100/60

# Pivot long and create stacked line charts
data %>%
  pivot_longer(cols = c(Interval_Pulses, Depth_mm), 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = Timestamp, y = Value, color = Metric)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  labs(
    title = "River Depth vs. Water Wheel Activity",
    x = "Timestamp",
    y = NULL
  ) +
  theme_minimal()

Interval_Pulses_smooth <- slide_index_dbl(
  data$Interval_Pulses, 
  data$Timestamp, 
  mean, 
  .before = minutes(5), 
  .after = minutes(5)
)

Speed_smooth <- slide_index_dbl(
  data$Speed, 
  data$Timestamp, 
  mean, 
  .before = minutes(5), 
  .after = minutes(5)
)

Depth_mm_smooth <- slide_index_dbl(
  data$Depth_mm, 
  data$Timestamp, 
  mean, 
  .before = minutes(5), 
  .after = minutes(5)
)

data$Interval_smooth <- Interval_Pulses_smooth
data$Speed_smooth <- Speed_smooth
data$Depth_smooth <- Depth_mm_smooth

# Pivot long and create stacked line charts
data %>%
  pivot_longer(cols = c(Speed_smooth, Depth_smooth), 
               names_to = "Metric", 
               values_to = "Value") %>%
  ggplot(aes(x = Timestamp, y = Value, color = Metric)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  labs(
    title = "River Depth vs. Water Wheel Activity",
    x = "Timestamp",
    y = NULL
  ) +
  theme_minimal()

#####################################################


# Manual rain entries (mm)
rain_manual <- tribble(
  ~time_str, ~Rain_mm,
  "2026-08-20 20:30:00", 0.4,
  "2026-08-20 20:40:00", 0.3,
  "2026-08-21 01:30:00", 0.3,
  "2026-08-21 01:40:00", 0.1,
  "2026-08-21 01:50:00", 0.1,
  "2026-08-21 02:00:00", 0.1,
  "2026-08-21 02:50:00", 0.3,
  "2026-08-21 03:00:00", 0.5,
  "2026-08-21 03:10:00", 0.4,
  "2026-08-21 07:00:00", 0.1,
  "2026-08-21 07:10:00", 0.1,
  "2026-08-21 07:20:00", 0.1,
  "2026-08-21 07:30:00", 0.3,
  "2026-08-21 07:40:00", 0.3,
  "2026-08-21 07:50:00", 0.3,
  "2026-08-21 08:00:00", 0.9,
  "2026-08-21 08:10:00", 1.8,
  "2026-08-21 08:20:00", 0.8,
  "2026-08-21 08:30:00", 1.7,
  "2026-08-21 08:40:00", 0.6,
  "2026-08-21 08:50:00", 0.6
) 

rain_manual$Timestamp <- ymd_hms(rain_manual$time_str)

# Floor timestamps to matching 10-min blocks and join rain
data <- data %>%
  mutate(Timestamp_10m = floor_date(Timestamp, "10 minutes")) %>%
  left_join(rain_manual, by = c("Timestamp_10m" = "Timestamp")) %>%
  mutate(Rain_mm = replace_na(Rain_mm, 0)) # Fill missing rain intervals with 0

# 1. Assign the smoothed vectors into the data frame as columns
data$Interval_Pulses_smooth <- Interval_Pulses_smooth
data$Speed_smooth <- Speed_smooth
data$Depth_mm_smooth <- Depth_mm_smooth

# 2. Now run the plot prep pipeline
data_plot <- data %>%
  select(Timestamp, Speed_smooth, Depth_mm_smooth, Rain_mm) %>%
  pivot_longer(
    cols = c(Speed_smooth, Depth_mm_smooth, Rain_mm), 
    names_to = "Metric", 
    values_to = "Value"
  ) %>%
  mutate(Metric = factor(Metric, 
                         levels = c("Rain_mm", "Depth_mm_smooth", "Speed_smooth"),
                         labels = c("Rainfall (mm)", "Depth (mm)", "Velocity (m/s)")))

# 3. Render the plot
ggplot() +
  geom_col(
    data = filter(data_plot, Metric == "Rainfall (mm)"),
    aes(x = Timestamp, y = Value),
    fill = "#56B4E9",
    width = 600
  ) +
  geom_line(
    data = filter(data_plot, Metric != "Rainfall (mm)"),
    aes(x = Timestamp, y = Value, color = Metric),
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Depth (mm)" = "#0072B2", "Velocity (m/s)" = "#D55E00")) +
  labs(
    title = "River Depth, Water Wheel Velocity, and Rainfall",
    x = "Timestamp",
    y = NULL
  ) +
  theme_minimal()


ggplot(data, aes(x=Interval_Pulses, y=Depth_mm)) + 
  geom_point()

# Requires the 'hexbin' package installed

ggplot(data, aes(x = Depth_mm, y = Speed)) +
  geom_jitter(alpha = 0.3, color = "#0072B2", width = 2, height = 0.5) +
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE) +
  labs(title = "Velocity vs. Depth (Jittered with Linear Fit)", x = "Depth (mm)", y = "Velocity (m/s)") +
  theme_minimal()

ggplot(data, aes(x = Depth_mm, y = Speed)) +
  geom_density_2d_filled(alpha = 0.8) +
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE) +
  labs(title = "Density Distribution: Velocity vs. Depth", x = "Depth (mm)", y = "Velocity (m/s)") +
  theme_minimal()

data_cut <- data[1:632,]

ggplot(data_cut, aes(x = Depth_mm, y = Speed)) +
  geom_jitter(alpha = 0.3, color = "#0072B2", width = 2, height = 0.5) +
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE) +
  labs(title = "Velocity vs. Depth (Jittered with Linear Fit)", x = "Depth (mm)", y = "Velocity (m/s)") +
  theme_minimal()


ggplot(data_cut, aes(x = Depth_mm, y = Speed)) +
  geom_density_2d_filled(alpha = 0.8) +
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE) +
  labs(title = "Density Distribution: Velocity vs. Depth", x = "Depth (mm)", y = "Velocity (m/s)") +
  theme_minimal()

ggplot(data_cut, aes(x = Depth_mm_smooth, y = Speed_smooth)) +
  geom_density_2d_filled(alpha = 0.8) +
  geom_smooth(method = "lm", color = "#D55E00", se = TRUE) +
  labs(title = "Density Distribution: Velocity vs. Depth, 10min intervals", x = "Depth (mm)", y = "Velocity (m/s)") +
  theme_minimal()

# 1. Fit the linear model (y ~ x)
fit <- lm(Interval_Pulses ~ Depth_mm, data = data)
fit_cut <- lm(Interval_Pulses ~ Depth_mm, data = data_cut)
fit_smooth <- lm(Interval_Pulses_smooth ~ Depth_mm_smooth, data = data)
fit_smooth_cut <- lm(Interval_Pulses_smooth ~ Depth_mm_smooth, data = data_cut)

# 2. View the full statistical output (includes p-value, R-squared, and coefficients)
summary(fit)
summary(fit_cut)
summary(fit_smooth)
summary(fit_smooth_cut)
