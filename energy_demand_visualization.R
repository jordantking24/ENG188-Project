# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr) # For unnest if needed, though less likely here
library(lubridate) # For date-time manipulation
library(scales) # For pretty_breaks and number formatting

# --- 0. Load and Prepare Data ---
# Load the dataset
demand <- read.csv("demand.csv")

# Convert Month, Day, Year, Hour to a DateTime object
# Note: Hour 0 is the start of the day.
demand <- demand %>%
  mutate(DateTime = make_datetime(Year, Month, Day, Hour))

# Add factors for Month, DayOfWeek, and define Seasons
demand <- demand %>%
  mutate(
    MonthName = factor(month.abb[Month], levels = month.abb), # Ordered factor for months
    DayOfMonth = Day, # Already have 'Day' column
    DayOfWeek = factor(wday(DateTime, label = TRUE, week_start = 1), # Mon, Tue, ...
                       levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                       ordered = TRUE),
    WeekOfYear = isoweek(DateTime), # ISO 8601 week number
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter", # Dec, Jan, Feb
      Month %in% c(3, 4, 5) ~ "Spring", # Mar, Apr, May
      Month %in% c(6, 7, 8) ~ "Summer", # Jun, Jul, Aug
      Month %in% c(9, 10, 11) ~ "Fall",   # Sep, Oct, Nov
    ),
    # Make Season an ordered factor
    Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"))
  )

# Load necessary libraries (if not already loaded)
# library(ggplot2)
# library(dplyr)
# library(lubridate) # For month.abb if demand$Month is numeric

# --- Prerequisite: Ensure 'demand' dataframe is prepared ---
# This includes 'MonthName' (factor: Jan, Feb,...), 'DayOfMonth', 'UCDdemand_kW'
# Example of demand preparation (if not run yet):
# demand <- read.csv("demand.csv")
# demand <- demand %>%
#   mutate(
#     DateTime = make_datetime(Year, Month, Day, Hour),
#     MonthName = factor(month.abb[Month], levels = month.abb),
#     DayOfMonth = Day
#   )

# --- Start of Modified Plot 1 Code ---

# 1. Calculate average demand by month and day of month (used for individual month lines)
avg_daily_by_day_month <- demand %>%
  group_by(MonthName, DayOfMonth) %>%
  summarise(AvgDemand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')

# 2. Identify the months with the overall lowest and highest average hourly consumption
overall_monthly_avg_consumption <- demand %>%
  group_by(MonthName) %>%
  summarise(OverallAvgMonthDemand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')

# Handle potential ties by taking the first month if multiple have the same min/max average
lowest_month_name <- overall_monthly_avg_consumption %>%
  filter(OverallAvgMonthDemand == min(OverallAvgMonthDemand, na.rm = TRUE)) %>%
  pull(MonthName)
if (length(lowest_month_name) > 1) lowest_month_name <- lowest_month_name[1]

highest_month_name <- overall_monthly_avg_consumption %>%
  filter(OverallAvgMonthDemand == max(OverallAvgMonthDemand, na.rm = TRUE)) %>%
  pull(MonthName)
if (length(highest_month_name) > 1) highest_month_name <- highest_month_name[1]

# 3. Prepare data for the lowest and highest months
data_lowest_month <- avg_daily_by_day_month %>%
  filter(MonthName == lowest_month_name) %>%
  mutate(LineType = paste0("Lowest Avg Month (", lowest_month_name, ")"))

data_highest_month <- avg_daily_by_day_month %>%
  filter(MonthName == highest_month_name) %>%
  mutate(LineType = paste0("Highest Avg Month (", highest_month_name, ")"))

# 4. Calculate the average of daily averages across all months for each day of the month
# This line represents, for each DayOfMonth, the average of that day's typical demand across all months.
data_average_all_months <- avg_daily_by_day_month %>%
  group_by(DayOfMonth) %>%
  summarise(AvgDemand = mean(AvgDemand, na.rm = TRUE), .groups = 'drop') %>%
  mutate(LineType = "Average of All Months")

# 5. Combine the three datasets for plotting
plot_data_combined_p1 <- bind_rows(
  data_highest_month %>% select(DayOfMonth, AvgDemand, LineType), # Highest first for legend order
  data_average_all_months %>% select(DayOfMonth, AvgDemand, LineType),
  data_lowest_month %>% select(DayOfMonth, AvgDemand, LineType)
)

# Ensure LineType is a factor to control legend order and appearance
plot_data_combined_p1$LineType <- factor(plot_data_combined_p1$LineType, levels = c(
  paste0("Highest Avg Month (", highest_month_name, ")"),
  "Average of All Months",
  paste0("Lowest Avg Month (", lowest_month_name, ")")
))

# 6. Create the modified plot 1
plot1_modified <- ggplot(plot_data_combined_p1,
                         aes(x = DayOfMonth, y = AvgDemand,
                             color = LineType, linetype = LineType, group = LineType)) +
  geom_line(size = 1.1) +
  # geom_point(size = 1.5, alpha = 0.7) + # Optional: add points
  scale_color_manual(values = c("firebrick3", "grey30", "dodgerblue3")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(title = "Daily Demand Profile Comparison by Day of Month",
       subtitle = "Comparing Highest, Lowest, and Overall Average Months (UC Davis, 2023)",
       x = "Day of Month",
       y = "Average Demand (kW)",
       color = "Profile Type", # Legend title for color
       linetype = "Profile Type") + # Legend title for linetype
  theme_light() +
  theme(legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  guides(color = guide_legend(nrow = 1)) # Ensure legend is on one row if space allows

print(plot1_modified)

# --- 2. Line graph: Energy demand over hours of the day ---
# Day with highest peak + average for each season.

# Find the day with the highest peak energy demand
peak_datetime_info <- demand[which.max(demand$UCDdemand_kW), ]
peak_date <- as.Date(peak_datetime_info$DateTime)

peak_day_data <- demand %>%
  filter(as.Date(DateTime) == peak_date)

# Calculate average energy demand by hour for each season
seasonal_hourly_avg <- demand %>%
  group_by(Season, Hour) %>%
  summarise(AvgDemand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')

plot2 <- ggplot() +
  # Line for the day with the highest peak
  geom_line(data = peak_day_data,
            aes(x = Hour, y = UCDdemand_kW, color = paste0("Peak Day (", format(peak_date, "%b %d"), ")")),
            size = 1) +
  # Lines for average seasonal demand
  geom_line(data = seasonal_hourly_avg,
            aes(x = Hour, y = AvgDemand, color = Season, group = Season),
            size = 1) +
  scale_color_manual(name = "Demand Type",
                     values = c(setNames(c("red", RColorBrewer::brewer.pal(4, "Set2")),
                                         c(paste0("Peak Day (", format(peak_date, "%b %d"), ")"),
                                           levels(seasonal_hourly_avg$Season)))) ) +
  labs(title = "Hourly Energy Demand Analysis",
       subtitle = "Peak Day vs. Seasonal Averages (UC Davis, 2023)",
       x = "Hour of Day",
       y = "Demand (kW)") +
  theme_light() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow=2, byrow=TRUE))


print(plot2)

# --- 3. Line graph: Average demand for days of the week ---
# One line for each full week (faint, colored by season)
# Average of all weeks (stronger line)

# Calculate average demand for each DayOfWeek within each WeekOfYear and Season
avg_demand_weekly_seasonal <- demand %>%
  filter(Year == 2023) %>% # Assuming 2023 as per data
  group_by(Season, WeekOfYear, DayOfWeek) %>%
  summarise(AvgDemand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop') %>%
  # Ensure all days of week are present for each week (important for geom_line)
  # This also handles partial weeks at start/end of year if they exist.
  # However, for "full weeks", we might want to filter weeks with less than 7 days of data.
  # For simplicity now, we'll plot all available data points per week.
  ungroup() # Make sure to ungroup before plotting if group aesthetic is complex


# Calculate overall average demand for each DayOfWeek
overall_avg_demand_dayofweek <- demand %>%
  group_by(DayOfWeek) %>%
  summarise(OverallAvgDemand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')

plot3 <- ggplot() +
  # Faint lines for each week, colored by season
  geom_line(data = avg_demand_weekly_seasonal,
            aes(x = DayOfWeek, y = AvgDemand, group = interaction(WeekOfYear, Season), color = Season),
            alpha = 0.25) + # Faint opacity
  # Stronger line for the overall average of all weeks
  geom_line(data = overall_avg_demand_dayofweek,
            aes(x = DayOfWeek, y = OverallAvgDemand, group = 1), # group=1 for a single line
            color = "black", size = 1) +
  scale_color_brewer(palette = "Set2", name = "Season (Weekly Avg)") + # Color for seasonal weeks
  labs(title = "Average Demand by Day of the Week",
       subtitle = "Faint lines: individual weeks by season.\nBold line: overall average. (UC Davis, 2023)",
       x = "Day of the Week",
       y = "Average Demand (kW)") +
  theme_light() +
  theme(legend.position = "top")

print(plot3)


# --- 4. Modify the existing code ---
# The user's original code for monthly_demand:
# months <- sort(unique(demand$Month)) # This would be 1, 2, ... 12
# monthly_values <- numeric() # Needs to be initialized properly
# for (m in months) {
#  # If demand$Month is numeric, m is numeric.
#  # monthly_values[m] <- mean(demand$UCDdemand_kW[m == demand$Month]) # this creates unnamed vector by numeric index
# }
# monthly_demand_original_logic <- data.frame(Month = month.abb, `Average Demand` = monthly_values)
# monthly_demand_original_logic$Month <- factor(monthly_demand_original_logic$Month, levels = month.abb)

# More robust calculation of monthly_demand using dplyr:
monthly_demand_calculated <- demand %>%
  group_by(MonthName) %>% # Group by the factor MonthName created earlier
  summarise(Average.Demand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')

# Original plot title: "Daily Demand Averages By Month (UC Davis, 2023)"
# The y-axis label: "Average Daily Demand (kW)"
# Based on the calculation `mean(demand$UCDdemand_kW[m == demand$Month])`
# this is actually the average of *all hourly demands* within that month, not "average daily demand".
# If "average daily demand" means first averaging by day, then averaging those daily averages for the month,
# the calculation would be different. Assuming the user's original intent was average of all hourly demands.

# Determine current y-axis range to inform new breaks
y_range <- range(monthly_demand_calculated$Average.Demand, na.rm = TRUE)
num_current_breaks <- length(pretty(y_range)) # Estimate current default breaks
desired_breaks_count <- max(6, num_current_breaks * 1.5) # Aim for 1.5 to 2x, min 6 breaks


plot4_modified <- ggplot(monthly_demand_calculated, aes(x = MonthName, y = Average.Demand)) +
  geom_col(fill = "skyblue") + # Changed to skyblue for better contrast with black text
  # Add demand number above each bar
  geom_text(aes(label = round(Average.Demand, 0)), # Round to 0 decimal places
            vjust = -0.5, # Adjust vertical position to be above the bar
            size = 3) + # Adjust text size as needed
  # Set y-axis limits if needed (user had it commented out, so I'll respect that unless data warrants it)
  # coord_cartesian(ylim = c(0, max(monthly_demand_calculated$Average.Demand, na.rm = TRUE) * 1.1)) + # Ensure space for labels
  scale_y_continuous(breaks = scales::pretty_breaks(n = as.integer(desired_breaks_count)),
                     labels = scales::comma) + # Use comma for thousands separator
  labs(title = "Average Hourly Demand By Month",
       subtitle = "UC Davis, 2023",
       x = "Month",
       y = "Average Hourly Demand (kW)") +
  theme_light()

print(plot4_modified)

# Assuming 'demand' dataframe is already loaded and prepared with
# DateTime, MonthName (factor), DayOfMonth, Year, UCDdemand_kW
# (as done in the previous comprehensive script)

# --- Plot 5: Average DAILY demand by month ---
# This is different from plot 4, which showed average HOURLY demand by month.

# 1. Calculate total demand for each day
daily_total_demand <- demand %>%
  group_by(Year, MonthName, DayOfMonth) %>%
  summarise(TotalDailyDemand_kW_sum = sum(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')
# If UCDdemand_kW is an average power for the hour, then summing it gives kWh for that hour.
# Summing these hourly kWh values gives total kWh for the day.
# If UCDdemand_kW is an instantaneous reading at the start of the hour,
# this sum still represents a proxy for daily energy, often treated as kWh if rate is per hour.
# For clarity, let's assume UCDdemand_kW is average kW over the hour, so sum is kWh for the day.

# 2. Calculate the average of these daily total demands for each month
avg_daily_demand_per_month <- daily_total_demand %>%
  group_by(MonthName) %>%
  summarise(AvgDailyDemand = mean(TotalDailyDemand_kW_sum, na.rm = TRUE), .groups = 'drop')

# Determine y-axis breaks for the new plot (similar logic to plot 4)
y_range_plot5 <- range(avg_daily_demand_per_month$AvgDailyDemand, na.rm = TRUE)
num_current_breaks_plot5 <- length(pretty(y_range_plot5))
desired_breaks_count_plot5 <- max(6, as.integer(num_current_breaks_plot5 * 1.5))

plot5 <- ggplot(avg_daily_demand_per_month, aes(x = MonthName, y = AvgDailyDemand)) +
  geom_col(fill = "steelblue") + # Using a different color for distinction
  geom_text(aes(label = round(AvgDailyDemand, 0)),
            vjust = -0.5, # Adjust vertical position to be above the bar
            size = 3) +   # Adjust text size as needed
  scale_y_continuous(breaks = scales::pretty_breaks(n = desired_breaks_count_plot5),
                     labels = scales::comma) + # Use comma for thousands separator
  labs(title = "Average Daily Energy Consumption By Month (UC Davis, 2023)",
       x = "Month",
       y = "Average Daily Energy (sum of hourly kW readings)") + # Units are effectively kWh if rate is per hour
  theme_light()

print(plot5)