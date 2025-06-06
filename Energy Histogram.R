# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate) # For date-time manipulation
library(scales)    # For scales in plots

# --- 0. Load Data ---
demand <- read.csv("demand.csv")

# --- New Functionality: Handle Negative Demand and Initial Box Plot ---

# 1. Identify and report negative demand values
negative_demand_count <- sum(demand$UCDdemand_kW < 0, na.rm = TRUE)
if (negative_demand_count > 0) {
  cat("Found", negative_demand_count, "records with negative UCDdemand_kW values.\n")
  cat("These negative values will be replaced with 0.\n")
} else {
  cat("No negative UCDdemand_kW values found.\n")
}

# 2. Replace negative demand values with 0
demand$UCDdemand_kW[demand$UCDdemand_kW < 0] <- 0
cat("Negative UCDdemand_kW values (if any) have been replaced with 0.\n\n")

annual_energy_use <- sum(demand$UCDdemand_kW)

# 3. Create and print a box and whisker plot of all demand values
# This plot shows the distribution after negatives are set to 0.
initial_demand_boxplot <- ggplot(demand, aes(y = UCDdemand_kW)) +
  geom_boxplot(fill = "lightgray", outlier.colour = "red", outlier.shape = 1) +
  scale_y_continuous(labels = scales::comma) + # Format y-axis labels
  labs(title = "Distribution of UCDavis Energy Demand (2023)",
       subtitle = "After replacing negative values with 0",
       y = "Demand (kW)",
       x = "") + # No x-axis category needed for a single box plot
  theme_light() +
  theme(axis.text.x = element_blank(), # Remove x-axis text
        axis.ticks.x = element_blank()) # Remove x-axis ticks

print(initial_demand_boxplot)
cat("Initial box plot of demand values has been generated and printed.\n\n")

# --- End of New Functionality ---

# --- Data Preparation (as per previous script, for subsequent plots) ---
demand <- demand %>%
  mutate(
    DateTime = make_datetime(Year, Month, Day, Hour),
    MonthName = factor(month.abb[Month], levels = month.abb), # Ordered factor for months
    DayOfMonth = Day
    # Add other necessary columns if other plots were to be reinstated:
    # DayOfWeek = factor(wday(DateTime, label = TRUE, week_start = 1),
    #                    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    #                    ordered = TRUE),
    # WeekOfYear = isoweek(DateTime),
    # Season = case_when(
    #   Month %in% c(12, 1, 2) ~ "Winter",
    #   Month %in% c(3, 4, 5) ~ "Spring",
    #   Month %in% c(6, 7, 8) ~ "Summer",
    #   Month %in% c(9, 10, 11) ~ "Fall",
    # ),
    # Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"))
  )
cat("Basic data preparation (DateTime, MonthName, DayOfMonth) complete.\n")

# --- Modified Plot 1 Code (Lowest, Highest, Average Month Profiles) ---

# 1. Calculate average demand by month and day of month
avg_daily_by_day_month <- demand %>%
  group_by(MonthName, DayOfMonth) %>%
  summarise(AvgDemand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')

# 2. Identify the months with the overall lowest and highest average hourly consumption
overall_monthly_avg_consumption <- demand %>%
  group_by(MonthName) %>%
  summarise(OverallAvgMonthDemand = mean(UCDdemand_kW, na.rm = TRUE), .groups = 'drop')

lowest_month_name <- overall_monthly_avg_consumption %>%
  filter(OverallAvgMonthDemand == min(OverallAvgMonthDemand, na.rm = TRUE)) %>%
  pull(MonthName)
if (length(lowest_month_name) > 1) lowest_month_name <- lowest_month_name[1] # Handle ties

highest_month_name <- overall_monthly_avg_consumption %>%
  filter(OverallAvgMonthDemand == max(OverallAvgMonthDemand, na.rm = TRUE)) %>%
  pull(MonthName)
if (length(highest_month_name) > 1) highest_month_name <- highest_month_name[1] # Handle ties

# 3. Prepare data for the lowest and highest months
data_lowest_month <- avg_daily_by_day_month %>%
  filter(MonthName == lowest_month_name) %>%
  mutate(LineType = paste0("Lowest Avg Month (", lowest_month_name, ")"))

data_highest_month <- avg_daily_by_day_month %>%
  filter(MonthName == highest_month_name) %>%
  mutate(LineType = paste0("Highest Avg Month (", highest_month_name, ")"))

# 4. Calculate the average of daily averages across all months for each day of the month
data_average_all_months <- avg_daily_by_day_month %>%
  group_by(DayOfMonth) %>%
  summarise(AvgDemand = mean(AvgDemand, na.rm = TRUE), .groups = 'drop') %>%
  mutate(LineType = "Average of All Months")

# 5. Combine the three datasets for plotting
plot_data_combined_p1 <- bind_rows(
  data_highest_month %>% select(DayOfMonth, AvgDemand, LineType),
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
  scale_color_manual(values = c("firebrick3", "grey30", "dodgerblue3")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  scale_y_continuous(labels = scales::comma) + # Format y-axis labels
  labs(title = "Daily Demand Profile Comparison by Day of Month",
       subtitle = "Comparing Highest, Lowest, and Overall Average Months (UC Davis, 2023)",
       x = "Day of Month",
       y = "Average Demand (kW)",
       color = "Profile Type",
       linetype = "Profile Type") +
  theme_light() +
  theme(legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  guides(color = guide_legend(nrow = 1))

print(plot1_modified)
cat("Modified Plot 1 has been generated and printed.\n")