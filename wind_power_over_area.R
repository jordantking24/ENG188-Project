library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# --- Configuration ---
# Specific gas constant for dry air (J/(kg·K))
R_specific <- 287.058

# Updated list of files and their corresponding altitudes (meters)
# CRITICAL: Ensure these are regular spaces, not non-breaking spaces.
file_info <- list(
#  list(name = "40m.csv", altitude = 40),
#  list(name = "60m.csv", altitude = 60),
  list(name = "100m.csv", altitude = 100)
#  list(name = "140m.csv", altitude = 140),
#  list(name = "160m.csv", altitude = 160)
)

# --- Helper Function for Pressure Interpolation ---
get_pressure_at_target_altitude <- function(P_surface, P_100m, P_200m, target_altitude) {
  known_altitudes <- c(0, 100, 200) # Surface is 0m
  known_pressures <- c(P_surface, P_100m, P_200m)
  
  # Check for NA values in input pressures, which would break approxfun
  if (any(is.na(known_pressures)) || any(!is.numeric(known_pressures))) { # Added check for numeric
    # message(paste("NA or non-numeric pressure input for target_altitude:", target_altitude, "- P_surface:", P_surface, "P_100m:", P_100m, "P_200m:", P_200m))
    return(NA_real_) # Return NA if any input pressure is NA or not numeric
  }
  
  # If target_altitude matches one of the known altitudes, use its pressure directly
  if (target_altitude %in% known_altitudes) {
    return(known_pressures[match(target_altitude, known_altitudes)])
  } else {
    # Create a linear interpolation function
    pressure_interpolator <- approxfun(known_altitudes, known_pressures, rule = 2)
    return(pressure_interpolator(target_altitude))
  }
}

# --- Main Processing ---
all_data_processed <- tibble()

message(paste("Number of files to process:", length(file_info))) # Diagnostic

for (info in file_info) {
  file_path <- info$name
  target_alt <- info$altitude
  
  message(paste("Attempting to process file:", file_path, "for target altitude:", target_alt, "m")) # Diagnostic
  
  # Construct dynamic column names
  wind_speed_col_name <- paste0("wind speed at ", target_alt, "m (m/s)")
  air_dir_col_name <- paste0("wind direction at ", target_alt, "m (deg)") # You added this
  air_temp_col_name <- paste0("air temperature at ", target_alt, "m (C)")
  
  expected_cols <- c(
    "Year", "Month", "Day", "Hour", "Minute",
    "surface air pressure (Pa)",
    "air pressure at 100m (Pa)",
    "air pressure at 200m (Pa)",
    wind_speed_col_name,
    air_dir_col_name, # Ensure this column exists in your CSVs if you require it
    air_temp_col_name
  )
  
  tryCatch({
    if (!file.exists(file_path)) {
      message(paste("File not found:", file_path))
      next # Skip to the next file
    }
    current_data_raw <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    message(paste("Successfully read:", file_path, "- Rows:", nrow(current_data_raw), "Cols:", ncol(current_data_raw))) # Diagnostic
    # print(head(current_data_raw, 2)) # Diagnostic: show first few rows
    # print(str(current_data_raw)) # Diagnostic: show structure and types
    
    # Check if all expected columns exist
    if (!all(expected_cols %in% names(current_data_raw))) {
      missing_cols <- expected_cols[!expected_cols %in% names(current_data_raw)]
      # Print a warning instead of stopping, to see if other files process
      warning(paste("Missing expected columns in", file_path, ":", paste(missing_cols, collapse=", "), ". Skipping this file."))
      next # Skip to the next file
    }
    
    processed_file_data <- current_data_raw %>%
      as_tibble() %>%
      rename(
        P_surface_Pa = `surface air pressure (Pa)`, # Use backticks for names with spaces/special chars
        P_100m_Pa = `air pressure at 100m (Pa)`,
        P_200m_Pa = `air pressure at 200m (Pa)`,
        Wind_Speed_mps_val = !!sym(wind_speed_col_name),
        # Air_Dir_deg_val = !!sym(air_dir_col_name), # Uncomment if you use this column
        Temperature_C_val = !!sym(air_temp_col_name)
      ) %>%
      # Ensure numeric types for calculation columns before rowwise
      mutate(
        across(c(P_surface_Pa, P_100m_Pa, P_200m_Pa, Wind_Speed_mps_val, Temperature_C_val, Year, Month, Day, Hour, Minute), as.numeric)
      ) %>%
      rowwise() %>%
      mutate(
        DateTime = make_datetime(Year, Month, Day, Hour, sec = 0, tz = "US/Pacific"),
        Interpolated_Pressure_Pa = get_pressure_at_target_altitude(
          P_surface_Pa, P_100m_Pa, P_200m_Pa, target_alt
        )
      ) %>%
      ungroup() %>%
      mutate(
        TargetAltitude_m = target_alt,
        Temperature_K = Temperature_C_val + 273.15,
        Air_Density_kg_m3 = Interpolated_Pressure_Pa / (R_specific * Temperature_K),
        Power_per_Area_W_m2 = 0.5 * Air_Density_kg_m3 * (Wind_Speed_mps_val^3),
        Month_lbl = month(DateTime, label = TRUE, abbr = TRUE),
        Hour_val = hour(DateTime)
      ) %>%
      select(TargetAltitude_m, DateTime, Month_lbl, Hour_val, Power_per_Area_W_m2)
    
    # message(paste("Processed data for", file_path, "- Rows:", nrow(processed_file_data))) # Diagnostic
    # print(head(processed_file_data, 2)) # Diagnostic
    # message("Summary of Power_per_Area_W_m2 for this file:") # Diagnostic
    # print(summary(processed_file_data$Power_per_Area_W_m2)) # Diagnostic
    
    all_data_processed <- bind_rows(all_data_processed, processed_file_data)
    
  }, error = function(e) {
    message(paste("Error processing file", file_path, ":", e$message))
  })
}

message(paste("Total rows in all_data_processed after loop:", nrow(all_data_processed))) # Diagnostic
if (nrow(all_data_processed) > 0) {
  # print(head(all_data_processed)) # Diagnostic
  # print(summary(all_data_processed)) # Diagnostic
}


# --- Plotting ---
if (nrow(all_data_processed) > 0) {
  all_data_processed <- all_data_processed %>%
    mutate(TargetAltitude_m_factor = factor(TargetAltitude_m))
  
  # Plot 1: Average hourly Power/Area per Month
  monthly_avg_power <- all_data_processed %>%
    filter(!is.na(Month_lbl) & !is.na(Power_per_Area_W_m2) & is.finite(Power_per_Area_W_m2)) %>% # Added is.finite
    group_by(TargetAltitude_m_factor, Month_lbl) %>%
    summarise(Mean_Power_per_Area = mean(Power_per_Area_W_m2, na.rm = TRUE), .groups = 'drop')
  
  message(paste("Rows in monthly_avg_power:", nrow(monthly_avg_power))) # Diagnostic
  # if(nrow(monthly_avg_power) > 0) print(head(monthly_avg_power)) # Diagnostic
  
  if(nrow(monthly_avg_power) > 0) {
    plot_monthly <- ggplot(monthly_avg_power, aes(x = Month_lbl, y = Mean_Power_per_Area, fill = TargetAltitude_m_factor)) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
      labs(
        title = "Average Hourly Power/Area per Month by Altitude",
        x = "Month",
        y = expression("Average Power per Area (W/m"^2*")"),
        fill = "Altitude (m)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
    
    print(plot_monthly)
    ggsave("monthly_average_power_per_area.png", plot_monthly, width = 10, height = 7, dpi = 300)
    message("Monthly plot saved as 'monthly_average_power_per_area.png'")
  } else {
    message("No data available for monthly plot after processing and filtering.")
  }
  
  # Plot 2: Average hourly Power/Area by Hour of Day
  hourly_avg_power <- all_data_processed %>%
    filter(!is.na(Hour_val) & !is.na(Power_per_Area_W_m2) & is.finite(Power_per_Area_W_m2)) %>% # Added is.finite
    group_by(TargetAltitude_m_factor, Hour_val) %>%
    summarise(Mean_Power_per_Area = mean(Power_per_Area_W_m2, na.rm = TRUE), .groups = 'drop')
  
  message(paste("Rows in hourly_avg_power:", nrow(hourly_avg_power))) # Diagnostic
  # if(nrow(hourly_avg_power) > 0) print(head(hourly_avg_power)) # Diagnostic
  
  if(nrow(hourly_avg_power) > 0) {
    # Ensure Hour_val is ordered correctly if it's a factor representing time.
    # If Hour_val is numeric (e.g., 0-23), converting to factor is fine for discrete points.
    # If it's already an ordered factor, this step might not be needed.
    # Example: hourly_avg_power$Hour_val <- factor(hourly_avg_power$Hour_val, levels = 0:23) # Or appropriate order
    
    plot_hourly <- ggplot(hourly_avg_power, aes(x = factor(Hour_val), 
                                                y = Mean_Power_per_Area, 
                                                color = TargetAltitude_m_factor,  # Changed from fill to color
                                                group = TargetAltitude_m_factor)) + # Added group aesthetic
      geom_line() + # Removed stat="identity" (default for geom_line if y is mapped) and position_dodge
      # geom_point() + # Optionally, add points to the lines
      labs(
        title = "Average Hourly Power/Area by Hour of Day and Altitude",
        x = "Hour of Day",
        y = expression("Average Power per Area (W/m"^2*")"),
        color = "Altitude (m)" # Changed legend title key from fill to color
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(plot_hourly)
    ggsave("hourly_average_power_per_area.png", plot_hourly, width = 10, height = 7, dpi = 300)
    message("Hourly plot saved as 'hourly_average_power_per_area.png'")
  }
  
  message("Plotting section complete.")
} else {
  message("No data was successfully processed into all_data_processed. Cannot generate plots.")
}

