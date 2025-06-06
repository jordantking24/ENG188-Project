import pandas as pd
import matplotlib.pyplot as plt
import time

# Load the datasets from your local directory
try:
    demand_df = pd.read_csv('demand.csv')
    wind_power_df = pd.read_csv('wind_power_data_100.csv')
    monthly_surplus_costs_df = pd.read_csv('monthly_surplus_costs.csv')
except FileNotFoundError as e:
    print(f"Error: {e}. Please make sure the CSV files are in the same directory as the script.")
    exit()

# --- Data Preparation ---

# Prepare the demand data
# Create a datetime column from the separate date and time columns
demand_df['date'] = pd.to_datetime(demand_df[['Year', 'Month', 'Day', 'Hour']])
demand_df.set_index('date', inplace=True)
# Rename the demand column for consistency
demand_df.rename(columns={'UCDdemand_kW': 'demand (kWh)'}, inplace=True)
# Keep only the demand column, as the others are now part of the index
demand_df = demand_df[['demand (kWh)']]

# Prepare the wind power data
# Convert the 'DateTime' column to datetime objects
wind_power_df['date'] = pd.to_datetime(wind_power_df['DateTime'])
# The wind data is from 2014, so we change the year to 2023 to align with the demand data
wind_power_df['date'] = wind_power_df['date'].apply(lambda dt: dt.replace(year=2023))
wind_power_df.set_index('date', inplace=True)
# Rename the power column for consistency, assuming the values are in kWh
wind_power_df.rename(columns={'Energy_Generated_kWh': 'wind power (kWh)'}, inplace=True)
# Keep only the power column
wind_power_df = wind_power_df[['wind power (kWh)']]

# Align the demand and wind power dataframes on their datetime index.
# The 'inner' join ensures that only timestamps present in both dataframes are kept.
aligned_demand, aligned_wind = demand_df.align(wind_power_df, join='inner', axis=0)

# Prepare the monthly surplus costs data
# Convert the month names to month numbers (1 for January, 2 for February, etc.)
monthly_surplus_costs_df['Month'] = pd.to_datetime(monthly_surplus_costs_df['Month'], format='%B').dt.month
monthly_surplus_costs_df.set_index('Month', inplace=True)
# Rename the cost column for consistency
monthly_surplus_costs_df.rename(columns={'NSC_Rate': 'Price per kWh'}, inplace=True)
# Keep only the price column, dropping the empty 'Unnamed: 2' column
monthly_surplus_costs_df = monthly_surplus_costs_df[['Price per kWh']]


# --- Model Execution ---

# Define the constants and parameters for the model
MAX_WIND_PRODUCTION_PER_TURBINE_KWH = 3000
DEMAND_SCALING_FACTOR = 1.1
WIND_EFFICIENCY_FACTOR = 0.9
TURBINE_INSTALL_COST_FACTOR = 1300 # $ / kWh
MAINTENANCE_COST_PER_KWH = 0.015
GRID_PURCHASE_COST_PER_KWH = 0.32197

# Apply the scaling factor to the demand data
aligned_demand['scaled_demand_kWh'] = aligned_demand['demand (kWh)'].clip(lower=0) * DEMAND_SCALING_FACTOR

# Apply the production cap to the wind power data for a single turbine
aligned_wind['capped_power_kWh'] = aligned_wind['wind power (kWh)'].clip(upper=MAX_WIND_PRODUCTION_PER_TURBINE_KWH)
#print(aligned_wind['capped_power_kWh'])

# Initialize a list to store the results for each turbine scenario
results_list = []

# Loop through a range of turbine numbers (from 5 to 15)
for num_turbines in range(50):
    # Calculate the total wind generation for the current number of turbines
    total_wind_generation = aligned_wind['capped_power_kWh'] * num_turbines * WIND_EFFICIENCY_FACTOR
    #print(total_wind_generation)
    
    # Calculate the hour-by-hour mismatch between generation and demand
    mismatch = total_wind_generation - aligned_demand['scaled_demand_kWh']
    total_mismatch = mismatch.sum()

    # Calculate the total annual energy produced and demanded
    total_energy_produced = total_wind_generation.sum()
    total_demand = aligned_demand['scaled_demand_kWh'].sum()
    #print(total_energy_produced)

    # --- Financial Calculations ---
    # Installation cost for all turbines
    install_cost = num_turbines * TURBINE_INSTALL_COST_FACTOR * (MAX_WIND_PRODUCTION_PER_TURBINE_KWH)
    # Maintenance cost based on total kWh produced
    maintenance_cost = total_energy_produced * MAINTENANCE_COST_PER_KWH
    # Cost to purchase energy from the grid when there is a deficit (mismatch < 0)
    purchase_cost = -mismatch[mismatch < 0].sum() * GRID_PURCHASE_COST_PER_KWH

# Calculate the revenue from selling surplus energy to the grid
    # First, identify only the hours where there was a surplus (generation > demand).
    hourly_surplus = mismatch[mismatch > 0]

    # Now, sum up these hourly surpluses for each month to get the total monthly surplus.
    monthly_total_surplus = hourly_surplus.resample('ME').sum()

    # Calculate revenue by multiplying the total surplus of each month by that month's price.
    surplus_revenue = 0
    for month_date, total_surplus_kwh in monthly_total_surplus.items():
        month_number = month_date.month
        price = monthly_surplus_costs_df.loc[month_number, 'Price per kWh']
        surplus_revenue += total_surplus_kwh * price

    # Calculate the total energy balance for the year
    total_energy_balance = total_energy_produced - total_demand
    
    # Calculate annual revenue and other notable financial metrics
    no_renewable_cost = total_demand * purchase_cost
    yearly_net_operating_cost = surplus_revenue - maintenance_cost - purchase_cost
    new_system_savings = yearly_net_operating_cost - no_renewable_cost

    # Store the results for the current scenario in a dictionary
    results_list.append({
        'Number of Turbines': num_turbines,
        'Annual Installation Cost': install_cost,
        'Annual Operation/Maintenance Cost': maintenance_cost,
        'Annual Cost of Purchasing Electricity': purchase_cost,
        'Annual Revenue from Selling Surplus': surplus_revenue,
        'Total Energy Produced (kWh)': total_energy_produced,
        'Total Demand (kWh)': total_demand,
        'Total Energy Balance (kWh)': total_energy_balance,
        'Cost of Grid Without Renewables': no_renewable_cost,
        'Annual Net Operating Cost': yearly_net_operating_cost,
        'Savings From Implementation of System': new_system_savings
    })

# Create a DataFrame from the list of results
results_df = pd.DataFrame(results_list)
results_df.to_csv('turbine_analysis_results.csv', index=False)

# --- Output and Visualization ---

# Create the bar chart for the monthly mismatch for the last scenario (15 turbines)
monthly_mismatch_kwh = mismatch.resample('ME').sum()
# Format the index to show month names instead of timestamps
monthly_mismatch_kwh.index = monthly_mismatch_kwh.index.strftime('%B')

# Create the plot
plt.figure(figsize=(12, 6))
# Color the bars green for surplus and red for deficit
monthly_mismatch_kwh.plot(kind='bar', color=(monthly_mismatch_kwh > 0).map({True: 'g', False: 'r'}))
plt.title('Monthly Mismatch between Wind Power Generation and Demand (X Turbines)')
plt.xlabel('Month')
plt.ylabel('Energy Mismatch (kWh)')
plt.xticks(rotation=45)
plt.grid(axis='y', linestyle='--')
plt.tight_layout()

# Display the plot
plt.show()