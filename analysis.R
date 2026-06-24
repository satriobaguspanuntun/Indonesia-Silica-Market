library(tidyverse)
library(openxlsx)


# Market Balance (forcasted)
market_balance <- silica_prod_bps %>% 
  mutate(reporter_iso = "IDN",
         cmd_code = "250510") %>% 
  
  # 1. Change to full_join so the missing 2025 year is safely injected
  full_join(inter_trade_data %>%
              select(ref_year, reporter_iso, reporter_desc, flow_desc, cmd_code, cmd_desc, primary_value, net_wgt) %>% 
              filter(cmd_code == "250510", flow_desc == "Export", reporter_iso == "IDN"), 
            by = join_by(year == ref_year, cmd_code, reporter_iso)) %>% 
  rename("export" = primary_value,
         "net_wgt_exp" = net_wgt) %>% 
  select(-flow_desc) %>% 
  
  # 2. Left join works perfectly now because the 2025 row was built above
  left_join(inter_trade_data %>%
              select(ref_year, reporter_iso, reporter_desc, flow_desc, cmd_code, cmd_desc, primary_value, net_wgt) %>% 
              filter(cmd_code == "250510", flow_desc == "Import", reporter_iso == "IDN"), 
            by = join_by(year == ref_year, cmd_code, reporter_iso)) %>% 
  
  # 3. Clean up the joined metadata descriptions safely using coalesce
  mutate(reporter_desc = coalesce(reporter_desc.y, reporter_desc.x),
         cmd_desc = coalesce(cmd_desc.y, cmd_desc.x)) %>%
  select(-reporter_desc.x, -cmd_desc.x, -reporter_desc.y, -cmd_desc.y, -flow_desc) %>% 
  
  rename("import" = primary_value,
         "net_wgt_imp" = net_wgt) %>% 
  select(type, year, cmd_desc, cmd_code, reporter_iso, production, prod_kt, export, net_wgt_exp, import, net_wgt_imp) %>% 
  
  # 4. Final cleaning and scaling
  mutate(across(c(production, prod_kt, export, net_wgt_exp, import, net_wgt_imp), ~ as.numeric(replace_na(., 0)))) %>% 
  mutate(net_wgt_exp = round(net_wgt_exp/1e6, 3),
         net_wgt_imp = round(net_wgt_imp/1e6, 3)) %>% 
  
  # 5. adjust 2024 prod_kt by using 
  arrange(year) %>%
  # 1. Calculate the year-over-year growth rate of actual export weights
  mutate(
    export_growth = (net_wgt_exp - lag(net_wgt_exp)) / lag(net_wgt_exp)
  ) %>%
  # 2. Reconstruct 2024 and 2025 production using the 2023 baseline as the anchor
  mutate(
    production = case_when(
      year == 2024 ~ lag(production, 1) * (1 + export_growth),
      year == 2025 ~ lag(production, 2) * (1 + lag(export_growth, 1)) * (1 + export_growth),
      TRUE ~ production
    ),
    # 3. Recalculate your volume-to-weight metric using your constant ratio
    prod_kt = case_when(
      year %in% c(2024, 2025) ~ round(production * 1.6 / 1000, 3),
      TRUE ~ prod_kt
    )
  ) %>%
  # Remove the temporary growth column
  select(-export_growth) %>% 
  
  mutate(
    # Ensure all trade variables are strictly weights in kilotons
    import_kt = net_wgt_imp, 
    export_kt = net_wgt_exp,
    
    # Calculate demand side
    apparent_domestic_consumption = prod_kt + import_kt - export_kt,
    total_demand = apparent_domestic_consumption + export_kt,
    
    # Export share of total supply
    export_share_pct = (export_kt / prod_kt) * 100
  )

# apparent domestic consumption vs production 
p1 <- market_balance %>% 
  pivot_longer(cols = production:export_share_pct,
               names_to = "var",
               values_to = "values") %>% 
  filter(var %in% c("prod_kt", "apparent_domestic_consumption")) %>% 
  ggplot(aes(year, values, fill = var)) +
  geom_bar(stat = "identity", position = "dodge")

# silica sand trade balance
p2 <- market_balance %>% 
  mutate(trade_balance = net_wgt_exp - net_wgt_imp) %>% 
  pivot_longer(cols = production:trade_balance,
               names_to = "var",
               values_to = "values") %>% 
  filter(var == "trade_balance") %>% 
  ggplot(aes(year, values, fill = var)) +
  geom_bar(stat = "identity")
  


# export price
p3 <- market_balance %>% 
  mutate(
    export_price = if_else(export_kt > 0, round(export / (export_kt * 1000), 2), 0),
    import_price = if_else(import_kt > 0, round(import / (import_kt * 1000), 2), 0)
  ) %>% 
  pivot_longer(cols = production:import_price,
               names_to = "var",
               values_to = "values") %>% 
  filter(var %in% c("export_price"), year >= 2021) %>% 
  ggplot(aes(year, values, colour = var)) +
  geom_col()


library(dplyr)

# 1. Input your exact historical dataset snapshot
historical_data <- tibble::tribble(
  ~year, ~prod_kt, ~import_kt, ~export_kt, ~apparent_domestic_consumption,
  2011,  1832,     35.7,       0.013,      1868,
  2012,  1948,     43.1,       0.185,      1991,
  2013,  2926,     35.6,       0,          2961,
  2014,  3915,     43.6,       0,          3958,
  2015,  1974,     17.2,       0,          1991,
  2016,  2789,     30.9,       0,          2820,
  2017,  3605,     35.0,       0,          3640,
  2018,  2566,     14.4,       0.012,      2581,
  2019,  3501,     24.6,       0,          3525,
  2020,  3001,     11.4,       0,          3012,
  2021,  3224,     10.6,       208,        3026,
  2022,  4916,     6.90,       800,        4122,
  2023,  5131,     10.4,       2387,       2754,
  2024,  5562,     15.3,       2587,       2990,
  2025,  7868,     17.2,       3660,       4225
)

# 2. Calculate the historical baseline average from the stable zero-export era (2013-2020)
# This represents what the domestic market naturally consumes (~3,061 kt/year)
stable_era_demand <- historical_data %>%
  filter(year >= 2013 & year <= 2020) %>%
  summarise(avg_demand = mean(apparent_domestic_consumption)) %>%
  pull(avg_demand)

# 3. Quantify the real Surplus or Deficit
market_balance_quantified <- historical_data %>%
  mutate(
    # Assume domestic industrial needs grow at a standard conservative 4% from the stable era baseline
    true_estimated_industrial_need = stable_era_demand * (1.04)^(year - 2020),
    
    # Override for historical zero-export years where apparent consumption WAS the true need
    true_estimated_industrial_need = if_else(year <= 2020, apparent_domestic_consumption, true_estimated_industrial_need),
    
    # Structural Balance = What was left in the country MINUS what factories actually needed
    net_structural_balance_kt = round(apparent_domestic_consumption - true_estimated_industrial_need, 2),
    
    market_condition = case_when(
      net_structural_balance_kt > 100  ~ "SURPLUS: Supply building up in inventories",
      net_structural_balance_kt < -100 ~ "DEFICIT: Domestic factories starved by exports",
      TRUE                             ~ "BALANCED: Equilibrium"
    )
  )


library(dplyr)

structural_squeeze <- market_balance %>%
  arrange(year) %>%
  mutate(
    # 1. Isolate what is physically left behind for Indonesia each year
    domestic_available_supply_kt = prod_kt + import_kt - export_kt,
    
    # 2. Calculate the Year-over-Year (YoY) growth rate of that domestic supply
    domestic_supply_growth_pct = (domestic_available_supply_kt - lag(domestic_available_supply_kt)) / lag(domestic_available_supply_kt) * 100,
    
    # 3. Quantify the market stress condition
    market_balance_signal = case_when(
      year <= 2020 ~ "STABLE: Zero-Export Insulation Era",
      domestic_supply_growth_pct < -5  ~ "DEFICIT SQUEEZE: Exports cannibalizing domestic supply",
      domestic_supply_growth_pct > 10  ~ "SURPLUS GLUT: Supply piling up faster than industrial growth",
      TRUE                              ~ "BALANCED: Supply tracking steady"
    )
  )


library(dplyr)

# 1. Define the planning horizon
forecast_years <- 2022:2031

# 2. Build the unified bottom-up industrial demand matrix
domestic_demand_matrix <- tibble(year = forecast_years) %>%
  mutate(
    # --- CEMENT SECTOR FOOTPRINT (From Ministry of Industry Data) ---
    # Actuals from chart for 2022-2024; 3.5% organic growth thereafter
    cement_production_mio_tons = case_when(
      year == 2022 ~ 64.5,
      year == 2023 ~ 66.9,
      year == 2024 ~ 67.8,
      TRUE         ~ 67.8 * (1.035)^(year - 2024)
    ),
    # Derive Sand Demand using our empirical coefficients (converted to kt)
    sand_cement_core_kt     = cement_production_mio_tons * 0.025417 * 1000,
    sand_cement_products_kt = cement_production_mio_tons * 0.008632 * 1000,
    total_cement_sand_kt    = sand_cement_core_kt + sand_cement_products_kt,
    
    # --- GLASS SECTOR FOOTPRINT (From ESDM Table 13.1) ---
    # 2022 Base Capacity = 2,158 kt. 
    # We factor in a conservative 4.0% CAGR for standard domestic glass expansion, 
    # PLUS a massive structural step-change in 2027 (+1,200 kt capacity) 
    # representing the landmark Xinyi Glass downstream mega-project in Batam/Rempang.
    glass_capacity_kt = case_when(
      year <= 2026 ~ 2158 * (1.04)^(year - 2022),
      year >= 2027 ~ (2158 * (1.04)^(year - 2022)) + 1200 # Downstreaming policy shock
    ),
    # Apply the capacity-normalized coefficient derived from the ESDM sheet
    total_glass_sand_kt = glass_capacity_kt * 0.372449,
    
    # --- OTHER INDUSTRIAL ALPHA SECTORS (From Table 13.1 rows 3, 4, 6, 7) ---
    # Combined 2022 baseline for Ceramics, Smelters, and Processing = 1,530.6 kt
    # Growing at a steady macro baseline of 3.0% YoY
    other_sectors_sand_kt = 1530.67 * (1.03)^(year - 2022),
    
    # --- FINAL BOTTOM-UP AGGREGATION ---
    clean_domestic_demand_kt = round(total_cement_sand_kt + total_glass_sand_kt + other_sectors_sand_kt, 2)
  )

# 3. View the final scannable demand vector for your report
final_demand_summary <- domestic_demand_matrix %>%
  select(year, cement_production_mio_tons, total_cement_sand_kt, glass_capacity_kt, total_glass_sand_kt, clean_domestic_demand_kt)

print(final_demand_summary)


library(dplyr)

# 1. Configure timeline and export contract roll-off rate
timeline <- 2022:2031
export_attrition_rate <- 0.133 # 13.3% annual decay based on contract lifecycle

export_attrition_outlook <- tibble(year = timeline) %>%
  mutate(
    # --- PRODUCTION (Grows organically at 3.5% baseline post-2025) ---
    prod_kt = case_when(
      year == 2022 ~ 4916.00,
      year == 2023 ~ 5131.00,
      year == 2024 ~ 5562.00,
      year == 2025 ~ 7868.00,
      TRUE         ~ 7868.00 * (1.035)^(year - 2025)
    ),
    
    # --- EXPORTS (Phased decline as 5-10 year licenses expire) ---
    export_kt = case_when(
      year <= 2026 ~ case_when(
        year == 2022 ~ 800.00,
        year == 2023 ~ 2387.00,
        year == 2024 ~ 2587.00,
        TRUE         ~ 3660.00
      ),
      TRUE         ~ 3660.00 * (1 - export_attrition_rate)^(year - 2026)
    ),
    
    # --- IMPORTS ---
    import_kt = case_when(
      year == 2022 ~ 6.90,
      year == 2023 ~ 10.40,
      year == 2024 ~ 15.30,
      year == 2025 ~ 17.20,
      TRUE         ~ 17.20
    ),
    
    # --- TRUE INDUSTRIAL DEMAND (Bottom-up base growing at 4.5% organically) ---
    true_industrial_demand_kt = 4523.837 * (1.045)^(year - 2022)
  ) %>%
  
  # --- REALIZED MARKET BALANCE ---
  mutate(
    apparent_domestic_consumption_kt = prod_kt + import_kt - export_kt,
    structural_balance_kt = round(apparent_domestic_consumption_kt - true_industrial_demand_kt, 2)
  )

# View clean, rounded summary matrix
print(export_attrition_outlook %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

# Reserve and Mine location expansion




# Trade position and direction (value and volume)

# Export prices

# supply and demand

# Supply
## mine output production
## Quality of the silica sand
## Supply crunch may happened if supply of high quality sand are not accompanied 
## with mine upgrading this add an extra cost to produce
## Logistic cost pressure amid high oil prices
## FDI within this industry
## Import

# Demand
## End use share and forecast
## China PV dynamics
## Glass manufacture capacity and consumption
### Glass trade export 
p1 <- inter_trade_data %>% 
  filter(reporter_iso == "IDN", cmd_code == c(7003, 7004, 7005, 7007, 7010, 7019))



### housing, household consumption, and highten interest rate
## Cement consumption
## Fabs for chips and semiconductor 
## FDI within demand industry
## Export ban how it affect forcasted market balance

# regulation summary

# Key takeaways
# For mining companies
# Traders
# Industry

# Methodlogy and caveats

