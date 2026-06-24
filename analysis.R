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

