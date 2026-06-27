library(tidyverse)
library(openxlsx)

# Read IMF data
imf_weo <-read_csv("~/Indonesia-Silica-Market/data/IMF.csv")

imf_weo_forecast <- imf_weo %>% 
  filter(INDICATOR %in% c("Gross domestic product (GDP), Constant prices, Domestic currency",
                          "Gross domestic product (GDP), Current prices, Domestic currency",
                          "Gross domestic product (GDP), Current prices, Per capita, US dollar",
                          "Gross domestic product (GDP), Constant prices, Percent change",
                          "Exports of goods and services, Volume, Free on board (FOB), Percent change",
                          "Imports of goods and services, Volume, Cost insurance freight (CIF), Percent change",
                          "Imports of goods, Volume, Cost insurance freight (CIF), Percent change",
                          "Exports of goods, Volume, Free on board (FOB), Percent change")) %>% 
  mutate(INDICATOR = case_when(
    INDICATOR == "Gross domestic product (GDP), Constant prices, Domestic currency" ~ "gdp_const_domestic",
    INDICATOR == "Gross domestic product (GDP), Current prices, Domestic currency" ~ "gdp_curr_domestic",
    INDICATOR == "Gross domestic product (GDP), Current prices, Per capita, US dollar" ~ "gdp_per_capita_usd",
    INDICATOR == "Gross domestic product (GDP), Constant prices, Percent change" ~ "gdp_growth_pct",
    INDICATOR == "Exports of goods and services, Volume, Free on board (FOB), Percent change" ~ "export_goods_serv_growth_pct",
    INDICATOR == "Imports of goods and services, Volume, Cost insurance freight (CIF), Percent change" ~ "import_goods_serv_growth_pct",
    INDICATOR == "Imports of goods, Volume, Cost insurance freight (CIF), Percent change" ~ "import_goods_growth_pct",
    INDICATOR == "Exports of goods, Volume, Free on board (FOB), Percent change" ~ "export_goods_growth_pct"
  ))

# ==============================================================================
# 1. PARAMETERS & FORECAST CONFIGURATION
# ==============================================================================

glass_projects <- tibble(
  year_commissioned = c(2024, 2026, 2028),
  capacity_add_kt   = c(838.0, 720.0, 400.0)
)

cement_kemenperin_base <- tibble(
  year           = c(2022, 2023, 2024),
  cement_prod_mt = c(64.5, 66.9, 67.8)
)

imf_growth_factors <- imf_weo_forecast %>%
  select(INDICATOR, `2022`:`2031`) %>%
  pivot_longer(cols = `2022`:`2031`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) %>%
  pivot_wider(names_from = INDICATOR, values_from = value) %>%
  mutate(
    gdp_macro_multiplier    = gdp_growth_pct / 100,
    export_macro_multiplier = export_goods_growth_pct / 100,
    import_macro_multiplier = import_goods_growth_pct / 100
  ) %>%
  select(year, gdp_macro_multiplier, export_macro_multiplier, import_macro_multiplier)


#  Block A
prep_trade_data <- function(trade_df, target_cmd = "250510", target_iso = "IDN") {
  # Isolate and transform export data
  exports <- trade_df %>%
    filter(cmd_code == target_cmd, flow_desc == "Export", reporter_iso == target_iso) %>%
    select(ref_year, cmd_code, reporter_iso, export = primary_value, net_wgt_exp = net_wgt)
  
  # Isolate and transform import data
  imports <- trade_df %>%
    filter(cmd_code == target_cmd, flow_desc == "Import", reporter_iso == target_iso) %>%
    select(ref_year, cmd_code, reporter_iso, import = primary_value, net_wgt_imp = net_wgt)
  
  # Consolidate and convert units to Kilotons (kt)
  exports %>%
    full_join(imports, by = join_by(ref_year, cmd_code, reporter_iso)) %>%
    rename(year = ref_year) %>%
    mutate(across(c(export, net_wgt_exp, import, net_wgt_imp), ~ as.numeric(tidyr::replace_na(., 0)))) %>%
    mutate(
      export_kt = round(net_wgt_exp / 1e6, 3),
      import_kt = round(net_wgt_imp / 1e6, 3)
    ) %>%
    select(-net_wgt_exp, -net_wgt_imp)
}

calculate_supply_framework <- function(prod_df, trade_clean_df, imf_growth, cfg, start_yr = 2011, end_yr = 2031, scenario = TRUE) {
  
  df <- prod_df %>%
    filter(year >= start_yr) %>%
    mutate(reporter_iso = "IDN", cmd_code = "250510") %>%
    
    # Merge Cleaned Trade Data
    full_join(trade_clean_df, by = join_by(year, cmd_code, reporter_iso)) %>%
    mutate(across(c(production, prod_kt, export_kt, import_kt), ~ as.numeric(tidyr::replace_na(., 0)))) %>%
    
    # Extend Horizon & Inject IMF Macros
    tidyr::complete(year = start_yr:end_yr, fill = list(reporter_iso = "IDN", cmd_code = "250510")) %>%
    mutate(type = if_else(year <= 2025, "Historical", "Forecast")) %>%
    arrange(year) %>%
    left_join(imf_growth, by = "year")
  
  if (scenario == TRUE) {
    df2 <- df %>% 
      # Setup Anchors and Compound Future Projections
      mutate(
        prod_2024_anchor   = max(prod_kt[year == 2024], na.rm = TRUE),
        export_2026_anchor = 352.92 * (12 / 4),                        # NEW: Annualized Jan-Apr 2026 data (~1,058.76 kt)
        import_2025_anchor = max(import_kt[year == 2025], na.rm = TRUE)
      ) %>%
      arrange(year) %>%
      mutate(
        prod_growth_factor = if_else(year <= 2024, 1, 1 + cfg$mining_organic_growth),
        prod_kt = case_when(
          year <= 2024 ~ prod_kt,
          TRUE         ~ prod_2024_anchor * cumprod(prod_growth_factor)
        ))
    
  } else if (scenario == FALSE) {
    df2 <- df %>% 
      mutate(
        prod_kt = prod_kt * 1.2,
        prod_2023_anchor   = max(prod_kt[year == 2023], na.rm = TRUE),
        export_2026_anchor = 352.92 * (12 / 4),                        # NEW: Annualized Jan-Apr 2026 data (~1,058.76 kt)
        import_2025_anchor = max(import_kt[year == 2025], na.rm = TRUE)
      ) %>%
      arrange(year) %>%
      mutate(
        prod_growth_factor = if_else(year <= 2023, 1, 1 + cfg$mining_organic_growth),
        prod_kt = case_when(
          year <= 2023 ~ prod_kt,
          TRUE         ~ prod_2023_anchor * cumprod(prod_growth_factor)
        ))
  }
  
  df3 <- df2 %>%      
    mutate(
      # NEW: Export growth factor stays flat (1) through 2026, then applies IMF growth from 2027+
      export_growth_factor = if_else(year <= 2026, 1, (1 + export_macro_multiplier)),
      export_kt = case_when(
        year >= 2026  ~ export_2026_anchor * cumprod(export_growth_factor), # Evaluates to exactly 1,058.76 in 2026
        TRUE          ~ export_kt
      ),
      
      # UNCHANGED: Imports continue to use the 2025 anchor scaled by IMF factors
      import_growth_factor = if_else(year <= 2025, 1, (1 + import_macro_multiplier)),
      import_kt = case_when(
        year > 2025  ~ import_2025_anchor * cumprod(import_growth_factor),
        TRUE         ~ import_kt
      ),
      apparent_domestic_supply_kt = prod_kt + import_kt - export_kt
    ) %>%
    select(-contains("anchor"), -contains("growth_factor"))
  
  return(df3) # Explicitly return the dataframe to the pipeline
}

# Block D,E,F
calculate_industrial_demand <- function(supply_df, cement_base, glass_projects, cfg) {
  supply_df %>%
    # D. Cement Demand mapping
    left_join(cement_base, by = "year") %>%
    mutate(
      cement_mt        = case_when(!is.na(cement_prod_mt) ~ cement_prod_mt, TRUE ~ cfg$cement_forecast_cap),
      demand_cement_kt = cement_mt * cfg$cement_silica_ratio * 1000
    ) %>%
    select(-cement_prod_mt) %>%
    
    # E. Other Industries Matrix
    mutate(demand_other_kt = cfg$esdm_other_demand_2022 * cfg$other_demand_capacity_util) %>%
    
    # F. Glass Infrastructure Projects Mapping
    left_join(
      glass_projects %>% 
        mutate(cum_additions = cumsum(capacity_add_kt)) %>% 
        select(year_commissioned, cum_additions),
      by = join_by(year == year_commissioned)
    ) %>%
    tidyr::fill(cum_additions, .direction = "down") %>%
    mutate(cum_additions = tidyr::replace_na(cum_additions, 0)) %>%
    mutate(
      base_capacity   = if_else(year <= 2026, cfg$esdm_glass_base_capacity,
                                cfg$esdm_glass_base_capacity * (1.02) ^ (year - 2026)),
      total_capacity  = base_capacity + cum_additions,
      util_rate       = case_when(
        year <= 2023 ~ 0.70,
        year == 2024 ~ 0.75,
        year == 2025 ~ 0.79,
        TRUE         ~ 0.85
      ),
      demand_glass_kt = total_capacity * util_rate * cfg$glass_intensity_factor
    ) %>%
    select(-cum_additions, -base_capacity, -total_capacity, -util_rate)
}


# Block G : Final outputs
evaluate_balances <- function(demand_df) {
  demand_df %>%
    mutate(
      total_industrial_demand_kt = demand_cement_kt + demand_glass_kt + demand_other_kt,
      domestic_balance_kt        = apparent_domestic_supply_kt - total_industrial_demand_kt,
      domestic_market_status     = case_when(
        domestic_balance_kt > 250  ~ "Market Glut",
        domestic_balance_kt < -250 ~ "Supply Deficit",
        TRUE                       ~ "Balanced"
      )
    )
}

run_market_balance_pipeline <- function(silica_prod_df, trade_df, imf_df, cement_df, glass_df, cfg, scenario) {
  
  # Step 1: Clean and restructure raw trade vectors
  trade_clean <- prep_trade_data(trade_df)
  
  # Step 2: Build baseline supply models & run macro compounding
  supply_framework <- calculate_supply_framework(silica_prod_df, trade_clean, imf_df, cfg, scenario = scenario)
  
  # Step 3: Run down-stream end-use industry equations
  demand_matrix <- calculate_industrial_demand(supply_framework, cement_df, glass_df, cfg)
  
  # Step 4: Finalize diagnostic output matrices
  final_balance <- evaluate_balances(demand_matrix)
  
  return(final_balance)
}

# Scenario 1 : keep BPS data as is and forecast mining growth to about 2.5% per annum
cfg <- list(
  mining_organic_growth    = 0.025,
  cement_forecast_cap      = 67.8,
  cement_silica_ratio      = 0.0254172,
  glass_intensity_factor   = 0.372449,
  esdm_other_demand_2022   = 1523.93,
  esdm_glass_base_capacity = 2158.0,
  other_demand_capacity_util = 0.70
)

market_balance_1 <- run_market_balance_pipeline(
  silica_prod_df = silica_prod_bps,
  trade_df       = inter_trade_data,
  imf_df         = imf_growth_factors,
  cement_df      = cement_kemenperin_base,
  glass_df       = glass_projects,
  cfg            = cfg,
  scenario       = TRUE
)
  
# Scenario 2: adjust BPS data by 2023 value with mining growth around 10% per annum
cfg <- list(
  mining_organic_growth    = 0.025,
  cement_forecast_cap      = 67.8,
  cement_silica_ratio      = 0.0254172,
  glass_intensity_factor   = 0.70,
  esdm_other_demand_2022   = 1523.93,
  esdm_glass_base_capacity = 2158.0,
  other_demand_capacity_util = 0.70
)

market_balance_2 <- run_market_balance_pipeline(
  silica_prod_df = silica_prod_bps,
  trade_df       = inter_trade_data,
  imf_df         = imf_growth_factors,
  cement_df      = cement_kemenperin_base,
  glass_df       = glass_projects,
  cfg            = cfg,
  scenario       = FALSE
)

market_bal_1 <- market_balance_1 %>% 
  select(year, domestic_balance_kt) %>% 
  filter(year >= 2022) %>% 
  pivot_longer(cols = domestic_balance_kt,
               names_to = "var",
               values_to = "values") %>% 
  ggplot(aes(x = year, y = values, fill = var)) +
  geom_bar(stat = "identity")

market_bal_2 <- market_balance_2 %>% 
  select(year, domestic_balance_kt) %>% 
  filter(year >= 2022) %>% 
  pivot_longer(cols = domestic_balance_kt,
               names_to = "var",
               values_to = "values") %>% 
  ggplot(aes(x = year, y = values, fill = var)) +
  geom_bar(stat = "identity")

# silica sand trade balance
p2 <- market_balance %>% 
  mutate(trade_balance = export_kt - import_kt) %>% 
  pivot_longer(cols = production:domestic_market_status,
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
reserve_data_final <- reserve_data_final %>%
  mutate(
    var = recode(var,
                 "jumlah lokasi"                = "number_of_locations",
                 "hipotetik (ton)"              = "hypothetical_resources_ton",
                 "sumber daya: tereka (ton)"    = "inferred_resources_ton",
                 "sumber daya: tertunjuk (ton)" = "indicated_resources_ton",
                 "sumber daya: terukur (ton)"   = "measured_resources_ton",
                 "cadangan: terkira (ton)"      = "probable_reserves_ton",
                 "cadangan: terbukti (ton)"     = "proven_reserves_ton"
    ),
    provinsi = recode(provinsi,
                      "Jawa Barat"                = "West Java",
                      "Jawa Tengah"               = "Central Java",
                      "Jawa Timur"                = "East Java",
                      "Kalimantan Selatan"        = "South Kalimantan",
                      "Kalimantan Tengah"         = "Central Kalimantan",
                      "Kalimantan Barat"          = "West Kalimantan",
                      "Kalimantan Timur"          = "East Kalimantan",
                      "Kalimantan Utara"          = "North Kalimantan",
                      "Kepulauan Bangka Belitung" = "Bangka Belitung Islands",
                      "Kepulauan Riau"            = "Riau Islands",
                      "Nusa Tenggara Barat"       = "West Nusa Tenggara",
                      "Nusa Tenggara Timur"       = "East Nusa Tenggara",
                      "Papua Barat"               = "West Papua",
                      "Sulawesi Selatan"          = "South Sulawesi",
                      "Sulawesi Tengah"           = "Central Sulawesi",
                      "Sulawesi Tenggara"         = "Southeast Sulawesi",
                      "Sumatera Barat"            = "West Sumatra",
                      "Sumatera Selatan"          = "South Sumatra",
                      "Sumatera Utara"            = "North Sumatra"
                      # Aceh, Banten, Lampung, Riau, TOTAL unchanged
    )
  )

# total reserve and mine location
p5 <- reserve_data_final %>%
  filter(provinsi %in% c("TOTAL"), var %in% c("number_of_locations", "proven_reserves_ton")) %>%
  pivot_longer(cols = 3:7, names_to = "year", values_to = "values") %>%
  arrange(year, var) %>%
  mutate(
    values = if_else(var == "proven_reserves_ton", values / 1e6, values),
    var = factor(var, 
                 levels = c("proven_reserves_ton", "number_of_locations"),
                 labels = c("Proven Reserves (Million Tons)", "Number of Locations"))
  ) %>%
  ggplot(aes(x = year, y = values, fill = var)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ var, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# reserve location by province (table)
p7 <- reserve_data_final %>% 
  filter()
  
# Trade position and direction (value and volume)
# Monthly exports 2022 to 2025-01 volumne and price (fix 202404 and 202501 this is tricky becuase fobvalue = 0.25 and net wgt 0.462)
# Export prices
p6 <- inter_trade_data_month %>% 
  filter(cmd_code == "250510", period >= "202502") %>% 
  mutate(net_wgt = if_else(period == "202403" & flow_code == "X", net_wgt * 1000, net_wgt)) %>% 
  group_by(flow_desc) %>% 
  mutate(
    unit_price = primary_value / (net_wgt/1000)) %>% 
  filter(flow_code == "X") %>% 
  ungroup() %>% 
  ggplot(aes(x = period, y = unit_price, group = cmd_desc)) +
  geom_line()

# Export weight and prices by port





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

