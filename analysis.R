library(tidyverse)
library(openxlsx)

# Read IMF data
imf_weo <-read_csv("~/Indonesia-Silica-Market/data/IMF.csv")

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

# ==============================================================================
# 1. PARAMETERS & FORECAST CONFIGURATION
# ==============================================================================
cfg <- list(
  # Macro & Growth Parameters
  bps_growth_b4_2025       = 0.0479,     # 4.79% Growth from BPS Category B.4
  mining_organic_growth    = 0.025,      # 2.5% steady-state growth post-2025
  other_growth_pre_2026    = 0.015,      # Conservative 1.5% growth for other industries
  export_attrition_rate    = 0.15,      # 13.3% structural decay post-2025
  cement_forecast_cap      = 67.8,       # Post-2024 operational capacity ceiling (MT)
  
  # Downstream Industry Intensity Ratios (Audited to Ministry Data)
  cement_silica_ratio      = 0.0254172,  # Single factor: Row 2 Sand / 2022 Cement Prod
  glass_intensity_factor    = 0.372449,  # Row 1 Sand Demand / Total Glass Capacity
  
  # Audited Table 13.1 Baselines (Kilotons)
  esdm_other_demand_2022   = 1523.93,    # Sum of Rows 3, 4, 6, and 7
  esdm_glass_base_capacity = 2158.0      # Sum of Glass Industry Capacities
)

# Downstream glass industrial expansions registry
glass_projects <- tibble(
  year_commissioned = c(2024, 2026, 2028),
  capacity_add_kt   = c(838.0, 720.0, 400.0)
)

# Kemenperin Official Cement Production
cement_kemenperin_base <- tibble(
  year           = c(2022, 2023, 2024),
  cement_prod_mt = c(64.5, 66.9, 67.8)
)

# ==============================================================================
# 2. PRE-PIPELINE STATIC SCALAR ANCHOR EXTRACTION
# ==============================================================================
prod_2024_anchor   <- max(silica_prod_bps$prod_kt[silica_prod_bps$year == 2024], na.rm = TRUE)
prod_2025_imputed  <- prod_2024_anchor * (1 + cfg$bps_growth_b4_2025)

export_2025_anchor <- 3660.0   
import_2025_anchor <- 17.2     

# ==============================================================================
# 3. MAIN FORECASTING PIPELINE (FILTERED TO 2022 ONWARDS)
# ==============================================================================
market_balance <- silica_prod_bps %>% 
  filter(year >= 2022) %>% 
  mutate(reporter_iso = "IDN", cmd_code = "250510") %>% 
  
  # A. Ingestion and trade balance standardization
  full_join(inter_trade_data %>%
              select(ref_year, reporter_iso, flow_desc, cmd_code, primary_value, net_wgt) %>% 
              filter(cmd_code == "250510", flow_desc == "Export", reporter_iso == "IDN"), 
            by = join_by(year == ref_year, cmd_code, reporter_iso)) %>% 
  rename("export" = primary_value, "net_wgt_exp" = net_wgt) %>% 
  select(-flow_desc) %>% 
  
  left_join(inter_trade_data %>%
              select(ref_year, reporter_iso, flow_desc, cmd_code, primary_value, net_wgt) %>% 
              filter(cmd_code == "250510", flow_desc == "Import", reporter_iso == "IDN"), 
            by = join_by(year == ref_year, cmd_code, reporter_iso)) %>% 
  rename("import" = primary_value, "net_wgt_imp" = net_wgt) %>% 
  select(-flow_desc) %>% 
  
  mutate(across(c(production, prod_kt, export, net_wgt_exp, import, net_wgt_imp), ~ as.numeric(replace_na(., 0)))) %>% 
  mutate(
    export_kt = round(net_wgt_exp / 1e6, 3),
    import_kt = round(net_wgt_imp / 1e6, 3)
  ) %>%
  select(-net_wgt_exp, -net_wgt_imp) %>%
  
  # B. Continuous Forecasting Horizon Extension (Locked to 2022-2031)
  complete(year = 2022:2031, fill = list(reporter_iso = "IDN", cmd_code = "250510")) %>%
  mutate(type = if_else(year <= 2025, "Historical", "Forecast")) %>%
  arrange(year) %>%
  
  # C. Clean Supply Framework 
  mutate(
    prod_kt = case_when(
      year <= 2024 ~ prod_kt,
      year == 2025 ~ prod_2025_imputed,
      year > 2025  ~ prod_2025_imputed * (1 + cfg$mining_organic_growth)^(year - 2025)
    ),
    import_kt = case_when(
      year > 2025  ~ import_2025_anchor,
      TRUE         ~ import_kt
    ),
    export_kt = case_when(
      year > 2025  ~ export_2025_anchor * (1 - cfg$export_attrition_rate)^(year - 2025),
      TRUE         ~ export_kt
    ),
    apparent_domestic_supply_kt = prod_kt + import_kt - export_kt
  ) %>%
  
  # D. Relational Cement Demand Mapping
  left_join(cement_kemenperin_base, by = "year") %>% 
  mutate(
    cement_mt = case_when(
      !is.na(cement_prod_mt) ~ cement_prod_mt, 
      TRUE                   ~ cfg$cement_forecast_cap 
    ),
    demand_cement_kt = cement_mt * cfg$cement_silica_ratio * 1000
  ) %>% 
  select(-cement_prod_mt) %>% 
  
  # E. Other Industries Matrix
  mutate(
    demand_other_kt = if_else(
      year <= 2026, 
      cfg$esdm_other_demand_2022 * (1 + cfg$other_growth_pre_2026)^(year - 2022),
      cfg$esdm_other_demand_2022 * (1 + cfg$other_growth_pre_2026)^(2026 - 2022) * (1 + cfg$mining_organic_growth)^(year - 2026)
    )
  ) %>%
  
  # F. Relational Glass Capacity Mapping (Fixed 2023 logic defect)
  left_join(glass_projects %>% 
              mutate(cum_additions = cumsum(capacity_add_kt)) %>% 
              select(year_commissioned, cum_additions), 
            by = join_by(year == year_commissioned)) %>% 
  fill(cum_additions, .direction = "down") %>% 
  mutate(cum_additions = replace_na(cum_additions, 0)) %>% 
  
  mutate(
    base_capacity = if_else(year <= 2026, cfg$esdm_glass_base_capacity, cfg$esdm_glass_base_capacity * (1.02)^(year - 2026)),
    total_capacity = base_capacity + cum_additions,
    util_rate = case_when(
      year <= 2023 ~ 1.00,  
      year == 2024 ~ 0.75,  
      year == 2025 ~ 0.79, 
      year == 2026 ~ 0.82, 
      TRUE         ~ 0.85
    ),
    demand_glass_kt = total_capacity * util_rate * cfg$glass_intensity_factor
  ) %>% 
  select(-cum_additions, -base_capacity, -total_capacity, -util_rate) %>%
  
  # G. Structural Balance and Market Status Outputs
  mutate(
    total_industrial_demand_kt = demand_cement_kt + demand_glass_kt + demand_other_kt,
    structural_balance_kt = apparent_domestic_supply_kt - total_industrial_demand_kt,
    market_status = case_when(
      structural_balance_kt > 250  ~ "Market Glut / Inventory Build",
      structural_balance_kt < -250 ~ "Supply Deficit (Alert)",
      TRUE                         ~ "Balanced / Tight Market"
    ),
    export_share_pct = (export_kt / prod_kt) * 100
  ) %>%
  
  # H. Final Safeguard Filter
  filter(year >= 2022)


p4 <- market_balance %>% 
  relocate(year, type, reporter_iso, cmd_code, market_status) %>% 
  pivot_longer(cols = production:export_share_pct,
               names_to = "var",
               values_to = "values") %>% 
  filter(var %in% c("structural_balance_kt")) %>% 
  ggplot(aes(year, values, fill = var)) +
  geom_bar(stat = "identity")


# ==============================================================================
# SILICA SAND MARKET BALANCE FORECAST
# ==============================================================================

# ==============================================================================
# 1. PARAMETERS & FORECAST CONFIGURATION
# ==============================================================================
cfg <- list(
  mining_organic_growth    = 0.100,
  cement_forecast_cap      = 67.8,
  cement_silica_ratio      = 0.0254172,
  glass_intensity_factor   = 0.372449,
  esdm_other_demand_2022   = 1523.93,
  esdm_glass_base_capacity = 2158.0,
  other_demand_capacity_util = 0.60
)

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

# ==============================================================================
# 2. MAIN FORECASTING PIPELINE WITH IMF MACRO DRIVERS
# ==============================================================================
market_balance <- silica_prod_bps %>%
  filter(year >= 2022) %>%
  mutate(reporter_iso = "IDN", cmd_code = "250510") %>%
  
  # A. Ingestion and trade balance standardization
  full_join(inter_trade_data %>%
              select(ref_year, reporter_iso, flow_desc, cmd_code, primary_value, net_wgt) %>%
              filter(cmd_code == "250510", flow_desc == "Export", reporter_iso == "IDN"),
            by = join_by(year == ref_year, cmd_code, reporter_iso)) %>%
  rename("export" = primary_value, "net_wgt_exp" = net_wgt) %>%
  select(-flow_desc) %>%
  
  left_join(inter_trade_data %>%
              select(ref_year, reporter_iso, flow_desc, cmd_code, primary_value, net_wgt) %>%
              filter(cmd_code == "250510", flow_desc == "Import", reporter_iso == "IDN"),
            by = join_by(year == ref_year, cmd_code, reporter_iso)) %>%
  rename("import" = primary_value, "net_wgt_imp" = net_wgt) %>%
  select(-flow_desc) %>%
  
  mutate(across(c(production, prod_kt, export, net_wgt_exp, import, net_wgt_imp),
                ~ as.numeric(replace_na(., 0)))) %>%
  mutate(
    export_kt = round(net_wgt_exp / 1e6, 3),
    import_kt = round(net_wgt_imp / 1e6, 3)
  ) %>%
  select(-net_wgt_exp, -net_wgt_imp) %>%
  
  # B. Continuous Forecasting Horizon Extension (2022-2031)
  complete(year = 2022:2031, fill = list(reporter_iso = "IDN", cmd_code = "250510")) %>%
  mutate(type = if_else(year <= 2025, "Historical", "Forecast")) %>%
  arrange(year) %>%
  
  # Inject IMF Macro Forecasts
  left_join(imf_growth_factors, by = "year") %>%
  
  # C. Supply Framework (Clean 8% Compounding from 2023 Anchor)
  mutate(
    prod_2023_anchor   = max(prod_kt[year == 2023], na.rm = TRUE),
    export_2025_anchor = max(export_kt[year == 2025], na.rm = TRUE),
    import_2025_anchor = max(import_kt[year == 2025], na.rm = TRUE)
  ) %>%
  arrange(year) %>%
  mutate(
    prod_growth_factor = if_else(year <= 2023, 1, 1 + cfg$mining_organic_growth),
    prod_kt = case_when(
      year <= 2023 ~ prod_kt,
      TRUE         ~ prod_2023_anchor * cumprod(prod_growth_factor)
    ),
    export_growth_factor = if_else(year <= 2025, 1, (1 + export_macro_multiplier)),
    export_kt = case_when(
      year > 2025  ~ export_2025_anchor * cumprod(export_growth_factor),
      TRUE         ~ export_kt
    ),
    import_growth_factor = if_else(year <= 2025, 1, (1 + import_macro_multiplier)),
    import_kt = case_when(
      year > 2025  ~ import_2025_anchor * cumprod(import_growth_factor),
      TRUE         ~ import_kt
    ),
    apparent_domestic_supply_kt = prod_kt + import_kt - export_kt
  ) %>%
  select(-prod_2023_anchor, -export_2025_anchor, -import_2025_anchor,
         -prod_growth_factor, -export_growth_factor, -import_growth_factor) %>%
  
  # D. Cement Demand Mapping (Constant post-2024 capacity ceiling)
  left_join(cement_kemenperin_base, by = "year") %>%
  mutate(
    cement_mt        = case_when(!is.na(cement_prod_mt) ~ cement_prod_mt, TRUE ~ cfg$cement_forecast_cap),
    demand_cement_kt = cement_mt * cfg$cement_silica_ratio * 1000
  ) %>%
  select(-cement_prod_mt) %>%
  
  # E. Other Industries Matrix (Assumed constant across horizon)
  mutate(demand_other_kt = cfg$esdm_other_demand_2022 * cfg$other_demand_capacity_util) %>%
  
  # F. Glass Mapping (2023 stability fix & structural project steps)
  left_join(glass_projects %>%
              mutate(cum_additions = cumsum(capacity_add_kt)) %>%
              select(year_commissioned, cum_additions),
            by = join_by(year == year_commissioned)) %>%
  fill(cum_additions, .direction = "down") %>%
  mutate(cum_additions = replace_na(cum_additions, 0)) %>%
  mutate(
    base_capacity   = if_else(year <= 2026, cfg$esdm_glass_base_capacity,
                              cfg$esdm_glass_base_capacity * (1.02) ^ (year - 2026)),
    total_capacity  = base_capacity + cum_additions,
    util_rate       = case_when(
      year <= 2023 ~ 1.00,
      year == 2024 ~ 0.75,
      year == 2025 ~ 0.79,
      TRUE         ~ 0.85
    ),
    demand_glass_kt = total_capacity * util_rate * cfg$glass_intensity_factor
  ) %>%
  select(-cum_additions, -base_capacity, -total_capacity, -util_rate) %>%
  
  # G. Final Outputs & Dual Balance Evaluation
  # ----------------------------------------------------------------------------
# Two balance measures are computed in parallel:
#
# (1) domestic_balance_kt — the ORIGINAL formula, retained as-is.
#     Measures domestically-available supply vs domestic industrial demand.
#     Relevant for domestic market adequacy / import dependency analysis.
#     Will correctly show tightness when exports divert supply away from
#     domestic industry.
#       = (prod + imp - exp) - domestic_industrial_demand

# ----------------------------------------------------------------------------
mutate(
  total_industrial_demand_kt = demand_cement_kt + demand_glass_kt + demand_other_kt,
  
  # (1) Domestic balance (original formula, unchanged)
  domestic_balance_kt    = apparent_domestic_supply_kt - total_industrial_demand_kt,
  domestic_market_status = case_when(
    domestic_balance_kt > 250  ~ "Market Glut",
    domestic_balance_kt < -250 ~ "Supply Deficit",
    TRUE                       ~ "Balanced"
  )
) %>%
  filter(year >= 2022)

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

# Block B
calculate_supply_framework <- function(prod_df, trade_clean_df, imf_growth, cfg, start_yr = 2022, end_yr = 2031) {
  prod_df %>%
    filter(year >= start_yr) %>%
    mutate(reporter_iso = "IDN", cmd_code = "250510") %>%
    
    # Merge Cleaned Trade Data
    full_join(trade_clean_df, by = join_by(year, cmd_code, reporter_iso)) %>%
    mutate(across(c(production, prod_kt, export_kt, import_kt), ~ as.numeric(tidyr::replace_na(., 0)))) %>%
    
    # Extend Horizon & Inject IMF Macros
    tidyr::complete(year = start_yr:end_yr, fill = list(reporter_iso = "IDN", cmd_code = "250510")) %>%
    mutate(type = if_else(year <= 2025, "Historical", "Forecast")) %>%
    arrange(year) %>%
    left_join(imf_growth, by = "year") %>%
    
    # Setup Anchors and Compound Future Projections
    mutate(
      prod_2023_anchor   = max(prod_kt[year == 2023], na.rm = TRUE),
      export_2025_anchor = max(export_kt[year == 2025], na.rm = TRUE),
      import_2025_anchor = max(import_kt[year == 2025], na.rm = TRUE)
    ) %>%
    arrange(year) %>%
    mutate(
      prod_growth_factor = if_else(year <= 2023, 1, 1 + cfg$mining_organic_growth),
      prod_kt = case_when(
        year <= 2023 ~ prod_kt,
        TRUE         ~ prod_2023_anchor * cumprod(prod_growth_factor)
      ),
      export_growth_factor = if_else(year <= 2025, 1, (1 + export_macro_multiplier)),
      export_kt = case_when(
        year > 2025  ~ export_2025_anchor * cumprod(export_growth_factor),
        TRUE         ~ export_kt
      ),
      import_growth_factor = if_else(year <= 2025, 1, (1 + import_macro_multiplier)),
      import_kt = case_when(
        year > 2025  ~ import_2025_anchor * cumprod(import_growth_factor),
        TRUE         ~ import_kt
      ),
      apparent_domestic_supply_kt = prod_kt + import_kt - export_kt
    ) %>%
    select(-contains("anchor"), -contains("growth_factor"))
}

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

# reserve location by province

  
# Trade position and direction (value and volume)
# Monthly exports 2022 to 2025-01 volumne and price (fix 202404 and 202501 this is tricky becuase fobvalue = 0.25 and net wgt 0.462)
# Export prices
  p6 <- inter_trade_data_month %>% 
    filter(cmd_code == "250510", period >= "202502") %>% 
    mutate(net_wgt = case_when(period == "202403" & flow_code == "X" ~ net_wgt * 1000))
  group_by(flow_desc) %>% 
    mutate(
      unit_price = primary_value / (net_wgt/1000)) %>% 
    filter(flow_code == "X") %>% 
    ungroup() %>% 
    ggplot(aes(x = period, y = unit_price, group = cmd_desc)) +
    geom_line()

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

