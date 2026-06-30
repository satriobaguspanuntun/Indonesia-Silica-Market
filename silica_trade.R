# Trade data for silica sand, silica metal, glass production

library(comtradr)
library(tidyverse)
library(openxlsx)

# set comtrade api key
set_primary_comtrade_key()

# Download UNcoMTRADE data
# it will be function 
# can be call for one country or multiple countries
# apply Lall technology clasification
# calculate primary and secondary trade

# check year date function
year_check <- function(x) {
  grepl("^\\d{4}$", x)
}
# check year month combination function
year_month_check <- function(x) {
  grepl("^\\d{4}-\\d{2}$", x)
}

# Refactored unified trade data pulling function
pull_trade <- function(reporter, 
                       partner = "everything", 
                       direction, 
                       commod_code, 
                       freq = "A", 
                       start, 
                       end,
                       chunk_by_year = TRUE) {
  
  # Sense check
  if ("everything" %in% reporter && "everthing" %in% partner) {
    warning("Using all_countries for both reporter and partner may hit the 100K row limit")
  }
  
  # Validate and create date range based on frequency
  range <- create_date_range(freq, start, end, chunk_by_year)
  
  # Pull data for all reporters
  output_goods_list <- list()
  
  for (i in reporter) {
    country_data <- list()
    cli::cli_h1(paste0("Downloading ", tolower(freq_name(freq)), " data for ", i))
    
    for (j in names(range)) {
      goods_data <- tryCatch({
        cli::cli_bullets(paste0("Pulling data for: ", j))
        
        # Get start and end dates for this iteration
        dates <- range[[j]]
        
        data <- ct_get_data(
          type = "goods",
          frequency = freq,
          commodity_classification = "HS",
          commodity_code = commod_code,
          flow_direction = direction,
          reporter = i,
          partner = partner,
          start_date = dates$start,
          end_date = dates$end
        )
        
        # Process the data
        process_trade_data(data, i, j)
        
      }, error = function(e) {
        message("Error for country: ", i, ", period: ", j, ": ", e$message)
        create_empty_row(i, j)
      })
      
      country_data[[j]] <- goods_data
      Sys.sleep(0.5)  # Avoid API rate limit issues
    }
    
    # Combine all data for this reporter
    if (length(country_data) > 0) {
      output_goods_list[[i]] <- do.call(rbind, country_data)
    }
  }
  
  # Combine all reporters
  goods_output <- as.data.frame(do.call(rbind, output_goods_list))
  rownames(goods_output) <- 1:nrow(goods_output)
  
  return(list(goods = goods_output))
}


# Helper function to create date ranges
create_date_range <- function(freq, start, end, chunk_by_year = TRUE) {
  
  if (freq == "A") {
    # Annual frequency
    if (!year_check(start) || !year_check(end)) {
      stop("For annual data, please use year format 'YYYY'")
    }
    
    years <- seq.Date(
      from = as.Date(paste(start, "01", "01", sep = "-")),
      to = as.Date(paste(end, "01", "01", sep = "-")),
      by = "1 year"
    )
    
    years <- substr(as.character(years), 1, 4)
    
    # Return as list with start/end dates
    range_list <- lapply(years, function(y) {
      list(start = y, end = y)
    })
    names(range_list) <- years
    
  } else if (freq == "M") {
    # Monthly frequency
    if (chunk_by_year && year_check(start) && year_check(end)) {
      # Pull monthly data in yearly chunks (more efficient)
      years <- seq.Date(
        from = as.Date(paste(start, "01", "01", sep = "-")),
        to = as.Date(paste(end, "01", "01", sep = "-")),
        by = "1 year"
      )
      years <- substr(as.character(years), 1, 4)
      
      range_list <- lapply(years, function(y) {
        list(start = paste0(y, "-01"), end = paste0(y, "-12"))
      })
      names(range_list) <- years
      
    } else if (year_month_check(start) && year_month_check(end)) {
      # Pull monthly data month by month
      months <- seq.Date(
        from = as.Date(paste(start, "01", sep = "-")),
        to = as.Date(paste(end, "01", sep = "-")),
        by = "1 month"
      )
      months <- substr(as.character(months), 1, 7)
      
      range_list <- lapply(months, function(m) {
        list(start = m, end = m)
      })
      names(range_list) <- months
      
    } else {
      stop("For monthly data, please use year format 'YYYY' or year-month format 'YYYY-MM'")
    }
    
  } else {
    stop("Frequency must be 'A' (Annual) or 'M' (Monthly)")
  }
  
  return(range_list)
}


# Helper function to process trade data
process_trade_data <- function(data, reporter_iso, period) {
  
  
  # Process valid data
  processed <- data %>% 
    select(
      freq_code, 
      ref_period_id,
      ref_year, 
      ref_month,
      period,
      reporter_iso, 
      reporter_desc, 
      flow_code, 
      flow_desc,
      partner_iso, 
      partner2desc, 
      classification_code,
      cmd_code, 
      cmd_desc, 
      aggr_level,
      customs_code,
      customs_desc,
      cifvalue,
      fobvalue,
      primary_value,
      net_wgt,
      qty_unit_abbr,
      alt_qty_unit_abbr
    ) %>% 
    mutate(
      # Fix Taiwan ISO code
      partner_iso = if_else(partner_iso == "S19", "TWN", partner_iso)
    )
  
  return(processed)
}


# Helper function to create empty row
create_empty_row <- function(reporter_iso, period) {
  data.frame(
    freq_code = NA, 
    ref_period_id = NA,
    ref_year = NA, 
    ref_month = NA,
    period = period,
    reporter_iso = reporter_iso, 
    reporter_desc = NA, 
    flow_code = NA, 
    flow_desc = NA,
    partner_iso = NA, 
    partner2desc = NA, 
    classification_code = NA,
    cmd_code = NA, 
    cmd_desc = NA, 
    aggr_level = NA,
    customs_code = NA,
    customs_desc = NA,
    cifvalue = NA,
    fobvalue = NA,
    primary_value = NA,
    net_wgt = NA,
    qty_unit_abbr = NA,
    alt_qty_unit_abbr = NA
  )
}


# Helper function for frequency name
freq_name <- function(freq) {
  switch(freq,
         "A" = "annual",
         "M" = "monthly",
         "unknown")
}


# Wrapper functions for backward compatibility (optional)
pull_monthly_trade <- function(reporter, 
                               partner, 
                               direction, 
                               commod_code, 
                               start, 
                               end) {
  result <- pull_trade(
    reporter = reporter,
    partner = partner,
    direction = direction,
    commod_code = commod_code,
    freq = "M",
    start = start,
    end = end,
    chunk_by_year = TRUE
  )
  
  # Return just the data frame for backward compatibility
  return(result$goods)
}

# function to wrap trade data
trade_data_wrap <- function(country_vector, start, end) {
  
  data <- pull_trade(
    reporter = country_vector,
    partner = "World",
    direction = c("export", "import"),
    commod_code = "everything",
    freq = "A",
    start = start,
    end = end
  ) %>% 
    bind_rows() 
  
  return(trade_data)
}

inter_trade_data <- pull_trade(
  reporter = c("IDN"),
  partner = "World",
  direction = c("export", "import"),
  commod_code = c("250510", "250590", "7003", "7004", "7005", "7007", "7010", "7019"),
  freq = "A",
  start = "2011",
  end = "2025"
) %>% 
  bind_rows() 

inter_trade_data_month <- pull_trade(
  reporter = c("IDN"),
  partner = "World",
  direction = c("export", "import"),
  commod_code = c("250510", "250590", "7003", "7004", "7005", "7007", "7010", "7019"),
  freq = "M",
  start = "2021",
  end = "2026"
) %>% 
  bind_rows() 

# trade by port
# trade value
trade_port_val <- read.xlsx(xlsxFile = "~/Indonesia-Silica-Market/data/trade_by_port.xlsx", sheet = 1)
trade_port_wgt <- read.xlsx(xlsxFile = "~/Indonesia-Silica-Market/data/trade_by_port.xlsx", sheet = 2)

clean_bps_trade <- function(df) {
  
  clean_data <- df[2:nrow(df), ]
  
  colnames(clean_data) <- tolower(colnames(clean_data))
  
  clean_data <- clean_data %>% 
    rename("year" = x1,
           "month" = x2,
           "country_destination" = x3) %>%
    fill(year, .direction = "down") %>% 
    fill(month, .direction = "down") %>% 
    select(-pelabuhan) %>% 
    mutate(month_num = str_extract_all(month, "[0-9]{2}"),
           year = as.character(year),
           period = ym(paste0(year,"-",month_num))) %>% 
    relocate(year, period) %>% 
    select(-month, -month_num) %>% 
    mutate(across(.cols = belitung:totals, ~ as.numeric(.)),
           across(.cols = belitung:totals, ~ replace_na(., 0))) %>% 
    pivot_longer(cols = belitung:totals,
                 names_to = "var",
                 values_to = "value")

}

clean_trade_port_wgt <- clean_bps_trade(trade_port_wgt) %>% 
  rename("weight" = value) %>% 
  mutate(weight = weight/1000)

clean_trade_port_val <- clean_bps_trade(trade_port_val)

combine_trade <- clean_trade_port_wgt %>%
  left_join(clean_trade_port_val, by = join_by(year, period, country_destination, var)) %>% 
  mutate(unit_value = value/weight) %>% 
  filter(!var %in% c("totals", "soekarno-hatta.(u)")) %>% 
  mutate(weight = case_when(period == "2024-03-01" & var == "ketapang.k..barat" ~ weight * 1000,
                            period == "2025-09-01" & var == "tanjung.perak" ~ weight * 1000, 
                            .default = weight),
         unit_value = value/weight,
         unit_value = replace_na(unit_value, 0))


