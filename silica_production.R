library(tidyverse)
library(readr)
library(pdftools)
library(openxlsx)

# BPS Data
# silica sand production volume
silica_prod_bps <- read.csv("data/Query Builder Result - Senin, 22 Juni 2026 pukul 14.13.41 WIB.csv")
silica_prod_bps <- silica_prod_bps[2:19,]
colnames(silica_prod_bps) <- c("type", as.character(seq(2011, 2015, by = 1)), as.character(seq(2017, 2024, by = 1))) 

silica_prod_bps <- silica_prod_bps %>%
  pivot_longer(cols = 2:14,
               names_to = "year",
               values_to = "production") %>% 
  filter(type == "Pasir Kwarsa") %>% 
  mutate(year = as.numeric(year)) %>% 
  complete(type, year = 2011:2024) %>% 
  group_by(type) %>% 
  mutate(production = zoo::na.approx(production)) %>% 
  ungroup() %>% 
  mutate(prod_kt = (production * 1.6)/1000)

# ESDM statistical yearbook reserve estimate 2020-2024

path <- "~/Indonesia-Silica-Market/data/demand_silica.xlsx"

reserve_reader <- function(path) {
  
  data_list <- list()
  
  sheets <- getSheetNames(path)
  
  for (sheet in 1:length(sheets)) {
    
    if (sheet < 4) {
      
      df <- openxlsx::read.xlsx(path, sheet = sheet)
      data_list[[sheet]] <- df
      
    } else {
      
      df <- openxlsx::read.xlsx(path, sheet = sheet)
      colnames(df) <- str_replace_all(tolower(colnames(df)),pattern = "\\.+", replacement = " ")
      df <- df %>% 
        mutate(across(4:ncol(.), ~ str_replace_all(., "\\.+", ""))) %>% 
        mutate(across(4:ncol(.), ~ str_replace_all(., "\\,", "."))) %>% 
        mutate(across(4:ncol(.), ~ str_replace_all(., "\\-", "0"))) %>% 
        mutate(across(4:ncol(.), ~ as.numeric(.))) %>% 
        mutate(year = as.numeric(sheets[sheet])) %>% 
        relocate(year)
      
      data_list[[sheet]] <- df
      
    }
    
  }
  
  
  names(data_list) <- sheets
  
  return(data_list)
  
}

reserve_data <- reserve_reader(path)

reserve_data_final <- reserve_data[4:8] %>% 
  bind_rows() %>% 
  pivot_longer(cols = 4:10,
               names_to = "var",
               values_to = "values") %>% 
  select(-`no `) %>% 
  pivot_wider(names_from = "year", values_from = "values")



