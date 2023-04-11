          #### Crude Oil Price Forecast ####

### Import ####

## Packages ####
library(zoo)
library(lubridate)
library(dplyr)
library(readxl)
library(magrittr)
library(data.table)
library(reshape2)
library(readr)

setwd("C:/Users/PC622R/OneDrive - 中華經濟研究院/Price Forecasting Model/Data")

## [Dependent Variable] ####
# Crude Oil Price ####

crude_oil_price_WTI <- read.zoo("DCOILWTICO.csv", 
                               read = read.csv, na.strings = ".")

comment(crude_oil_price_WTI) <- "WTI Crude Oil Price"

# crude_oil_price_WTI - read.csv(DCOILWTICO.csv, na.strings = .,
#                                 col.names = c(date, crude_oil_price_WTI),
#                                 colClasses = c(Date, numeric)) %% 
#   with(zoo(x = crude_oil_price_WTI, order.by = date))

## [Supply] ####
# U.S. Crude Oil Production ####

crude_oil_production_US <- read_xls("MCRFPUS1m.xls", sheet = "Data 1", skip = 3,
                                   col_names = c("date", "crude_oil_production_US")) %>% 
  mutate(date = floor_date(as.Date(date), unit = "month")) %>% 
  with(zoo(x = crude_oil_production_US, order.by = date))

comment(crude_oil_production_US) <- "U.S. Crude Oil Production"

# OPEC 

# Capacity Utilization Rate ####

utility_rate <- read_xls("MOPUEUS2m.xls", sheet = "Data 1", skip = 3,
                        col_names = c("date", "utility_rate")) %>% 
  mutate(date = floor_date(as.Date(date), unit = "month")) %>% 
  with(zoo(x = utility_rate, order.by = date))

comment(utility_rate) <- "Capacity Utility Rate"

# Weekly U.S. Ending Stocks excluding SPR of Crude Oil ####

stocks_US <- read_xls("WCESTUS1w.xls", sheet = "Data 1", 
                      skip = 3, col_names = c("date", "stocks_US")) %>% 
  mutate(date = as.Date(date)) %>% 
  with(zoo(x = stocks_US, order.by = date))

comment(stocks_US) <- "U.S. Crude Oil Stock"

## [Demand] ####
# Product Supplied of Petroleum ####

petroleum <- read_xls("MTPUPUS1m.xls", sheet = "Data 1", skip = 3,
                      col_names = c("date", "petroleum")) %>% 
  mutate(date = floor_date(as.Date(date), unit = "month")) %>% 
  with(zoo(x = petroleum, order.by = date))

comment(petroleum) <- "Petroleum Price"

# Kilian Global Economic Index ####

kilian_index <- read_xlsx("igrea.xlsx", skip = 1, 
                         col_names = c("date", "kilian_index")) %>% 
  mutate(date = as.Date(date)) %>% 
  with(zoo(x = kilian_index, order.by = date))

comment(kilian_index) <- "Kilian Global Economic Index"

# US CPI: Seasonally Adjusted ####

cpi_US <- read.csv("file.csv") %>% 
  mutate(date = ym(Label)) %>% 
  with(zoo(x = Value, order.by = date))

comment(cpi_US) <- "U.S. CPI"

# US CPI: All items in U.S. city average, all urban consumers, not seasonally adjusted ####

cpi_US_nonseason.adj <- read_xlsx("SeriesReport-20230323222815_56de07.xlsx",
                                  skip = 11) %>% 
  select(!c(HALF1, HALF2)) %>% 
  melt(id.vars = "Year", variable.name = "month",
       value.name = "cpi_US_nonseason.adj") %>% 
  mutate(date = ym(paste(Year, month))) %>% 
  arrange(date) %>% 
  with(zoo(x = cpi_US_nonseason.adj, order.by = date))

comment(cpi_US_nonseason.adj) <- "US CPI, All items in U.S. city average, all urban consumers, not seasonally adjusted"

## [Market] ####

# S&P 500 Index ####

sp500 <- read.csv("^spx_d.csv", colClasses = c("Date", rep("numeric", 5))) %>% 
  with(zoo(x = Close, order.by = Date))

comment(sp500) <- "S&P 500 Index"

# DOW JONES UTILITY AVERAGE ####

dju <- rbind(read.csv("Dow Jones Utility Average_03_23_23-03_01_93.csv", header = FALSE,
                skip = 1, nrows = 5653,
                col.names = c("open", "close", "high", "low", "volumn", "miss1", "miss2", "date")) %>% 
  select(!c(miss1, miss2)),
  read.csv("Dow Jones Utility Average_03_23_23-03_01_93.csv", header = FALSE,
           skip = 5654,
           col.names = c("open", "close", "high", "low", "volumn","date"))) %>% 
  mutate(date = mdy(date)) %>% 
  filter(!is.na(date)) %>% 
  with(zoo(x = close, order.by = date))

comment(dju) <- "Dow Jones Utility Average"

# CFTC Non-commercial Net Long ####

net_long <- read.csv("CFTC_crude_oil.csv") %>% 
  mutate(date = as.Date(date)) %>% 
  with(zoo(x = non.comercial_long, order.by = date))

comment(net_long) <- "CFTC Crude Oil Non-commercial Net Long"

# Gold Futures - Apr 23 (GCJ3) ####

gold <- rbind(read.csv("Gold Futures Historical Data86-05.csv"),
              read.csv("Gold Futures Historical Data05-23.csv")) %>% 
  mutate(Date = mdy(Date),
         Vol. = as.numeric(sub("K", "", Vol.))*1000,
         Price = parse_number(Price),
         Change.. = parse_number(Change..)) %>% 
  with(zoo(x = Price, order.by = Date))

comment(gold) <- "Gold Futures"

## [Monetary] ####
# Money Market Funds; Total Financial Assets, Level ####

money_funds <- read_xls("MMMFFAQ027S.xls", skip = 11, 
                        col_names = c("date", "money_funds")) %>% 
  mutate(date = as.Date(date)) %>% 
  with(zoo(x = money_funds, order.by = date))

comment(money_funds) <- "Money Market Funds, Total Financial Assets"

## [Political] ####
# Global Economic Policy Uncertainty Index ####
# without parity

gepu <- read_xlsx("Global_Policy_Uncertainty_Data.xlsx", n_max = 312) %>% 
  mutate(date = ym(paste(Year, Month))) %>% 
  with(zoo(x = GEPU_current, order.by = date))

comment(gepu) <- "Global Economic Policy Uncertainty Index"

## Merge ####

mulzoo <- merge(crude_oil_price_WTI, crude_oil_production_US, 
            utility_rate, stocks_US,  #supply
            petroleum, kilian_index, cpi_US, cpi_US_nonseason.adj, #demand
            sp500, dju, net_long, gold,   #market
            money_funds,   #monetary
            gepu  #polical
            )

list_zoo <- list(crude_oil_price_WTI, crude_oil_production_US, 
                 utility_rate, stocks_US,  #supply
                 petroleum, kilian_index, cpi_US, cpi_US_nonseason.adj, #demand
                 sp500, dju, net_long, gold,   #market
                 money_funds,   #monetary
                 gepu  #polical
)

names(list_zoo) <- c("Crude Oil Price",
                     "U.S. Crude Oil Production",
                     "Capacity Utility Rate",
                     "Weekly U.S. Ending Stocks excluding SPR of Crude Oil",
                     "Kilian Global Economic Index", 
                     "US CPI: Seasonally Adjusted",
                     "CPI for All Urban Consumers (CPI-U)" ,
                     "All items in U.S. city average, all urban consumers, not seasonally adjusted",
                     "Crude Oil Non-commercial Net Long", 
                     "S&P 500 Index",
                     "DOW JONES UTILITY AVERAGE", 
                     "Gold Futures - Apr 23 (GCJ3)",
                     "Money Market Funds; Total Financial Assets, Level",
                     "Global Economic Policy Uncertainty Index")

