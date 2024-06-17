library(fpp3)
library(tidyverse)
library(gridExtra)

# Read Visa 5-year daily data
visa_5_years <- read.csv("resources/V-5y.txt")

# Now save the data as tsibble
visa_5_years_ts <- visa_5_years %>% mutate(
  Date = ymd(Date)) %>% as_tsibble(index = Date)

# Compute simple returns and log returns
visa_5_years_ts <- visa_5_years_ts %>%
  mutate(simple_rtn = difference(Close)/lag(Close),
         log_rtn = difference(log(Close)))

# Let's plot the daily Close prices for Visa in the 5-year period
visa_5yr_prices_plot <- visa_5_years_ts %>% autoplot(
  Close) + ylab("Closing price ($USD)") + xlab("Date") + theme_bw() +
  ggtitle("Visa (ticker 'V') price, from 2019 to 2024")
ggsave(visa_5yr_prices_plot, 
       filename = "output/visa/visa_yr_price_plot.png",
       device = "png",
       height = 6, width = 6, units = "in")

# Plot simple returns
visa_5yr_simple_rtns_plot <- visa_5_years_ts %>% autoplot(
  simple_rtn) + ylab("Simple returns") + xlab("Date") + theme_bw() +
  ggtitle("Visa (ticker 'V') simple returns, from 2019 to 2024")
ggsave(visa_5yr_simple_rtns_plot, 
       filename = "output/visa/visa_5yr_simple_rtns_plot.png",
       device = "png",
       height = 6, width = 6, units = "in")

# Plot log returns
visa_5yr_log_rtns_plot <- visa_5_years_ts %>% autoplot(
  log_rtn) + ylab("Log returns") + xlab("Date") + theme_bw() +
  ggtitle("Visa (ticker 'V') log returns, from 2019 to 2024")
ggsave(visa_5yr_log_rtns_plot, 
       filename = "output/visa/visa_5yr_log_rtns_plot.png",
       device = "png",
       height = 6, width = 6, units = "in")

#-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# Plot the ACF and PACF functions for the closing prices in this period
visa_correlograms <- visa_5_years_ts %>% mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

# Maximum lag, of 1249 trading days
png(file="output/visa/visa_5yr_price_acf_lag1249.png",
    height = 6, width = 6, res=1080, units = "in")
acf(visa_correlograms$Close, lag.max=1249,
    main="ACF of Visa prices",
    xlab="Lag (#trading days)")
dev.off()
# Lag of only 60 trading days
png(file="output/visa/visa_5yr_price_acf_lag60.png",
     height = 6, width = 6, res=1080, units = "in")
acf(visa_correlograms$Close, lag.max=60,
    main="ACF of Visa prices",
    xlab="Lag (#trading days)")
dev.off()

# Plot the PACF function for the closing prices in this period
# Maximum lag, of 1249 trading days
png(file="output/visa/visa_5yr_price_pacf_lag1249.png",
    height = 6, width = 6, res=1080, units = "in")
pacf(visa_correlograms$Close, lag.max=1249,
     main="PACF of Visa prices",
     xlab="Lag (#trading days)")
dev.off()

# Lag of only 60 trading days
png(file="output/visa/visa_5yr_price_pacf_lag60.png",
    height = 6, width = 6, res=1080, units = "in")
pacf(visa_correlograms$Close, lag.max=60,
    main="PACF of Visa prices",
    xlab="Lag (#trading days)")
dev.off()

#-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# Plot the ACF and PACF functions for the log returns prices in this period
visa_correlograms <- na.omit(visa_correlograms)

# Maximum lag, of 1249 trading days
png(file="output/visa/visa_5yr_log_rtn_acf_lag1249.png",
    height = 6, width = 6, res=1080, units = "in")
acf(visa_correlograms$log_rtn, lag.max=1249,
    main="ACF of Visa log returns",
    xlab="Lag (#trading days)")
dev.off()
# Lag of only 60 trading days
png(file="output/visa/visa_5yr_log_rtn_acf_lag60.png",
    height = 6, width = 6, res=1080, units = "in")
acf(visa_correlograms$log_rtn, lag.max=60,
    main="ACF of Visa log returns",
    xlab="Lag (#trading days)")
dev.off()

# Plot the PACF function for the closing prices in this period
# Maximum lag, of 1249 trading days
png(file="output/visa/visa_5yr_log_rtn_pacf_lag1249.png",
    height = 6, width = 6, res=1080, units = "in")
pacf(visa_correlograms$log_rtn, lag.max=1249,
     main="PACF of Visa log returns",
     xlab="Lag (#trading days)")
dev.off()

# Lag of only 60 trading days
png(file="output/visa/visa_5yr_log_rtn_pacf_lag60.png",
    height = 6, width = 6, res=1080, units = "in")
pacf(visa_correlograms$log_rtn, lag.max=60,
     main="PACF of Visa log returns",
     xlab="Lag (#trading days)")
dev.off()
