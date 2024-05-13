# Setup ####
rm(list = ls()) #clear env
library("fpp3") #load packages
library("tidyverse")
library("urca")
library("forecast")


# a) ####
#Task:
#Discuss the time series History: when do you observe a higher volatility?
#What were the most relevant events that affected the time series behaviour?

#load, select only close, convert Date to date-object, convert to tsiblle 
v_all = read.csv("data/V - All.csv") %>%  select(Date, Close) %>% 
  mutate(Date = date(Date))%>% as_tsibble(index = Date)
msft_all = read.csv("data/MSFT - All.csv") %>%  select(Date, Close) %>% 
  mutate(Date = date(Date))%>% as_tsibble(index = Date)

#plot TS
v_all %>% autoplot() + ggtitle("Visa Stock") + xlab("Date") +  ylab("Close")
msft_all %>% autoplot() + ggtitle("Microsoft Stock") + xlab("Date") +  ylab("Close")

#TODO Fit model, plot squared residuals (to visualize Volatility)
#TODO when are outliers observed, what happened around big global events. e.g y2k (dot-com-bubble), 2001(9-11), 2008-2009 (housing financial crisis), 2018 (US-CHina trade), 2022 (covid)
#NOTE could also just do subjectively based on plots, other stuff is required in later tasks

# b) ####
# Obtain the log returns and plot the time series graph of prices and log returns.
# Obtain the correlogram and descriptive statistics for prices and log returns.
# Comment on the distributional and dynamical properties of the data (mean, volatility, skewness, kurtosis, stationarity, outliers,. . .).
# Which stylized facts about financial returns you observe in the data?
  
# use 5 yeats for rest of Q1. 
#load, select only close, convert Date to date-object, convert to tsible
v_5 = read.csv("data/V - 5 Years.csv") %>%  select(Date, Close) %>% 
  mutate(Date = date(Date)) %>% as_tsibble(index = Date)

msft_5 = read.csv("data/MSFT - 5 Years.csv") %>%  select(Date, Close) %>% 
  mutate(Date = date(Date))%>% as_tsibble(index = Date)

#get log-retuns 
v_5_log_rtn = v_5 %>% 
  mutate(log_rtn = difference(log(Close))) %>% 
  filter(!is.na(log_rtn))

msft_5_log_rtn = msft_5 %>% 
  mutate(log_rtn = difference(log(Close))) %>% 
  filter(!is.na(log_rtn))

#plt price and log return
scale_logs_by = 150 #scale log returns by this factor
ggplot(v_5_log_rtn) +
  #plt prices
  geom_line(mapping = aes(x = Date, y = Close, color = "black"))+
  #plt (scaled) log returns
  geom_line(mapping = aes(x = Date, y = (log_rtn*scale_logs_by)+mean(Close), color = "orange"), alpha = .5) +
  #add second y-axis
  scale_y_continuous(
    name = "Close Price",
    sec.axis = sec_axis(~(. - mean(v_5_log_rtn$Close))/scale_logs_by, name = "Log Returns")
  ) + 
  #add legend
  scale_color_manual(values = c("black", "orange"), 
                     name = "Lines", 
                     labels = c("Close Price", "Log Returns")) + 
  #add title
  ggtitle("Close Price and Log-Returns of Visa stock")

ggplot(msft_5_log_rtn) +
  #plt prices
  geom_line(mapping = aes(x = Date, y = Close, color = "black"))+
  #plt (scaled) log returns
  geom_line(mapping = aes(x = Date, y = (log_rtn*scale_logs_by)+mean(Close), color = "orange"), alpha = .5) +
  #add second y-axis
  scale_y_continuous(
    name = "Close Price",
    sec.axis = sec_axis(~(. - mean(msft_5_log_rtn$Close))/scale_logs_by, name = "Log Returns")
  ) + 
  #add legend
  scale_color_manual(values = c("black", "orange"), 
                     name = "Lines", 
                     labels = c("Close Price", "Log Returns")) + 
  #add title
  ggtitle("Close Price and Log-Returns of Microsoft stock")


# c) ####
# Apply the Augmented Dickey-Fuller (ADF) test to the log prices. Describe
# in detail the conclusions of the test.

# The Augmented Dickey-Fuller (ADF) test is a statistical test commonly used
# to determine whether a unit root is present in a time series dataset. 
# A unit root would indicate that a time series is non-stationary.

# Function to conduct ADF test and print results
adf_test <- function(data, variable_name) {
  adf_result <- ur.df(data, type = "drift", lags = 0, selectlags = "Fixed")
  summary(adf_result)
}

# Apply ADF test to log prices of Visa stock
adf_result_visa <- adf_test(v_5$Close, "Close")
print(adf_result_visa)

# Apply ADF test to log prices of Microsoft stock
adf_result_msft <- adf_test(msft_5$Close, "Close")
print(adf_result_msft)


# d) ####
# Perform a simplified Box-Jenkins analysis to find an ARMA model (AR, MA or mixed) that best describes the log returns. Justify your choice.

# The simplified Box-Jenkins analysis is a technique used to identify and estimate ARMA models for time series data.
# This analysis involves identifying the best-fitting ARMA model, based on criteria such as the Akaike Information Criterion (AIC), and diagnostic checks of the residuals.

# Function to perform simplified Box-Jenkins analysis and find ARMA model
simplified_box_jenkins <- function(log_returns) {
  # Identify the best ARMA model using AIC criterion
  arma_model <- auto.arima(log_returns)
  
  # Print summary of the identified ARMA model
  print(summary(arma_model))
  
  # Plot the ACF and PACF of log returns
  acf_pacf_plot <- ggtsdisplay(log_returns)
  print(acf_pacf_plot)
  
  return(arma_model)
}

# Perform simplified Box-Jenkins analysis for Visa stock log returns
visa_arma_model <- simplified_box_jenkins(v_5_log_rtn$log_rtn)

# Perform simplified Box-Jenkins analysis for Microsoft stock log returns
msft_arma_model <- simplified_box_jenkins(msft_5_log_rtn$log_rtn)

# e) ####
# f) ####
# g) ####
# h) ####
# i) ####
# j) ####
# k) ####