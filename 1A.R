################################################################################
library(fpp3)
library(tidyverse)
library(gridExtra)
library(stlplus)

################################################################################


#Set Directory----------------------------------------------------------------------
getwd();
setwd("C:\\Users\\P.Teigão\\Desktop\\Rstudio Files\\Econometria Financeira")


# Visa yearly data-------------------------------------------------------
visa_all <- read.csv("V-all.txt")

# Now save the data as tsibble
visa_ts <- visa_all %>% 
  mutate(Date = ymd(Date)) %>% 
  as_tsibble(index = Date)

# Compute simple returns and log returns
visa_ts <- visa_ts %>%
  mutate(simple_rtn = difference(Close)/lag(Close),
         log_rtn = difference(log(Close)))
#Graphs
visa_ts %>%
  autoplot(Close) + ylab("Close Price ($USD)") + xlab("Date") + theme_bw() +
  ggtitle("Visa (ticker 'V') ,Close Price since IPO")

visa_ts %>%
  autoplot(log_rtn) + ylab("Log Return") + xlab("Date") + theme_bw() +
  ggtitle("Visa (ticker 'V') ,Log Return since IPO")


#trying to amnipulate de graph
#visa_ts %>%
  #autoplot(log_rtn) + 
  #ylab("Log Return") + 
  #xlab("Date") + 
#  theme_bw() +
 # ggtitle("Visa (ticker 'V') , Log Return since IPO") +
  #geom_rect(aes(xmin = as.Date("2008-02-01"), xmax = as.Date("2010-01-01"), ymin = -0.15, ymax = 0.15), 
   #         fill = NA, color = "blue", alpha = 0.2) +
  #scale_color_manual(name = "Volatility Cluster", values = "blue", labels = c("Blue Volatility Cluster")) +
  #guides(color = guide_legend(override.aes = list(fill = NA)))  


#STL treatment
tseriesstl1 <- ts(visa_ts$Close , frequency = 252)
y1=stl(tseriesstl1,s.window="period")
plot(y1)
residuals1=y1$time.series[,3]



# Read MSFT yearly data-------------------------------------------------------
MSFT_all <- read.csv("MSFT-all.txt")

# Now save the data as tsibble
MSFT_ts <- MSFT_all %>% 
  mutate(Date = ymd(Date)) %>% 
  as_tsibble(index = Date)

# Compute simple returns and log returns
MSFT_ts <- MSFT_ts %>%
  mutate(simple_rtn = difference(Close)/lag(Close),
         log_rtn = difference(log(Close)))
#Graphs
MSFT_ts %>%
  autoplot(Close) + ylab("Close Price ($USD)") + xlab("Date") + theme_bw() +
  ggtitle("Microsoft (ticker 'MSFT') ,Close Price since IPO")

MSFT_ts %>%
  autoplot(log_rtn) + ylab("Log Return ($USD)") + xlab("Date") + theme_bw() +
  ggtitle("Microsoft (ticker 'MSFT') ,Log Return since IPO")

#STL treatment
tseriesstl2 <- ts(MSFT_ts$Close , frequency = 252 )
y2=stl(tseriesstl2,s.window="period")
plot(y2)
residuals1=y2$time.series[,3]
