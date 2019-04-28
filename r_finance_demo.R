# demo of chart_cum_returns
library(tidyverse)
library(PerformanceAnalytics)
source("utilities.r")
load("loan_dur_Data_raw.RData")
index_names<-c("BarcAgg",
               "Tsy",
               "SPLoan",
               "Loans",
               "BarcHY",
               "LIBOR3m_ret",
               "LIBOR3m_rate")

funds<-c("BKLN Equity",
         "HYG Equity",
         "OOSAX Equity")

fund_names<-c("Loan ETF",
              "HY Bond ETF",
              "Opp SFR")

common_names<-tibble(ticker=c(indices,funds),name=c(index_names,fund_names)) %>% 
  mutate(ticker=word(ticker))
#create return series
#except where LAST_PRICE is a rate not a price, then compute with monthly compounding
ret_data_ndx<-raw_ret_data_ndx %>% 
  group_by(ticker) %>% 
  mutate(return=if_else(ticker=="US0003M",(1+LAST_PRICE/1200)-1,LAST_PRICE/lag(LAST_PRICE)-1))
# below is the right way to convert a rate to a monthly return but simple compounding comes closer
# to the BBG return series.
#  mutate(return=if_else(ticker=="US0003M",((1+LAST_PRICE/100))^(1/12)-1,LAST_PRICE/lag(LAST_PRICE)-1))

#add common names
ret_data_ndx<-ret_data_ndx %>%
  ungroup() %>% 
  left_join(common_names) %>% 
  group_by(name)

r<-ret_data_ndx %>% 
  group_by() %>% 
  select(date,ticker,return) %>%
  #mutate(return = ifelse(is.na(return),0,return)) %>% 
  spread(ticker,return)
  

r[106:110,]  %>% 
  column_to_rownames(var='date') %>% 
  chart.CumReturns(wealth.index = TRUE,main="Not Indexed to 1",legendloc= "right")

r[106:110,]  %>% chart_cum_returns(shape="wide") + 
  labs(title='"There. Fixed It."') + 
  scale_x_date(date_labels = "%b %Y") + 
  theme_minimal() +  theme(legend.position = "none")



