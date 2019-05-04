#Using tidy_bdh()
library(tidyverse)
library(Rblpapi)

secs <- c("SPX Index" , "LBUSTRUU Index")
start_date <- as.Date("2008-09-01")
end_date <- as.Date("2009-04-01")
fields = "PX_LAST"
bdh_opts = c("periodicitySelection"="MONTHLY")

#must have a valid blp session running
blpConnect() 
bbg_dat <- bdh(secs,
               start.date = start_date,
               end.date = end_date, 
               fields = fields,
               options = bdh_opts)
bbg_dat


# -------------------------------------------------

# Get Tidy BBG Data
#Turn a list of data frames into a single "tidy" data frame.
# CAUTION: The order of the returned secs series may not be the same as order of the secs inputs
# CAUTION: There is no guarantee that the all securities will have values for the same dates.
#          This is low risk at frequencies of weekly or lower
tidy_bdh<-function(secs,...){
  blpConnect() #must have a valid blp session running
  blp_bdh  <-bdh(secs,...=...)
  blp_bdh_tidy<-bind_rows(blp_bdh,.id='ticker') %>%
    mutate(sector=word(ticker,-1)) %>% 
    mutate(ticker=word(ticker,1)) %>% 
    select(date,ticker,everything())%>%
    group_by(sector,ticker)
  #special case of one ticker
  if (length(secs)==1){
    blp_bdh_tidy$ticker=word(secs,1)
    blp_bdh_tidy$sector=word(secs,-1)
  }
  
  return(blp_bdh_tidy)
}

#get bbg data in tidy format
bbg_dat_tidy <- tidy_bdh(secs,
               start.date = start_date,
               end.date = end_date, 
               fields = fields,
               options = bdh_opts)
bbg_dat_tidy



save(bbg_dat,file="data/bbg_dat.rdata")
save(bbg_dat_tidy,file="data/bbg_dat_tidy.rdata")

# -------------------------------------------------


# demo of chart_cum_returns
library(tidyverse)
library(PerformanceAnalytics)
source("utilities.r")
load("data/loan_dur_Data_raw.RData")
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


blp_bdh_tidy %>% 
  group_by(ticker) %>% 
  mutate(return=log(PX_LAST/lag(PX_LAST))) %>% 
  ggplot(aes(date,return)) + geom_line()
