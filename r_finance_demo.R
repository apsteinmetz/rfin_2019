#Using tidy_bdh()

library(tidyverse)
library(Rblpapi)

secs <- c("SPX Index" , "LBUSTRUU Index")
start_date <- as.Date("2008-09-01")
end_date <- as.Date("2009-04-01")
fields = "PX_LAST"
bdh_opts = c("periodicitySelection"="MONTHLY")

if (file.exists("data/bbg_dat.rdata")) {
  load("data/bbg_dat.rdata")
} else {
  #must have a valid blp session running
  blpConnect() 
  # Normal access to blp api
  bbg_dat <- bdh(secs,
                 start.date = start_date,
                 end.date = end_date, 
                 fields = fields,
                 options = bdh_opts)
  save(bbg_dat,file="data/bbg_dat.rdata")
}

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
if (file.exists("data/bbg_dat.rdata")) {
  load("data/bbg_dat_tidy.rdata")
} else {
  bbg_dat_tidy <- tidy_bdh(secs,
                         start.date = start_date,
                         end.date = end_date, 
                         fields = fields,
                         options = bdh_opts)
  save(bbg_dat_tidy,file="data/bbg_dat_tidy.rdata")
}
bbg_dat_tidy

# easy plot
bbg_dat_tidy %>% 
  ggplot(aes(date,
             PX_LAST,
             color=ticker)) + 
  geom_line(size=2)

#make returns, stripping NA in first date
rets <- bbg_dat_tidy %>% 
  group_by(ticker) %>%
  mutate(return = log(PX_LAST/lag(PX_LAST))) %>% 
  filter(date > min(date))

# make a 60/40 portfolio
# first make 'wide'
port <- rets %>% 
  select(date,ticker,return) %>% 
  spread(ticker,return) %>%
  mutate(Blend = LBUSTRUU * 0.4 + 
           SPX * 0.6)
port

#back to long
port %>% gather(ticker,return,-date)

#fix cumret
library(PerformanceAnalytics)
port %>% column_to_rownames("date") %>% 
  chart.CumReturns(legend.loc = "right", 
                   wealth.index = TRUE,
    main="Cumulative Return - Does this Bug You?")

#---------------chart_cum_return-------------------
chart_cum_returns<-function(Ra, 
                            ret_col='return',
                            name_col='name',
                            date_col='date',
                            shape=c('narrow','wide')){
  #returns a ggplot2 line chart object for plotting or further annotation
  #crucially, this function adds a dummy date at the beginning of the series to start
  #the plot at the origin without a gap.
  Ra<-ungroup(Ra)
  shape<-match.arg(shape)
  if (shape=='wide'){
    Ra<- Ra %>%
      gather(name,return,-date)
  } else{
    Ra<-select(Ra,name = name_col,return=ret_col,date=date_col)
    Ra<- Ra %>% select(date,name,return)
  }
  #create wealth index. Add a date to start at "1"
  # that is equal to the length between the first and second dates
  new_dates<-(Ra$date[1]-as.numeric(Ra$date[2]-Ra$date[1])) %>% c(Ra$date)
  #new_dates <- new_dates - 29
  Ra<- Ra %>% 
    group_by(name)%>% 
    complete(date=new_dates,fill=list(return=0)) %>% 
    mutate(wealth=cumprod(1+return))
  gg<-Ra %>% 
    group_by(name) %>% 
    ggplot(aes(x=date,y=wealth,color=name))+geom_line()
  return(gg) 
}
# --------------------------------------------------------------
port %>% chart_cum_returns(shape="wide") + 
  labs(title='"There. Fixed It."') + 
  scale_x_date(date_labels = "%b %Y") + 
  theme_minimal() + 
  scale_color_manual(values=c("green","black","red")) + 
  geom_line(size=1)



#back to long
port %>% 
  gather(ticker,return,-date) %>% 
  chart_cum_returns(name_col="ticker",shape="narrow") + 
  labs(title='"There. Fixed It."') + 
  scale_x_date(date_labels = "%b %Y") + 
  theme_minimal() + 
  scale_color_manual(values=c("black","red","green")) + 
  geom_line(size=1)

