#utility functions for financial time series and fetching bbg data
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
  Ra<- Ra %>% 
    group_by(name)%>% 
    complete(date=new_dates,fill=list(return=0)) %>% 
    mutate(wealth=cumprod(1+return))
  
  gg<-Ra %>% 
    group_by(name) %>% 
    ggplot(aes(x=date,y=wealth,color=name))+geom_line()
  
  return(gg) 
}
#--------------------tidy_bdh--------------

# Get BBG Data
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

# example
#fields=c('DAY_TO_DAY_TOT_RETURN_GROSS_DVDS',"LAST_PRICE",'FUND_TOTAL_ASSETS')
#START_DATE=as.Date("2002-03-31")
#secs = c(' OIGAX Equity','DODFX Equity')
#BDH_OPTIONS = c("periodicitySelection"="MONTHLY")
#
#bbg_ret_data  <-tidy_bdh(secs,
#                         fields=fields, 
#                         start.date = START_DATE,
#                         end.date=Sys.Date(),
#                         options=BDH_OPTIONS)

#------------- mutate_cond -------------------------
# from https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows/45443415#45443415
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  condition[is.na(condition)] = FALSE
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

