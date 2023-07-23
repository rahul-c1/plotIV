
require("jsonlite");require("stringr");require("RQuantLib");require("derivmkts");require("pbapply")
require("httr");require("rvest");require("purrr");require("data.table");library(quantmod);library(dplyr)
library(lubridate)
#library(plotly)
# getSymbols("AAPL")
# 
# options.expiry(AAPL)
# futures.expiry(AAPL)
# 
# AAPL[options.expiry(AAPL)]
library(ggplot2)

# function to extract Expirations, Flag, and Strike from Option Name
getEFS = function(x)
{
  expiry = str_sub(x, -15,-10)
  expiry = as.character(as.Date(expiry,format="%y%m%d"))
  flag   = str_sub(x,-9,-9)
  strike = str_sub(x,-8,-1)
  left   = str_sub(strike,-8,-4)
  right  = str_sub(strike,-3,-1)
  strike = paste0(left,".",right)
  strike = as.numeric(strike)
  as.data.frame(cbind(expiry,flag,strike))
}

# get Options + Calculate IV & Greeks
CBOE_Options = function(symbol,EXERCISE){
  # url to get options - will read in all using json
  #url = "https://cdn.cboe.com/api/global/delayed_quotes/options/_SPX.json"
  url = paste0("https://cdn.cboe.com/api/global/delayed_quotes/options/",symbol,".json")
  # read in data from page
  df = read_json(url,simplifyVector = TRUE)
  # convert as data frame
  opts = as.data.frame(df$data$options)
  # get Expiration, Flag, & Strike
  efs <- getEFS(opts$option)
  # combine with options data
  opts <- cbind(opts,efs)
  # fix last_trade_time
  opts$last_trade_time <- as.character(as.POSIXct(opts$last_trade_time,
                                                  format="%Y-%m-%dT%H:%M:%S"))
  opts$stkClose <- df$data$close
  # add date pulled
  opts$Date = as.character(Sys.Date())
  # add Days to Expiration
  opts$days2Exp = as.numeric(as.Date(opts$expiry) - as.Date(opts$Date))
  # Option Mid Price
  opts$Mid = round((opts$bid + opts$ask)/2,2)
  
  # calculate IV
  if(EXERCISE == "european")
  {
    ivs = pblapply(as.list(1:nrow(opts)),function(ii){
      tmp = try(EuropeanOptionImpliedVolatility(
        type = ifelse(opts$flag[ii] == "C","call","put"),
        value=as.numeric(opts$Mid)[ii],
        underlying=as.numeric(df$data$close),
        strike=as.numeric(opts$strike)[ii],
        dividendYield=0,
        riskFreeRate=0,
        maturity=as.numeric(yearFraction(as.Date(opts$Date[ii]),
                                         as.Date(opts$expiry[ii]),
                                         1)),
        volatility=as.numeric(df$data$iv30/100)),
        silent=TRUE)
      if(inherits(tmp,'try-error')){
        iv = round(as.numeric(df$data$iv30/100),4)
      }else{
        iv = round(tmp[[1]],4)
      }
      iv
    })
    
  }else{
    ivs = pblapply(as.list(1:nrow(opts)),function(ii){
      tmp = try(AmericanOptionImpliedVolatility(
        type = ifelse(opts$flag[ii] == "C","call","put"),
        value=as.numeric(opts$Mid)[ii],
        underlying=as.numeric(df$data$close),
        strike=as.numeric(opts$strike)[ii],
        dividendYield=0,
        riskFreeRate=0,
        maturity=as.numeric(yearFraction(as.Date(opts$Date[ii]),
                                         as.Date(opts$expiry[ii]),
                                         1)),
        volatility=as.numeric(df$data$iv30/100)),
        silent=TRUE)
      if(inherits(tmp,'try-error')){
        iv = round(as.numeric(df$data$iv30/100),4)
      }else{
        iv = round(tmp[[1]],4)
      }
      iv
    })
  }
  
  # add Caluclated IVs to Options Date
  opts$calc_IV = do.call(rbind,ivs)
  
  # calculate greeks
  CALLS = subset(opts, opts$flag == "C")
  PUTS = subset(opts, opts$flag == "P")
  
  # greeks for calls
  cGREEKS = greeks2(bscall,list(s=as.numeric(CALLS$stkClose),
                                k=as.numeric(CALLS$strike),
                                v=as.numeric(CALLS$calc_IV),
                                r=rep(0,nrow(CALLS)),
                                tt=as.numeric(CALLS$days2Exp)/252,
                                d=rep(0,nrow(CALLS))))
  # transpose greeks
  cGREEKS = t(cGREEKS)
  # combine with call options
  CALLS = cbind(CALLS,cGREEKS)
  
  # greeks for calls
  pGREEKS = greeks2(bsput,list(s=as.numeric(PUTS$stkClose),
                               k=as.numeric(PUTS$strike),
                               v=as.numeric(PUTS$calc_IV),
                               r=rep(0,nrow(PUTS)),
                               tt=as.numeric(PUTS$days2Exp)/252,
                               d=rep(0,nrow(PUTS))))
  # transpose greeks
  pGREEKS = t(pGREEKS)
  # combine with call options
  PUTS = cbind(PUTS,pGREEKS)
  # combine calls/puts
  opts = rbind(CALLS,PUTS)
  # add ticker column
  opts$Symbol = symbol
  opts
}
# authenticate


# function to update data set
update_data <- function(since_id) {
  DAYTODAY = format(Sys.Date(), format="%Y%m%d")
  # td <- readRDS(paste0("spy",DAYTODAY,".rds"))
  # yt <- readRDS(paste0("spy",format(Sys.Date()-1, format="%Y%m%d"),".rds"))
  # iv <- bind_rows(td,yt)
  # saveRDS(iv,paste0("iv",".rds"))
  iv <- readRDS(paste0("iv",".rds"))
  max_dt <- unique(iv$Date) %>% tail(7) %>% head(1)
  iv <- iv %>% filter(Date>max_dt)
  
  spy <- CBOE_Options(symbol="SPY",EXERCISE = "american")  #input$symb
  
  saveRDS(spy,paste0("spy",format(Sys.Date(), format="%Y%m%d"),".rds"))
  
  iv <- bind_rows(iv,spy)
  
  saveRDS(iv,paste0("iv",".rds"))
  
  # First day of next month
  first <- ceiling_date(Sys.Date(), unit = "month")
  # Last day of next month
  last <- ceiling_date(first, unit= "month") - days(1)
  fridays <- seq.Date(Sys.Date(),last,by="1 day") 
  choicedt <- format(fridays[weekdays(fridays)=="Friday"],format = "%Y-%m-%d")
  
  
  friday3 <- function(start.year, end.year,interval = "3 month"){
    d <- seq(ISOdate(start.year - 1, 12, 1), ISOdate(end.year, 12, 1), by = "1 month")[-1]
    d <- as.Date(d)
    res <- lapply(d, function(x){
      s <- seq(x, by = "day", length.out = 28)
      i <- format(s, "%u") == "5"
      s[i][3]
    })
    
    res <- Reduce(c, res)
    data.frame(Month = format(d, "%Y-%B"), Day = res)
  }
  monthlyexpiry <- friday3(year(first),year(first)) %>% filter(Day>tail(choicedt,1)) %>% pull(2)# %>% head(1)
  
  choicedt <- c(choicedt,as.character(monthlyexpiry))
  
  weeklies4month <- iv %>% filter(expiry %in% choicedt) 
  
  last2Dates <- weeklies4month %>% count(Date) %>% slice_max(Date,n=2) %>% pull(Date)
  
  yt_OI_C <- weeklies4month %>% filter(Date==last2Dates[2]) %>% 
    filter(Symbol=="SPY") %>%
    #  filter(option %like%  c("SPX22")) %>%
    
   # filter(expiry=={{expiry}}) %>%
    filter(flag=="C") %>%
    select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
    mutate(strike=as.numeric(strike)) %>%
    mutate(OI_Dollar=open_interest*last_trade_price) %>%
    mutate(rnk=percent_rank(OI_Dollar)) %>%
    mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
    #slice_max(rnk,n = 100) %>%
    mutate(cum_sep_OI = cumsum(OI_pct))
  
  yt_OI_P <- weeklies4month %>% filter(Date==last2Dates[2]) %>% 
    filter(Symbol=="SPY") %>%
    #  filter(option %like%  c("SPX22")) %>%
    
   # filter(expiry=={{expiry}}) %>%
    filter(flag=="P") %>%
    select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
    mutate(strike=as.numeric(strike)) %>%
    mutate(OI_Dollar=open_interest*last_trade_price) %>%
    mutate(rnk=percent_rank(OI_Dollar)) %>%
    mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
    #slice_max(rnk,n = 100) %>%
    mutate(cum_sep_OI = cumsum(OI_pct))
  
  
  
  todays_OI_C <- weeklies4month %>% filter(Date==last2Dates[1]) %>% 
    filter(Symbol=="SPY") %>%
    #filter(option %like%  c("SPX22")) %>%
   # filter(expiry=={{expiry}}) %>%
    filter(flag=="C") %>%
    select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
    mutate(strike=as.numeric(strike)) %>%
    mutate(OI_Dollar=open_interest*last_trade_price) %>%
    mutate(rnk=percent_rank(OI_Dollar)) %>%
    mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
    #slice_max(rnk,n = 100) %>%
    mutate(cum_sep_OI = cumsum(OI_pct))
  
  todays_OI_P <- weeklies4month %>% filter(Date==last2Dates[1]) %>% 
    filter(Symbol=="SPY") %>%
    #filter(option %like%  c("SPX22")) %>%
   # filter(expiry=={{expiry}}) %>%
    filter(flag=="P") %>%
    select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
    mutate(strike=as.numeric(strike)) %>%
    mutate(OI_Dollar=open_interest*last_trade_price) %>%
    mutate(rnk=percent_rank(OI_Dollar)) %>%
    mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
    #slice_max(rnk,n = 100) %>%
    mutate(cum_sep_OI = cumsum(OI_pct))
  

  
  cmp_C <- yt_OI_C %>% left_join(todays_OI_C,by=c("strike","expiry"),suffix = c(".yt",".td")) %>%
    select(strike,open_interest.yt,open_interest.td,OI_Dollar.yt,OI_Dollar.td,Date.td,expiry) %>%
    mutate(diff_oi=open_interest.td-open_interest.yt,
           diff_oi_d=OI_Dollar.td-OI_Dollar.yt) %>%
    arrange(strike) %>% setDT()
  
  cmp_P <- yt_OI_P %>% left_join(todays_OI_P,by=c("strike","expiry"),suffix = c(".yt",".td")) %>%
    select(strike,open_interest.yt,open_interest.td,OI_Dollar.yt,OI_Dollar.td,Date.td,expiry) %>%
    mutate(diff_oi=open_interest.td-open_interest.yt,
           diff_oi_d=OI_Dollar.td-OI_Dollar.yt) %>%
    arrange(strike) %>% setDT()
  
  
  
  cmp_C <- cmp_C %>% mutate(pct=diff_oi_d/OI_Dollar.td)
  cmp_P <- cmp_P %>% mutate(pct=diff_oi_d/OI_Dollar.td)  
  
  
  fwrite(cmp_C,"cmpC.csv")
  fwrite(cmp_P,"cmpP.csv")
  
}

# run update if it's a business day or yesterday was a business day
#if(isBusinessDay(calendar="UnitedStates/NYSE", dates = Sys.Date())){
update_data()
#}