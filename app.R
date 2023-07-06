#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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
library(shinyWidgets)

# Run the application
ui <- fluidPage(
  titlePanel("IV Skew"),
  helpText("Here we can see the skew of SPY over past few days. Source: CBOE"),
  #textInput("symb", "Symbol", value="SPY"),
  #dateRangeInput("dates","Date range",start = Sys.Date(),end = ceiling_date(Sys.Date(),"month") - days(1)), #as.character(Sys.Date())
  airDatepickerInput("dates",
                     label = "Expiry month",
                     value = format(Sys.Date(), format="%Y-%m-%d"),
                     maxDate = format(Sys.Date()+365, format="%Y%m%d"),
                     minDate = format(Sys.Date(), format="%Y-%m-%d"),
                     view = "months", 
                     minView = "months", 
                     dateFormat = "MMM-yyyy"),


  plotOutput("plot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot <- renderPlot({
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
    expiry <- friday3(year(input$dates[1]),year(input$dates[1])) %>% filter(Day>input$dates[1]) %>% pull(2) %>% head(1)
    
    #stock_data_tbl <- input$symb %>% tq_get(from=input$dates[1],to=input$dates[2])

#    stocks <- xts(st1[,-1],order.by=as.Date(st1$date))
#    nowTS <- ts_ts(stocks)
#    seasonal <- decompose(na.locf(nowTS,fromLast=TRUE))$seasonal
    
    
  
    
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
    DAYTODAY = format(Sys.Date(), format="%Y%m%d")
    DAY1DAYSBACK = format(Sys.Date()-1, format="%Y-%m-%d")
    DAY3DAYSBACK = format(Sys.Date()-4, format="%Y-%m-%d")
    # td <- readRDS(paste0("spy",DAYTODAY,".rds"))
    # yt <- readRDS(paste0("spy",format(Sys.Date()-1, format="%Y%m%d"),".rds"))
    # iv <- bind_rows(td,yt)
    # saveRDS(iv,paste0("iv",".rds"))
    iv <- readRDS(paste0("iv",".rds"))
    
    
    #db4 <- readRDS(paste0("spy",format(Sys.Date()-3, format="%Y%m%d"),".rds"))
    p <- iv %>%  #bind_rows(yt,td) %>%
      filter(Symbol=="SPY") %>%
      filter(expiry=={{expiry}})%>%
      #filter(expiry=="2023-07-21") %>%
      filter((Date>=DAY3DAYSBACK)& (Date<=DAY1DAYSBACK)) %>% 
      select(strike,iv,flag,Date,expiry) %>%
      mutate(strike=as.numeric(strike)) %>%
      #filter(between(strike,370,435)) %>%
      ggplot(aes(x=strike,y=iv,color=Date)) +
      geom_line()+
      facet_wrap(~flag,scales = "free")+
      scale_color_manual(values=c('grey','pink','maroon','red'))+
      ggthemes::theme_excel_new()+
      labs(title = paste0("Change in IV Skew for ",{{unique(iv$Symbol)}}," ",{{expiry}}," Expiry as of ",lubridate::today()),
           subtitle = "IV skew for Calls and Put strikes for Month expiry",
           x     = "Strikes",
           y     = "IV")
    
p    
  },height = 640, width = 1080)

  

  
 
  
  # get CBOE options
  # opts1 = CBOE_Options(symbol="_SPX",EXERCISE = "european")
  # opts2 = CBOE_Options(symbol="TSLA",EXERCISE = "american")
  
  
 # spy <- CBOE_Options(symbol="SPY",EXERCISE = "american")  #input$symb
#  saveRDS(spy,paste0("spy",DAYTODAY,".rds"))
  

  
  
  # library(dplyr)
  # library(ggplot2)
  # 
  # plot_iv <- function(df,expiry){
  #   monthlyexp <- df %>%
  #     filter(expiry=={{expiry}}) %>%
  #     select(strike,iv,flag,Date,expiry) %>%
  #     mutate(strike=as.numeric(strike))
  #   
  #   
  #   monthlyexp %>% ggplot(aes(x=strike,y=iv,color=Date)) +
  #     geom_line()+
  #     facet_wrap(~flag,scales = "free")+
  #     scale_color_manual(values=c('pink','grey','maroon','red'))+
  #     ggthemes::theme_excel_new()+
  #     labs(title = paste0("Change in IV Skew for ",{{unique(monthlyexp$Symbol)}}," ",{{unique(monthlyexp$expiry)}}," Expiry as of ",lubridate::today()-1),
  #          subtitle = "IV skew for Calls and Put strikes for Month expiry",
  #          x     = "Strikes",
  #          y     = "IV")
  # }
  
  # expiry="2023-06-23"
  # plot_iv(spy,expiry="2023-06-23")
  # 
  # monthlyexp <- spy %>%
  #   #filter(Symbol=="_SPX") %>%
  #   filter(expiry=="2023-06-23") %>%
  #   select(strike,iv,flag,Date,expiry) %>%
  #   mutate(strike=as.numeric(strike))
  # monthlyexp %>% ggplot(aes(x=strike,y=iv,color=Date)) +
  #   geom_line()+
  #   facet_wrap(~flag,scales = "free")+
  #   scale_color_manual(values=c('pink','grey','maroon','red'))+
  #   ggthemes::theme_excel_new()+
  #   labs(title = paste0("Change in IV Skew for ",{{unique(monthlyexp$Symbol)}}," ",{{unique(monthlyexp$expiry)}}," Expiry as of ",lubridate::today()-1),
  #        subtitle = "IV skew for Calls and Put strikes for Month expiry",
  #        x     = "Strikes",
  #        y     = "IV")
  # 
  # 
  # spy %>%
  #   filter(strike=="410") %>%
  #   select(expiry,iv,flag) %>% mutate(dt=lubridate::mday((expiry))) %>%
  #   ggplot(aes(x=expiry,y=iv)) +
  #   geom_point()+
  #   facet_wrap(~flag,scales = "free")+
  #   scale_color_manual(values=c('grey','pink','maroon','red'))+
  #   ggthemes::theme_excel_new()+coord_flip()
  # 
  # 
  # monthlyexp <- spy %>%
  #   #filter(Symbol=="_SPX") %>%
  #   filter(expiry=="2023-06-23") %>%
  #   select(strike,iv,flag,Date,expiry) %>%
  #   mutate(strike=as.numeric(strike))
  # monthlyexp %>% ggplot(aes(x=strike,y=iv,color=Date)) +
  #   geom_line()+
  #   facet_wrap(~flag,scales = "free")+
  #   scale_color_manual(values=c('pink','grey','maroon','red'))+
  #   ggthemes::theme_excel_new()+
  #   labs(title = paste0("Change in IV Skew for ",{{unique(monthlyexp$Symbol)}}," ",{{unique(monthlyexp$expiry)}}," Expiry as of ",lubridate::today()-1),
  #        subtitle = "IV skew for Calls and Put strikes for Month expiry",
  #        x     = "Strikes",
  #        y     = "IV")
  
  

}# Run the application
shinyApp(ui = ui, server = server)




