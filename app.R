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
    p1 <- iv %>%  #bind_rows(yt,td) %>%
      filter(Symbol=="SPY") %>%
      filter(expiry=={{expiry}})%>%
      #filter(expiry=="2023-07-21") %>%
      #filter((Date>=DAY3DAYSBACK)& (Date<=DAY1DAYSBACK)) %>% 
      filter(Date>=DAY3DAYSBACK) %>% 
      select(strike,iv,flag,Date,expiry) %>%
      mutate(strike=as.numeric(strike)) %>%
      #filter(between(strike,370,435)) %>%
      ggplot(aes(x=strike,y=iv,color=Date)) +
      geom_line()+
      facet_wrap(~flag,scales = "free")+
      scale_color_manual(values=c('grey','pink','maroon','red'))+
      ggthemes::theme_excel_new()+
      labs(title = paste0("Change in IV Skew for ",{{unique(iv$Symbol)}}," ",{{expiry}}," Expiry as of ",lubridate::today()-1),
           subtitle = "IV skew for Calls and Put strikes for Month expiry",
           x     = "Strikes",
           y     = "IV")
    
    stk <- iv %>% count(stkClose) %>% arrange(-n) %>% slice(1) %>% pull(stkClose) %>% plyr::round_any(10)
    p2 <- iv %>% filter(Date == max(Date)) %>% 
      filter(strike=={{ stk }}) %>%
      select(expiry,iv,flag) %>% mutate(dt=lubridate::mday((expiry))) %>%
      ggplot(aes(x=expiry,y=iv)) +
      geom_point()+
      facet_wrap(~flag,scales = "free")+
      scale_color_manual(values=c('grey','pink','maroon','red'))+
      ggthemes::theme_excel_new()+coord_flip()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      labs(title = paste0("IV for ",{{stk}}," Strike"," for all Expiries"),
           subtitle = paste0("Data as of ",lubridate::today()),
           x     = "Strikes",
           y     = "IV")
    
    
    yt_OI_C <- iv %>% filter(Date==lubridate::today()-1) %>% 
      filter(Symbol=="SPY") %>%
      #  filter(option %like%  c("SPX22")) %>%
      
      filter(expiry=={{expiry}}) %>%
      filter(flag=="C") %>%
      select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
      mutate(strike=as.numeric(strike)) %>%
      mutate(OI_Dollar=open_interest*last_trade_price) %>%
      mutate(rnk=percent_rank(OI_Dollar)) %>%
      mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
      #slice_max(rnk,n = 100) %>%
      mutate(cum_sep_OI = cumsum(OI_pct))
    
    yt_OI_P <- iv %>% filter(Date==lubridate::today()-1) %>% 
      filter(Symbol=="SPY") %>%
      #  filter(option %like%  c("SPX22")) %>%
      
      filter(expiry=={{expiry}}) %>%
      filter(flag=="P") %>%
      select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
      mutate(strike=as.numeric(strike)) %>%
      mutate(OI_Dollar=open_interest*last_trade_price) %>%
      mutate(rnk=percent_rank(OI_Dollar)) %>%
      mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
      #slice_max(rnk,n = 100) %>%
      mutate(cum_sep_OI = cumsum(OI_pct))
    
    
    
    todays_OI_C <- iv %>% filter(Date==lubridate::today()) %>% 
      filter(Symbol=="SPY") %>%
      #filter(option %like%  c("SPX22")) %>%
      filter(expiry=={{expiry}}) %>%
      filter(flag=="C") %>%
      select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
      mutate(strike=as.numeric(strike)) %>%
      mutate(OI_Dollar=open_interest*last_trade_price) %>%
      mutate(rnk=percent_rank(OI_Dollar)) %>%
      mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
      #slice_max(rnk,n = 100) %>%
      mutate(cum_sep_OI = cumsum(OI_pct))
    
    todays_OI_P <- iv %>% filter(Date==lubridate::today()) %>% 
      filter(Symbol=="SPY") %>%
      #filter(option %like%  c("SPX22")) %>%
      filter(expiry=={{expiry}}) %>%
      filter(flag=="P") %>%
      select(strike,flag,volume,open_interest,last_trade_price,Premium,Delta,Date,expiry) %>%
      mutate(strike=as.numeric(strike)) %>%
      mutate(OI_Dollar=open_interest*last_trade_price) %>%
      mutate(rnk=percent_rank(OI_Dollar)) %>%
      mutate(OI_pct=(OI_Dollar/sum(OI_Dollar))*100) %>%
      #slice_max(rnk,n = 100) %>%
      mutate(cum_sep_OI = cumsum(OI_pct))
    
    
    cmp_C <- yt_OI_C %>% left_join(todays_OI_C,by="strike",suffix = c(".yt",".td")) %>%
      select(strike,open_interest.yt,open_interest.td,OI_Dollar.yt,OI_Dollar.td,Date.td,expiry.td) %>%
      mutate(diff_oi=open_interest.td-open_interest.yt,
             diff_oi_d=OI_Dollar.td-OI_Dollar.yt) %>%
      arrange(strike) %>% setDT()
    
    cmp_P <- yt_OI_P %>% left_join(todays_OI_P,by="strike",suffix = c(".yt",".td")) %>%
      select(strike,open_interest.yt,open_interest.td,OI_Dollar.yt,OI_Dollar.td,Date.td,expiry.td) %>%
      mutate(diff_oi=open_interest.td-open_interest.yt,
             diff_oi_d=OI_Dollar.td-OI_Dollar.yt) %>%
      arrange(strike) %>% setDT()
    
    
    
    blue <- "#0171CE"
    
    red <- "#DE4433"
    
    
    
    percent_first <- function(x) {
      
      x <- sprintf("%d%%", round(x*100))
      
      x[2:length(x)] <- sub("%$", "", x[2:length(x)])
      
      x
      
    }
    
    cmp_C <- cmp_C %>% mutate(pct=diff_oi_d/OI_Dollar.td)
    cmp_P <- cmp_P %>% mutate(pct=diff_oi_d/OI_Dollar.td)
    
    dataC <- cmp_C%>% filter(expiry=={{expiry}}) %>% arrange(-OI_Dollar.td) %>% slice(1:20) %>% mutate(strike=as.factor(strike))
    dataP <- cmp_P%>% filter(expiry=={{expiry}}) %>% arrange(-OI_Dollar.td) %>% slice(1:20) %>% mutate(strike=as.factor(strike))
    
    library(ggplot2)
    
    library(ggalt)
    
    #library(tidyverse)
    
    
    
   p3 <-  ggplot() +
      
      # geom_segment(data=c %>% arrange(-oid.td) %>% slice(1:10), aes(y=0, yend=st, x=oid.yt, xend=oid.td),
      
      #               color="#b2b2b2", size=0.15)
      
      geom_dumbbell(data=dataC, aes(y=strike, x=OI_Dollar.yt, xend=OI_Dollar.td),
                    
                    size=1.5, color="#b2b2b2", size_x=3, size_xend = 3,
                    
                    colour_x = 'grey', colour_xend = blue)+
      
      
      
      # geom_text(data=filter(c%>% arrange(-oid.td) %>% slice(1:10), diff_oi_d>0),
      
      #           aes(x=oid.td, y=st, label="Up"),
      
      #           color=blue, size=3, vjust=-1.5, fontface="bold", family="Lato") +
      
         # geom_text(data=filter(dataC%>% arrange(-OI_Dollar.td) %>% slice(1:10), diff_oi_d<0),
         # 
         #         aes(x=oid.td, y=st, label="Dn"),
         # 
         #         color=red, size=3, vjust=-1.5, fontface="bold", family="Lato")
         # 
    
    
    

    geom_text(data=dataC, aes(x=OI_Dollar.td, y=strike, label=scales::dollar(round(c(OI_Dollar.td)/1e6,1),suffix='M')),

              color=red, size=2.75, vjust=2.5, family="Lato") +

      geom_text(data=dataC,aes(x=OI_Dollar.yt, y=strike, label=scales::dollar(round(c(OI_Dollar.yt)/1e6,1),suffix='M')),

                color=blue, size=2.75, vjust=2.5, family="Lato")+



      geom_rect(data=dataC, aes(xmin=max(cmp_C$OI_Dollar.td)+1e5, xmax=max(cmp_C$OI_Dollar.td)+2e5, ymin=-Inf, ymax=Inf), fill="grey") +

      geom_text(data=filter(dataC, diff_oi_d>0), aes(label=scales::percent(pct), y=strike, x=max(cmp_C$OI_Dollar.td)+1.5e5), fontface="bold", size=3, family="Lato") +

      # geom_text(data=cmp_C,
      #
      #           aes(x=max(cmp_C$OI_Dollar.td)+1.5e5, y=470, label="%Change"),
      #
      #           color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +

      # scale_x_continuous(expand=c(1e5,max(c$oid.td)+.5e7), limits=c(0, 1e6))

      # scale_y_discrete(expand=c(350,450)) +



    labs(x=NULL, y=NULL, title="Call $OI",

         #subtitle="Change",

         caption=paste0("As of:\n\n" , lubridate::today()))+





      theme_bw(base_family="Lato") +

      theme(

        #panel.grid.major=element_blank(),

        panel.grid.minor=element_blank(),

        panel.border=element_blank(),

        axis.ticks=element_blank(),

        axis.text.x=element_blank(),

        plot.title=element_text(size = 16, face="bold"),

        plot.title.position = "plot",

        plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),

        plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")

      )
    
   p4 <-  ggplot() +

     # geom_segment(data=c %>% arrange(-oid.td) %>% slice(1:10), aes(y=0, yend=st, x=oid.yt, xend=oid.td),

     #               color="#b2b2b2", size=0.15)

     geom_dumbbell(data=dataP, aes(y=strike, x=OI_Dollar.yt, xend=OI_Dollar.td),

                   size=1.5, color="#b2b2b2", size_x=3, size_xend = 3,

                   colour_x = 'grey', colour_xend = blue)+



   #   # geom_text(data=filter(c%>% arrange(-oid.td) %>% slice(1:10), diff_oi_d>0),
   #   
   #   #           aes(x=oid.td, y=st, label="Up"),
   #   
   #   #           color=blue, size=3, vjust=-1.5, fontface="bold", family="Lato") +
   #   
   #   #   geom_text(data=filter(c%>% arrange(-oid.td) %>% slice(1:10), diff_oi_d<0),
   #   
   # #             aes(x=oid.td, y=st, label="Dn"),
   # 
   # #             color=red, size=3, vjust=-1.5, fontface="bold", family="Lato")
   # 
   # 
   # 
   # 
   # 
   geom_text(data=dataP, aes(x=OI_Dollar.td, y=strike, label=scales::dollar(round(c(OI_Dollar.td)/1e6,1),suffix='M')),

             color=red, size=2.75, vjust=2.5, family="Lato") +

     geom_text(data=dataP,aes(x=OI_Dollar.yt, y=strike, label=scales::dollar(round(c(OI_Dollar.yt)/1e6,1),suffix='M')),

               color=blue, size=2.75, vjust=2.5, family="Lato")+

   #   
   #   
   #   geom_rect(data=dataP, aes(xmin=max(dataP$OI_Dollar.td)+1e5, xmax=max(dataP$OI_Dollar.td)+2e5, ymin=-Inf, ymax=Inf), fill="grey") +
   #   
     geom_text(data=filter(dataP, diff_oi_d>0), aes(label=scales::percent(pct), y=strike, x=max(dataP$OI_Dollar.td)+1.5e5), fontface="bold", size=3, family="Lato") +

     # geom_text(data=cmp_C,
     #
     #           aes(x=max(cmp_C$OI_Dollar.td)+1.5e5, y=470, label="%Change"),
     #
     #           color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +

     # scale_x_continuous(expand=c(1e5,max(c$oid.td)+.5e7), limits=c(0, 1e6))

     # scale_y_discrete(expand=c(350,450)) +



   labs(x=NULL, y=NULL, title="Puts $OI",

       # subtitle="Change",

        caption=paste0("As of:\n\n" , lubridate::today()))+





     theme_bw(base_family="Lato") +

     theme(

       #panel.grid.major=element_blank(),

       panel.grid.minor=element_blank(),

       panel.border=element_blank(),

       axis.ticks=element_blank(),

       axis.text.x=element_blank(),

       plot.title=element_text(size = 16, face="bold"),

       plot.title.position = "plot",

       plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),

       plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")

     )

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    cowplot::plot_grid(
      p1, p2, p3,p4,
      #ncol = 1,nrow=4,labels = "",
           labels = "", ncol = 1
    )
  },height = 1200, width = 1080)

  

  
 
  
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




