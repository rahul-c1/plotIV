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

# Run the application
ui <- fluidPage(
  titlePanel("IV Skew"),
  helpText("Source: CBOE"),
  #textInput("symb", "Symbol", value="SPY"),
  #dateRangeInput("dates","Date range",start = Sys.Date(),end = ceiling_date(Sys.Date(),"month") - days(1)), #as.character(Sys.Date())
#  airDatepickerInput("dates",label = "Expiry month",value = format(Sys.Date(), format="%Y-%m-%d"),maxDate = format(Sys.Date()+365, format="%Y%m%d"), minDate = format(Sys.Date(), format="%Y-%m-%d"), view = "months",  minView = "months", dateFormat = "MMM-yyyy"),



  mainPanel(
    tabsetPanel(
      tabPanel("IV",airDatepickerInput("dates1",label = "Expiry month",value = format(Sys.Date(), format="%Y-%m-%d"),maxDate = format(Sys.Date()+365, format="%Y%m%d"), minDate = format(Sys.Date(), format="%Y-%m-%d"), view = "months",  minView = "months", dateFormat = "MMM-yyyy"),plotOutput("plotiv")),
      #tabPanel("$OI",dateRangeInput("dates2","Date range",start = Sys.Date(),end = ceiling_date(Sys.Date(),"month") - days(1)),plotOutput("plotoi")) #,
      tabPanel("$OI",pickerInput("weeklyexpiry","Expiry: ",choices = choicedt, options = list(`live-search` = TRUE)),plotOutput("plotoi")) #,
      #tabPanel("Seasonality Monthly",textInput("symb", "Symbol", value="SPY"),dateRangeInput("seasonDates","Date range",start = '1990-01-01',end = ceiling_date(Sys.Date(),"month") - days(1)), #as.character(Sys.Date())
          #     plotOutput("plotseason")),
      #tabPanel("Seasonality Month-Day",textInput("symb", "Symbol", value="SPY"),dateRangeInput("seasonDates","Date range",start = '1990-01-01',end = ceiling_date(Sys.Date(),"month") - days(1)), #as.character(Sys.Date())
           #    plotOutput("plotseason"))
    )
  ),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plotiv <- renderPlot({
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
    expiry <- friday3(year(input$dates1[1]),year(input$dates1[1])) %>% filter(Day>input$dates1[1]) %>% pull(2) %>% head(1)
    
    #stock_data_tbl <- input$symb %>% tq_get(from=input$dates[1],to=input$dates[2])

#    stocks <- xts(st1[,-1],order.by=as.Date(st1$date))
#    nowTS <- ts_ts(stocks
#    seasonal <- decompose(na.locf(nowTS,fromLast=TRUE))$seasonal
    

    #dates[format(dates,"%w")==5]
  
    
   
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
    
    
    cowplot::plot_grid(
      p1, p2, 
      #ncol = 1,nrow=4,labels = "",
           labels = "", ncol = 1
    )
  },height = 1200, width = 1080)
  
  
  output$plotoi <- renderPlot({
    
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
    
   # fridays <- seq.Date(input$dates2[1],input$dates2[2],by="1 day") #as.Date.character(input$dates2[1], format="%Y-%m-%d")
  #  expiry <- head(fridays[weekdays(fridays)=="Friday"],1)
    expiry <- input$weeklyexpiry
    #print(weeklyExpiry)
    
    #weekdays(lubridate::today())
    DAYTODAY = format(Sys.Date(), format="%Y%m%d")
    DAY1DAYSBACK = format(Sys.Date()-1, format="%Y-%m-%d")
    DAY3DAYSBACK = format(Sys.Date()-4, format="%Y-%m-%d")
    # td <- readRDS(paste0("spy",DAYTODAY,".rds"))
    # yt <- readRDS(paste0("spy",format(Sys.Date()-1, format="%Y%m%d"),".rds"))
    # iv <- bind_rows(td,yt)
    # saveRDS(iv,paste0("iv",".rds"))
    
    iv <- readRDS(paste0("iv",".rds"))
    
    
    percent_first <- function(x) {
      
      x <- sprintf("%d%%", round(x*100))
      
      x[2:length(x)] <- sub("%$", "", x[2:length(x)])
      
      x
      
    }
    
    
    
    blue <- "#0171CE"
    
    red <- "#DE4433"

    cmp_C <- fread("cmpC.csv")
    cmp_P <- fread("cmpP.csv")
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
    
    
    
    
    geom_text(data=dataC, aes(x=OI_Dollar.td, y=strike, label=scales::dollar(round(c(OI_Dollar.td)/1e6,2),suffix='M')),
              
              color=red, size=2.75, vjust=2.5) + #, family="Lato"
      
      geom_text(data=dataC,aes(x=OI_Dollar.yt, y=strike, label=scales::dollar(round(c(OI_Dollar.yt)/1e6,2),suffix='M')),
                
                color=blue, size=2.75, vjust=2.5)+  #, family="Lato"
      
      
      
      #geom_rect(data=dataC, aes(xmin=max(cmp_C$OI_Dollar.td)+1e5, xmax=max(cmp_C$OI_Dollar.td)+2e5, ymin=-Inf, ymax=Inf), fill="grey") +
      
      #geom_text(data=filter(dataC, diff_oi_d>0), aes(label=scales::percent(pct), y=strike, x=max(cmp_C$OI_Dollar.td)+1.5e5), fontface="bold", size=3, family="Lato") +
      
      # geom_text(data=cmp_C,
      #
      #           aes(x=max(cmp_C$OI_Dollar.td)+1.5e5, y=470, label="%Change"),
      #
      #           color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +
      
      # scale_x_continuous(expand=c(1e5,max(c$oid.td)+.5e7), limits=c(0, 1e6))
      
      # scale_y_discrete(expand=c(350,450)) +
      
    
    
    labs(x=NULL, y=NULL, title=paste0("Call $OI for Expiry: ",{{expiry}}),
         
         #subtitle="Change",
         
        # caption=paste0("As of:\n\n" , lubridate::today())
        )+
      
      
      
      
      
      theme_bw() + #base_family="Lato"
      
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
    geom_text(data=dataP, aes(x=OI_Dollar.td, y=strike, label=scales::dollar(round(c(OI_Dollar.td)/1e6,2),suffix='M')),
              
              color=red, size=2.75, vjust=2.5) + #, family="Lato"
      
      geom_text(data=dataP,aes(x=OI_Dollar.yt, y=strike, label=scales::dollar(round(c(OI_Dollar.yt)/1e6,2),suffix='M')),
                
                color=blue, size=2.75, vjust=2.5)+ #, family="Lato"
      
      #   
      #   
      #   geom_rect(data=dataP, aes(xmin=max(dataP$OI_Dollar.td)+1e5, xmax=max(dataP$OI_Dollar.td)+2e5, ymin=-Inf, ymax=Inf), fill="grey") +
      #   
      #geom_text(data=filter(dataP, diff_oi_d>0), aes(label=scales::percent(pct), y=strike, x=max(dataP$OI_Dollar.td)+1.5e5), fontface="bold", size=3, family="Lato") +
      
      # geom_text(data=cmp_C,
      #
      #           aes(x=max(cmp_C$OI_Dollar.td)+1.5e5, y=470, label="%Change"),
      #
      #           color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +
      
      # scale_x_continuous(expand=c(1e5,max(c$oid.td)+.5e7), limits=c(0, 1e6))
      
      # scale_y_discrete(expand=c(350,450)) +
      
    
    
    labs(x=NULL, y=NULL, title=paste0("Puts $OI for Expiry: ",{{expiry}}),
         
         # subtitle="Change",
         
         caption=paste0("As of:\n\n" , lubridate::today()))+
      
      
      
      
      
      theme_bw() + #base_family="Lato"
      
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
    
    
    # 
    # output$plotseason <- renderPlot({
    #   
    #   stock_data_tbl <- input$symb %>% tq_get(from=input$seasonDates[1],to=input$seasonDates[2])
    #   # 
    #   #     df_returns_monthly <- stock_data_tbl %>% group_by(symbol) %>% tq_transmute(select=adjusted,mutate_fun=periodReturn,period="monthly",col_rename = "monthly.returns")
    #   #     
    #   #     df_returns_monthly %>% filter(symbol=="SPY") %>%
    #   #       mutate(mnth= as.factor(month(date,label=T)),yr=as.factor(year(date))) %>%
    #   #       ggplot(aes(yr,monthly.returns))+
    #   #       geom_col()+ #aes(fill=mnth)
    #   #       #facet_wrap(~mnth,scales="free",ncol=4)+
    #   #       theme_tq()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none")
    #   st1 <- stock_data_tbl %>% #filter(sybmol==ticker) %>%
    #     select(-1) %>%
    #     select(date,adjusted)
    #   
    #   stocks <- xts(st1[,-1],order.by=as.Date(st1$date))
    #   nowTS <- ts_ts(stocks)
    #   seasonal <- decompose(na.locf(nowTS,fromLast=TRUE))$seasonal
    #   
    #   tsbox::ts_df(seasonal) %>%
    #     filter(year(time)==max(year(time))) %>%
    #     rename_all(tolower) %>%
    #     mutate(yr=as.factor(year(time))) %>%
    #     mutate(year = factor(year(time)),
    #            date = update(time,year=1)
    #     ) %>%
    #     ggplot(aes(date,value,color=year))+
    #     scale_x_date(date_breaks="1 month",date_labels = "%b")+theme_classic()+ geom_line(color="black",size=2)+
    #     # Change line size
    #     geom_hline(yintercept=1, linetype="dashed",
    #                color = "red", size=1)+
    #     geom_hline(yintercept=0, linetype="dashed",
    #                color = "red", size=2)+
    #     geom_hline(yintercept=-1, linetype="dashed",
    #                color = "red", size=1)+theme_tq() + geom_smooth(method = "gam")+ 
    #     labs(y=NULL, title="Seasonality", # subtitle="Change",
    #          caption=paste0("As of:\n\n" , lubridate::today()))
    #     
    #   
    # },height = 760, width = 1200)
    
    
  
    # output$plotseasonDW <- renderPlot({
    #   
    #   stock_data_tbl <- input$symb %>% tq_get(from=input$dates[1],to=input$dates[2])
    #   df_returns_daily <- stock_data_tbl %>%
    #     group_by(symbol) %>%
    #     tq_transmute(select=adjusted,
    #                  mutate_fun=periodReturn,
    #                  period="daily",
    #                  col_rename="daily.returns")
    #   
    #   df_returns_daily %>%
    #     mutate(color=if_else(daily.returns>=0.04,"h","l")) %>%
    #     mutate(dd=as.factor(wday(date,label=T)),
    #            mnth=as.factor(month(date,label=T))) %>%
    #     ggplot(aes(dd,daily.returns))+
    #     geom_violin( width=0.75) +
    #     geom_point(alpha = 0.1,position = position_dodge(width=0.75))+
    #     facet_wrap(~mnth,scales="free")+
    #     scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    #     theme_tq()
    #   
    #   
    #   
    # },height = 760, width = 1200)
    
    
    
    
    
    
    
    
    
    cowplot::plot_grid(
      p4,p3,
      ncol = 1,labels = ""
      
    )
  },height = 1068, width = 1200)

  

  
 
  
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




