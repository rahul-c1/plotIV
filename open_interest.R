
yt_OI_C <- iv %>% filter(Date==lubridate::today()-1) %>% 
  filter(Symbol=="SPY") %>%
  #  filter(option %like%  c("SPX22")) %>%
  
  filter(expiry=="2023-08-18") %>%
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
  
  filter(expiry=="2023-08-18") %>%
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
  filter(expiry=="2023-08-18") %>%
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
  filter(expiry=="2023-08-18") %>%
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

#data.table::fwrite(cmp_C %>% rename(st=strike,oi.yt=open_interest.yt,oi.td=open_interest.td,oid.yt=OI_Dollar.yt,oid.td=OI_Dollar.td),"cmp_c.csv")
#data.table::fwrite(cmp_P %>% rename(st=strike,oi.yt=open_interest.yt,oi.td=open_interest.td,oid.yt=OI_Dollar.yt,oid.td=OI_Dollar.td),"cmp_p.csv")


cmp_C[order(-OI_Dollar.td)] %>% View()
cmp_P[order(-OI_Dollar.td)] %>% View()

cmp_C$color <- 'C'
cmp_P$color <- 'p'

cmp_C[open_interest.td>1000][,st_iod:=sum(open_interest.td),keyby=strike][order(-st_iod)]
cmp_P[open_interest.td>1000][,st_iod:=sum(open_interest.td),keyby=strike][order(-st_iod)]

p1 <- cmp_C %>% filter(between(strike,425,470)) %>% #[diff_oi_d>=500] %>% 
  ggplot(aes(strike,OI_Dollar.td,size=open_interest.td))+
  #geom_jitter(alpha=0.5,color='green')+
  geom_segment(aes(x=strike,xend=strike,y=0,yend=OI_Dollar.td,color=color),size=1.3,alpha=0.7)+
  theme_light()+
  theme(legend.position = "none",
    panel.border = element_blank(),)+
  scale_y_continuous(labels = scales::dollar)+
  xlab("Strikes")+ylab("OIDollar") +
  coord_flip()
p2 <- cmp_P[diff_oi_d>=500] %>% ggplot(aes(strike,OI_Dollar.td,size=open_interest.td))+geom_jitter(alpha=0.8,color='red')+coord_flip()


plotly::ggplotly(p1)

cowplot::plot_grid(p1,p2)


c_p <- bind_rows(cmp_C,cmp_P)
p1 <- ggplot(c_p,aes(x=(strike),y=diff_oi_id))+
  geom_segment(aes(x=(strike),xend=(strike),y=0,yend=diff_oi_d,color=color),size=1.3,alpha=0.7)+
  facet_wrap(~color,scales = "free")+
  theme_light()+
  theme(#legend.position = "none",
        panel.border = element_blank(),)+
  scale_y_continuous(labels= scales::dollar)+
  xlab("Strikes")+ylab("Dollar Change") 


plotly::ggplotly(p1)


df <- cmp_C %>% arrange(-OI_Dollar.td) %>%  mutate(c.td = cumsum(OI_Dollar.td),c.yd=cumsum(OI_Dollar.yt)) %>% 
  mutate(c.td=cumsum(OI_Dollar.td), c.yd = cumsum(OI_Dollar.yt)) %>% 
  mutate(total=sum(OI_Dollar.td)) %>% mutate(pct = c.td/total)
  
df %>% filter(pct>.59) %>% slice(1)
df %>% filter(pct>.39) %>% slice(1)

df %>% View()




blue <- "#0171CE"

red <- "#DE4433"



percent_first <- function(x) {
  
  x <- sprintf("%d%%", round(x*100))
  
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  
  x
  
}

cmp_C <- cmp_C %>% mutate(pct=diff_oi_d/OI_Dollar.td)

dataC <- cmp_C%>% arrange(-OI_Dollar.td) %>% slice(1:30) %>% mutate(strike=as.factor(strike))

library(ggplot2)

library(ggalt)

library(tidyverse)



ggplot() +
  
  # geom_segment(data=c %>% arrange(-oid.td) %>% slice(1:10), aes(y=0, yend=st, x=oid.yt, xend=oid.td),
  
  #               color="#b2b2b2", size=0.15)
  
  geom_dumbbell(data=dataC, aes(y=strike, x=OI_Dollar.yt, xend=OI_Dollar.td),
                
                size=1.5, color="#b2b2b2", size_x=3, size_xend = 3,
                
                colour_x = 'grey', colour_xend = blue)+
  
  
  
  # geom_text(data=filter(c%>% arrange(-oid.td) %>% slice(1:10), diff_oi_d>0),
  
  #           aes(x=oid.td, y=st, label="Up"),
  
  #           color=blue, size=3, vjust=-1.5, fontface="bold", family="Lato") +
  
  #   geom_text(data=filter(c%>% arrange(-oid.td) %>% slice(1:10), diff_oi_d<0),
  
#             aes(x=oid.td, y=st, label="Dn"),

#             color=red, size=3, vjust=-1.5, fontface="bold", family="Lato")





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
  
  
  
  labs(x=NULL, y=NULL, title="Change",
       
       subtitle="Change",
       
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



library(dplyr)   # for data manipulation

library(tidyr)   # for reshaping the data frame

library(stringr) # string manipulation

library(ggplot2) # graphing



# create the data frame

# (in wide format, as needed for the line segments):

dat_wide = tibble::tribble(
  
  ~Country,   ~Y1990,   ~Y2015,
  
  'Russia',  71.5, 101.4,
  
  'Canada',  74.4, 102.9,
  
  'Other non-OECD Europe/Eurasia',  60.9, 135.2,
  
  'South Korea',   127, 136.2,
  
  'China',  58.5, 137.1,
  
  'Middle East', 170.9, 158.8,
  
  'United States', 106.8,   169,
  
  'Australia/New Zealand', 123.6, 170.9,
  
  'Brazil', 208.5, 199.8,
  
  'Japan',   181, 216.7,
  
  'Africa', 185.4,   222,
  
  'Other non-OECD Asia', 202.7,   236,
  
  'OECD Europe', 173.8, 239.9,
  
  'Other non-OECD Americas', 193.1, 242.3,
  
  'India', 173.8, 260.6,
  
  'Mexico/Chile', 221.1, 269.8
  
)



# a version reshaped to long format (for the points):

dat_long = c %>%
  
  gather(key = 'dt', value = 'oid', oid.yt:oid.td)  # %>%

#mutate(Year = str_replace(Year, 'Y', ''))



# create the graph:

ggplot() +
  
  geom_segment(data = c %>% filter(between(st,4200,4500)),
               
               aes(x    = oid.yt,
                   
                   xend = oid.td,
                   
                   y    = st, #reorder(st, -diff_oi_d),
                   
                   yend = st #reorder(st, -diff_oi_d)
                   
               ),
               
               size = 3, colour = '#D0D0D0') +
  
  geom_point(data = dat_long %>% filter(between(st,4200,4500)),
             
             aes(x      = oid,
                 
                 y      = st,
                 
                 colour = dt),
             
             size = 4) +
  
  labs(title = 'Energy productivity in selected countries \nand regions',
       
       subtitle = 'Billion dollars GDP per quadrillion BTU',
       
       caption = 'Source: EIA, 2016',
       
       x = NULL, y = NULL) +
  
  scale_colour_manual(values = c('#1082CD', '#042B41')) +
  
  theme_bw() +
  
  theme(legend.position = c(0.92, 0.20),
        
        legend.title = element_blank(),
        
        legend.box.background = element_rect(colour = 'black'),
        
        panel.border = element_blank(),
        
        axis.ticks = element_line(colour = '#E6E6E6'))



ggsave('energy.png', width = 20, height = 10, units = 'cm')







ggplot() +
  
  # geom_segment(data=c %>% arrange(-oid.td) %>% slice(1:10), aes(y=0, yend=st, x=oid.yt, xend=oid.td),
  
  #               color="#b2b2b2", size=0.15)
  
  geom_dumbbell(data=p%>% arrange(-oid.td) %>% slice(1:10), aes(y=st, x=oid.yt, xend=oid.td),
                
                size=1.5, color="#b2b2b2", size_x=3, size_xend = 3,
                
                colour_x = 'grey', colour_xend = blue)+
  
  
  
  # geom_text(data=filter(c%>% arrange(-oid.td) %>% slice(1:10), diff_oi_d>0),
  
  #           aes(x=oid.td, y=st, label="Up"),
  
  #           color=blue, size=3, vjust=-1.5, fontface="bold", family="Lato") +
  
  #   geom_text(data=filter(c%>% arrange(-oid.td) %>% slice(1:10), diff_oi_d<0),
  
#             aes(x=oid.td, y=st, label="Dn"),

#             color=red, size=3, vjust=-1.5, fontface="bold", family="Lato")





geom_text(data=(p %>% arrange(-oid.td) %>% slice(1:10)), aes(x=oid.td, y=st, label=scales::dollar(round(c(oid.td)/1e6,1),suffix='M')),
          
          color=red, size=2.75, vjust=2.5, family="Lato") +
  
  geom_text(data=(p %>% arrange(-oid.td) %>% slice(1:10)),aes(x=oid.yt, y=st, label=scales::dollar(round(c(oid.yt)/1e6,0),suffix='M')),
            
            color=blue, size=2.75, vjust=2.5, family="Lato") +
  
  
  
  # geom_rect(data=p %>% arrange(-oid.td) %>% slice(1:10), aes(xmin=max(p$oid.td)+1e6, xmax=max(p$oid.td)+.5e7, ymin=-Inf, ymax=Inf), fill="grey")
  
  # geom_text(data=filter(p %>% arrange(-oid.td) %>% slice(1:10), diff_oi_d>0), aes(label=percent_first(pct), y=st, x=max(p$oid.td)+1.5e6), fontface="bold", size=3, family="Lato")+
  
  # geom_text(data=filter(p , st==-1000),
  
  #           aes(x=max(p$oid.td)+2.5e6, y=-1000, label="%Change"),
  
#           color="black", size=3.1, vjust=-2, fontface="bold", family="Lato") +

# # scale_x_continuous(expand=c(1e5,max(c$oid.td)+.5e7), limits=c(0, 1e6))

# #  scale_y_discrete(expand=c(1000,4500))



labs(x=NULL, y=NULL, title="Change",
     
     subtitle="Change",
     
     caption=paste0("Source:\n\n" , lubridate::today()))+
  
  
  
  
  
  theme_bw(base_family="Lato") +
  
  theme(
    
    panel.grid.major=element_blank(),
    
    panel.grid.minor=element_blank(),
    
    panel.border=element_blank(),
    
    axis.ticks=element_blank(),
    
    axis.text.x=element_blank(),
    
    plot.title=element_text(size = 16, face="bold"),
    
    plot.title.position = "plot",
    
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
    
  )

This e-mail is covered by the Electronic Communications Privacy Act, 18 U.S.C. Sections 2510-25

