
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


cmp_C[order(-open_interest.td)] %>% View()
cmp_P[order(-open_interest.td)] %>% View()

cmp_C$color <- 'C'
cmp_P$color <- 'p'

cmp_C[open_interest.td>1000][,st_iod:=sum(open_interest.td),keyby=strike][order(-st_iod)]
cmp_P[open_interest.td>1000][,st_iod:=sum(open_interest.td),keyby=strike][order(-st_iod)]

cmp_C[diff_oi_d>=500] %>% ggplot(aes(strike,diff_oi_d,size=diff_oi))+geom_jitter(alpha=0.1,color='green')
cmp_P[diff_oi_d>=500] %>% ggplot(aes(strike,diff_oi_d,size=diff_oi))+geom_jitter(alpha=0.1,color='green')


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





