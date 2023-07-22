FROM rocker/shiny:4.2.1
RUN install2.r tidyquant xts tsbox quantmod plyr clipr cowplot shinyWidgets rsconnect jsonlite dplyr stringr RQuantLib derivmkts lubridate pbapply httr rvest purrrr data.table quantmod RColorBrewer colorspace farver ggplot2 ggthemes gtable purrr dbplyr googledrive ggalt
WORKDIR /home/ivplot 
COPY app.R app.R
COPY deploy.R deploy.R
COPY iv.rds iv.rds
COPY cmp_C.csv cmp_C.csv
COPY cmp_P.csv cmp_P.csv
CMD Rscript deploy.R
