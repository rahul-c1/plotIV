FROM rocker/shiny:4.3
RUN install2.r readr scales tidyquant gridExtra xts tsbox quantmod plyr clipr cowplot shinyWidgets rsconnect jsonlite dplyr stringr RQuantLib derivmkts lubridate pbapply httr rvest purrrr data.table quantmod RColorBrewer colorspace farver ggplot2 ggthemes gtable purrr dbplyr googledrive ggalt
WORKDIR /home/ivplot 
COPY app.R app.R
COPY deploy.R deploy.R
COPY iv.rds iv.rds
COPY cmpC.csv cmpC.csv
COPY cmpP.csv cmpP.csv
COPY OI_C.csv OI_C.csv
COPY OI_P.csv OI_P.csv
CMD Rscript deploy.R
