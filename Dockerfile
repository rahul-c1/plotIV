FROM rocker/shiny:4.2.1
RUN install2.r plyr ggalt cowplot shinyWidgets rsconnect jsonlite dplyr stringr RQuantLib derivmkts lubridate pbapply httr rvest purrrr data.table quantmod RColorBrewer colorspace farver ggplot2 ggthemes gtable purrr 
WORKDIR /home/ivplot 
COPY app.R app.R
COPY deploy.R deploy.R
COPY iv.rds iv.rds
CMD Rscript deploy.R
