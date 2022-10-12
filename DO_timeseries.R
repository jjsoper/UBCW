# AUTHOR: JOSH SOPER
# DATE 10/7/2022
# PURPOSE: PREPARE 2021 DO/T BLACKSTONE TIMESERIES FOR STORYMAP
# CHECKED BY:

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(plotly)
library(odbc)
library(dataRetrieval)

# set connections
filePath <- '../UBWPAD_NutrientandContinuous.accdb'
db <- dbConnect(drv = odbc(),
                .connection_string = 
                  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                         filePath,
                         ";"))

# download flow for usgs millbury
siteNumber <- "01109730"
pCode <- "00060" # discharge in cfs
startDate <- "2021-07-10"
endDate <- "2021-11-10"

flow <- siteNumber %>% 
  readNWISuv(siteNumbers = .,
             parameterCd = pCode, 
             startDate = startDate,
             endDate = endDate,
             tz = "America/New_York") %>% 
  renameNWISColumns() %>% 
  tibble() %>% 
  mutate(Flow_Inst = round(Flow_Inst,2))


# collect DO data from database
do <- db %>% 
  tbl('CorrectedContinuousData') %>% 
  # filter(station == 'UBWPAD2') %>% 
  collect() %>% 
  filter(datetime >= as.POSIXct('2021-01-01')) %>% 
  mutate(DO = round(DO, 2)) %>% 
  arrange(station,datetime) %>% 
  mutate(station = case_when(
    station == 'rt20Overpass' ~ 'Route 20',
    station == 'UBChannel' ~ 'Outfall Channel',
    T ~ station
    
  ))

add_station <- function(p, site, y2Overlay, color) {
  
  df <- do %>% filter(station == site)
  ymin <- 0
  ymax <- max(df$DO)
  showFlowLegend <- ifelse(y2Overlay == 'y7', T, F)
  
  p %>% 
    # add DO
    add_lines(
      data = df,
      x = ~datetime,
      y = ~DO,
      name = ~station,
      yaxis = 'y',
      line = list(
        color = color,
        # color = 'rgb(35,148,65)',
        width = 1
      )
    ) %>%
    # add flow to secondary y axis
    add_lines(
      data = flow,
      x = ~dateTime,
      y = ~Flow_Inst,
      yaxis = "y2",
      name = "Flow (cfs)",
      showlegend = showFlowLegend,
      line = list(
        # color = 'rgb(77,148,255)',
        color = '#75787d',
        width = 1
      )
    ) %>% 
    # layout parameters (titles, margins, legends, etc.)
    layout(
      xaxis = list(
        title = '',
        zeroline = F
      ),
      yaxis = list(
        zeroline = F,
        range = c(ymin,ymax),
        side = 'left'
      ),
      yaxis2 = list(
        # add line break with \n to get more space between title and label
        overlaying = y2Overlay,
        side = "right",
        showgrid = F,
        zeroline = F,
        range=c(0,6000)
      ),
      # showlegend = F,
      hovermode = 'x unified',
      margin = list(r = 100)
    )
} 


# add annotations
create_annotations <- function(plt) {
  
  plt %>% 
    # Diel DO variation lab
    add_annotations(
      # start arrow
      ax = as.POSIXct('2021-08-05'),
      ay = 2.5,
      # end arrow
      x = as.POSIXct('2021-08-14 20:00'),
      y = 6.3,
      text = 'Diel DO Variation',
      arrowwidth = 1.5,
      font = list(size = 10),
      xref = "x",
      yref = "y3",
      axref = "x",
      ayref = "y3",
      arrowhead = 2,
      showarrow = TRUE
    ) %>% 
    # Diel DO variation arrow
    add_annotations(
      # start arrow
      ax = as.POSIXct('2021-08-15 08:00'),
      ay = 6,
      # end arrow
      x = as.POSIXct('2021-08-15 13:00'),
      y = 7.8,
      text = '',
      arrowsize = 1,
      arrowwidth = 1.5,
      arrowcolor = 'blue',
      arrowside = 'end+start',
      xref = "x",
      yref = "y3",
      axref = "x",
      ayref = "y3",
      arrowhead = 2,
      showarrow = TRUE
    ) %>% 
    # continuous DO lab
    add_annotations(
      # start arrow
      ax = as.POSIXct('2021-09-12'),
      ay = 4,
      # end arrow
      x = as.POSIXct('2021-09-22 12:00'),
      y = 6.2,
      arrowwidth = 1.5,
      text = 'Continuous\nDissolved Oxygen\nConcentration (mg/L)',
      xref = "x",
      yref = "y3",
      axref = "x",
      ayref = "y3",
      font = list(size = 10),
      arrowhead = 2,
      showarrow = TRUE
    ) %>% 
    # streamflow lab
    add_annotations(
      # start arrow
      ax = as.POSIXct('2021-10-20'),
      ay = 4,
      # end arrow
      x = as.POSIXct('2021-10-05 11:30'),
      y = 1.5,
      # arrowsize = 0.8,
      arrowwidth = 1.5,
      text = 'Streamflow at\nMillbury\nUSGS Gage (cfs)',
      xref = "x",
      yref = "y3",
      axref = "x",
      ayref = "y3",
      font = list(size = 10),
      arrowhead = 2,
      showarrow = TRUE
    )
  
}

# CREATE CHART ------------------------------------------------------------

# plotly interactive chart
main <- plot_ly(
  height = 800
)

pal <- RColorBrewer::brewer.pal(4,'Dark2')
m <- m <- list(
  l = 125,
  r = 125,
  b = 100,
  t = 25
  #pad = 4
)

commonlayout <- list(showarrow=F,xref='paper',yref='paper')

p <- subplot(
  main %>% add_station('Route 20', 'y', pal[1]),
  main %>% add_station('UBWPAD2', 'y3', pal[2]) %>% create_annotations(),
  main %>% add_station('W1258', 'y5', pal[3]),
  main %>% add_station('Outfall Channel', 'y7', pal[4]),
  shareX = T,
  shareY = F,
  nrows = 4
) %>% 
  layout(
    margin = m,
    showlegend = F,
    # legend
    # legend = list(
    #   orientation = 'h',
    #   x = 0.3,
    #   y = -0.12
    #   ),
    annotations = list(
      # y axis title
      list(x = -0.05 ,
           y = 0.5,
           textangle = 270,
           text = "Dissolved Oxygen (mg/L)",
           showarrow = F,
           xref='paper',
           yref='paper'
           ),
      # y2 axis title
      list(x = 1.07,
           y = 0.5,
           textangle = 90,
           text = "Streamflow (cfs)",
           showarrow = F,
           xref='paper',
           yref='paper'
           ),
      # xaxis title
      list(x = 0.5 ,
           y = -0.1,
           text = "Date",
           showarrow = F,
           xref='paper',
           yref='paper'
           ),
      # station titles
      list(x=0,y=0.98,font=list(color=pal[1]),text="<b>Route 20</b>") %>% append(commonlayout),
      list(x=0,y=0.73,font=list(color=pal[2]),text="<b>UBWPAD2</b>") %>% append(commonlayout),
      list(x=0,y=0.45,font=list(color=pal[3]),text="<b>W1258</b>") %>% append(commonlayout),
      list(x=0,y=0.22,font=list(color=pal[4]),text="<b>Outfall Channel</b>") %>% append(commonlayout)
      )
  )

# export to .html
htmlwidgets::saveWidget(widget = as_widget(p),
                        file = "DO_timeseries_2021.html",
                        title = 'Blackstone DO Timeseries 2021')
  
