library(shiny)
library(crosstalk)
library(DT)
library(leaflet)
library(tidyverse)

overall <- read_rds(path = "overall.rds")

pal <- colorNumeric(
  palette = "Reds",
  domain = overall$Aug)

sd <- SharedData$new(overall)

# bscols(
#   filter_slider("temp", "Temperature", sd, column=~Aug, step=0.1, width=250),
#   filter_slider("duration", "Time", sd, column=~minutes, step=0.1, width=250)
#   )

leaflet(sd) %>% 
  addTiles() %>% 
  setView(
    lng = 15, 
    lat = 10, 
    zoom = 2) %>% 
  addCircles(
    lng = ~Longitude, 
    lat = ~Latitude, 
    weight = 2,
    radius = ~sqrt(minutes) * 10000, 
    popup = ~paste(City, "- time: ", round(minutes/60, 1), "hrs; temp: ", Aug, "C"), 
    color = ~pal(Aug)
  )

datatable(
  sd, 
  extensions="Scroller", 
  style="bootstrap", 
  class="compact", 
  width="100%",
  options=list(deferRender=TRUE, scrollY=300, scroller=TRUE)
  )
