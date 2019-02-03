library(airportr)
library(rvest)
library(tidyverse)
library(xml2)
 
tags <- c('.airport-content-destination-list-time', '.airport-content-destination-list-name')
index_page <- 'https://www.flightsfrom.com/LHR/destinations'

airports <- read_html(index_page) %>%
  html_nodes(".airport-content-destination-list-name") %>% 
  html_text() %>% 
  str_trim()

times <- read_html(index_page) %>%
  html_nodes(".airport-content-destination-list-time") %>% 
  html_text() %>% 
  str_trim()

data <- tibble(airports, times) %>% 
  rowwise() %>% 
  mutate(
    code = str_extract(airports, '[[:upper:]][[:upper:]][[:upper:]]'),
    position = str_locate_all(airports, '[[:upper:]][[:upper:]][[:upper:]]')
    ) %>% 
  separate(col = times, into = c('split', 'detail'), sep = ": ") %>% 
  separate(col = detail, into = c('hours', 'mins'), sep = "h ") %>% 
  rowwise() %>% 
  mutate(
    mins = as.integer(str_sub(mins, start = 1L, end = -2L)),
    hours = as.integer(hours),
    minutes = (60 * hours) + mins,
    position1 = position[[1]] - 2, 
    position2 = position[[2]] + 2,
    city = str_trim(str_to_lower(str_sub(string = airports, start = 1L, end = position1))),
    country = str_trim(str_to_lower(str_sub(string = airports, start = position2, end = -1L))))

airports <- airportr::airports

output <- data[, c('code', 'city', 'country', 'minutes')] %>% 
  left_join(airports, by = c('code' = 'IATA')) %>% 
  filter(!is.na(ICAO)) %>% 
  rowwise() %>% 
  mutate(distance = airport_distance('LHR', code))

index_page_1 <- 'https://en.m.wikipedia.org/wiki/List_of_cities_by_average_temperature'

temp_data <- read_html(index_page_1) %>%
  html_nodes("table") %>% 
  html_table(header = TRUE)

