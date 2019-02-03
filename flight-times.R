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
    city = str_sub(string = airports, start = 1L, end = position1),
    country = str_sub(string = airports, start = position2, end = -1L))

output <- data[, c('code', 'city', 'country', 'minutes')]

