library(airportr)
library(leaflet)
library(rvest)
library(tidyverse)
library(xml2)
 
index_page_LHR <- 'https://www.flightsfrom.com/LHR/destinations'
index_page_LGW <- 'https://www.flightsfrom.com/LGW/destinations'

airports_LHR <- read_html(index_page_LHR) %>%
  html_nodes(".airport-content-destination-list-name") %>% 
  html_text() %>% 
  str_trim()

times_LHR <- read_html(index_page_LHR) %>%
  html_nodes(".airport-content-destination-list-time") %>% 
  html_text() %>% 
  str_trim()

airports_LGW <- read_html(index_page_LGW) %>%
  html_nodes(".airport-content-destination-list-name") %>% 
  html_text() %>% 
  str_trim()

times_LGW <- read_html(index_page_LGW) %>%
  html_nodes(".airport-content-destination-list-time") %>% 
  html_text() %>% 
  str_trim()

data <- bind_rows(
  tibble(airports = airports_LHR, times = times_LHR, home = 'LHR'),
  tibble(airports = airports_LGW, times = times_LGW, home = 'LGW')
) %>% 
  arrange(home) %>% 
  group_by(airports) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(airports) %>% 
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

airport_data <- airportr::airports

output <- data[, c('code', 'city', 'home', 'country', 'minutes')] %>% 
  left_join(airport_data, by = c('code' = 'IATA')) %>% 
  filter(!is.na(ICAO)) %>% 
  rowwise() %>% 
  mutate(
    distance = airport_distance(home, code),
    City = str_to_lower(City)
    )

temp_data <- read_html('https://en.m.wikipedia.org/wiki/List_of_cities_by_average_temperature') %>%
  html_nodes("table") %>% 
  html_table(header = TRUE) %>% 
  bind_rows() %>% 
  select(-Year, -Ref.) %>% 
  mutate_at(vars(-Country, -City), str_remove, '\\s*\\([^\\)]+\\)') %>% 
  mutate_at(vars(-Country, -City), as.numeric) %>% 
  mutate(
    City = str_to_lower(City),
    Country = str_to_lower(Country)) %>% 
  arrange(City)

overall <- output %>% 
  left_join(temp_data, by = c('City' = 'City')) %>% 
  select(code, City, Country.x, minutes, ICAO, Latitude, Longitude, UTC, distance, Aug) %>% 
  filter(!is.na(Aug)) %>% 
  arrange(code) %>% 
  group_by(Country.x, City) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(City)

