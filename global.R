library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(glue)
library(leaflet)
library(skimr)
library(countrycode)
library(shiny)
library(shinydashboard)
library(sf)
library(rgeos)
library(ggspatial)
library(rworldmap)
library(padr)
library(zoo)

data <- read.csv("master.csv")
suicide <- data %>%
  select(-(country.year),-(gdp_per_capita....),-(generation)) %>%
  setNames(c("country",
             "year",
             "sex",
             "group_age",
             "suicides_numbers",
             "population",
             "suicides_rate",
             "HDI.for.year",
             "gdp_for_year"
             )
  ) %>% 
  mutate(country=as.factor(country),
         country_code=countrycode(country,origin="country.name",destination="iso3c"),
         continent=as.factor(countrycode(country,origin="country.name",destination = "continent")),
         year=as.numeric(year),
         sex=as.factor(sex),
         date_year=year(as.Date(as.character(year),format="%Y")),
         group_age=as.factor(group_age),
         gdp_for_year = as.numeric(gsub(',', '', gdp_for_year))
  ) %>% filter(country!="Dominica") %>% filter(country!="Saint Kitts and Nevis") %>% 
  filter(country!="Macau") %>% filter(country!="Cabo Verde") %>% mutate(tahun=as.factor(year))

# -------------------- LINE PLOT -----------------------------

continent_worldwide_line <-
  suicide %>%
  group_by(year) %>%
  summarise(rate = 100000*sum(suicides_numbers)/sum(population)) %>%
  mutate(continent = "Worldwide") %>%
  select(continent, everything())

continent_line_data <- suicide %>%
  group_by(continent, year) %>%
  summarise(rate = 100000*sum(suicides_numbers)/sum(population))%>%
  ungroup() %>%
  rbind(continent_worldwide_line) %>%
  filter(rate != 0) %>%
  mutate(label = paste0('<b>', continent, '</b>',
                        ' (', year, ')<br>',
                        'Rate: ', round(rate, 2), ' per 100,000'))

continent_line_plot <-
  ggplot(data = continent_line_data,
         aes(x = year, y = rate,
             group = continent, color = continent,
             text = label)) +
  geom_line(lwd = 0.75) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(1985, 2016, by = 5),
                     limits = c(1985, 2016)) +
  labs(x = "Year", y = "Suicide per 100,000 population",
       title = "<b>Suicide Rates Over the Years</b>",
       color = "") +
  theme_minimal()

country_worldwide_line <-
  suicide %>%
  group_by(year) %>% 
  summarise(total_suicides = sum(suicides_numbers),
            total_pop = sum(population)) %>% 
  mutate(country = "Worldwide",
         rate = 100000*total_suicides/total_pop) %>% 
  select(country, everything())

line_data <-
  suicide %>% 
  group_by(country, year) %>% 
  summarise(total_suicides = sum(suicides_numbers),
            total_pop = sum(population)) %>% 
  ungroup() %>% 
  mutate(rate = 100000*total_suicides/total_pop) %>% 
  rbind(country_worldwide_line) %>% 
  mutate(label = paste0('<b>', country, '</b>',
                        ' (', year, ')<br>',
                        'Rate: ', round(rate, 2), ' per 100,000'))

# -------------------- LINE PLOT -----------------------------

scatter_data <-
  suicide %>% 
  group_by(country, year) %>% 
  summarise(gdp_per_cap = gdp_for_year/sum(population),
            HDI = mean(HDI.for.year)) %>% 
  ungroup() %>% 
  unique() %>% 
  
  # join data
  left_join(
    line_data %>% 
      filter(country != "Worldwide") %>% 
      select(country, year, rate, total_pop),
    by = c("country", "year")) %>% 
  mutate(continent = countrycode(sourcevar = country,
                                 origin = "country.name",
                                 destination = "continent")) %>% 
  
  # padding
  mutate(year = as.Date(as.yearmon(year))) %>% 
  pad(group = "country") %>% 
  mutate(year = year(year)) %>% 
  fill(-c("HDI"), .direction = "down") %>% 
  
  mutate(label_gdp = paste0('<b>', country, '</b><br>',
                            'GDPPC: $', format(round(gdp_per_cap, 2),
                                               big.mark = ','), '<br>',
                            'Rate: ', round(rate, 2), ' per 100,000<br>',
                            'Population: ', format(total_pop, big.mark = ',')),
         label_hdi = paste0('<b>', country, '</b><br>',
                            'HDI: ', format(round(HDI, 2), big.mark = ','), '<br>',
                            'Rate: ', round(rate, 2), ' per 100,000<br>',
                            'Population: ', format(total_pop, big.mark = ',')))


