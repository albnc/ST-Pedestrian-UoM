library(tidyverse)
library(leaflet)
library(lubridate)
library(tidygraph)
library(osmdata)
library(dygraphs)
library(xts)
library(plotly)
library(viridis)
library(waveslim)

# Loading the dataset
ped.data <- read.csv("../data/sample2019.csv")
ped.data <- ped.data %>% 
  mutate(datetime = make_datetime(year, month, day, hour, 0)) %>%  #, tz="Australia/Melbourne")) %>% 
  drop_na() %>% 
  select(sensorID, lat, long, datetime, count)

## Compute a summary for mapping
ped.summary <- ped.data %>% 
  group_by(sensorID, lat, long) %>% 
  summarise(total=sum(count), avg=mean(count), stdev=sd(count)) %>% 
  mutate(content=paste(sep="; ", 
                       paste0("ID: ", sensorID), 
                       paste0(round(avg,2), " \U00B1 ", round(stdev,2))))

## Function to calculate Wavelet coefficients
wav.func <- function(rawdata) {
  wav <- mra(rawdata$count, wf="haar", J=4, method="modwt")
  tibble(
    datetime = rawdata$datetime,
    count = rawdata$count,
    D4 = wav$D4,
    D3 = wav$D3,
    D2 = wav$D2,
    D1 = wav$D1,
    S4 = wav$S4,
    S3 = S4 + D4,
    S2 = S3 + D3,
    S1 = S2 + D2,
    S0 = S1 + D1
  )
}
