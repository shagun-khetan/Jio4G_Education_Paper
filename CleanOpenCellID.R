library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)
library(data.table)
library(vroom)
library(sf)

#==============================
# OpenCellID Data
#==============================

## Loading Datasets
# test <- read_excel("MCC-MNC.xlsx")
mcc_mnc <- read_excel("Data/MCC-MNC.xlsx")
df <- vroom::vroom('Data/cell_towers.csv.gz')

## Cleaning up Dates
df$created_date <- as.Date(as.POSIXct(df$created, origin = "1970-01-01", tz = "UTC"))
df$year  <- year(df$created_date)
df$month <- month(df$created_date)

## Filtering to India Bounds
df_india <- df %>%
  filter(lon > 67.533 & lon < 97.889272) %>%
  filter(lat > 7.494831 & lat < 37.512701)

## Creating Shapefile for India Cell Towers
df_india$lon <- as.numeric(df_india$lon)
df_india$lat <- as.numeric(df_india$lat)

df_india <- df_india[!is.na(df_india$lon) & !is.na(df_india$lat), ]

pts_sf <- st_as_sf(df_india, coords = c("lon", "lat"), crs = 4326)  # EPSG:4326 = WGS84
# plot(pts_sf["year"])  -- will take time to run
# st_write(pts_sf, "cell_towers_india.shp", delete_layer = TRUE)

# Plot of all India Towers
ggplot(df_india %>% filter(radio == "LTE"), aes(x = lon, y = lat)) +
  geom_point(alpha = 0.6, size = 0.4) +
  coord_quickmap() +
  labs(
    title = "Cell Tower Locations in India (LTE)",
    x = "Longitude",
    y = "Latitude"
  )+
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),                      
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.4), 
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
  )


## Distribution of Created Dates
ggplot(df_india %>% filter(year > 2013), aes(x = created_date)) +
  geom_density(
    color = "darkblue",
    bins = 50
  ) + scale_x_date(
    date_labels = "%Y",   
    date_breaks = "1 year"  
  ) + 
  labs(
    x = "Year",
    y = "Density"
  ) + 
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )


## Cleaning MCC-MNC data and merging with OpenCellID data
mcc_mnc_jio <- mcc_mnc %>% 
  filter(Country == "India", Brand %in% c("Jio")) %>% 
  select(MCC, MNC, Bands)

mcc_mnc_airtel <- mcc_mnc %>% 
  filter(Country == "India", Brand %in% c("Airtel", "AirTel")) %>% 
  select(MCC, MNC, Bands)

df_india <- df_india %>% 
  mutate(
    mcc = as.character(mcc),
    net = as.character(net)
  ) %>% 
  left_join(
    mcc_mnc_airtel,
    by = c("mcc" = "MCC", "net" = "MNC")
  ) %>% 
  mutate(airtel = if_else(!is.na(Bands), 1, 0))

## Also filtering on LTE because there might be some non-LTE towers built by reliance
## under their MNC-MCC codes which later got transferred to JIO (Jio is LTE only)
df_airtel<- df_india %>% filter(airtel == 1, radio == "LTE") 

## Distribution of Created Dates for Jio Towers
ggplot(df_airtel, aes(x = created_date)) +
  geom_density(
    color = "black",          
  ) + scale_x_date(
    date_labels = "%b %y",   
    date_breaks = "2 month"  
  ) + 
  labs(
    title = "Distribution of Created Dates for Jio Towers",
    x = "Date",
    y = "Density"
  ) + 
  theme_minimal(base_size = 13)

# Plot of all India Jio Towers
ggplot(df_jio, aes(x = lon, y = lat)) +
  geom_point(alpha = 0.2, size = 0.4) +
  coord_quickmap() +
  labs(
    title = "Cell Tower Locations in India (Jio)",
    x = "Longitude",
    y = "Latitude"
  )+
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(), # no gridlines
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.4), 
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
  )
