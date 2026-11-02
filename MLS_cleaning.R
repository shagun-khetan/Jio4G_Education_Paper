library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(readxl)
library(data.table)
library(vroom)
library(sf)
library(stringr)


#==============================
# MLS Data
#==============================
df_mls <- vroom::vroom('Data/MLS-full-cell-export-final.csv.gz')
mcc_mnc <- read_excel("Data/MCC-MNC.xlsx")

df_mls_ranged <- df_mls %>%
  filter(lon > 67.533 & lon < 97.889272) %>%
  filter(lat > 7.494831 & lat < 37.512701)

df_mls_ranged$created_date <- as.Date(as.POSIXct(df_mls_ranged$created, origin = "1970-01-01", tz = "UTC"))
df_mls_ranged$year  <- year(df_mls_ranged$created_date)
df_mls_ranged$month <- month(df_mls_ranged$created_date)

## Distribution of Created Dates
ggplot(df_mls_ranged, aes(x = created_date)) +
  geom_density(color = "black") +
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") +
  labs(title = "Distribution of Created Dates", x = "Date", y = "Density") +
  theme_minimal(base_size = 13)

mcc_mnc_india <- mcc_mnc %>% 
  mutate(
    Brand = Brand %>%
      str_replace("AirTel", "Airtel") %>%
      str_replace("IDEA ", "Vi") %>%  
      str_replace("Vodafone India", "Vi")
  ) %>% 
  filter(Country == "India", Brand %in% c("Jio", "Airtel", "Vi")) %>% 
  select(MCC, MNC, Bands, Brand)

## Cleaned tower dataset for India with Brand Name (LTE towers only)
df_mls_india <- df_mls_ranged %>% 
  mutate(
    mcc = as.character(mcc),
    net = as.character(net)
  ) %>% 
  left_join(
    mcc_mnc_india,
    by = c("mcc" = "MCC", "net" = "MNC")
  ) %>% filter(!is.na(Brand)) %>% filter(radio == "LTE") %>% 
  select(area, lat, lon, created_date, year, month, Brand)

## Creating Shapefile for India Cell Towers
df_mls_india$lon <- as.numeric(df_mls_india$lon)
df_mls_india$lat <- as.numeric(df_mls_india$lat)

df_mls_india <- df_mls_india[!is.na(df_mls_india$lon) & !is.na(df_mls_india$lat), ]

treatment_df <- df_mls_india %>% 
  filter(Brand == "Jio", year <= 2016)

treatment_all_df <- df_mls_india %>% 
  filter(year <= 2016)

control_jio_df <- df_mls_india %>% 
  filter(Brand == "Jio", year <= 2018)
  
control_all_df <- df_mls_india %>% 
  filter(year <= 2018)

treatment <- st_as_sf(treatment_df, coords = c("lon", "lat"), crs = 4326)  # EPSG:4326 = WGS84
treatment_all <- st_as_sf(treatment_all_df, coords = c("lon", "lat"), crs = 4326)  # EPSG:4326 = WGS84
control_jio <- st_as_sf(control_jio_df, coords = c("lon", "lat"), crs = 4326)  # EPSG:4326 = WGS84
control_all <- st_as_sf(control_all_df, coords = c("lon", "lat"), crs = 4326)  # EPSG:4326 = WGS84

# st_write(treatment, "Data/treatment/treatment.shp", delete_layer = TRUE)
# st_write(control_jio, "Data/control_jio/control_jio.shp", delete_layer = TRUE)
# st_write(control_all, "Data/control_all/control_all.shp", delete_layer = TRUE)
# st_write(treatment_all,"Data/treatment_all/treatment_all.shp", delete_layer = TRUE)



# df_mls_jio_shock <- df_mls_india %>%
#   filter(
#     Brand == "Jio",
#     (year < 2016) | (year == 2017 & month <= 8)
#   )
# 
# df_mls_airtel <- df_mls_india %>%
#   filter(
#     Brand %in% c("Airtel", "AirTel"),
#     (year < 2016)
#   )


#========================
# Plots
#========================
ggplot(df_mls_india %>% filter(year <= 2022) %>% filter(), aes(x = created_date)) +
  geom_density() + 
  scale_x_date(
    date_labels = "%y",   
    date_breaks = "1 year"  
  ) + 
  labs(
    title = "Distribution of Created Dates for Jio Towers",
    x = "Date",
    y = "Density"
  ) + 
  theme_minimal(base_size = 13)

ggplot(df_mls_jio_shock, aes(x = lon, y = lat)) +
  geom_point(alpha = 0.2, size = 0.1) +
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


ggplot(df_mls_jio, aes(x = lon, y = lat, color = Brand)) +
  geom_point(alpha = 0.5, size = 0.1) +
  coord_quickmap() +
  facet_wrap(~ year) +
  labs(
    title = "Cell Tower Locations in India by Year",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )


ggplot(df_mls_india, aes(x = created_date, color = Brand)) +
  stat_ecdf(size = 1) +
  scale_x_date(
    date_labels = "%y",
    date_breaks = "1 year"
  ) +
  labs(
    title = "CDF of Created Dates for Jio Towers",
    x = "Date",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 13)
