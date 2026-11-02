library(terra)
library(sf)
library(exactextractr)
library(tidyverse)
target_crs <- "EPSG:32644"
setwd("/Users/yashkhaitan/Desktop/Monsoon 2025/Development Econ/Term Paper")

# Resampling PopDensity File ----------------------------------------------

pop_dens_2017 <- rast("Data/pop_dense_2017.tif" )
pop_dens_2019 <- rast("Data/pop_dense_2019.tif" )
pop_dens_2017_split <- disagg(pop_dens_2017, fact = 3, method = "near")
pop_dens_2019_split <- disagg(pop_dens_2019, fact = 3, method = "near")
pop_dens_2017_split <- pop_dens_2017_split / 9
pop_dens_2019_split <- pop_dens_2019_split / 9

# writeRaster(pop_dens_2017_split, "Data/pop_dense_2017_div.tif", overwrite = TRUE)
# writeRaster(pop_dens_2019_split, "Data/pop_dense_2019_div.tif", overwrite = TRUE)

# Zonal Statistics (PopDensity Aggregate) ---------------------------------

## Importing population raster (divided)
pop_dens_2017_split <- rast("Data/pop_dense_2017_div.tif" )
pop_dens_2019_split <- rast("Data/pop_dense_2019_div.tif" )
pop_dens_2017_split <- project(pop_dens_2017_split, target_crs, method="near")
pop_dens_2019_split <- project(pop_dens_2019_split, target_crs, method="near")
## Importing District Raster
districts <- st_read("Data/India_Shapefile/district.shp") 
districts <- st_transform(districts, target_crs)

## Doing aggregate zonal statistics at district level
zonal_stats_2017 <- exact_extract(pop_dens_2017_split, districts, c('sum','mean','count'))
agg_stats_2017 <- cbind(districts, zonal_stats_2017)
agg_stats_2017_df <- agg_stats_2017 %>% as_tibble()
zonal_stats_2019 <- exact_extract(pop_dens_2019_split, districts, c('sum','mean','count'))
agg_stats_2019 <- cbind(districts, zonal_stats_2019)
agg_stats_2019_df <- agg_stats_2019 %>% as_tibble()

# Clipping to Buffers ---------------------------------
treatment_buffers <- st_read("Data/treatment/treatment_buffer.shp")
treatment_buffers <- st_transform(treatment_buffers, target_crs)

#Control Jio
control_jio_points <- st_read("Data/control_jio/control_jio.shp")
control_jio_points <- st_transform(control_jio_points, target_crs)
control_jio_points_d <- st_join(control_jio_points, districts %>% 
                                  select(d_name, pc11_s_id), left = TRUE)

## Creating 5 km buffers around Jio Control Points
control_jio_buffers <- st_buffer(control_jio_points_d, dist = 5000)

#Control all
control_all_points <- st_read("Data/control_all/control_all.shp")
control_all_points <- st_transform(control_all_points, target_crs)
control_all_points_d <- st_join(control_all_points, districts %>% 
                                  select(d_name, pc11_s_id), left = TRUE)

## Creating 5 km buffers around All Control Points
control_all_buffers <- st_buffer(control_all_points_d, dist = 5000)

############ CONTROL JIO
set.seed(123)
buf_unioncj <- st_union(control_jio_buffers)
hitcj <- st_intersects(districts, buf_unioncj, sparse = FALSE)

out_geomcj <- vector("list", nrow(districts))

# For intersecting districts: extract *single* sfg geometry
out_geomcj[hitcj] <- lapply(
  which(hitcj),
  function(i) {
    geomcj <- st_intersection(districts[i, ], buf_unioncj)
    st_geometry(geomcj)[[1]]      # extract sfg
  }
)

# For non-intersecting districts: sample 1 random point INSIDE district
out_geomcj[!hitcj] <- lapply(
  which(!hitcj),
  function(i) {
    geomcj <- st_sample(districts[i, ], size = 1, type = "random")
    geomcj[[1]]                   # extract sfg
  }
)

district_buffer_intersectioncj <- st_sfc(out_geomcj, crs = target_crs)

district_buffer_intersection_finalcj <- st_sf(
  districts %>% st_set_geometry(NULL),   # keep attributes
  geometry = district_buffer_intersectioncj
)

district_buffer_intersection_finalcj_buff <- st_buffer(district_buffer_intersection_finalcj, dist = 0.00001)

# Zonal Stats Within Buffers -----------------------------------------------
# Zonal Stats for Treatment Points
treatment_clipped <- rast("Data/treatment/treatment_clipped_jio.tif" )
zonal_stats_treatment <- exact_extract(treatment_clipped, districts, c('sum','mean','count'))
buffer_stats_treatment <- cbind(districts, zonal_stats_treatment)
buffer_stats_treatment_df <- buffer_stats_treatment %>% as_tibble()

# Zonal Stats for Treatment Points All
treatment_all_clipped <- rast("Data/treatment_all/treatment_all_clipped.tif" )
zonal_stats_treatment_all <- exact_extract(treatment_all_clipped, districts, c('sum','mean','count'))
buffer_stats_treatment_all <- cbind(districts, zonal_stats_treatment_all)
buffer_stats_treatment_all_df <- buffer_stats_treatment_all %>% as_tibble()


# Zonal Stats for Jio Control Points 
statscj <- exact_extract(pop_dens_2019_split, 
                       district_buffer_intersection_finalcj_buff, c('sum', 'count', 'mean'))
zonal_stats_jio_controls <- cbind(districts, statscj) %>%
  as_tibble() 

############ CONTROL ALL

buf_union <- st_union(control_all_buffers)
hit <- st_intersects(districts, buf_union, sparse = FALSE)

out_geom <- vector("list", nrow(districts))

# For intersecting districts: extract *single* sfg geometry
out_geom[hit] <- lapply(
  which(hit),
  function(i) {
    geom <- st_intersection(districts[i, ], buf_union)
    st_geometry(geom)[[1]]      # extract sfg
  }
)

# For non-intersecting districts: sample 1 random point INSIDE district
out_geom[!hit] <- lapply(
  which(!hit),
  function(i) {
    geom <- st_sample(districts[i, ], size = 1, type = "random")
    geom[[1]]                   # extract sfg
  }
)

district_buffer_intersection <- st_sfc(out_geom, crs = target_crs)

district_buffer_intersection_final <- st_sf(
  districts %>% st_set_geometry(NULL),   # keep attributes
  geometry = district_buffer_intersection
)

district_buffer_intersection_final_buff <- st_buffer(district_buffer_intersection_final, dist = 0.00001)


# Zonal Stats Within Buffers -----------------------------------------------
# Zonal Stats for ALL Control Points 
stats <- exact_extract(pop_dens_2019_split, 
                       district_buffer_intersection_final_buff, c('sum', 'count', 'mean'))
zonal_stats_all_controls <- cbind(districts, stats) %>%
  as_tibble() 

# Merging Treatment and Control Stats 
treatment_control <- agg_stats_2017_df %>% 
  left_join(buffer_stats_treatment_df,
            by = c("pc11_s_id", "pc11_d_id", "d_name"),
            suffix = c("_district", "_treatment")) %>% 
  select(-geometry_treatment) %>%
  mutate(ppc_treatment = (sum_treatment / sum_district) * 100) %>%
  left_join(zonal_stats_jio_controls,
            by = c("pc11_s_id", "pc11_d_id", "d_name"),
            suffix = c("", "_control_jio")) %>%
  mutate(ppc_control_jio = (sum_control_jio / sum_district) * 100) %>% 
  select(pc11_s_id, pc11_d_id, d_name, sum_district, geometry_district,
         sum_treatment, ppc_treatment, sum_control_jio, 
         ppc_control_jio) %>% 
  rename(c("geom_dist" = "geometry_district", 
           "sum_cont_j" = "sum_control_jio",
           "sum_treat" = "sum_treatment",
           "ppc_treat" = "ppc_treatment",
           "ppc_cont_j" = "ppc_control_jio",
           "sum_dist" = "sum_district")) %>% 
  mutate(t_c_stat = ifelse(ppc_treat > 50, 1, 
                           ifelse(ppc_cont_j < 50, 2, 0))) %>%
  left_join(zonal_stats_all_controls,
            by = c("pc11_s_id", "pc11_d_id", "d_name"),
            suffix = c("", "_control_all")) %>%
  mutate(ppc_control_all = (sum_control_all / sum_dist) * 100) %>% 
  select(pc11_s_id, pc11_d_id, d_name, sum_dist, geom_dist,
         sum_treat, ppc_treat, sum_cont_j, 
         ppc_cont_j, sum_control_all, ppc_control_all, t_c_stat) %>% 
  rename(c("sum_cont_a" = "sum_control_all",
           "ppc_cont_a" = "ppc_control_all")) %>% 
  mutate(t_c_statA = ifelse(ppc_treat > 50, 1, 
                            ifelse(ppc_cont_a < 50, 2, 0)))



# Corrected Version -------------------------------------------------------
## Corrected version: Merging_control_stats_2
treatment_buffers_merged <- buffer_stats_treatment_df %>% 
  left_join(buffer_stats_treatment_all_df,
            by = c("pc11_s_id", "pc11_d_id", "d_name"),
            suffix = c("_tj", "_ta"))

# to check that treatment all sums are higher than treatment jio

control_buffers_merged <- zonal_stats_jio_controls %>% 
  left_join(zonal_stats_all_controls,
            by = c("pc11_s_id", "pc11_d_id", "d_name"),
            suffix = c("_cj", "_ca"))

agg_stats_2017_df <- agg_stats_2017_df %>% 
  rename("sum_17_d" = "sum",
         "mean_17_d" = "mean",
         "count_17_d" = "count",
         "geometry_17_d" = "geometry")

agg_stats_2019_df <- agg_stats_2019_df %>% 
  rename("sum_19_d" = "sum",
         "mean_19_d" = "mean",
         "count_19_d" = "count",
         "geometry_19_d" = "geometry")


treatment <- agg_stats_2017_df %>% 
  left_join(treatment_buffers_merged,
            by = c("pc11_s_id", "pc11_d_id", "d_name")) %>% 
  mutate(ppc_tj = (sum_tj/sum_17_d)*100,
         ppc_ta = (sum_ta/sum_17_d)*100) %>% 
  select(-geometry_tj, -geometry_ta)

control <- agg_stats_2019_df %>% 
  left_join(control_buffers_merged,
            by = c("pc11_s_id", "pc11_d_id", "d_name")) %>%
  mutate(ppc_cj = (sum_cj/sum_19_d)*100,
         ppc_ca = (sum_ca/sum_19_d)*100) %>% 
  select(-geometry_cj, -geometry_ca)

treatment_control <- treatment %>% 
  left_join(control,
            by = c("pc11_s_id", "pc11_d_id", "d_name")) %>% 
  select(-geometry_17_d) %>% 
  rename("geometry" = "geometry_19_d")


treatment_control_shp <- st_as_sf(treatment_control)


st_write(treatment_control_shp, "Data/treatment/treatment_control.shp", 
         delete_layer = TRUE)

write_csv(treatment_control, 'Data/treatment_control/treatment_control.csv')


