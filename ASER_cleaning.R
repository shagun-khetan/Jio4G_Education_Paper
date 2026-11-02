library(tidyverse)
library(naniar)
library(sf)
library(dplyr)
library(ggplot2)
library(vroom)
library(kableExtra)
library(fixest)
library(ggfixest)

####################
# Importing Data
####################

# ASER Data
aser_2008 <- read.table("Data/ASER Household_Village Data 2007-18/ASER 2008/ASER Household2008.txt",
                        sep = ",", header = TRUE)
aser_2010 <- read.table("Data/ASER Household_Village Data 2007-18/ASER 2010/ASER 2010 Household Data.txt",
                        sep = ",", header = TRUE)
aser_2012 <- read.table("Data/ASER Household_Village Data 2007-18/ASER 2012/ASER 2012 Household.txt",
                        sep = ",", quote = '"', header = TRUE) ## Not working
aser_2014 <- read.table("Data/ASER Household_Village Data 2007-18/ASER 2014/ASER 2014 Household Data.txt",
                        sep = ",", header = TRUE)
aser_2016 <- read.table("Data/ASER Household_Village Data 2007-18/ASER 2016/ASER 2016 Household Data.txt",
                        sep = ",", header = TRUE)
aser_2018 <- read.table("Data/ASER Household_Village Data 2007-18/ASER 2018/ASER 2018 Household Data.txt",
                        sep = "\t", header = TRUE)

# District Shapefile
dist_shp <- st_read("Data/India_Shapefile/district.shp")

# Treatment Control Districts
treatment_control <- read_csv("Data/treatment_control/treatment_control.csv") %>% 
  mutate(district_name = str_to_lower(d_name)) %>% select(-d_name)

####################
# Cleaning ASER
####################

aser_2018_clean <- aser_2018 %>% 
  select(state_code, state_name, district_code, district_name, village_code, 
         hh_id, hh_no, total_member, hh_mobile, child_no, child_age, school_class,
         oos_never_enr, oos_dropout, tuition, read_code, math_code,
         vlg_electricity, hh_multiplier) %>% 
  filter(child_age %in% 5:16, !is.na(read_code), !is.na(math_code)) %>% 
  mutate(district_name = str_to_lower(district_name))

aser_2016_clean <- aser_2016 %>% 
  select(state_code, state_name, district_code, district_name, village_code, 
         hh_id, hh_no, total_member, hh_mobile, child_no, child_age, school_class,
         oos_never_enr, oos_dropout, tuition, read_code, math_code,
         vlg_electricity, hh_multiplier) %>% 
  filter(child_age %in% 5:16, !is.na(read_code), !is.na(math_code)) %>% 
  mutate(district_name = str_to_lower(district_name))

aser_2014_clean <- aser_2014 %>% 
  select(state_code, state_name, district_code, district_name, village_code, 
         hh_id, hh_no, total_member, hh_mobile, child_no, child_age, school_class,
         oos_never_enr, oos_dropout, tuition, read_code, math_code,
         vlg_electricity, hh_multiplier) %>% 
  filter(child_age %in% 5:16, !is.na(read_code), !is.na(math_code)) %>% 
  mutate(district_name = str_to_lower(district_name))

aser_2012_clean <- aser_2012 %>% 
  mutate(across(
    c(read_nothing, read_letter, read_word, read_level_1, read_level_2,
      math_nothing, math_num_1_9, math_num_10_99, math_subtraction, math_division),
    ~ replace_na(as.numeric(.), 0)
  )) %>% 
  mutate(
      read_code =
        read_nothing  * 1 +
        read_letter   * 2 +
        read_word     * 3 +
        read_level_1  * 4 +
        read_level_2  * 5,
      math_code  =
        math_nothing     * 1 +
        math_num_1_9     * 2 +
        math_num_10_99   * 3 +
        math_subtraction * 4 +
        math_division    * 5
  ) %>% 
  select(state_code, state_name, district_code, district_name, village_code, 
         hh_id, hh_no, total_member, hh_mobile, child_no, child_age, school_class,
         oos_never_enr, oos_dropout, tuition, read_code, math_code,
         vlg_electricity, hh_multiplier) %>% 
  filter(child_age %in% 5:16, !is.na(read_code), !is.na(math_code), 
         read_code > 0, math_code > 0) %>% 
  mutate(district_name = str_to_lower(district_name))

aser_2010_clean <- aser_2010 %>% 
  mutate(across(
    c(read_nothing, read_letter, read_word, read_level_1, read_level_2,
      math_nothing, math_num_1_9, math_num_10_99, math_subtraction, math_division),
    ~ replace_na(as.numeric(.), 0)
  )) %>% 
  mutate(
    read_code =
      read_nothing  * 1 +
      read_letter   * 2 +
      read_word     * 3 +
      read_level_1  * 4 +
      read_level_2  * 5,
    math_code  =
      math_nothing     * 1 +
      math_num_1_9     * 2 +
      math_num_10_99   * 3 +
      math_subtraction * 4 +
      math_division    * 5
  ) %>% 
  select(state_code, state_name, district_code, district_name, village_code, 
         hh_id, hh_no, total_member, hh_mobile_yes, child_no, child_age, school_class,
         oos_never_enr, oos_dropout, tuition_yes, read_code, math_code,
         vlg_electricity, hh_multiplier) %>% 
  filter(child_age %in% 5:16, !is.na(read_code), !is.na(math_code), 
         read_code > 0, math_code > 0) %>% 
  mutate(district_name = str_to_lower(district_name)) %>% 
  rename(hh_mobile = hh_mobile_yes,
    tuition = tuition_yes
  )

aser_2018_clean$oos_never_enr <- ifelse(is.na(aser_2018_clean$oos_never_enr),
                                        0, aser_2018_clean$oos_never_enr)
aser_2018_clean$oos_dropout <- ifelse(is.na(aser_2018_clean$oos_dropout),
                                        0, aser_2018_clean$oos_dropout)
aser_2016_clean$oos_never_enr <- ifelse(is.na(aser_2016_clean$oos_never_enr),
                                        0, aser_2016_clean$oos_never_enr)
aser_2016_clean$oos_dropout <- ifelse(is.na(aser_2016_clean$oos_dropout),
                                      0, aser_2016_clean$oos_dropout)
aser_2014_clean$oos_never_enr <- ifelse(is.na(aser_2014_clean$oos_never_enr),
                                        0, aser_2014_clean$oos_never_enr)
aser_2014_clean$oos_dropout <- ifelse(is.na(aser_2014_clean$oos_dropout),
                                      0, aser_2014_clean$oos_dropout)
aser_2012_clean$oos_never_enr <- ifelse(is.na(aser_2012_clean$oos_never_enr),
                                        0, aser_2012_clean$oos_never_enr)
aser_2012_clean$oos_dropout <- ifelse(is.na(aser_2012_clean$oos_dropout),
                                      0, aser_2012_clean$oos_dropout)
aser_2010_clean$oos_never_enr <- ifelse(is.na(aser_2010_clean$oos_never_enr),
                                        0, aser_2010_clean$oos_never_enr)
aser_2010_clean$oos_dropout <- ifelse(is.na(aser_2010_clean$oos_dropout),
                                      0, aser_2010_clean$oos_dropout)

aser_names <- c("aser_2010_clean", "aser_2012_clean", "aser_2014_clean", 
                "aser_2016_clean", "aser_2018_clean")

agg_list <- list()


for(i in aser_names){
    df <- get(i)
    df_agg <- df %>%
      mutate(tuition = ifelse(tuition == 2, 0, tuition), 
             hh_mobile = ifelse(hh_mobile == 2, 0, hh_mobile)) %>%
    group_by(state_code, state_name, district_code, district_name) %>% 
    summarise(
      read_code = weighted.mean(read_code, hh_multiplier, na.rm = TRUE),
      math_code = weighted.mean(math_code, hh_multiplier, na.rm = TRUE),
      oos_rate = sum((oos_never_enr + oos_dropout) * hh_multiplier) / sum(hh_multiplier),
      tuition = weighted.mean(tuition, hh_multiplier, na.rm = TRUE),
      hh_size = weighted.mean(total_member, hh_multiplier, na.rm = TRUE),
      age = weighted.mean(child_age, hh_multiplier, na.rm = TRUE),
      mobile = weighted.mean(hh_mobile, hh_multiplier, na.rm = TRUE),
      class = weighted.mean(school_class, hh_multiplier, na.rm = TRUE)
    ) %>% ungroup()
    
    agg_list[[i]] <- df_agg   # store in the list
}

####################
# Panel Aggregation
####################

treatment_control$pc11_d_id <- as.numeric(treatment_control$pc11_d_id)

aser_panel <- bind_rows(
  agg_list[[1]]  %>% mutate(year = 2010),
  agg_list[[2]]  %>% mutate(year = 2012),
  agg_list[[3]]  %>% mutate(year = 2014),
  agg_list[[4]]  %>% mutate(year = 2016),
  agg_list[[5]]  %>% mutate(year = 2018)
) %>% 
  mutate(
    district_code = as.character(district_code),       # match ID type
    district_name = str_to_lower(district_name)
  )

clean_name <- function(x) {
  x %>% 
    str_to_lower() %>%                 # lowercase
    str_replace_all('"', "") %>%       # remove double-quotes
    str_replace_all("[*]", "") %>%     # remove asterisks
    str_squish()                     # remove extra spaces
}


recode_vec <- c(
  "barabanki"     = "bara banki",
  "bulandshahar"  = "bulandshahr",
  "east nimar"    = "khandwa (east nimar)",
  "janjgir champa" = "janjgir - champa",
  "janjgir_champa" = "janjgir - champa",
  "kanker" = "uttar bastar kanker",
  "leh (ladakh)" = "leh(ladakh)",
  "logleng" = "longleng",
  "cuddapah" = "y.s.r.",
  "medinipur" = "paschim medinipur",
  "medinipur" = "purba medinipur",
  "marigaon" = "morigaon",
  "hathras" = "mahamaya nagar",
  "nawanshahr" = "shahid bhagat singh nagar",
  "nellore" = "sri potti sriramulu nellore",
  "north cachar hills" = "dima hasao",
  "north district" = "north  district",
  "pakaur" = "pakur",
  "pondicherry" = "puducherry",
  "rajauri" = "rajouri",
  "rangareddi" = "rangareddy",
  "ri bhoi" = "ribhoi",
  "sant ravidas nagar bhadohi" = "sant ravidas nagar (bhadohi)",
  "sibsagar" = "sivasagar",
  "sindega" = "simdega",
  "sonapur" = "subarnapur",
  "ssa nagar" = "sahibzada ajit singh nagar",
  "tarn-taran" = "tarn taran",
  "west nimar" = "khargone (west nimar)",
  "kawardha" = "kabeerdham",
  "south" = "south district",
  "north" = "north  district",
  "west" = "west district",
  "east" = "east district"
)

recode_vec_state <- c(
  "arunachal pardesh" = "arunachal pradesh",
  "orissa"            = "odisha",
  "tamilnadu"         = "tamil nadu",
  "pondicherry"       = "puducherry",
  "up"                = "uttar pradesh",
  "uttaranchal"       = "uttarakhand"
)

aser_panel_final <- aser_panel %>%
  mutate(district_name = clean_name(district_name),
         state_name    = clean_name(state_name)) %>% 
  mutate(district_name = recode(district_name, !!!recode_vec),
         state_name = recode(state_name, !!!recode_vec_state)) %>% 
  mutate(
    state_name = ifelse(state_name == "telangana", "andhra pradesh", state_name),
    state_code = ifelse(state_name == "andhra pradesh", 28, state_code)
  ) %>% 
  arrange(state_name, district_name, year)

# aser_panel_final %>% select(state_name, state_code) %>% unique() %>% View()

treatment_control <- treatment_control %>% mutate(pc11_s_id = as.numeric(pc11_s_id))

## Merging Nightlights Data
nightlights <- read_csv('Data/nightlights/nightlights_dist.csv')
nightlights_clean <- nightlights %>% filter(category == "average-masked") %>% 
  select(pc11_district_id, pc11_state_id, year, viirs_annual_mean) %>% 
  rename("mean_nightlight" = "viirs_annual_mean", "state_code" = "pc11_state_id", 
         "district_code" = "pc11_district_id") %>% 
  mutate(district_code = as.numeric(district_code),
         state_code = as.numeric(state_code))

## Dropping 1 unmerged district (saraikela)
aser_panel_tc <- aser_panel_final %>% 
  left_join(treatment_control, by = c("district_name", "state_code" = "pc11_s_id")) %>%
  filter(!is.na(sum_cj)) %>%
  select(district_name, year, everything()) %>% 
  mutate(
    t_c_statA = ifelse(ppc_tj > 50, 1,
                       ifelse(ppc_ca < 50, 2, 0)),
    ta_c_statA = ifelse(ppc_ta > 50, 1,
                       ifelse(ppc_ca < 50, 2, 0)),
    t_c_stat = ifelse(ppc_tj > 50, 1,
                      ifelse(ppc_cj < 50, 2, 0))
  ) %>%
  mutate(district_code = as.numeric(district_code)) %>% 
  left_join(nightlights_clean,
              by = c("year", "district_code", "state_code"))

############################
# Summary Statistics Basic
############################

vars <- c("read_code","math_code","oos_rate",
          "tuition","hh_size","age",
          "mobile","class", "mean_nightlight", "ppc_tj", "ppc_ca")

var_labels <- c(
  read_code = "Reading Score",
  math_code = "Math Score",
  oos_rate  = "Drop/Non-Enroll Rate",
  tuition   = "Tuition Rate",
  hh_size   = "Household Size",
  age       = "Age",
  mobile    = "Mobile Ownership",
  class     = "School Class",
  mean_nightlight = "Nighlights",
  ppc_tj = "Population Under 4G (2016 %)",
  ppc_ca = "Population Under 4G (2018 %)"
)

summary_basic <- aser_panel_tc %>%
  summarise(across(
    all_of(vars),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      SD   = ~sd(.x, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      P25 = ~quantile(.x, 0.25, na.rm = TRUE),
      P75 = ~quantile(.x, 0.75, na.rm = TRUE),
      Min  = ~min(.x, na.rm = TRUE),
      Max  = ~max(.x, na.rm = TRUE),
      N    = ~sum(!is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  ))

stats_table <- summary_basic %>%
  pivot_longer(
    everything(),
    names_to = c("Variable",".value"),
    names_sep = "__"
  ) %>% 
  mutate(Variable = var_labels[Variable])

latex_table <- kable(stats_table,
                     digits = 3,
                     caption = "Summary Statistics",
                     format = "latex",
                     booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

writeLines(latex_table, "Data/summary_statistics.tex")

############################
# Variable Descriptions
############################

var_desc <- tribble(
  ~Variable,   ~Description,                                         ~Unit,
  "read_code", "ASER Reading achievement score",                           "0–5 scale",
  "math_code", "ASER Math achievement score",                              "0–5 scale",
  "oos_rate",  "Out-of-school rate (Not Enrolled + Dropout)",             "Share (0–1)",
  "tuition",   "Private Tuition Rate",                                "Share(0-1)",
  "hh_size",   "Household size",                                       "Count",
  "age",       "Child Age",                                            "Years",
  "mobile",    "Mobile Ownership",                                     "Share (0–1)",
  "class",     "School Class",                                      "Grade (1–10)"
)

var_desc_table <- kable(var_desc, "latex", booktabs=TRUE) %>% kable_styling()

# writeLines(var_desc_table, "Data/var_desc.tex")

############################
# Event Study Plots
############################

estimation_df <- aser_panel_tc %>% filter(t_c_statA %in% c(1,2)) %>% 
  mutate(event_time = year - 2016,
         treat = ifelse(t_c_statA == 2, 0, 1))

es_read <- feols(read_code ~ i(event_time, treat, ref = -2) + mobile + hh_size + class + oos_rate + tuition| year + state_code,
                   data = estimation_df,
                   cluster = ~state_code)

es_math <- feols(math_code ~ i(event_time, treat, ref = -2) + mobile + hh_size + class + oos_rate + tuition| year + state_code,
                 data = estimation_df,
                 cluster = ~state_code)

ggiplot(
  es_read,
  col = "darkblue",
  geom_style = "errorbar",
  pt.pch = 19,
  pt.size = 1.5,
  ci.lwd = 0.8,
  ci_level = 0.99
) +
  labs(
    title = "",
    x = "Time Relative to Jio Shock (t = 0)",
    y = "Reading Score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

ggiplot(
  es_math,
  col = "darkblue",
  geom_style = "errorbar",
  pt.pch = 19,
  pt.size = 1.5,
  ci.lwd = 0.8,
  ci_level = 0.99
) +
  labs(
    title = "",
    x = "Time Relative to Jio Shock (t = 0)",
    y = "Math Score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

############################
# Plots
############################

## Data Prices and Usage (TRAI)

gb_price_graph <- tribble(
  ~year,   ~gb_price, ~usage,                                      
  2014, 268.97, 3.18,                    
  2015, 226.30, 4.77,
  2016, 75.57, 14.16,
  2017, 19.35, 49.59,
  2018, 11.78, 92.29
)

ggplot(gb_price_graph, aes(x = year)) +
  geom_col(aes(y = usage * 2.5),
           width = 0.6,
           fill = "darkblue",
           alpha = 0.25) +  
  geom_line(aes(y = gb_price),
            color = "darkblue",
            linewidth = 1.2) +
  scale_y_continuous(
    name = "Data Price Per GB (INR)",
    sec.axis = sec_axis(~ . / 2.5, name = "Average Data Usage (GB)")
  ) +
  labs(
    title = "",
    x = "Year"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left  = element_text(color = "black"),
    legend.position = "none"
  )


## Exporting the panel to a csv file without the polygon column
write_csv(aser_panel_tc %>% select(-geometry), 'Data/final_panel.csv')
aser_panel_tc$state_code <- as.character(aser_panel_tc$state_code)
aser_panel_tc$district_code <- as.character(aser_panel_tc$district_code)

export_shapefile <- dist_shp %>% 
  left_join(aser_panel_tc %>% select(-geometry), by = c("pc11_s_id" = "state_code", "pc11_d_id" = "district_code"))
export_shapefile_shp <- st_as_sf(export_shapefile)

st_write(export_shapefile_shp, "Data/treatment_control/aser_panel_shapefile.shp", 
         delete_layer = TRUE)



