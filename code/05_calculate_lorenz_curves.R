## 05_calculate_lorenz_curves.R ----
## Using the analytic data set, calculate different distributions of student-
## days of wildfire exposure using the Lorenz curve.

## Imports ----
library(tidyverse)
library(here)
library(foreach)
library(doParallel)
library(ineq)
library(config)
source(here::here("code", "utils.R"))

## Constants ----
N_CORE <- config::get("n_cores")

## Data ----
analytic_df <- readRDS(here::here("data", "analytic_data_long.RDS"))

## Helper functions ----
summarize_data <- function(df) {
    df %>%
        dplyr::summarize(
            school_type = dplyr::first(school_type),
            school_level = dplyr::first(school_level),
            n_free_lunch_eligible = mean(n_free_lunch_eligible, na.rm = TRUE),
            n_free_reduced_lunch = mean(n_free_reduced_lunch, na.rm = TRUE),
            latitude = mean(latitude),
            longitude = mean(longitude),
            county_name = dplyr::first(county_name),
            fipschar = dplyr::first(fipschar),
            n_obs = dplyr::n(),
            total_students = sum(n_students, na.rm = TRUE),
            value = sum(value, na.rm = TRUE),
            sdays_value = sum(sdays_value, na.rm = TRUE),
            mean_value = mean(value, na.rm = TRUE),
            mean_sdays_value = mean(sdays_value, na.rm = TRUE)
        ) %>%
        dplyr::ungroup()
}

## Aggregate data to different levels ----
## We need to remove the 2005-2006 SY because we only have exposure data
## for half of it. Also remove small racial/ethnic groups.
analytic_df <- analytic_df %>%
    dplyr::filter(grepl("gt", metric)) %>%
    categorize_school_year() %>%
    categorize_race() %>%
    dplyr::mutate(sdays_value = n_students * value) %>%
    dplyr::filter(race_recode != "multiracial",
                  race_recode != "hopi") %>%
    dplyr::filter(school_year != "2005_06")

### School * race ----
agg_school_race <- analytic_df %>%
    dplyr::group_by(
        school_name,
        nces_id,
        school_year,
        race_recode,
        year_cat,
        year_cat_rev,
        race_cat,
        race_cat_rev,
        metric
    ) %>%
    summarize_data()

## Create a parameter grid to iterate over ----
param_grid <- agg_school_race %>%
    dplyr::select(school_year, race_recode, metric) %>%
    dplyr::distinct()

## Student days values ----
doParallel::registerDoParallel(N_CORES)
holder_sdays_value <-
    foreach::foreach(i = 1:NROW(param_grid), .inorder = FALSE) %dopar% {
        year_x <- param_grid$school_year[i]
        race_x <- param_grid$race_recode[i]
        metr_x <- param_grid$metric[i]
        
        sub_df <- agg_school_race %>%
            dplyr::filter(race_recode == race_x,
                          school_year == year_x,
                          metric == metr_x)
        
        calculate_lorenz_curve(sub_df$sdays_value) %>%
            dplyr::mutate(race_recode = race_x,
                          school_year = year_x,
                          metric = metr_x)
    }
doParallel::stopImplicitCluster()
closeAllConnections()

## Just raw values (unweighted) ----
doParallel::registerDoParallel(N_CORES)
holder_raw_value <-
    foreach::foreach(i = 1:NROW(param_grid), .inorder = FALSE) %dopar% {
        year_x <- param_grid$school_year[i]
        race_x <- param_grid$race_recode[i]
        metr_x <- param_grid$metric[i]
        
        sub_df <- agg_school_race %>%
            dplyr::filter(race_recode == race_x,
                          school_year == year_x,
                          metric == metr_x)
        
        calculate_lorenz_curve(sub_df$value) %>%
            dplyr::mutate(race_recode = race_x,
                          school_year = year_x,
                          metric = metr_x)
    }
doParallel::stopImplicitCluster()
closeAllConnections()

## Bind and save ----
lorenz_sdays <- dplyr::bind_rows(holder_sdays_value)
lorenz_raw <- dplyr::bind_rows(holder_raw_value)
saveRDS(lorenz_sdays,
        here::here("data", "lorenz_studentdays.RDS"),
        compress = "xz")
saveRDS(lorenz_raw, here::here("data", "lorenz_raw.RDS"), compress = "xz")
