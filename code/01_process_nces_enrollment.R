## 01_process_nces_enrollment.R ----
## Take the raw NCES data pulls, ingest them, process and reshape them,
## then save all of them in a file to debug later and a subset for analysis.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(janitor)
library(future)
library(furrr)
library(config)
library(doParallel)
source(here::here("code", "utils.R"))

## Constants ----
N_CORE <- config::get("n_cores")

## List all NCES raw data pulls ----
nces_files <- fs::dir_ls(here::here("data_raw", "nces"),
                         recurse = TRUE,
                         glob = "*.csv")

## Remove the school characteristics files ----
enrollment_files <- nces_files[!grepl("raw_pulls_school_char",
                                      nces_files,
                                      fixed = TRUE)]

## ETL ----
## Read in the file, clean column names, remove weird chars from rows,
## reshape, and append
future::plan(future::multisession(workers = N_CORE))
school_enrollment <- furrr::future_map_dfr(
    .x = enrollment_files,
    .f = ~ {
        ## I'm not 100% sure we always skip the same number of lines, so this
        ## little helper will load the file and take a guess.
        skip_ix <-
            find_skip_lines(.x)
        temp_x <-
            readr::read_csv(.x,
                            skip = skip_ix,
                            na = c("†", "", "NA", "-", "–")) %>%
            janitor::clean_names() %>%
            dplyr::mutate_all(function(x)
                gsub('"|=', "", x)) %>%
            rename_cols() %>%
            dplyr::mutate(state = stringr::str_to_title(state))
        
        ## Reshape to key:value pairs
        temp_x <- temp_x %>%
            tidyr::pivot_longer(cols = names(temp_x)[4:NCOL(temp_x)]) %>%
            dplyr::mutate(value = as.numeric(value)) %>%
            dplyr::filter(!is.na(value)) %>%
            rename(n_students = value)
        
        ## Now reshape and parse the value column to extract grade, race,
        ## and school year
        temp_x %>%
            tidyr::separate(
                into = c("grade", "race", "school_year"),
                remove = FALSE,
                sep = "_students_|_nat_|_public_school_|public_school_",
                col = name
            ) %>%
            recode_race() %>%
            recode_gender() %>%
            recode_grade() %>%
            recode_year()
    }
)

## Close connections ----
doParallel::stopImplicitCluster()
closeAllConnections()

## Save this gigantic file just for debugging purposes
# saveRDS(school_enrollment,
#         here::here("data", "full_nces_debugging.RDS"),
#         compress = "xz")

## Save a subset for the project ----
saveRDS(
    school_enrollment %>%
        dplyr::select(-name,
                      -race) %>%
        dplyr::filter(state == "California",
                      n_students > 0,
                      gender == "all") %>%
        dplyr::arrange(nces_id, gender, race_recode, year, grade),
    here::here("data", "nces_enrollment_data.RDS"),
    compress = "xz"
)
