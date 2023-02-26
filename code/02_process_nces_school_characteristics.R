## 02_process_nces_school_characteristics.R ----
## Take the raw NCES data pulls, ingest them, process and reshape them,
## then save all of them in a file to debug later and a subset for analysis.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(janitor)
library(future)
library(furrr)
source(here::here("code", "utils.R"))

## Constants ----
N_CORE <- config::get("n_cores")

## Data ----
seda_df <- readr::read_csv(
    here::here("data_raw", "seda", "seda_crosswalk_4.1.csv.zip")
)

### Import and munge NCES data ----
#### List all NCES raw data pulls ----
nces_files <- fs::dir_ls(here::here("data_raw", "nces"),
                         recurse = TRUE,
                         glob = "*.csv")

#### Just school characteristics files ----
char_files <- nces_files[grepl("raw_pulls_school_char",
                               nces_files,
                               fixed = TRUE)]

### Read in the file, clean column names, remove weird chars from rows, ----
### reshape, and append
future::plan(future::multisession)
school_chars <- furrr::future_map_dfr(
    .x = char_files,
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
            dplyr::filter(!is.na(value))
        
        ## Now reshape and parse the value column to extract grade, race,
        ## and school year
        temp_x %>%
            tidyr::separate(
                into = c("school_char", "school_year"),
                sep = "_public_school_|_district_",
                col = name,
                remove = FALSE
            ) %>%
            recode_year()
    }
)

## Close connections ----
doParallel::stopImplicitCluster()
closeAllConnections()

## Reshape it back into wide now that we have {year, school} keys ----
school_wide <- school_chars %>%
    tidyr::pivot_wider(
        id_cols = c(school_name, state, nces_id, year, school_year),
        names_from = school_char,
        values_from = value
    ) %>%
    dplyr::arrange(nces_id, year)

## Subset to what we'll want  ----
sub_school_chars <- school_wide %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::transmute(
        school_name,
        state,
        nces_id,
        nces_num = as.numeric(nces_id),
        year = as.integer(year),
        school_year,
        school_type,
        school_level,
        school_status = start_of_year_status,
        virtual_school_status,
        n_free_lunch_eligible = as.numeric(free_lunch_eligible),
        n_reduced_lunch_eligible = as.numeric(reduced_price_lunch_eligible_students),
        n_free_reduced_lunch =  as.numeric(free_and_reduced_lunch_students),
        # lunch_program = national_school_lunch_program,
        # title_i_eligible = title_i_eligible_school,
        # title_i_status = title_i_school_status,
        teacher_ftes = as.numeric(full_time_equivalent_fte_teachers),
        student_teacher_ratio = as.numeric(pupil_teacher_ratio),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude),
        county_name = gsub(" County", "", stringr::str_to_title(county_name)),
        county_fips = county_number,
        address_street = stringr::str_to_title(location_address_1),
        city = stringr::str_to_title(location_city),
        usps_zip = as.numeric(location_zip)
    )

## Add in SEDA crosswalk information ----
sub_school_chars <- sub_school_chars %>%
    dplyr::left_join(
        seda_df %>%
            dplyr::transmute(
                seda_name = sedaschname,
                seda_id = sedasch,
                nces_num = ncessch,
                year,
                virtual,
                charter,
                magnet,
                seda_school_status = status
            ) %>%
            dplyr::distinct()
    )

## Save ----
saveRDS(
    sub_school_chars,
    here::here("data", "nces_school_characteristics.RDS"),
    compress = "xz"
)
