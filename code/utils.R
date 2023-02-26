## utils.R ----
library(tidyverse)
library(ineq)

## Helper functions ----
find_skip_lines <- function(f_path) {
    x <- readr::read_lines(f_path)
    which(substr(x, 1, 6) == "School") - 1
}

rename_cols <- function(df) {
    df %>%
        dplyr::rename(state = state_name_public_school_latest_available_year,
                      nces_id = school_id_nces_assigned_public_school_latest_available_year)
}

recode_race <- function(df) {
    ## Be very explicit about how we are recoding race/ethnicity
    df %>%
        dplyr::mutate(
            race_recode = dplyr::case_when(
                race == "" ~ "all",
                race == "american_indian_alaska_native" ~ "aian",
                race == "american_indian_alaska_native_female" ~ "aian",
                race == "american_indian_alaska_native_male" ~ "aian",
                race == "asian_or_asian_pacific_islander" ~ "api",
                race == "asian_or_asian_pacific_islander_female" ~ "api",
                race == "asian_or_asian_pacific_islander_male" ~ "api",
                race == "black_or_african_american" ~ "black",
                race == "black_or_african_american_female" ~ "black",
                race == "black_or_african_american_male" ~ "black",
                race == "hawaiian_or_other_pacific_isl" ~ "hopi",
                race == "hispanic" ~ "hispanic",
                race == "hispanic_female" ~ "hispanic",
                race == "hispanic_male" ~ "hispanic",
                race == "nat_hawaiian_or_other_pacific_isl" ~ "hopi",
                race == "nat_hawaiian_or_other_pacific_isl_female" ~ "hopi",
                race == "nat_hawaiian_or_other_pacific_isl_male" ~ "hopi",
                race == "two_or_more_races" ~ "multiracial",
                race == "two_or_more_races_female" ~ "multiracial",
                race == "two_or_more_races_male" ~ "multiracial",
                race == "white" ~ "white",
                race == "white_female" ~ "white",
                race == "white_male" ~ "white"
            )
        )
}

recode_gender <- function(df) {
    ## Be very explicit about how we are recoding race/ethnicity
    df %>%
        dplyr::mutate(
            gender = dplyr::case_when(
                race == "" ~ "all",
                race == "american_indian_alaska_native" ~ "all",
                race == "american_indian_alaska_native_female" ~ "female",
                race == "american_indian_alaska_native_male" ~ "male",
                race == "asian_or_asian_pacific_islander" ~ "all",
                race == "asian_or_asian_pacific_islander_female" ~ "female",
                race == "asian_or_asian_pacific_islander_male" ~ "male",
                race == "black_or_african_american" ~ "all",
                race == "black_or_african_american_female" ~ "female",
                race == "black_or_african_american_male" ~ "male",
                race == "hawaiian_or_other_pacific_isl" ~ "all",
                race == "hispanic" ~ "all",
                race == "hispanic_female" ~ "female",
                race == "hispanic_male" ~ "male",
                race == "nat_hawaiian_or_other_pacific_isl" ~ "all",
                race == "nat_hawaiian_or_other_pacific_isl_female" ~ "female",
                race == "nat_hawaiian_or_other_pacific_isl_male" ~ "male",
                race == "two_or_more_races" ~ "all",
                race == "two_or_more_races_female" ~ "female",
                race == "two_or_more_races_male" ~ "male",
                race == "white" ~ "all",
                race == "white_female" ~ "female",
                race == "white_male" ~ "male"
            )
        )
}

recode_grade <- function(df) {
    ## Be very explicit about how we are recoding grades
    df %>%
        dplyr::mutate(
            grade = dplyr::case_when(
                grade == "prekindergarten" ~ -1,
                grade == "kindergarten" ~ 0,
                grade == "grade_1" ~ 1,
                grade == "grade_2" ~ 2,
                grade == "grade_3" ~ 3,
                grade == "grade_4" ~ 4,
                grade == "grade_5" ~ 5,
                grade == "grade_6" ~ 6,
                grade == "grade_7" ~ 7,
                grade == "grade_8" ~ 8,
                grade == "grade_9" ~ 9,
                grade == "grade_10" ~ 10,
                grade == "grade_11" ~ 11,
                grade == "grade_12" ~ 12,
                grade == "grade_13" ~ 13,
                grade == "adult_educat" ~ 14,
                grade == "ungraded" ~ NA_real_
            )
        )
}

categorize_race <- function(df) {
    df %>%
        dplyr::mutate(race_cat = factor(
            race_recode,
            levels = c("hispanic",
                       "aian",
                       "api",
                       "black",
                       "hopi",
                       "white",
                       "all"),
            labels = c(
                "Hispanic",
                "NH American Indian\nor Alaska Native",
                "NH Asian",
                "NH Black",
                "NH Hawaiian or Other Pacific Islander",
                "NH White",
                "All"
            ),
            ordered = TRUE
        )) %>%
        dplyr::mutate(race_cat_rev = factor(
            race_recode,
            levels = rev(
                c("hispanic",
                  "aian",
                  "api",
                  "black",
                  "hopi",
                  "white",
                  "all")
            ),
            labels = rev(
                c(
                    "Hispanic",
                    "NH American Indian\nor Alaska Native",
                    "NH Asian",
                    "NH Black",
                    "NH Hawaiian or\nOther Pacific Islander",
                    "NH White",
                    "All"
                )
            ),
            ordered = TRUE
        ))
}

categorize_school_year <- function(df) {
    df %>%
        dplyr::mutate(year_cat = factor(
            school_year,
            levels = c(
                "2005_06",
                "2006_07",
                "2007_08",
                "2008_09",
                "2009_10",
                "2010_11",
                "2011_12",
                "2012_13",
                "2013_14",
                "2014_15",
                "2015_16",
                "2016_17",
                "2017_18",
                "2018_19",
                "2019_20",
                "2020_21"
            ),
            labels = c(
                "'05-'06",
                "'06-'07",
                "'07-'08",
                "'08-'09",
                "'09-'10",
                "'10-'11",
                "'11-'12",
                "'12-'13",
                "'13-'14",
                "'14-'15",
                "'15-'16",
                "'16-'17",
                "'17-'18",
                "'18-'19",
                "'19-'20",
                "'20-'21"
            ),
            ordered = TRUE
        )) %>%
        dplyr::mutate(year_cat_rev = factor(
            school_year,
            levels = rev(
                c(
                    "2005_06",
                    "2006_07",
                    "2007_08",
                    "2008_09",
                    "2009_10",
                    "2010_11",
                    "2011_12",
                    "2012_13",
                    "2013_14",
                    "2014_15",
                    "2015_16",
                    "2016_17",
                    "2017_18",
                    "2018_19",
                    "2019_20",
                    "2020_21"
                )
            ),
            labels = rev(
                c(
                    "'05-'06",
                    "'06-'07",
                    "'07-'08",
                    "'08-'09",
                    "'09-'10",
                    "'10-'11",
                    "'11-'12",
                    "'12-'13",
                    "'13-'14",
                    "'14-'15",
                    "'15-'16",
                    "'16-'17",
                    "'17-'18",
                    "'18-'19",
                    "'19-'20",
                    "'20-'21"
                )
            ),
            ordered = TRUE
        ))
}

recode_year <- function(df) {
    df %>%
        dplyr::mutate(year = as.integer(substr(school_year, 1, 4)))
}

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
            total_sdays_wf25 = sum(sdays_wf25, na.rm = TRUE),
            total_sdays_pm25 = sum(sdays_pm25, na.rm = TRUE),
            mean_sdays_wf25 = mean(sdays_wf25, na.rm = TRUE),
            mean_sdays_pm25 = mean(sdays_pm25, na.rm = TRUE)
        ) %>%
        dplyr::ungroup()
}

## Lorenz ----
#' Given a vector x, return the lorenz curve and generalized lorenz curve
#'
#' @param x numeric vector of positive values
#' @param length_out how long the lorenz curve should be (resolution)
#'
#' @return a dataframe with lorenz curves of specified length (or shorter)
calculate_lorenz_curve <- function(x, length_out = 25000) {
    ## ineq::Lc() returns a vector of length(x + 1), which is generally
    ## far larger than we need. Instead, I will just take every nth estimate
    ## such that the vector will be no more than length_out elements.
    
    keep_bool <- TRUE
    
    if (NROW(x) < 10) {
        dplyr::tibble(p = NA,
                      Lp = NA,
                      Lgen = NA)
    } else if (NROW(x) > length_out) {
        keep_bool <- as.integer(round(c(
            1, seq(0L, NROW(x),
                   length.out = length_out + 1)[-1]
        )))
    }
    
    temp_x <- ineq::Lc(x)
    dplyr::tibble(p = temp_x$p[keep_bool],
                  Lp = temp_x$L[keep_bool],
                  Lgen = temp_x$L.general[keep_bool]) %>%
        dplyr::filter(!is.na(p))
}

## Aux data ----
return_st_fips_map <- function() {
    structure(
        list(
            name = c(
                "Alabama",
                "Alaska",
                "American Samoa",
                "Arizona",
                "Arkansas",
                "California",
                "Colorado",
                "Connecticut",
                "Delaware",
                "District of Columbia",
                "Florida",
                "Federated States of Micronesia",
                "Georgia",
                "Guam",
                "Hawaii",
                "Idaho",
                "Illinois",
                "Indiana",
                "Iowa",
                "Kansas",
                "Kentucky",
                "Louisiana",
                "Maine",
                "Marshall Islands",
                "Maryland",
                "Massachusetts",
                "Michigan",
                "Minnesota",
                "Mississippi",
                "Missouri",
                "Montana",
                "Nebraska",
                "Nevada",
                "New Hampshire",
                "New Jersey",
                "New Mexico",
                "New York",
                "North Carolina",
                "North Dakota",
                "Northern Mariana Islands",
                "Ohio",
                "Oklahoma",
                "Oregon",
                "Palau",
                "Pennsylvania",
                "Puerto Rico",
                "Rhode Island",
                "South Carolina",
                "South Dakota",
                "Tennessee",
                "Texas",
                "U.S. Minor Outlying Islands",
                "Utah",
                "Vermont",
                "Virginia",
                "Virgin Islands of the U.S.",
                "Washington",
                "West Virginia",
                "Wisconsin",
                "Wyoming"
            ),
            abbrev = c(
                "AL",
                "AK",
                "AS",
                "AZ",
                "AR",
                "CA",
                "CO",
                "CT",
                "DE",
                "DC",
                "FL",
                "FM",
                "GA",
                "GU",
                "HI",
                "ID",
                "IL",
                "IN",
                "IA",
                "KS",
                "KY",
                "LA",
                "ME",
                "MH",
                "MD",
                "MA",
                "MI",
                "MN",
                "MS",
                "MO",
                "MT",
                "NE",
                "NV",
                "NH",
                "NJ",
                "NM",
                "NY",
                "NC",
                "ND",
                "MP",
                "OH",
                "OK",
                "OR",
                "PW",
                "PA",
                "PR",
                "RI",
                "SC",
                "SD",
                "TN",
                "TX",
                "UM",
                "UT",
                "VT",
                "VA",
                "VI",
                "WA",
                "WV",
                "WI",
                "WY"
            ),
            fips = c(
                1,
                2,
                60,
                4,
                5,
                6,
                8,
                9,
                10,
                11,
                12,
                64,
                13,
                66,
                15,
                16,
                17,
                18,
                19,
                20,
                21,
                22,
                23,
                68,
                24,
                25,
                26,
                27,
                28,
                29,
                30,
                31,
                32,
                33,
                34,
                35,
                36,
                37,
                38,
                69,
                39,
                40,
                41,
                70,
                42,
                72,
                44,
                45,
                46,
                47,
                48,
                74,
                49,
                50,
                51,
                78,
                53,
                54,
                55,
                56
            ),
            nchs = c(
                1,
                2,
                62,
                3,
                4,
                5,
                6,
                7,
                8,
                9,
                10,
                NA,
                11,
                54,
                12,
                13,
                14,
                15,
                16,
                17,
                18,
                19,
                20,
                NA,
                21,
                22,
                23,
                24,
                25,
                26,
                27,
                28,
                29,
                30,
                31,
                32,
                33,
                34,
                35,
                62,
                36,
                37,
                38,
                NA,
                39,
                52,
                40,
                41,
                42,
                43,
                44,
                NA,
                45,
                46,
                47,
                53,
                48,
                49,
                50,
                51
            ),
            fipschar = c(
                "01",
                "02",
                "60",
                "04",
                "05",
                "06",
                "08",
                "09",
                "10",
                "11",
                "12",
                "64",
                "13",
                "66",
                "15",
                "16",
                "17",
                "18",
                "19",
                "20",
                "21",
                "22",
                "23",
                "68",
                "24",
                "25",
                "26",
                "27",
                "28",
                "29",
                "30",
                "31",
                "32",
                "33",
                "34",
                "35",
                "36",
                "37",
                "38",
                "69",
                "39",
                "40",
                "41",
                "70",
                "42",
                "72",
                "44",
                "45",
                "46",
                "47",
                "48",
                "74",
                "49",
                "50",
                "51",
                "78",
                "53",
                "54",
                "55",
                "56"
            )
        ),
        row.names = c(NA,
                      -60L),
        class = c("tbl_df", "tbl", "data.frame")
    )
}

return_st_info <- function() {
    st_info <- dplyr::tibble(
        abbrev   = datasets::state.abb,
        division = as.character(datasets::state.division),
        st_lat   = datasets::state.center$y,
        st_lon   = datasets::state.center$x
    ) %>%
        ## We have to add DC because it's not a state
        dplyr::add_row(
            abbrev = "DC",
            division = "South Atlantic",
            st_lat = 38.9072,
            st_lon = -77.0369
        ) %>%
        dplyr::left_join(return_st_fips_map() %>%
                             dplyr::select(abbrev,
                                           name,
                                           fips = fipschar),
                         by = "abbrev") %>%
        ## Add in whole US and NA
        dplyr::add_row(
            abbrev = "US",
            name = "zzWhole US",
            division = "Whole US",
            st_lat = 0,
            st_lon = 200,
            fips = "999"
        ) %>%
        dplyr::add_row(
            abbrev = NA,
            name = "zzzUnknown State",
            division = "Unknown",
            st_lat = 0,
            st_lon = 199,
            fips = NA
        ) %>%
        dplyr::rename(st_fips = fips) %>%
        dplyr::arrange(st_lon) %>%
        dplyr::mutate(
            lon_rank = dplyr::dense_rank(st_lon),
            alpha_rank = dplyr::dense_rank(name)
        ) %>%
        dplyr::mutate(name = gsub("zz|zzz", "", name))
    
    st_info <- st_info %>%
        dplyr::mutate(
            st_cat = factor(
                abbrev,
                levels = st_info %>%
                    dplyr::arrange(lon_rank) %>%
                    dplyr::pull(abbrev),
                ordered = TRUE
            ),
            name_cat = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(name) %>%
                    dplyr::pull(name),
                ordered = TRUE
            ),
            name_cat_alpha = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(alpha_rank) %>%
                    dplyr::pull(name),
                ordered = TRUE
            )
        )
    
    st_info %>%
        dplyr::arrange(abbrev)
}
