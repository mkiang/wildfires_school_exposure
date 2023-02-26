## Imports ----
library(tidyverse)
library(here)
library(zipzcta) # remotes::install_github("jjchern/zipzcta")
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Data ----
pm25_df <- readRDS(here::here("data", "wildfire_pm25_exposure.RDS"))
school_char_orig <-
    readRDS(here::here("data", "nces_school_characteristics.RDS"))
enroll_df_orig <-
    readRDS(here::here("data", "nces_enrollment_data.RDS"))

## Work on school characteristics data first ----
### Remove schools that were not open ----
school_char <- school_char_orig %>%
    dplyr::filter(!(school_status %in% c("2-Closed", "7-Future", "6-Inactive")))

### Remove virtual schools ----
## NOTE: Again, highly unoptimized but I want to keep track of which schools
## are virtual vs not *and* where we got that information from.
## These are schools that *only* ever show up as virtual schools (even if
## missing).
virtual_school_ids <- school_char %>%
    dplyr::group_by(nces_id) %>%
    dplyr::filter(!is.na(virtual_school_status)) %>%
    dplyr::filter(dplyr::n_distinct(virtual_school_status) == 1) %>%
    dplyr::filter(virtual_school_status == "A virtual school") %>%
    dplyr::pull(nces_id) %>%
    unique()

## These are schools that *only* ever show up as *not* virtual schools (even if
## missing).
not_virtual_school_ids <- school_char %>%
    dplyr::group_by(nces_id) %>%
    dplyr::filter(!is.na(virtual_school_status)) %>%
    dplyr::filter(dplyr::n_distinct(virtual_school_status) == 1) %>%
    dplyr::filter(virtual_school_status == "Not a virtual school") %>%
    dplyr::pull(nces_id) %>%
    unique()

## These are schools that are all missing official status but SEDA says they
## are virtual
virtual_seda <- school_char %>%
    dplyr::group_by(nces_id) %>%
    dplyr::filter(all(is.na(virtual_school_status))) %>%
    dplyr::filter(!is.na(seda_id)) %>%
    dplyr::filter(virtual == 1) %>%
    dplyr::pull(nces_id) %>%
    unique()

## These are schools that are all missing official status but SEDA says they
## are *not* virtual
not_virtual_seda <- school_char %>%
    dplyr::group_by(nces_id) %>%
    dplyr::filter(all(is.na(virtual_school_status))) %>%
    dplyr::filter(!is.na(seda_id)) %>%
    dplyr::filter(virtual == 0) %>%
    dplyr::pull(nces_id) %>%
    unique()

## These are just straight up missing virtual status in both NCES and SEDA
missing_virtual <- school_char %>%
    dplyr::group_by(nces_id) %>%
    dplyr::filter(all(is.na(virtual_school_status))) %>%
    dplyr::filter(!(nces_id %in% virtual_seda)) %>%
    dplyr::filter(!(nces_id %in% not_virtual_seda)) %>%
    dplyr::pull(nces_id) %>%
    unique()

### Subset to necessary columns of non-virtual schools ----
sub_school <- school_char %>%
    dplyr::filter(nces_id %in% c(not_virtual_seda, not_virtual_school_ids)) %>%
    dplyr::select(
        nces_id,
        school_name,
        year,
        school_year,
        school_type,
        school_level,
        n_free_lunch_eligible,
        n_reduced_lunch_eligible,
        n_free_reduced_lunch,
        teacher_ftes,
        student_teacher_ratio,
        latitude,
        longitude,
        county_name,
        fipschar = county_fips,
        usps_zip,
        seda_id,
        seda_name
    )

### Join with ZCTA ----
sub_school <- sub_school %>%
    dplyr::left_join(zipzcta::zipzcta %>%
                         dplyr::transmute(zcta = as.numeric(zcta),
                                          usps_zip = as.numeric(utils::zip)))

## Clean up enrollment data ----

### Subset enrollment to open, non-virtual schools ----
sub_enroll <- enroll_df_orig %>%
    dplyr::filter(!is.na(grade)) %>%
    dplyr::filter(nces_id %in% unique(sub_school$nces_id))

## Subset PM2.5 data to zip codes and aggregate to school year ----
pm25_df <- pm25_df %>%
    dplyr::mutate(
        day = lubridate::day(date),
        month = lubridate::month(date),
        year = lubridate::year(date),
        doy = lubridate::yday(date)
    ) %>%
    dplyr::mutate(school_year = dplyr::case_when(
        month >= 8 ~ sprintf("%s_%s", year, substr(year + 1, 3, 4)),
        month <= 6 ~ sprintf("%s_%s", year - 1, substr(year, 3, 4))
    ))

## Just zip codes with schools
sub_pm25 <- pm25_df %>%
    dplyr::filter(zcta %in% sub_school$zcta)

## Remove weekends ----
sub_pm25 <- sub_pm25 %>%
    dplyr::filter(!(wday %in% c("Sun", "Sat")))

## Remove summer (assume summer is Jun 15 to August 15)
sub_pm25 <- sub_pm25 %>%
    dplyr::filter(month != 7,
                  !(month == 6 & day > 15),
                  !(month == 8 & day < 15))

## Remove winter break (assume Dec 20 to Jan 10)
sub_pm25 <- sub_pm25 %>%
    dplyr::filter(!(month == 12 & day > 20),
                  !(month == 1 & day < 10))

## Remove thanksgiving week
t_days <- as.Date(sprintf(
    "%s-11-%s",
    2006:2020,
    c(23, 22, 27, 26, 25, 24, 22, 28, 27, 26, 24, 24, 22, 28, 26)
))

t_week <- as.Date(NULL)
for (i in 1:NROW(t_days)) {
    t_week <- c(
        t_week,
        seq.Date(
            t_days[i] - lubridate::days(3),
            t_days[i] + lubridate::days(1),
            by = "days"
        )
    )
}

sub_pm25 <- sub_pm25 %>%
    dplyr::filter(!(date %in% t_week))

pm25_df <- pm25_df %>%
    dplyr::mutate(status = dplyr::case_when(
        date %in% sub_pm25$date ~ "in_school",
        !(date %in% sub_pm25$date) ~ "break"
    ))

## Count number of days at different thresholds ----
pm25_school_year <- pm25_df %>%
    dplyr::group_by(zcta, school_year, status) %>%
    dplyr::summarize(
        date = min(date),
        pm25_avg = mean(pm25_all),
        wf25_avg = mean(pm25_wildfire),
        wf25_gt00 = sum(pm25_wildfire >= 0),
        pm25_gt05 = sum(pm25_all >= 5),
        wf25_gt05 = sum(pm25_wildfire >= 5),
        pm25_gt12 = sum(pm25_all >= 12),
        wf25_gt12 = sum(pm25_wildfire >= 12),
        pm25_gt35 = sum(pm25_all >= 35),
        wf25_gt35 = sum(pm25_wildfire >= 35)
    ) %>%
    dplyr::ungroup()

## Merge ----
status_df <- sub_enroll %>%
    dplyr::left_join(sub_school %>%
                         dplyr::select(-year)) %>%
    dplyr::left_join(pm25_school_year)

status_df <- status_df %>%
    categorize_school_year() %>%
    categorize_race() %>%
    dplyr::mutate(sdays_wf25 = n_students * wf25_gt12,
                  sdays_pm25 = n_students * pm25_gt12) %>%
    dplyr::filter(race_recode != "multiracial",
                  race_recode != "hopi") %>%
    dplyr::filter(school_year != "2005_06")

#### Race ----
agg_race <- status_df %>%
    dplyr::group_by(status,
                    school_year,
                    race_recode,
                    year_cat,
                    year_cat_rev,
                    race_cat,
                    race_cat_rev) %>%
    summarize_data() %>%
    dplyr::mutate(avg_sdays_per_student = total_sdays_wf25 / total_students)

sub_agg <- agg_race %>%
    dplyr::filter(race_recode == "all", !is.na(status)) %>%
    dplyr::select(year_cat,
                  race_cat,
                  status,
                  total_sdays_wf25,
                  avg_sdays_per_student) %>%
    dplyr::arrange(year_cat, race_cat, status)

p2 <- ggplot2::ggplot(sub_agg,
                      ggplot2::aes(
                          x = year_cat,
                          y = total_sdays_wf25,
                          fill = status,
                          color = status
                      )) + ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::scale_x_discrete("School year") +
    ggplot2::scale_y_continuous(
        "Total student-days of wildfire\nPM2.5 > 12ug/m3 (in millions)",
        labels = function(x)
            round(x / 1000000),
        expand = c(0, 0)
    ) +
    mk_nytimes() +
    ggplot2::scale_color_brewer("School status",
                                labels = c("Out of school", "In school"),
                                palette = "Set1") +
    ggplot2::scale_fill_brewer("School status",
                               labels = c("Out of school", "In school"),
                               palette = "Set1")

## Make a figure ----
all_ca <- pm25_df %>%
    dplyr::group_by(date, wday, day, month, year, doy, school_year, status) %>%
    dplyr::summarize(pm25_wildfire = mean(pm25_wildfire)) %>%
    dplyr::ungroup()

p1 <- ggplot2::ggplot(data = all_ca,
                      ggplot2::aes(
                          x = date,
                          y = pm25_wildfire,
                          fill = status,
                          color = status
                      )) +
    ggplot2::geom_col(alpha = .8) +
    ggplot2::scale_y_continuous("Daily average wildfire-\nspecific PM2.5 (across California)",
                                expand = c(0, .1)) +
    ggplot2::scale_x_date(NULL, expand = c(0, 0)) +
    mk_nytimes() +
    ggplot2::scale_color_brewer("School status",
                                labels = c("Out of school", "In school"),
                                palette = "Set1") +
    ggplot2::scale_fill_brewer("School status",
                               labels = c("Out of school", "In school"),
                               palette = "Set1")

ggplot2::ggsave(
    here::here("plots", "figureS99_total_sdays_by_status.jpg"),
    p2,
    width = 9,
    height = 4,
    scale = 1,
    dpi = 300
)
ggplot2::ggsave(
    here::here("plots", "figureS99_daily_exposure_by_status.jpg"),
    p1,
    width = 9,
    height = 4,
    scale = 1,
    dpi = 300
)
