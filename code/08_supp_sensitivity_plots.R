## Imports ----
library(tidyverse)
library(here)
library(patchwork)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Data ----
analytic_df <- readRDS(here::here("data", "analytic_data.RDS"))
pm25_df <- readRDS(here::here("data", "wildfire_pm25_exposure.RDS"))

## Constants ----
Y_BREAKS <- c(1000, 10000, 100000, 1000000, 10000000, 30000000)
X_LABELS <-
    c(
        "'06-'07",
        # "'07-'08",
        "",
        "'08-'09",
        # "'09-'10",
        "",
        "'10-'11",
        # "'11-'12",
        "",
        "'12-'13",
        # "'13-'14",
        "",
        "'14-'15",
        # "'15-'16",
        "",
        "'16-'17",
        # "'17-'18",
        "",
        "'18-'19",
        # "'19-'20",
        "",
        "'20-'21"
    )

agg_pm25 <- pm25_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(pm25_all = mean(pm25_all),
                     pm25_wildfire = mean(pm25_wildfire)) %>%
    tidyr::pivot_longer(
        cols = dplyr::starts_with("pm25"),
        names_to = "pm25_type",
        values_to = "value"
    )

## Student days of PM2.5 ----
### Aggregate data to different levels ----
## We need to remove the 2005-2006 SY because we only have exposure data
## for half of it. Also remove small racial/ethnic groups.
analytic_df <- analytic_df %>%
    categorize_school_year() %>%
    categorize_race() %>%
    dplyr::mutate(sdays_wf25 = n_students * wf25_gt05,
                  sdays_pm25 = n_students * pm25_gt35) %>%
    dplyr::filter(race_recode != "multiracial",
                  race_recode != "hopi") %>%
    dplyr::filter(school_year != "2005_06")

#### School * race ----
agg_school_race <- analytic_df %>%
    dplyr::group_by(
        school_name,
        nces_id,
        school_year,
        race_recode,
        year_cat,
        year_cat_rev,
        race_cat,
        race_cat_rev
    ) %>%
    summarize_data()

#### Race ----
agg_race <- analytic_df %>%
    dplyr::group_by(school_year,
                    race_recode,
                    year_cat,
                    year_cat_rev,
                    race_cat,
                    race_cat_rev) %>%
    summarize_data()

#### School ----
agg_school <- analytic_df %>%
    dplyr::group_by(school_name,
                    nces_id,
                    school_year,
                    year_cat,
                    year_cat_rev) %>%
    summarize_data()

## Student days of exposure ----
p1a <- ggplot2::ggplot(
    data = agg_race,
    ggplot2::aes(
        x = year_cat,
        y = total_sdays_wf25,
        color = race_cat,
        group = interaction(race_cat)
    )
) +
    ggplot2::geom_rect(
        data = agg_race %>% dplyr::slice(1),
        xmin = .5,
        xmax = 1.5,
        ymin = -Inf,
        ymax = Inf,
        color = NA,
        fill = "black",
        alpha = .15
    ) +
    ggplot2::geom_rect(
        data = agg_race %>% dplyr::slice(1),
        xmin = 7.5,
        xmax = 8.5,
        ymin = -Inf,
        ymax = Inf,
        color = NA,
        fill = "black",
        alpha = .15
    ) +
    ggplot2::geom_rect(
        data = agg_race %>% dplyr::slice(1),
        xmin = 14.5,
        xmax = 15.5,
        ymin = -Inf,
        ymax = Inf,
        color = NA,
        fill = "black",
        alpha = .15
    ) +
    ggplot2::geom_line(size = 1, alpha = .8) +
    ggplot2::geom_point(
        data = agg_race %>%
            dplyr::filter(
                race_recode == "all",
                school_year %in% c("2020_21",
                                   "2006_07",
                                   "2013_14")
            ),
        size = 3,
        color = "white",
        alpha = 1
    ) +
    ggplot2::geom_point(
        data = agg_race %>%
            dplyr::filter(
                race_recode == "all",
                school_year %in% c("2020_21",
                                   "2006_07",
                                   "2013_14")
            ),
        size = 2
    ) +
    ggplot2::scale_y_continuous(
        "(log) Student-days of wildfire\nPM2.5 > 5 (in millions)",
        expand = c(0.01, 0),
        trans = "log",
        breaks = Y_BREAKS,
        labels = Y_BREAKS / 1000000
    ) +
    ggplot2::scale_x_discrete("School year",
                              labels = X_LABELS) +
    ggsci::scale_color_jama(name = "Race/Ethnicity") +
    mk_nytimes(legend.position = "right")
p1a

## Student days of exposure ----
p1b <- ggplot2::ggplot(
    data = agg_race,
    ggplot2::aes(
        x = year_cat,
        y = total_sdays_pm25,
        color = race_cat,
        group = interaction(race_cat)
    )
) +
    ggplot2::geom_rect(
        data = agg_race %>% dplyr::slice(1),
        xmin = .5,
        xmax = 1.5,
        ymin = -Inf,
        ymax = Inf,
        color = NA,
        fill = "black",
        alpha = .15
    ) +
    ggplot2::geom_rect(
        data = agg_race %>% dplyr::slice(1),
        xmin = 7.5,
        xmax = 8.5,
        ymin = -Inf,
        ymax = Inf,
        color = NA,
        fill = "black",
        alpha = .15
    ) +
    ggplot2::geom_rect(
        data = agg_race %>% dplyr::slice(1),
        xmin = 14.5,
        xmax = 15.5,
        ymin = -Inf,
        ymax = Inf,
        color = NA,
        fill = "black",
        alpha = .15
    ) +
    ggplot2::geom_line(size = 1, alpha = .8) +
    ggplot2::geom_point(
        data = agg_race %>%
            dplyr::filter(
                race_recode == "all",
                school_year %in% c("2020_21",
                                   "2006_07",
                                   "2013_14")
            ),
        size = 3,
        color = "white",
        alpha = 1
    ) +
    ggplot2::geom_point(
        data = agg_race %>%
            dplyr::filter(
                race_recode == "all",
                school_year %in% c("2020_21",
                                   "2006_07",
                                   "2013_14")
            ),
        size = 2
    ) +
    ggplot2::scale_y_continuous(
        "(log) Student-days of ambient\nPM2.5 > 35 (in millions)",
        expand = c(0.01, 0),
        trans = "log",
        breaks = Y_BREAKS,
        labels = Y_BREAKS / 1000000
    ) +
    ggplot2::scale_x_discrete("School year",
                              labels = X_LABELS) +
    ggsci::scale_color_jama(name = "Race/Ethnicity") +
    mk_nytimes(legend.position = "right")
p1b

## Focusing on some school years ----
sub_agg <- agg_school %>%
    dplyr::filter(
        school_year %in% c("2020_21",
                           "2006_07",
                           "2013_14"),
        !is.na(longitude),
        !is.na(latitude),
        latitude > 30
    ) %>%
    dplyr::rename(lon = longitude,
                  lat = latitude)

p_map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
        data = usmapdata::us_map(regions = "county",
                                 include = "CA"),
        ggplot2::aes(x = x, y = y, group = group),
        fill = "white",
        color = "grey80"
    ) +
    ggplot2::coord_equal() +
    ggplot2::geom_point(
        data = usmap::usmap_transform(sub_agg),
        ggplot2::aes(
            x = x,
            y = y,
            size = total_sdays_wf25,
            alpha = total_sdays_wf25
        ),
        color = ggsci::pal_jama()(1)
    ) +
    ggplot2::scale_size_area("Student-days of\n wildfire PM2.5 > 5",
                             breaks = c(0, 10000, 25000, 50000, 100000)) +
    ggplot2::scale_alpha(
        "Student-days of\nPM2.5 > 5",
        breaks = c(0, 10000, 25000, 50000, 100000),
        range = c(.01, .1),
        trans = "log1p",
        guide = "none"
    ) +
    mk_nytimes() +
    ggplot2::theme(
        legend.position = "right",
        # legend.position = c(1, 1),
        #   legend.justification = c(1, 1),
        plot.background = ggplot2::element_rect(fill = "white"),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
    ) +
    ggplot2::guides(size = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::facet_wrap(~ year_cat)

p_map_b <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
        data = usmapdata::us_map(regions = "county",
                                 include = "CA"),
        ggplot2::aes(x = x, y = y, group = group),
        fill = "white",
        color = "grey80"
    ) +
    ggplot2::coord_equal() +
    ggplot2::geom_point(
        data = usmap::usmap_transform(sub_agg),
        ggplot2::aes(
            x = x,
            y = y,
            size = total_sdays_pm25,
            alpha = total_sdays_pm25
        ),
        color = ggsci::pal_jama()(1)
    ) +
    ggplot2::scale_size_area("Student-days of\nambient PM2.5 > 35",
                             breaks = c(0, 10000, 25000, 50000, 100000)) +
    ggplot2::scale_alpha(
        "Student-days of\nambient PM2.5 > 35",
        breaks = c(0, 10000, 25000, 50000, 100000),
        range = c(.01, .1),
        trans = "log1p",
        guide = "none"
    ) +
    mk_nytimes() +
    ggplot2::theme(
        legend.position = "right",
        # legend.position = c(1, 1),
        #   legend.justification = c(1, 1),
        plot.background = ggplot2::element_rect(fill = "white"),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
    ) +
    ggplot2::guides(size = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::facet_wrap(~ year_cat)

fig1 <-
    p1a + p_map + patchwork::plot_layout(ncol = 1, heights = c(1, 2))
fig1b <-
    p1b + p_map_b + patchwork::plot_layout(ncol = 1, heights = c(1, 2))

## Save ----
ggplot2::ggsave(
    here::here("plots", "figureS2_wf_gt5.pdf"),
    fig1,
    width = 7,
    height = 5,
    scale = 1.2,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figureS2_wf_gt5.jpg"),
    fig1,
    width = 7,
    height = 5,
    scale = 1.2,
    dpi = 300
)

## Save ----
ggplot2::ggsave(
    here::here("plots", "figureS3_pm_gt35.pdf"),
    fig1b,
    width = 7,
    height = 5,
    scale = 1.2,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figureS3_pm_gt35.jpg"),
    fig1b,
    width = 7,
    height = 5,
    scale = 1.2,
    dpi = 300
)

## Save the data
readr::write_csv(
    agg_race %>%
        dplyr::select(
            race_cat,
            year_cat,
            n_obs,
            total_students,
            total_sdays_wf25
        ) %>%
        dplyr::arrange(race_cat, year_cat),
    here::here("output", "figureS2a_data.csv")
)
readr::write_csv(
    sub_agg %>%
        dplyr::select(
            school_name,
            nces_id,
            year_cat,
            school_type,
            school_level,
            lat,
            lon,
            county_name,
            fipschar,
            n_obs,
            total_students,
            total_sdays_wf25
        ),
    here::here("output", "figureS2b_data.csv")
)
