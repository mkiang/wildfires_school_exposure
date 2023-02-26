## Imports ----
library(tidyverse)
library(here)
library(ggsci)
library(patchwork)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Data ----
analytic_df <- readRDS(here::here("data", "analytic_data.RDS"))

## Student days of PM2.5 ----
### Aggregate data to different levels ----
## We need to remove the 2005-2006 SY because we only have exposure data
## for half of it. Also remove small racial/ethnic groups.
analytic_df <- analytic_df %>%
    categorize_school_year() %>%
    categorize_race() %>%
    dplyr::mutate(sdays_wf25 = n_students * wf25_gt12,
                  sdays_pm25 = n_students * pm25_gt12) %>%
    dplyr::filter(race_recode != "multiracial",
                  race_recode != "hopi") %>%
    dplyr::filter(school_year != "2005_06")

#### School ----
agg_school <- analytic_df %>%
    dplyr::group_by(school_name,
                    nces_id,
                    school_year,
                    year_cat,
                    year_cat_rev) %>%
    summarize_data()

## Focusing on some school years ----
sub_agg <- agg_school %>%
    dplyr::filter(!is.na(longitude),
                  !is.na(latitude),
                  latitude > 30) %>%
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
    ggplot2::scale_size_area("Student-days of\nhigh wildfire PM2.5",
                             breaks = c(0, 10000, 25000, 50000, 100000)) +
    ggplot2::scale_alpha(
        "Student-days of\nHigh PM2.5",
        breaks = c(0, 10000, 25000, 50000, 100000),
        range = c(.01, .1),
        trans = "log1p",
        guide = "none"
    ) +
    mk_nytimes() +
    ggplot2::theme(
        legend.position = "bottom",
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
    ggplot2::facet_wrap(~ year_cat, ncol = 5)

## Save ----
ggplot2::ggsave(
    here::here("plots", "figureS1.pdf"),
    p_map,
    width = 10,
    height = 10,
    scale = 1.2,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figureS1.jpg"),
    p_map,
    width = 10,
    height = 10,
    scale = 1.2,
    dpi = 300
)

## Save the data ---
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
    here::here("output", "figureS1_data.csv")
)
