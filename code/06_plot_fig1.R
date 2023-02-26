## 06_plot_fig1.R ----

## Imports ----
library(tidyverse)
library(here)
library(patchwork)
library(ggsci)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Data ----
analytic_df <- readRDS(here::here("data", "analytic_data.RDS"))
pm25_df <- readRDS(here::here("data", "wildfire_pm25_exposure.RDS"))
lorenz_df <- readRDS(here::here("data", "lorenz_studentdays.RDS"))

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

#### Race ----
agg_race <- analytic_df %>%
    dplyr::group_by(school_year,
                    race_recode,
                    year_cat,
                    year_cat_rev,
                    race_cat,
                    race_cat_rev) %>%
    summarize_data() %>%
    dplyr::mutate(avg_sdays_per_student = total_sdays_wf25 / total_students)

## Total number of student days of exposure ----
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
    ggplot2::scale_y_continuous(
        "(log) Student-days of wildfire\nPM2.5 > 12ug/m3 (in millions)",
        expand = c(0.01, 0),
        trans = "log",
        breaks = Y_BREAKS,
        labels = Y_BREAKS / 1000000
    ) +
    ggplot2::scale_x_discrete(NULL) +
    ggsci::scale_color_jama(name = "Race/Ethnicity") +
    mk_nytimes(legend.position = "right",
               axis.text.x = ggplot2::element_blank()) +
    ggplot2::guides(ggplot2::guide_legend(reverse = TRUE))

## Average number of student-days of exposure ----
p1b <- ggplot2::ggplot(
    agg_race,
    ggplot2::aes(
        x = year_cat,
        y = avg_sdays_per_student,
        color = race_cat,
        group = interaction(race_cat)
    )
)  +
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
    ggplot2::scale_y_continuous("Average number of student-days\nof wildfire PM2.5 > 12ug/m3",
                                expand = c(0.01, 0)) +
    ggplot2::scale_x_discrete("School year",
                              labels = X_LABELS) +
    ggsci::scale_color_jama(name = "Race/Ethnicity") +
    mk_nytimes(legend.position = "none")

## Lorenz distribution of student-days of exposure ----
sub_lorenz <- lorenz_df %>%
    categorize_school_year() %>%
    categorize_race() %>%
    dplyr::filter(race_recode != "multiracial",
                  race_recode != "hopi") %>%
    dplyr::filter(school_year %in% c("2006_07", "2013_14", "2020_21")) %>%
    dplyr::filter(metric == "wf25_gt12") %>%
    dplyr::mutate(p1 = 1 - p,
                  Lp1 = 1 - Lp)

p1c <- ggplot2::ggplot(sub_lorenz,
                       ggplot2::aes(x = p1,
                                    y = Lp1,
                                    color = race_cat)) +
    ggplot2::geom_line(size = 1,
                       alpha = .8) +
    mk_nytimes() +
    ggplot2::facet_wrap(~ year_cat,
                        nrow = 1) +
    ggsci::scale_color_jama(name = "Race/Ethnicity") +
    mk_nytimes(legend.position = "none") +
    ggplot2::scale_x_continuous(
        "Top percent of schools (ranked)",
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, .2)
    ) +
    ggplot2::scale_y_continuous("Proportion of students-days\nwith wildfire PM2.5 > 12 ug/m3",
                                expand = c(.01, 0))

## Combine plots ----
fig1 <- p1a + p1b + p1c + patchwork::plot_layout(ncol = 1)

## Save ----
ggplot2::ggsave(
    here::here("plots", "figure1.pdf"),
    fig1,
    width = 7,
    height = 7,
    scale = 1.2,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figure1.jpg"),
    fig1,
    width = 7,
    height = 7,
    scale = 1.2,
    dpi = 300
)

## Save the data ----
readr::write_csv(
    agg_race %>%
        dplyr::select(
            race_cat,
            year_cat,
            n_obs,
            total_students,
            total_sdays_wf25,
            avg_sdays_per_student
        ) %>%
        dplyr::arrange(race_cat, year_cat),
    here::here("output", "figure1ab_data.csv")
)
readr::write_csv(
    sub_lorenz %>%
        dplyr::select(p1, Lp1, race_cat, year_cat),
    here::here("output", "figure1c_data.csv")
)
