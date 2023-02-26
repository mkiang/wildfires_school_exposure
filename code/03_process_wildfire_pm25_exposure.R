## 03_process_wildfire_pm25_exposure.R ----
## Imports Rosana and Tarik's WF data. For details, see:
## https://pubmed.ncbi.nlm.nih.gov/36592523/

## Imports ----
library(tidyverse)
library(here)

## Raw data ----
load(here("data_raw", 
          "tarik_wf_data",
          "wfPM25_imp_06t20_intrsct_ML_Aug2021.RData"))

## Bit of cleaning and munging ----
wfpm25 <- wfpm25_CA %>% 
    rename(zcta = zip,
           pm25_nonwildfire = PM25, 
           pm25_wildfire = wf_pm25_imp) %>% 
    mutate(pm25_all = pm25_nonwildfire + pm25_wildfire,
           wday = lubridate::wday(date, 
                                  label = TRUE, 
                                  week_start = 1)) %>% 
    as_tibble() %>% 
    distinct() 

## Save ----
saveRDS(wfpm25, 
        here("data", "wildfire_pm25_exposure.RDS"),
        compress = "xz")
