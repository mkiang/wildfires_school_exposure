## `data_raw`

- `ca_shapefile` contains the California county-level TIGER/Line shape file from 2016. It was downloaded from [data.gov](https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-california-current-county-subdivision-state-based) in April 2022.
- `tarik_wf_data` contains wildfire-specific and all-source PM2.5 estimates at the zip-day level. More details about the data are available in Rosana and Tarik's [Environment International](https://www.sciencedirect.com/science/article/pii/S0160412022006468?via%3Dihub) paper. 
- `nces` contains raw data pulls from [the National Center for Education Statistics ElSi](https://nces.ed.gov/ccd/elsi/) web portal. They were retrieved in April 2022. 

To get the raw data files, go to: https://osf.io/f4aku/ to download the zip and decompress it in the root directory or run `00_get_data_files.R`.
