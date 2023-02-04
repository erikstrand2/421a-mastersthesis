# This file reads in NYC-level boundaries, converts them to the NAD83 
# NY Long Island (US Feet) projection, and writes output shapefiles. 

# Author: Erik Strand
# Version: 2021-10-19

# Libraries
library(tidyverse)
library(sf)

# Parameters

crs_desired <- 2263

paths_ct <- 
    tribble(
        ~year, ~path_in, ~path_out, 
        2020, "../data_download/nyct2020_21c", "../data_clean/ct_2020_boundaries/ct_2020_boundaries.shp",
        2010, "../data_download/nyct2010_21c", "../data_clean/ct_2010_boundaries/ct_2010_boundaries.shp", 
        2000, "../data_download/nyct2000_20d", "../data_clean/ct_2000_boundaries/ct_2000_boundaries.shp"
    )

gea_path_in = "../data_clean/gea_tract_boundaries_updated"

nyc_fips_char <- 
    tibble(
        BoroName = 
            c("Bronx", "Queens", "Manhattan", "Brooklyn", "Staten Island"),
        countyfips = c("005", "081", "061", "047", "085")
    )

#===============================================================================

gea_tracts <- 
    read_sf(gea_path_in) %>% 
    select(-c(boro, area_mi)) %>% 
    st_drop_geometry()

get_sf_ct <- function(year, path_in, path_out){
    fips_var = paste0("CT", {year})
    read_sf({path_in}) %>% 
        st_transform(crs = crs_desired) %>% 
        left_join(nyc_fips_char, by = "BoroName") %>% 
        rename(tractfips = fips_var) %>% 
        transmute(
            geoid = paste0("36", countyfips, tractfips),
            boro = BoroName
        ) %>% 
        left_join(gea_tracts, by = "geoid") %>% 
        mutate(
            gea = replace_na(gea, 0) 
        ) %>% 
        write_sf(path_out)
}

pmap(paths_ct, get_sf_ct)
