# This file calculates multipliers for reclassifying 2000 tract-level Census data 
# into 2010 Census tract boundaries for New York City. 

# The relationship file was accessed from: 
# https://www.census.gov/geographies/reference-files/2010/geo/relationship-files.html

# Author: Erik Strand
# Version: 2021-10-18

# Libraries
library(tidyverse)
library(sf)

# Parameters

# Column names for the relationship file
ct_rels_colnames <- 
    c(
        "statefips2000", 
        "countyfips2000", 
        "tractfips2000",
        "geoid2000", 
        "pop2010_2000",
        "units2010_2000", 
        "tractpart2000",
        "area2000",
        "landarea2000",
        "statefips2010",
        "countyfips2010", 
        "tractfips2010",
        "geoid2010",
        "pop2010",
        "units2010", 
        "tractpart2010",
        "area2010",
        "landarea2010",
        "area_record",
        "landarea_record",
        "areapct_2000",
        "landareapct_2000",
        "areapct_2010",
        "landareapct_2010",
        "pop2010_calc",
        "pop2000_calc_pct",
        "pop2010_calc_pct",
        "units2010_calc",
        "units2000_calc_pct",
        "units2010_calc_pct"
    )

# FIPS codes for NYC counties
nyc_fips_char <- 
    c(
        "005", # bronx
        "081", # queens
        "061", # manhattan
        "047", # brooklyn
        "085"  # staten island
    )

ct_rels_path <- "../data_download/ny36trf.txt"
multipliers_output_path <- "../data_clean/tract_multipliers_2000-2010.csv"

#===============================================================================

ct_rels <- 
    read_csv(ct_rels_path, col_names = ct_rels_colnames) %>% 
    filter(countyfips2000 %in% nyc_fips_char)

multipliers <- 
    ct_rels %>% 
    transmute(
        geoid2000, 
        geoid2010, 
        pop_multiplier_2000b_to_2010b = (pop2000_calc_pct / 100), 
        units_multiplier_2000b_to_2010b = (units2000_calc_pct / 100)
    )

multipliers_missing <- 
    multipliers %>% 
    group_by(geoid2000) %>% 
    summarize_at(
        vars(pop_multiplier_2000b_to_2010b, units_multiplier_2000b_to_2010b), 
        sum
    ) %>% 
    filter(
        pop_multiplier_2000b_to_2010b == 0 | units_multiplier_2000b_to_2010b == 0
    )

ct2000s_missing_pop <- 
    multipliers_missing %>% 
    filter(pop_multiplier_2000b_to_2010b == 0) %>% 
    pull(geoid2000)

ct2000s_missing_units <- 
    multipliers_missing %>% 
    filter(units_multiplier_2000b_to_2010b == 0) %>% 
    pull(geoid2000)

multipliers_final <- 
    multipliers %>% 
    mutate(
        pop_mult_missing = 
            ifelse(geoid2000 %in% ct2000s_missing_pop, TRUE, FALSE), 
        units_mult_missing = 
            ifelse(geoid2000 %in% ct2000s_missing_units, TRUE, FALSE)
    )

write_csv(multipliers_final, multipliers_output_path)