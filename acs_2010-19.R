# This file reads in data from the 2010-19 American Community Survey, cleans  
# the data, and exports the result as a single CSV for later combination and 
# analysis with American Community Survey Data from 2009 as well as data from 
# the 2000 Decennial Census.

# Author: Erik Strand
# Version: 2022-02-27

# Libraries
library(tidyverse)
library(tidycensus)

# Parameters

acs_vars_path <- "../data_clean/acs_vars.csv"

path_out <- "../data_clean/acs_census/2010-19.csv"

api <- "4878f9292ac0c11cddc6fabc8cd8530c4d0e12f0"

nyc_fips_char <- 
    c(
        "005", # bronx
        "081", # queens
        "061", # manhattan
        "047", # brooklyn
        "085"  # staten island
    )

#===============================================================================

acs_vars <- 
    read_csv(acs_vars_path) %>% 
    filter(!is.na(Table_ACS)) %>% 
    mutate(
        Variable_ACS = 
            str_pad(Variable_ACS, width = 3, side = "left", pad = "0"), 
        Code = paste0(Table_ACS, "_", Variable_ACS)
    ) %>% 
    select(
        Code, Descriptor, Index
    )

census_api_key(api)

acs <- function(yr){
    acs_temp <- 
        get_acs(
            geography = "tract", 
            variables = acs_vars$Code, 
            year = yr, 
            state = "36", 
            county = nyc_fips_char, 
            survey = "acs5"
        ) %>% 
        left_join(
            acs_vars %>% select(Code, Descriptor), 
            by = c("variable" = "Code")
        ) %>% 
        mutate(year = yr) %>% 
        group_by(
            geoid2010 = GEOID, 
            descriptor = Descriptor, 
            year
        ) %>% 
        summarize(total = sum(estimate)) %>% 
        select(geoid2010, descriptor, total, year)
    
    return(acs_temp)
}

data_acs_all <- map_dfr(c(2010:2019), acs)

write_csv(data_acs_all, path_out)
