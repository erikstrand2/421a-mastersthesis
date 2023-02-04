# This file reads in data on construction permits from the NYC Department of
# Buildings (DOB) and new residential units from the NYC Department of City 
# Planning (DCP), aggregates the data by year and Census tract, and exports
# the data as a CSV. 

# Author: Erik Strand
# Version: 2022-04-07

# Libraries
library(tidyverse)
library(sf)

# Parameters

ct2010_path <- "../data_clean/ct_2010_boundaries"
dob_path <- "../data_download/DOB_Permit_Issuance.csv"
dcp_path <- "../data_download/nychdb_ctract_20q4_shp"
path_out <- "../data_clean/permits.csv"

nyc_fips_char <- 
    tribble(
        ~boro_code, ~boro, 
        "005", "BRONX", 
        "081", "QUEENS", 
        "061", "MANHATTAN", 
        "047", "BROOKLYN", 
        "085", "STATEN ISLAND"
    )

#===============================================================================

ct2010_sf <- read_sf(ct2010_path)

ct2010 <- 
    ct2010_sf %>% 
    st_drop_geometry() %>% 
    mutate(
        boro = toupper(boro),
        tract_1 = as.integer(str_sub(geoid, 6, 9)), 
        tract_2 = str_sub(geoid, 10, 11), 
        tract_2 = ifelse(tract_2 == "00", NA_character_, tract_2),
        tract = 
            ifelse(
                is.na(tract_2), 
                tract_1, 
                paste0(tract_1, tract_2)
            ) %>% 
            as.integer()
    ) %>% 
    left_join(
        nyc_fips_char, 
        by = "boro"
    ) %>% 
    select(-c(boro, tract_1, tract_2))

dob <- read_csv(dob_path)

dob_clean <- 
    dob %>% 
    transmute(
        boro = BOROUGH, 
        tract = as.integer(CENSUS_TRACT), 
        bin = `Bin #`,
        bldg_type = `Bldg Type`, 
        residential = Residential, 
        job_type = `Job Type`, 
        work_type = `Work Type`, 
        permit_type = `Permit Type`,
        permit_status = `Permit Status`, 
        filing_type = `Filing Status`, 
        filing_date = lubridate::ymd(`Filing Date`), 
        issue_date = lubridate::ymd(`Issuance Date`), 
        expir_date = lubridate::ymd(`Expiration Date`), 
        work_start_date = lubridate::ymd(`Job Start Date`), 
        permit_year = lubridate::year(filing_date)
    ) %>% 
    left_join(
        nyc_fips_char, 
        by = "boro"
    ) %>% 
    left_join(
        ct2010, 
        by = c("boro_code", "tract")
    ) %>% 
    drop_na(geoid) %>% 
    mutate(
        job_type = 
            case_when(
                job_type == "A1" ~ "Alteration - Major", 
                job_type %in% c("A2", "A3") ~ "Alteration - Minor",
                job_type == "NB" ~ "New Construction", 
                job_type == "DM" ~ "Demolition", 
                TRUE ~ NA_character_
            ), 
        year = permit_year, 
        geoid2010 = as.numeric(geoid)
    ) %>% 
    drop_na(job_type, permit_year)

dob_agg_long <- 
    dob_clean %>%
    count(
        geoid2010, year, filing_type, permit_status, 
        job_type, bldg_type, residential
    )

dob_agg <- 
    dob_clean %>% 
    filter(year %in% c(2000, 2009:2019)) %>% 
    mutate(
        new_const = 
            ifelse(
                job_type == "New Construction" & filing_type == "INITIAL", 
                1, 
                0
            ), 
        demolitions = ifelse(job_type == "Demolition", 1, 0), 
        alts_and_rnwls = 
            ifelse(
                str_detect(job_type, "Alteration") | filing_type == "RENEWAL", 
                1, 
                0
            )
        
    ) %>% 
    group_by(geoid2010, year) %>% 
    summarize_at(vars(new_const, demolitions, alts_and_rnwls), sum)

dcp1 <- read_sf(dcp_path)

dcp_clean <- 
    dcp1 %>% 
    st_drop_geometry() %>% 
    select(geoid2010 = centract10, contains("comp"), -comp2010ap) %>% 
    pivot_longer(
        -c(geoid2010), 
        names_to = "year", 
        values_to = "net_new_units"
    ) %>% 
    mutate(year = as.integer(str_sub(year, 5, 8)))

permits <- 
    dob_agg %>% 
    left_join(dcp_clean, by = c("geoid2010", "year"))

write_csv(permits, path_out)