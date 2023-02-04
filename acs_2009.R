# This file reads in data from the 2009 American Community Survey, cleans the 
# data, normalizes data from 2000 to 2010 Census tract boundaries, and exports  
# the data as a single CSV for later combination and analysis with American
# Community Survey Data from 2010 and beyond as well as data from the 2000
# Decennial Census.

# This script also calculates the Gini coefficient and 20/80 income percentiles
# at a Census tract level for 2009, as these datapoints are not available from 
# the 2009 ACS. 

# Author: Erik Strand
# Version: 2022-02-26

# Libraries
library(tidyverse)
library(tidycensus)

# Parameters

acs_vars_path <- "../data_clean/acs_vars.csv"

multipliers_path <- "../data_clean/tract_multipliers_2000-2010.csv"

path_out <- "../data_clean/acs_census/2009.csv"

api <- "4878f9292ac0c11cddc6fabc8cd8530c4d0e12f0"

nyc_fips_char <- 
    c(
        "005", # bronx
        "081", # queens
        "061", # manhattan
        "047", # brooklyn
        "085"  # staten island
    )

inc_midpoint <- 
    tribble(
        ~descriptor, ~avg_inc,
        "gini_under10k", 5000,
        "gini_10-15k", 12500,
        "gini_15-20k", 17500,
        "gini_20-25k", 22500,
        "gini_25-30k", 27500,
        "gini_30-35k", 32500,
        "gini_35-40k", 37500,
        "gini_40-45k", 42500,
        "gini_45-50k", 47500, 
        "gini_50-60k", 55000,
        "gini_60-75k", 67500,
        "gini_75-100k", 87500,
        "gini_100-125k", 112500,
        "gini_125-150k", 137500,
        "gini_150-200k", 175000,
        "gini_200kplus", 200000
    )

#===============================================================================

acs_vars <- 
    read_csv(acs_vars_path)

census_vars_2009 <- 
    acs_vars %>% 
    drop_na(Table_ACS_09) %>% 
    mutate(
        Variable_ACS = 
            str_pad(Variable_ACS_09, width = 3, side = "left", pad = "0"), 
        Code = paste0(Table_ACS_09, "_", Variable_ACS)
    ) %>% 
    select(
        Code, Descriptor, Index
    )

census_api_key(api)

data_acs_09 <- 
    get_acs(
        geography = "tract", 
        variables = census_vars_2009$Code, 
        year = 2009, 
        state = "36", 
        county = nyc_fips_char, 
        survey = "acs5"
    ) %>% 
    mutate(
        statefips = str_sub(GEOID, 1, 2), 
        cntyfips = str_sub(GEOID, 3, 5), 
        tractfips = str_sub(GEOID, 6, 11)
    ) %>% 
    filter(cntyfips %in% nyc_fips_char) %>% 
    left_join(
        census_vars_2009 %>% select(c(Code, Descriptor)), 
        by = c("variable" = "Code")
    ) %>% 
    group_by(cntyfips, tractfips, descriptor = Descriptor) %>% 
    summarize(total = sum(estimate))

data_acs_2009_gini <- 
    data_acs_09 %>% 
    filter(
        str_detect(descriptor, "gini|total_hh|med_hh_inc")
    ) %>% 
    pivot_wider(
        names_from = descriptor, 
        values_from = total
    ) %>% 
    mutate(total_inc = total_hh * med_hh_inc) %>% 
    pivot_longer(
        -c(cntyfips, tractfips, total_hh, med_hh_inc, total_inc), 
        names_to = "descriptor", 
        values_to = "total"
    ) %>% 
    left_join(inc_midpoint, by = "descriptor") %>% 
    arrange(cntyfips, tractfips, avg_inc) %>% 
    group_by(cntyfips, tractfips) %>% 
    mutate(
        tot_inc = avg_inc * total, 
        pop_frac = total / total_hh, 
        richer_frac = 1 - cumsum(pop_frac),
        cum_inc = cumsum(tot_inc),
        tot_inc = 
            ifelse(
                avg_inc == 200000, 
                ifelse(
                    total_inc - cum_inc < 0,
                    0, 
                    total_inc - cum_inc),
                tot_inc
            ), 
        inc_frac = tot_inc / total_inc, 
        score = inc_frac * (pop_frac + 2 * richer_frac), 
        cum_score = cumsum(score), 
        cum_perc =
            ifelse(
                cum_inc / total_inc > 1,
                1,
                cum_inc / total_inc
            ),
        perc_20 = 
            ifelse(
                cum_perc >= 0.2 & lag(cum_perc) <= 0.2, 
                avg_inc, 
                NA_integer_
            ),
        perc_80 = 
            ifelse(
                cum_perc >= 0.8 & lag(cum_perc) <= 0.8, 
                avg_inc, 
                NA_integer_
            )
    ) %>% 
    group_by(cntyfips, tractfips) %>% 
    summarize(
        gini = 
            ifelse(
                abs(1 - sum(score)) > 1,
                1, 
                abs(1 - sum(score))
            ),
        perc_20 = max(perc_20, na.rm = TRUE), 
        perc_80 = max(perc_80, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
        perc_20 = 
            ifelse(
                !is.na(gini) & perc_20 < 0, 
                5000, 
                perc_20
            ),
        perc_80 = 
            ifelse(
                !is.na(gini) & perc_80 < 0, 
                200000, 
                perc_80
            )
    ) %>% 
    mutate_all(~ replace(., is.infinite(.), NA_integer_)) %>% 
    pivot_longer(
        -c(cntyfips, tractfips), 
        names_to = "descriptor", 
        values_to = "total"
    )

multipliers <- 
    read_csv(multipliers_path)

data_acs_09 <- 
    data_acs_09 %>% 
    filter(!str_detect(descriptor, "gini")) %>% 
    rbind(data_acs_2009_gini) %>% 
    arrange(cntyfips, tractfips) %>% 
    mutate(
        geoid2000 = as.numeric(paste0("36", cntyfips, tractfips))
    ) %>% 
    left_join(multipliers, by = "geoid2000") %>% 
    mutate(
        total = total * pop_multiplier_2000b_to_2010b
    ) %>% 
    group_by(geoid2010, descriptor) %>% 
    summarize(total = sum(total)) %>% 
    mutate(year = 2009)

write_csv(data_acs_09, path_out)
