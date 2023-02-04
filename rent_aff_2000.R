# This file calculates the renter affordability index for 2000 by 2010 Census 
# tract in NYC using CHAS data, provided by HUD based on 2000 Census data. 

# The data was accessed from tables B4 and B6 here: 
# https://www.huduser.gov/portal/datasets/cp/CHAS/State%20Files.htm

# Renter affordability index as calculated here is adopted from the 
# Homeownership Affordability for Renters Index (HARI) proposed by 
# Goodman et al. (2018, The Urban Institute): 
# https://www.urban.org/sites/default/files/publication/97496/housing_affordability_local_and_national_perspectives_1.pdf

# Author: Erik Strand
# Version: 2021-10-19

# Libraries
library(tidyverse)

# Parameters

chas2000_b4_names <- 
    tribble(
        ~code, ~variable, 
        "B4C1", "eli_eli", 
        "B4C2", "eli_vli",
        "B4C3", "eli_li",
        "B4C4", "eli_modplus",
        "B4C5", "eli_modplus2",
        "B4C6", "vli_eli", 
        "B4C7", "vli_vli", 
        "B4C8", "vli_li", 
        "B4C9", "vli_modplus", 
        "B4C10", "vli_modplus2", 
        "B4C11", "li_eli", 
        "B4C12", "li_vli", 
        "B4C13", "li_li", 
        "B4C14", "li_modplus", 
        "B4C15", "li_modplus2", 
        "B4C16", "modplus_eli", 
        "B4C17", "modplus_vli", 
        "B4C18", "modplus_li", 
        "B4C19", "modplus_modplus", 
        "B4C20", "modplus_modplus2", 
        "TOTAL", "total",
        "SUM091", "geoid_block",
        "SUMLEVEL", "sumlevel", 
        "FILTER__", "filter"
    )

chas2000_b6_names <- 
    tribble(
        ~code, ~variable, 
        "B6C1", "eli_1", 
        "B6C2", "eli_2",
        "B6C3", "eli_3",
        "B6C4", "vli_1",
        "B6C5", "vli_2",
        "B6C6", "vli_3",
        "B6C7", "li_1",
        "B6C8", "li_2",
        "B6C9", "li_3",
        "B6C10", "modplus_1",
        "B6C11", "modplus_2",
        "B6C12", "modplus_3",
        "TOTAL", "total",
        "SUM091", "geoid_block",
        "SUMLEVEL", "sumlevel", 
        "FILTER__", "filter"
    )

nyc_fips_char <- 
    c(
        "005", # bronx
        "081", # queens
        "061", # manhattan
        "047", # brooklyn
        "085"  # staten island
    )

chas2000_b4_path <- "../data_download/chas/2000/NY-B4091r.dbf"
chas2000_b6_path <- "../data_download/chas/2000/NY-B6091r.dbf"
multipliers_path <- "../data_clean/tract_multipliers_2000-2010.csv"
rent_aff_index_path_out <- 
    "../data_clean/rental_affordability_index/2000/rent_aff_index_2000.csv"
city_props_path_out <- 
    "../data_clean/rental_affordability_index/2000/city_props.csv"

#===============================================================================

chas2000_b4 <- 
    foreign::read.dbf(chas2000_b4_path) %>% 
    as_tibble() %>% 
    rename_all(~chas2000_b4_names %>% pull(variable))

chas2000_clean_b4 <- 
    chas2000_b4 %>% 
    mutate(
        statefips = str_sub(geoid_block, 0, 2),
        countyfips = str_sub(geoid_block, 3, 5),
        tractfips = 
            str_sub(
                geoid_block, 
                str_length(geoid_block) - 6, 
                str_length(geoid_block) - 1
            ),
        blockgroupfips = 
            str_sub(
                geoid_block, 
                str_length(geoid_block), 
                str_length(geoid_block)
            ), 
        geoid = paste0(statefips, countyfips, tractfips)
    ) %>% 
    filter(countyfips %in% nyc_fips_char) %>% 
    select(-c(geoid_block, sumlevel, filter, statefips, countyfips, 
              tractfips, blockgroupfips)) %>% 
    pivot_longer(
        -geoid, 
        names_to = "variable", 
        values_to = "hh"
    ) %>% 
    separate(
        col = variable, 
        sep = "_", 
        into = c("affordability", "income")
    ) %>% 
    mutate(
        income = replace_na(income, "total"), 
        income = replace(income, income == "modplus2", "modplus")
    ) 

chas2000_b6 <- 
    foreign::read.dbf(chas2000_b6_path) %>% 
    as_tibble() %>% 
    rename_all(~chas2000_b6_names %>% pull(variable))

chas2000_clean_b6 <- 
    chas2000_b6 %>% 
    mutate(
        statefips = str_sub(geoid_block, 0, 2),
        countyfips = str_sub(geoid_block, 3, 5),
        tractfips = 
            str_sub(
                geoid_block, 
                str_length(geoid_block) - 6, 
                str_length(geoid_block) - 1
            ),
        blockgroupfips = 
            str_sub(
                geoid_block, 
                str_length(geoid_block), 
                str_length(geoid_block)
            ), 
        geoid = paste0(statefips, countyfips, tractfips)
    ) %>% 
    filter(countyfips %in% nyc_fips_char) %>% 
    select(-c(geoid_block, sumlevel, filter, statefips, countyfips, 
              tractfips, blockgroupfips)) %>% 
    pivot_longer(
        -geoid, 
        names_to = "variable", 
        values_to = "hh"
    ) %>% 
    separate(
        col = variable, 
        sep = "_", 
        into = c("ami", "index")
    ) %>% 
    select(-index) %>% 
    group_by(geoid, ami) %>% 
    summarize(hh = sum(hh))

housing_aff <- 
    chas2000_clean_b4 %>% 
    rename(ami = affordability) %>% 
    group_by(geoid, ami) %>% 
    summarize(units_avail = sum(hh)) %>% 
    left_join(chas2000_clean_b6, by = c("geoid", "ami")) %>% 
    mutate(
        hh = replace_na(hh, 0),
        units_avail = units_avail + hh) %>% 
    select(-hh)

hh_income <- 
    chas2000_clean_b4 %>% 
    rename(ami = income) %>% 
    group_by(geoid, ami) %>% 
    summarize(existing_hh = sum(hh))

multipliers <- 
    read_csv(multipliers_path) %>% 
    select(-contains("pop")) %>% 
    mutate_at(vars(geoid2000, geoid2010), as.character)

rent_aff_index_long <- 
    housing_aff %>% 
    left_join(hh_income, by = c("geoid", "ami")) %>% 
    filter(ami != "total") %>% 
    mutate(
        ami = factor(ami, levels = c("eli", "vli", "li", "modplus"))
    ) %>% 
    arrange(geoid, ami) %>% 
    full_join(multipliers, by = c("geoid" = "geoid2000")) %>% 
    mutate(
        units_avail_reclass = 
            (units_avail * units_multiplier_2000b_to_2010b) %>% round(), 
        existing_hh_reclass = 
            (existing_hh * units_multiplier_2000b_to_2010b) %>% round()
    ) %>% 
    mutate_at(
        vars(units_avail_reclass, existing_hh_reclass), ~ replace_na(., 0)
    ) %>% 
    group_by(geoid2010, ami) %>% 
    summarize(
        units_avail = sum(units_avail_reclass), 
        existing_hh = sum(existing_hh_reclass)
    ) %>% 
    mutate(
        prop_avail = units_avail / sum(units_avail), 
        prop_hh = existing_hh / sum(existing_hh), 
        cum_prop_avail = cumsum(prop_avail), 
        rent_aff = prop_hh * cum_prop_avail, 
        rntafflcl = cumsum(rent_aff)
    ) %>% 
    ungroup()

cty_props <- 
    rent_aff_index_long %>% 
    group_by(ami) %>% 
    summarize(
        existing_hh_cty = sum(existing_hh)
    ) %>% 
    mutate(
        prop_hh_cty = existing_hh_cty / sum(existing_hh_cty)
    ) %>% 
    drop_na()

rent_aff_index_long <- 
    rent_aff_index_long %>% 
    left_join(cty_props, by = "ami") %>% 
    mutate_all(
        ~ ifelse(is.na(.), 0, .)
    ) %>% 
    group_by(geoid2010) %>% 
    mutate(
        rent_aff_cty = prop_hh_cty * cum_prop_avail, 
        rntaffcty = cumsum(rent_aff_cty)
    )

rent_aff_index <- 
    rent_aff_index_long %>% 
    group_by(geoid2010) %>% 
    slice_max(order_by = rntafflcl, n = 1) %>% 
    mutate(year = 2000) %>% 
    select(geoid2010, rntafflcl, rntaffcty, year)

write_csv(rent_aff_index, rent_aff_index_path_out)
write_csv(cty_props, city_props_path_out)

