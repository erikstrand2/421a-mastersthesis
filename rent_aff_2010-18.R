# This file calculates the renter affordability index for 2010-2018 by 2010 
# Census tract in NYC using CHAS data, provided by HUD based on ACS 5-year 
# estimates. 

# The data was accessed from tables 15C and 17B here: 
# https://www.huduser.gov/portal/datasets/cp.html#2006-2017

# Renter affordability index as calculated here is adopted from the 
# Homeownership Affordability for Renters Index (HARI) proposed by 
# Goodman et al. (2018, The Urban Institute): 
# https://www.urban.org/sites/default/files/publication/97496/housing_affordability_local_and_national_perspectives_1.pdf

# Author: Erik Strand
# Version: 2022-02-27

# Libraries
library(tidyverse)

# Parameters

chas_15c_names <- 
    tribble(
        ~code, ~variable, 
        "T15C_est2", "excluded_units",
        "T15C_est5", "eli_eli_est",
        "T15C_est9", "eli_vli_est",
        "T15C_est13", "eli_li_est",
        "T15C_est17", "eli_modplus_est",
        "T15C_est21", "eli_modplus2_est",
        "T15C_est26", "vli_eli_est",
        "T15C_est30", "vli_vli_est",
        "T15C_est34", "vli_li_est",
        "T15C_est38", "vli_modplus_est",
        "T15C_est42", "vli_modplus2_est",
        "T15C_est47", "li_eli_est",
        "T15C_est51", "li_vli_est",
        "T15C_est55", "li_li_est",
        "T15C_est59", "li_modplus_est",
        "T15C_est63", "li_modplus2_est",
        "T15C_est68", "modplus_eli_est",
        "T15C_est72", "modplus_vli_est",
        "T15C_est76", "modplus_li_est",
        "T15C_est80", "modplus_modplus_est",
        "T15C_est84", "modplus_modplus2_est",
        "T15C_moe5", "eli_eli_moe",
        "T15C_moe9", "eli_vli_moe",
        "T15C_moe13", "eli_li_moe",
        "T15C_moe17", "eli_modplus_moe",
        "T15C_moe21", "eli_modplus2_moe",
        "T15C_moe26", "vli_eli_moe",
        "T15C_moe30", "vli_vli_moe",
        "T15C_moe34", "vli_li_moe",
        "T15C_moe38", "vli_modplus_moe",
        "T15C_moe42", "vli_modplus2_moe",
        "T15C_moe47", "li_eli_moe",
        "T15C_moe51", "li_vli_moe",
        "T15C_moe55", "li_li_moe",
        "T15C_moe59", "li_modplus_moe",
        "T15C_moe63", "li_modplus2_moe",
        "T15C_moe68", "modplus_eli_moe",
        "T15C_moe72", "modplus_vli_moe",
        "T15C_moe76", "modplus_li_moe",
        "T15C_moe80", "modplus_modplus_moe",
        "T15C_moe84", "modplus_modplus2_moe"
    )

chas_17b_names <- 
    tribble(
        ~code, ~variable, 
        "T17B_est3", "eli_1_est",
        "T17B_est4", "vli_1_est",
        "T17B_est5", "li_1_est",
        "T17B_est6", "modplus_1_est",
        "T17B_est8", "eli_2_est",
        "T17B_est9", "vli_2_est",
        "T17B_est10", "li_2_est",
        "T17B_est11", "modplus_2_est",
        "T17B_est13", "eli_3_est",
        "T17B_est14", "vli_3_est",
        "T17B_est15", "li_3_est",
        "T17B_est16", "modplus_3_est",
        "T17B_est18", "eli_4_est",
        "T17B_est19", "vli_4_est",
        "T17B_est20", "li_4_est",
        "T17B_est21", "modplus_4_est",
        "T17B_moe3", "eli_1_moe",
        "T17B_moe4", "vli_1_moe",
        "T17B_moe5", "li_1_moe",
        "T17B_moe6", "modplus_1_moe",
        "T17B_moe8", "eli_2_moe",
        "T17B_moe9", "vli_2_moe",
        "T17B_moe10", "li_2_moe",
        "T17B_moe11", "modplus_2_moe",
        "T17B_moe13", "eli_3_moe",
        "T17B_moe14", "vli_3_moe",
        "T17B_moe15", "li_3_moe",
        "T17B_moe16", "modplus_3_moe",
        "T17B_moe18", "eli_4_moe",
        "T17B_moe19", "vli_4_moe",
        "T17B_moe20", "li_4_moe",
        "T17B_moe21", "modplus_4_moe"
    )

nyc_fips_char <- 
    c(
        "005", # bronx
        "081", # queens
        "061", # manhattan
        "047", # brooklyn
        "085"  # staten island
    )

years <- c(2010:2018)

rent_aff_index_path_out <- 
    "../data_clean/rental_affordability_index/2010-18/rent_aff_index_2010-18.csv"

#===============================================================================

rent_aff_calc <- function(yr){
    path_15c <- paste0("../data_download/chas/", yr, "/Table15C.csv")
    path_17b <- paste0("../data_download/chas/", yr, "/Table17B.csv")
    city_props_path_out <- 
        paste0(
            "../data_clean/rental_affordability_index/2010-18/city_props_", 
            yr, 
            ".csv"
        )
    
    chas_15c <- 
        read_csv(path_15c) %>% 
        mutate(
            statefips = str_sub(geoid, 8, 9), 
            cntyfips = str_sub(geoid, 10, 12), 
            tractfips = 
                str_sub(
                    geoid, 
                    str_length(geoid) - 5, 
                    str_length(geoid)
                )
        ) %>% 
        select(statefips, cntyfips, tractfips, all_of(chas_15c_names$code)) %>% 
        rename_with(
            ~ ifelse(. %in% chas_15c_names$code, 
                     chas_15c_names$variable[match(., chas_15c_names$code)], .)
        ) %>% 
        filter(
            statefips == "36", 
            cntyfips %in% nyc_fips_char
        ) %>% 
        pivot_longer(
            -c(statefips, cntyfips, tractfips, excluded_units), 
            names_to = "variable", 
            values_to = "hh"
        ) %>% 
        separate(variable, into = c("cost", "income", "var"), sep = "_") %>% 
        mutate(
            var = ifelse(str_detect(var, "est"), "hh", "MOE_hh"), 
            cost = ifelse(str_detect(cost, "modplus2"), "modplus", cost), 
            income = ifelse(str_detect(income, "modplus2"), "modplus", income) 
        ) %>% 
        group_by(statefips, cntyfips, tractfips, excluded_units, var, cost, income) %>% 
        summarize(hh = max(hh)) %>% 
        ungroup() %>% 
        pivot_wider(
            names_from = var, 
            values_from = hh
        )
    
    chas_17b <- 
        read_csv(path_17b) %>% 
        mutate(
            statefips = str_sub(geoid, 8, 9), 
            cntyfips = str_sub(geoid, 10, 12), 
            tractfips = 
                str_sub(
                    geoid, 
                    str_length(geoid) - 5, 
                    str_length(geoid)
                )
        ) %>% 
        select(statefips, cntyfips, tractfips, all_of(chas_17b_names$code)) %>% 
        rename_with(
            ~ ifelse(. %in% chas_17b_names$code, 
                     chas_17b_names$variable[match(., chas_17b_names$code)], .)
        ) %>% 
        filter(
            statefips == "36", 
            cntyfips %in% nyc_fips_char
        ) %>% 
        pivot_longer(
            -c(statefips, cntyfips, tractfips), 
            names_to = "variable", 
            values_to = "hh"
        ) %>% 
        separate(variable, into = c("cost", "bedrooms", "var"), sep = "_") %>% 
        mutate(
            var = ifelse(str_detect(var, "est"), "Estimate", "MOE")
        ) %>% 
        pivot_wider(
            names_from = var, 
            values_from = hh
        ) %>% 
        group_by(statefips, cntyfips, tractfips, cost) %>% 
        summarize(
            vacant_units = sum(Estimate), 
            MOE_vu = median(MOE)
        ) %>% 
        ungroup()
    
    housing_aff <- 
        chas_17b %>% 
        left_join(
            chas_15c, 
            by = c("statefips", "cntyfips", "tractfips", "cost")
        ) %>% 
        mutate(
            hh = replace_na(hh, 0),
            units_avail = vacant_units + hh
        ) %>% 
        group_by(statefips, cntyfips, tractfips, ami = cost) %>% 
        summarise(units_avail = sum(units_avail))
    
    hh_income <- 
        chas_15c %>% 
        group_by(statefips, cntyfips, tractfips, ami = income) %>% 
        summarize(
            existing_hh = sum(hh), 
            excluded_units = median(excluded_units), 
            moe = median(MOE_hh)
        )
    
    rent_aff_index_long <- 
        hh_income %>% 
        left_join(
            housing_aff, by = c("statefips", "cntyfips", "tractfips", "ami")
        ) %>% 
        mutate(
            ami = factor(ami, levels = c("eli", "vli", "li", "modplus")), 
            geoid2010 = paste0(statefips, cntyfips, tractfips)
        ) %>% 
        arrange(geoid2010, ami) %>% 
        mutate_at(
            vars(
                units_avail, 
                existing_hh, 
                excluded_units, 
                moe
            ), 
            ~ replace_na(., 0)
        ) %>% 
        group_by(geoid2010, ami) %>% 
        summarize(
            units_avail = sum(units_avail), 
            existing_hh = sum(existing_hh), 
            excluded_units = sum(excluded_units), 
            moe = sum(moe)
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
        mutate(year = yr) %>% 
        select(geoid2010, rntafflcl, rntaffcty, year)

    write_csv(cty_props, city_props_path_out)
    
    return(rent_aff_index)
}

rent_aff_index_all <- map_dfr(years, rent_aff_calc)

write_csv(rent_aff_index_all, rent_aff_index_path_out)



