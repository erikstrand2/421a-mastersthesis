# This file reads in all cleaned data used in my analysis and joins all data
# together into one master database. This master database will later be used 
# for the statistical analyses in my thesis, various maps and data 
# visualizations, and will be made publicly available upon completion of my 
# thesis. 

# Author: Erik Strand
# Version: 2022-02-27

# Libraries
library(tidyverse)
library(sf)

# Parameters

path_ct2010 <- "../data_clean/ct_2010_boundaries"

path_census_2000 <- "../data_clean/acs_census/2000.csv"
path_acs_2009 <- "../data_clean/acs_census/2009.csv"
path_acs_2010_19 <- "../data_clean/acs_census/2010-19.csv"

path_chas_2000 <- 
    "../data_clean/rental_affordability_index/2000/rent_aff_index_2000.csv"
path_chas_2009 <- 
    "../data_clean/rental_affordability_index/2009/rent_aff_index_2009.csv"
path_chas_2010_18 <- 
    "../data_clean/rental_affordability_index/2010-18/rent_aff_index_2010-18.csv"

path_permits <- 
    "../data_clean/permits.csv"

path_out <- "../data_clean/master_database.shp"

inflation_multipliers <- 
    tribble(
        ~year, ~mult, 
        2000, 1.4847,
        2009, 1.1917,
        2010, 1.1724,
        2011, 1.1366,
        2012, 1.1135,
        2013, 1.0974, 
        2014, 1.0799, 
        2015, 1.0786, 
        2016, 1.0652,
        2017, 1.0430, 
        2018, 1.0176, 
        2019, 1
    )


#===============================================================================
ct2010 <- 
    read_sf(path_ct2010) %>% 
    mutate(geoid2010 = as.numeric(geoid)) %>% 
    select(geoid2010, boro, gea)

census_2000 <- 
    read_csv(path_census_2000)
acs_2009 <- 
    read_csv(path_acs_2009)
acs_2010_19 <- 
    read_csv(path_acs_2010_19)

chas_2000 <- 
    read_csv(path_chas_2000)
chas_2009 <- 
    read_csv(path_chas_2009)
chas_2010_18 <- 
    read_csv(path_chas_2010_18)

permits <- 
    read_csv(path_permits)

census_all <- 
    census_2000 %>% 
    rbind(acs_2009) %>% 
    rbind(acs_2010_19) %>% 
    pivot_wider(
        names_from = descriptor, 
        values_from = total
    ) %>% 
    arrange(geoid2010) %>% 
    left_join(inflation_multipliers, by = "year") %>% 
    mutate(med_hh_inc = med_hh_inc * mult) %>% 
    mutate(ratio_8020 = perc_80 / perc_20) %>% 
    select(-c(mult, perc_20, perc_80)) %>% 
    group_by(geoid2010) %>% 
    arrange(year) %>% 
    mutate(
        white_lag = (white_pop - lag(white_pop)) / lag(white_pop), 
        black_lag = (black_pop - lag(black_pop)) / lag(black_pop), 
        college_lag = (college - lag(college)) / lag(college), 
        inc_lag = (med_hh_inc - lag(med_hh_inc)) / lag(med_hh_inc), 
        white_ent = (white_pop / total_pop) * log(1 / (white_pop / total_pop)), 
        black_ent = (black_pop / total_pop) * log(1 / (black_pop / total_pop)), 
        api_ent = (api_pop / total_pop) * log(1 / (api_pop / total_pop)), 
        latinx_ent = (latinx_pop / total_pop) * log(1 / (latinx_pop / total_pop)), 
        other_ent = (other_pop / total_pop) * log(1 / (other_pop / total_pop))
    ) %>% 
    mutate_at(
        vars(white_lag, black_lag, college_lag, inc_lag), 
        ~ ifelse(is.infinite(.) | is.na(.), 1, .)
    ) %>% 
    mutate_at(
        vars(white_ent, black_ent, api_ent, latinx_ent, other_ent),
        ~ ifelse(is.infinite(.) | is.na(.), 0, .)
    ) %>%
    mutate(
        chng_score = 
            abs(
                ifelse(
                    year == 2000, 
                    NA_integer_, 
                    (white_lag - black_lag + college_lag + inc_lag) / 4
                )
            ), 
        entropy = 
            white_ent + black_ent + api_ent + latinx_ent
    ) %>% 
    select(
        -c(white_lag, black_lag, college_lag, inc_lag, white_ent, 
           black_ent, api_ent, latinx_ent, other_ent)
    )

chas_all <- 
    chas_2000 %>% 
    rbind(chas_2009) %>% 
    rbind(chas_2010_18)

master_database <- 
    ct2010 %>% 
    left_join(census_all, by = "geoid2010") %>% 
    left_join(chas_all, by = c("geoid2010", "year")) %>% 
    left_join(permits, by = c("geoid2010", "year")) %>% 
    arrange(geoid2010, year) %>% 
    group_by(year) %>% 
    mutate(
        newcomers = migrant / (local + migrant),
        period = 
            case_when(
                year < 2010 ~ "Pre", 
                year < 2017 ~ "Post1", 
                TRUE ~ "Post2"
            ), 
        net_new_units = 
            ifelse(
                is.na(net_new_units), 
                ifelse(
                    year == 2009, 
                    round((total_hh - lag(total_hh)) / 9), 
                    NA_integer_
                ), 
                net_new_units
            ), 
        cnstprop = new_const / sum(new_const, na.rm = T), 
        demoprop = demolitions / sum(demolitions, na.rm = T), 
        newhhprop= net_new_units / sum(net_new_units, na.rm = T)
    ) %>% 
    select(
        geoid2010, boro, gea, year, period, 
        total_pop, total_hh, 
        white_pop, black_pop, api_pop, other_pop, latinx_pop, 
        hs_or_less, college, 
        pop_u18, `pop_18-34`, `pop_35-64`, pop_65plus, 
        u_pov, u_pov150, 
        not_rb, rb_mod, rb_sev, 
        local, migrant,
        med_hh_inc, 
        new_const, demos = demolitions, 
        alts_rnwls = alts_and_rnwls, new_units = net_new_units, 
        gini, ratio_8020, chng_score, entropy, rntafflcl, rntaffcty, newcomers, 
        cnstprop, demoprop, newhhprop
    )

write_sf(master_database, path_out)

