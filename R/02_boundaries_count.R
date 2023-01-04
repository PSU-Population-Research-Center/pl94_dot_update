rm(list=ls())
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
sf_use_s2(FALSE)

inst_count_merge <- function(point_x, geo_y){
    # get the intersection for the two of them
    overlap_sf <- st_intersects(point_x, geo_y)

    count_table <- table(unlist(overlap_sf))

    geo_y %>%
        mutate(ID = 1:n()) %>%
        left_join(
            tibble(
                ID = as.integer(names(count_table)),
                N = unname(as.vector(count_table))),
            by = "ID"
        ) %>%
        mutate(N = if_else(is.na(N), 0L, N)) %>%
        select(-ID)
}

dotmap_sf <- "./Data/pl94_dotmap_update/pl94_dotmap_update.shp" %>%
    read_sf()

ugb_dlcd_2021 <- "../UGB_forecasts/Data/UGB_2021/UGB_2021.shp" %>%
    read_sf() %>%
    st_transform(st_crs(dotmap_sf))

city_dlcd_2021 <- "../UGB_forecasts/Data/city_data/citylim_2021/citylim.shp" %>%
    read_sf() %>%
    st_transform(st_crs(dotmap_sf))

# need to get geography information separate from census api which is messed up
# for 2020
city_census_2020 <- places("OR", year = 2020) %>%
    select(GEOID, geometry) %>%
    left_join(
        get_decennial(
            "place", "P1_001N", state = "OR", year = 2020, sumfile = "pl"),
        by = "GEOID"
    )

# need to get geography information separate from census api which is messed up
# for 2020
county_census_2020 <- counties("OR", year = 2020) %>%
    select(GEOID, geometry) %>%
    left_join(
        get_decennial(
            "county", "P1_001N", state = "OR", year = 2020, sumfile = "pl"),
        by = "GEOID"
    )

city_census_count_2020 <- inst_count_merge(dotmap_sf, city_census_2020)
county_census_count_2020 <- inst_count_merge(dotmap_sf, county_census_2020)

# there should be no difference here
county_census_count_2020 %>%
    filter((value - N) != 0)

# there should be no difference here
city_census_count_2020 %>%
    filter((value - N) != 0)

city_dlcd_count_2021 <- inst_count_merge(dotmap_sf, city_dlcd_2021)
ugb_dlcd_count_2021 <- inst_count_merge(dotmap_sf, ugb_dlcd_2021)

city_dlcd_count_2021 %>%
    as_tibble() %>%
    select(CITY_NAME, Population = N) %>%
    write_csv("Data/diagnostic_tables/city_dlcd_population.csv")

ugb_dlcd_count_2021 %>%
    as_tibble() %>%
    select(UGB_NAME = instName, Population = N) %>%
    write_csv("Data/diagnostic_tables/ugb_dlcd_population.csv")

city_census_count_2020 %>%
    as_tibble() %>%
    select(CITY_NAME = NAME, Population = N) %>%
    write_csv("Data/diagnostic_tables/city_census_population.csv")

county_census_count_2020 %>%
    as_tibble() %>%
    select(COUNTY_NAME = NAME, Population = N) %>%
    write_csv("Data/diagnostic_tables/county_census_population.csv")
