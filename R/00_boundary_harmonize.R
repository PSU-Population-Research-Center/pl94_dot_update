rm(list=ls())
library(tidyverse)
library(sf)
library(tigris)
sf_use_s2(FALSE)


county_sf <- counties("OR", year = 2020) %>%
    select(GEOID, COUNTY = NAME, geometry)

ugb_dlcd_2021_sf <- "../UGB_forecasts/Data/UGB_2021/UGB_2021.shp" %>%
    read_sf() %>%
    st_transform(st_crs(county_sf)) %>%
    select(UGBinstCode = instCode, UGB = instName, geometry) %>%
    mutate(ugb_area = as.numeric(st_area(.)))

city_dlcd_2021_sf <- "../UGB_forecasts/Data/city_data/citylim_2021/" %>%
    str_c("citylim.shp") %>%
    read_sf() %>%
    st_transform(st_crs(county_sf)) %>%
    select(CITYinstCode = instCode, CITY = CITY_NAME, geometry) %>%
    mutate(city_area = as.numeric(st_area(.)))

county_ugb_sf <- st_intersection(county_sf, ugb_dlcd_2021_sf) %>%
    mutate(ov_area = as.numeric(st_area(.))) %>%
    # remove small weird spaces of ugbs which fall in multipe counties
    filter((ov_area/ugb_area) > .01) %>%
    select(-ov_area, -ugb_area)

# includes Unincorpoarted areas
full_county_ugb_sf <- st_difference(county_sf, st_union(county_ugb_sf)) %>%
    mutate(UGBinstCode = NA, UGB = "Unincorporated") %>%
    bind_rows(county_ugb_sf)

any(st_geometry_type(full_county_ugb_sf) == "GEOMETRYCOLLECTION")

# now we can do th same thing for cities
city_county_ugb_sf <- full_county_ugb_sf %>%
    st_intersection(city_dlcd_2021_sf) %>%
    select(-city_area)

# this overalap function creates GEOMETRYCOLLECTION types as lines are now
# made when borders are shared. We  need to remove these to have a proper
# save file
any(st_geometry_type(city_county_ugb_sf) == "GEOMETRYCOLLECTION")

cleaned_city_county_ugb_sf <- city_county_ugb_sf %>%
    st_collection_extract(type= "POLYGON")

any(st_geometry_type(cleaned_city_county_ugb_sf) == "GEOMETRYCOLLECTION")

full_city_county_ugb_sf <- full_county_ugb_sf %>%
    st_difference(st_union(cleaned_city_county_ugb_sf)) %>%
    mutate(CITYinstCode = NA, CITY = "Unincorporated") %>%
    bind_rows(cleaned_city_county_ugb_sf) %>%
    arrange(COUNTY, UGBinstCode, CITYinstCode)

any(st_geometry_type(full_city_county_ugb_sf) == "GEOMETRYCOLLECTION")

final_cleaned_sf <- full_city_county_ugb_sf %>%
    st_collection_extract(type= "POLYGON")
any(st_geometry_type(final_cleaned_sf) == "GEOMETRYCOLLECTION")

st_write(
    final_cleaned_sf,
    "./Data/harmonized_boundaries/county_UGB_city.shp",
    delete_layer = TRUE)
