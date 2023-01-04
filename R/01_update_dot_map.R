rm(list=ls())
library(tidyverse)
library(sf)
library(tidycensus)

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
        mutate(N = if_else(is.na(N), 0L, N))
}

# read original fot map
point_sf <- "./Data/pl94_dotmap/pl94_dotmap_totpop.shp" %>%
    st_read()

# read 2020 pop data by block
block_pop_df <- get_decennial(
    "block", "P1_001N", state = "OR", year = 2020, sumfile = "pl",
    geometry = TRUE)

# get the intersection for the two of them
point_block_sf <- inst_count_merge(point_sf, block_pop_df)

diff_geos_sf <- point_block_sf %>%
    mutate(diff = value - N) %>%
    filter(diff != 0)

diff_geos_sf %>%
    ggplot() +
    geom_sf() +
    theme_void() +
    ggtitle("Blocks with missing people in spatial dot file")

# both of these should equal each other, the 2020 countof the OR census pop
sum(diff_geos_sf$diff) + nrow(point_sf) == sum(block_pop_df$value)

set.seed(123)
sampled_sfc <- diff_geos_sf %>%
    st_sample(diff_geos_sf$diff)


missing_sf <- tibble(CID = rep(1, length(sampled_sfc)))
st_geometry(missing_sf) <- sampled_sfc

# miss_idx <- unlist(sapply(1:length(diff_geos_sf$diff), function(i){
#     rep(i, diff_geos_sf$diff[i])
# }))
#
# suppressWarnings(missing_df <- st_centroid(diff_geos_sf)[miss_idx,] %>%
#     mutate(CID = 1) %>%
#     select(CID, geometry))

temp_df <- inst_count_merge(missing_sf, block_pop_df)


# create the updated point map
updated_point_df <- bind_rows(point_sf, missing_sf)

# make sure the updates didnt mess anything up
# get the intersection for the two of them
updated_point_block_sf <- inst_count_merge(updated_point_df, block_pop_df)

# these should both equal 4237256
sum(updated_point_block_sf$N)
sum(updated_point_block_sf$value)

# no differences in the observed points
updated_point_block_sf  %>%
    mutate(diff = value - N) %>%
    filter(diff != 0)


st_write(updated_point_df, "./Data/pl94_dotmap_update/pl94_dotmap_update.shp")
