require(readxl)
require(readr)
require(dplyr)
require(tidyverse)
require(geosphere)
require(ggmap)

locs <- read_xlsx("author_locations_data.xlsx") %>%
  slice(1:100)

## latitude and longitude for settings
setting <- locs %>% 
  select(book_title, book_setting_1, book_setting_2, book_setting_3, book_setting_4, book_setting_5, book_setting_6, book_setting_7) %>% 
  gather(key = "x", value = "location", -book_title) %>% 
  drop_na() %>% 
  select(book_title, location)
setting_geos <- setting %>% 
  select(location) %>%
  distinct() %>% 
  mutate_geocode(location)
setting <- left_join(setting, setting_geos)


## latitude and longitude for residences
residence <- locs %>% 
  select(author_name, author_place_birth, author_place_2, author_place_3, author_place_4,
         author_place_5, author_place_6, author_place_7, author_place_8, author_place_9,
         author_place_10, author_place_11, author_place_12) %>% 
  distinct() %>% 
  gather(key = "x", value = "location", -author_name) %>% 
  drop_na() %>% 
  select(author_name, location)
residence_geos <- residence %>%
  select(location) %>%
  distinct() %>% 
  mutate_geocode(location)
residence <- left_join(residence, residence_geos)


book_list <- locs[, 2:3]
composite <- bind_rows(
  left_join(setting, book_list, by = "book_title") %>% 
    select(book_title, author_name, location, lon, lat) %>% 
    mutate(type = "setting"),
  left_join(residence, book_list, by = "author_name") %>% 
    select(book_title, author_name, location, lon, lat) %>% 
    mutate(type = "residence")
)

final <- c()
for(i in 1:nrow(book_list)) {
  final[[i]] <- list(
    composite %>%
      filter(book_title == book_list$book_title[i] & type == "setting") %>% 
      select(location, lon, lat) %>% 
      as.matrix(), 
    composite %>%
      filter(book_title == book_list$book_title[i] & type == "residence") %>% 
      select(location, lon, lat) %>% 
      as.matrix() # matrix form for Haversine function
  )
}


distances <- rep(list(NA), nrow(book_list))
for(i in 1:nrow(book_list)) {
  for(j in 1:nrow(final[[i]][[1]])) {
    for(k in 1:nrow(final[[i]][[2]])) {
      distances[[i]] <- c(distances[[i]], 
                          str_c(distHaversine(final[[i]][[1]][j, 2:3] %>% as.numeric() %>% matrix(ncol = 2), 
                                              final[[i]][[2]][k, 2:3] %>% as.numeric() %>% matrix(ncol = 2)) / (1.609344*1000), 
                                final[[i]][[1]][j, 1], final[[i]][[2]][k, 1], sep = " -- "))
    }
  }
  distances[[i]] <- distances[[i]][!is.na(distances[[i]])]
}

dist_len <- lengths(distances)
titles <- c()
for(i in 1:nrow(book_list)) {
  titles <- c(titles, rep(book_list$book_title[i], dist_len[i]))
}

labeled_dist_tbl <- tibble(book_title = titles, dist = unlist(distances)) %>% 
  separate(dist, c("distance", "setting", "residence"), sep = " -- ") %>% 
  mutate(distance = as.numeric(distance) %>% round(2))


lat_long_list <- union(residence[,2:4], setting[2:4]) 
nearest_setting_residence_by_book <- labeled_dist_tbl %>% 
  inner_join(book_list, by = "book_title") %>% 
  inner_join(lat_long_list, by = c("setting" = "location")) %>% 
  inner_join(lat_long_list, by = c("residence" = "location")) %>% 
  select(book_title, author_name, setting, lon.x, lat.x, residence, lon.y, lat.y, distance) %>% 
  rename(nearest_setting_lat = "lat.x", nearest_setting_lon = "lon.x", 
         nearest_residence_lat = "lat.y", nearest_residence_lon = "lon.y") %>% 
  group_by(book_title) %>% 
  top_n(-1) %>% 
  distinct()

min_dist_tbl <- labeled_dist_tbl %>% 
  group_by(book_title) %>% 
  summarize(min_dist = min(distance)) 

## for how many works does minimum distance = 0?
num_0s <- min_dist_tbl %>% 
  pull(min_dist) == 0
sum(num_0s)

# what's the median distance for those that aren't 0?
min_dist_tbl %>% 
  filter(min_dist != 0) %>% 
  pull(min_dist) %>% 
  median()
