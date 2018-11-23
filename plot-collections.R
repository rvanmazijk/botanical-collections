# Plotting my collections on a map
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(sp)

# Define functions ------------------------------------------------------------

dms_to_dd <- function(degrees, minutes, seconds, dir = c("N", "S", "E", "W")) {
  if (degrees == "?") {
    dd <- NA
  } else {
    degrees <- if (!is.na(degrees)) as.numeric(degrees) else NA
    minutes <- if (!is.na(minutes)) as.numeric(minutes) else 0
    seconds <- if (!is.na(seconds)) as.numeric(seconds) else 0
    dd <- degrees + (minutes + (seconds / 60)) / 60
    dd %<>% multiply_by(ifelse(dir %in% c("S", "W"), -1, 1))
  }
  dd
}

# Import collection data -------------------------------------------------------

collections <- read_csv("botanical-collections_rvm.csv")

# Make SpatialPointsDataframe of collections -----------------------------------

# Convert all georefs to DD
collections$lat <- NA
collections$lon <- NA
for (i in 1:nrow(collections)) {
  collections$lat[[i]] <- collections %$% dms_to_dd(
    lat_degrees[[i]], lat_minutes[[i]], lat_seconds[[i]],
    north_south[[i]]
  )
  collections$lon[[i]] <- collections %$% dms_to_dd(
    lon_degrees[[i]], lon_minutes[[i]], lon_seconds[[i]],
    east_west[[i]]
  )
}

# Subset
collections %<>%
  select(
    collection_no, collection_date, genus, species,
    associated_spp, time_since_last_burn_years,
    lat, lon
  ) %>%
  filter(lat != "?", lon != "?")

# Make SpatialPoints
collections <- SpatialPointsDataFrame(
  coords = collections[, c(7, 8)],
  data = collections[, -c(7, 8)]
)

plot(collections)

ggplot(collections@data, aes(lon, lat, col = genus)) +
  geom_point()
