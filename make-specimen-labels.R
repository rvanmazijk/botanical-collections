# Produce specimen labels from collection data using RMarkdown
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(rmarkdown)
library(tidyverse)
library(magrittr)
library(glue)

# Define functions -------------------------------------------------------------

# .... General -----------------------------------------------------------------

specimen_filter <- function(x, specimen_no_) {
  stopifnot(exprs = {
    is.data.frame(x)
    is.character(specimen_no_)
  })
  filter(x, specimen_no == specimen_no_)
}

tibble_to_list <- function(x) {
  x %<>% as.list()
  attr(x, "spec") <- NULL
  x
}

unknown_to_na <- function(x) {
  no_nas <- !is.na(x)
  x[no_nas][x[no_nas] == "?"] <- NA
  x
}

na_to_blank <- function(x) {
  x[is.na(x)] <- " "
  x
}

# .... Georeferencing ----------------------------------------------------------

get_decimals <- function(x) {
  if (!is.numeric(x)) {
    x %<>% as.numeric()
  }
  x %% 1
}

degrees_to_minutes <- function(x) {
  x %>%
    get_decimals() %>%
    multiply_by(60)
}
minute_to_seconds <- degrees_to_minutes

dms_lon_lat <- function(x) {
  x %>%
    mutate(
      lat_minutes = ifelse(is.na(lat_minutes),
        degrees_to_minutes(lat_degrees),
        lat_minutes
      ),
      lat_seconds = ifelse(is.na(lat_seconds),
        minute_to_seconds(lat_minutes),
        lat_seconds
      ),
      lon_minutes = ifelse(is.na(lon_minutes),
        degrees_to_minutes(lon_degrees),
        lon_minutes
      ),
      lon_seconds = ifelse(is.na(lon_seconds),
        minute_to_seconds(lon_minutes),
        lon_seconds
      )
    ) %>%
    # Trim decimals if used in a smaller lon lat unit
    mutate(
      lat_degrees = floor(as.numeric(lat_degrees)),
      lat_minutes = floor(as.numeric(lat_minutes)),
      lon_degrees = floor(as.numeric(lon_degrees)),
      lon_minutes = floor(as.numeric(lon_minutes))
    ) %>%
    # Trim decimal points on seconds for display purposes
    mutate(
      lat_seconds = format(lon_seconds, digits = 5),
      lon_seconds = format(lon_seconds, digits = 5)
    )
}

get_georef <- function(x, specimen_no_) {
  x %<>% specimen_filter(specimen_no_)
  x %>%
    select(
      north_south, lat_degrees, lat_minutes, lat_seconds,
      east_west, lon_degrees, lon_minutes, lon_seconds,
      elevation_m, aspect
    ) %>%
    dms_lon_lat() %>%
    tibble_to_list() %>%
    unknown_to_na() %>%
    na_to_blank()
}

# .... Locality ----------------------------------------------------------------

get_loc <- function(x, specimen_no_) {
  x %<>% specimen_filter(specimen_no_)
  x %>%
    select(
      country, province, district, municipality,
      locality, locality_details
    ) %>%
    mutate(municipality = ifelse(municipality == district,  # for City of CPT
      NA,
      municipality
    )) %>%
    mutate(locality_details = locality_details %>%
      str_replace_all("≤", "$\\\\leq$") %>%
      str_replace_all("≥", "$\\\\geq$")
    ) %>%
    tibble_to_list() %>%
    unknown_to_na() %>%
    na_to_blank()
}

# .... Habitat, vegetation, taxonomic and no. details --------------------------

get_details <- function(columns_) {
  stopifnot(is.character(columns_))
  function(x, specimen_no_) {
    x %<>% specimen_filter(specimen_no_)
    x %>%
      select_at(columns_) %>%
      tibble_to_list() %>%
      unknown_to_na() %>%
      na_to_blank()
  }
}

get_hab <- get_details(c(
  "habitat_details",
  "geology", "soil",
  "notes"
))

get_veg <- get_details(c(
  "vegetation", "time_since_last_burn_years",
  "associated_spp"
))

get_taxo <- get_details(c(
  "specimen_no",
  "species", "det",
  "family", "sub_family", "tribe",
  "collection_no", "collectors", "collection_date"
))

# .... Creating the label ------------------------------------------------------

concatenate_ranks <- function(...) {
  ranks <- c(...)
  paste(ranks[ranks != " "], collapse = ": ")
}

collate_label <- function(x, specimen_no_) {
  x <- x %$% list(
    georef = get_georef(., specimen_no_),
    loc = get_loc(., specimen_no_),
    hab = get_hab(., specimen_no_),
    veg = get_veg(., specimen_no_),
    taxo = get_taxo(., specimen_no_)
  )
  x %$% glue(label_template_)
}

# .... Testing -----------------------------------------------------------------

collections <- read_csv("botanical-collections_rvm.csv")

label_template <- "label-template.txt" %>%
  read_lines() %>%
  paste(collapse = "\n")

get_georef(collections, "RVM_27")
get_loc(collections, "RVM_27")
get_hab(collections, "RVM_27")
get_veg(collections, "RVM_27")
get_taxo(collections, "RVM_27")

collate_label(collections, "RVM_27", label_template)

# Write Rmarkdown for all labels -----------------------------------------------

write_lines(path = "labels.Rmd", append = FALSE,
"---
geometry: a6paper, landscape, margin=1cm
output: pdf_document
---

\\pagenumbering{gobble}
\\raggedright
\\footnotesize"
)

for (specimen_no_ in collections$specimen_no[1:35]) {
  write_lines(path = "labels.Rmd", append = TRUE,
    collate_label(collections, specimen_no_)
  )
}
render("labels.Rmd")
