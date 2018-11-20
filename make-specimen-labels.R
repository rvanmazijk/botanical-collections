# Produce specimen labels from collection data using RMarkdown
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(rmarkdown)
library(tidyverse)
library(magrittr)
library(glue)

# Define functions ------------------------------------------------------------

specimen_filter <- function(x, specimen_no_) {
  # Subsets a dataframe to just the data for one collection
  stopifnot(exprs = {
    is.data.frame(x)
    is.character(specimen_no_)
  })
  filter(x, specimen_no == specimen_no_)
}

concatenate_ranks <- function(...) {
  # Write taxonomic ranks with ": " between them
  ranks <- c(...)
  paste(ranks[!(ranks %in% c(" ", "?"))], collapse = ": ")
}

get_decimals <- function(x) {
  as.numeric(x) %% 1
}
degrees_to_minutes <- function(x) {
  x %>%
    get_decimals() %>%
    multiply_by(60)
}
minute_to_seconds <- degrees_to_minutes
floor_if_can <- function(x) {
  ifelse(x != "?",
    floor(as.numeric(x)),
    x
  )
}
format_georef <- function(x) {
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
      lat_degrees = floor_if_can(lat_degrees),
      lat_minutes = floor_if_can(lat_minutes),
      lon_degrees = floor_if_can(lon_degrees),
      lon_minutes = floor_if_can(lon_minutes)
    ) %>%
    # Trim decimal points on seconds for display purposes
    mutate(
      lat_seconds = format(lat_seconds, digits = 5),
      lon_seconds = format(lon_seconds, digits = 5)
    )
}

make_label <- function(x, specimen_no_, label_template_) {
  x %<>%
    specimen_filter(specimen_no_) %>%
    format_georef() %>%
    # Replace ≤ & ≥ with LaTeX compatible
    mutate(locality_details = locality_details %>%
      str_replace_all("≤", "$\\leq$") %>%
      str_replace_all("≥", "$\\geq$")
    )
  # Use this data in the template and print
  glue(label_template_) %>%
    as.character() %>%
    str_remove_all("NA")
}

# Import collection data & label template --------------------------------------

collections <- "botanical-collections_rvm.csv" %>%
  read_csv() %>%
  arrange(collection_no)

label_template <- "label-template.txt" %>%
  read_lines() %>%
  paste(collapse = "\n")

# Write blank .Rmd with formatting details -------------------------------------

write_lines(path = "labels.Rmd", append = FALSE,
"---
geometry: a6paper, landscape, margin=1cm
output: pdf_document
---

\\pagenumbering{gobble}
\\raggedright
\\footnotesize")

# Add labels to .Rmd -----------------------------------------------------------

for (specimen_no_ in collections$specimen_no) {
  write_lines(path = "labels.Rmd", append = TRUE,
    make_label(collections, specimen_no_, label_template)
  )
}
