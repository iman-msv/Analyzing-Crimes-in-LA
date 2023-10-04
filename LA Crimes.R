# Loading Pacman (Packages Management)
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# Loading Required Packages
p_load(tidyverse, magrittr,scales, ggmap)

# Reading Crimes Data
crimes <- read_csv("crimes.csv")
problems()

# Exploratory Data Analysis
# Structure of Data
str(crimes)

# Standardize Column Names
names(crimes) <- names(crimes) |> 
  str_replace_all(" ", "_") |> 
  str_to_lower()

# Data Types Correction
crimes$date_occ <- mdy_hms(crimes$date_occ)

crimes <- crimes |>
  mutate(
    time_occ = str_pad(time_occ, width = 4, side = "left", pad = "0"),
    hour = as.integer(substr(time_occ, 1, 2)),
    minute = as.integer(substr(time_occ, 3, 4)),
    time_occ = hms::hms(hour = hour, minute = minute)
  ) %>%
  select(-hour, -minute)

# Checking for Updates
head(crimes)

# Summary Statistics
summary(crimes)

# Replacing NAs with Negative Ages
crimes |> 
  filter(vict_age < 0)

crimes <- crimes |>
  mutate(vict_age = ifelse(vict_age < 0, NA, vict_age))








