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

# Areas with the highest number of crimes
# Constant theme options used for all plots
text_in_plots <- theme(text = element_text(size = 14, family = "Roboto"))

crimes |>
  count(area_name) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = fct_reorder(area_name, prop), y = prop)) + 
  geom_col(aes(fill = prop)) + 
  scale_fill_gradient(low = "darkred", high = "red") +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 0.07, 0.01)) +
  coord_flip() + 
  labs(
    x = "Proportional Frequency", 
    y = "Area Name", 
    title = "Number of Crimes in Descending Order"
  ) + 
  text_in_plots +
  theme( 
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

# Months of Crimes Occurances
crimes <- crimes |>
  mutate(month_occ = month(date_occ, label = TRUE))

crimes |>
  count(month_occ) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(month_occ, prop)) + 
  geom_col() +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 0.1, 0.01)) +
  labs(
    x = "Month", 
    y = "Proportional Frequency", 
    title = "Month of Crime Occurance"
  ) + 
  text_in_plots +
  theme( 
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )


crimes |>
  ggplot(aes(x = time_occ)) + 
  geom_histogram(binwidth = 7200, boundary = 0, closed = "left") +
  labs(
    x = "Time", 
    y = "Crime Frequency", 
    title = "Time of Crime Occurance"
  ) + 
  text_in_plots +
  theme(
    text = element_text(size = 14)
  )

# Most Common Crimes
crimes |>
  count(crm_cd_desc) |>
  mutate(prop = n / sum(n)) |>
  slice_max(n = 20, order_by = prop) |>
  ggplot(aes(x = fct_reorder(crm_cd_desc, prop), y = prop)) + 
  geom_col(aes(fill = prop)) + 
  scale_fill_gradient(low = "darkred", high = "red") +
  scale_y_continuous(labels = label_percent()) +
  coord_flip() + 
  labs(
    x = "Proportional Frequency", 
    y = "Crime", 
    title = "Crime Description Frequency in Descending Order"
  ) + 
  text_in_plots +
  theme( 
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

# Age and Sex of Victims
# Age
crimes |>
  ggplot(aes(vict_age)) +
  geom_histogram(aes(y = after_stat(density)), boundary = 0, binwidth = 5) + 
  scale_x_continuous(breaks = seq(0, 125, 10)) +
  scale_y_continuous(labels = label_percent()) +
  text_in_plots +
  labs(
    x = "Victim Age", 
    y = "Proportional Frequency",
    title = "Victim Age Proportional Histogram"
  )

crimes |>
  filter(vict_age < 5)

print(paste("Number of rows with victim age of zero:", nrow(filter(crimes, vict_age == 0))))
print(paste("Number of rows with victim age of one:", nrow(filter(crimes, vict_age == 1))))
print(paste("Number of rows with victim age of two:", nrow(filter(crimes, vict_age == 2))))
print(paste("Number of rows with victim age of three:", nrow(filter(crimes, vict_age == 3))))
print(paste("Number of rows with victim age of four:", nrow(filter(crimes, vict_age == 4))))

# Sex
crimes |>
  count(vict_sex) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = vict_sex, y = prop)) + 
  geom_col(width = 0.9) +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 0.45, 0.1)) + 
  text_in_plots + 
  labs(
    x = "Victim Sex", 
    y = "Proportional Frequency",
    title = "Victim Sex Proportional Frequency"
  ) +
  theme(panel.grid.major.x = element_blank())

# "H" is not a predifined sex type
crimes <- crimes |>
  mutate(vict_sex = ifelse(vict_sex == "H", NA, vict_sex))

# Victim's Descent
crimes <- crimes |>
  mutate(vict_descent = ifelse(vict_descent == "-", NA, vict_descent))

crimes |>
  count(vict_descent) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = vict_descent, y = prop)) + 
  geom_col(width = 0.9) +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 0.3, 0.05)) + 
  text_in_plots + 
  labs(
    x = "Victim Descent", 
    y = "Proportional Frequency",
    title = "Victim Descent Proportional Frequency"
  ) +
  theme(panel.grid.major.x = element_blank())

# Type of Structure, Vehicle, or Location
crimes |>
  count(premis_desc) |>
  mutate(prop = n / sum(n)) |>
  slice_max(n = 20, prop) |>
  ggplot(aes(x = fct_reorder(premis_desc, prop), y = prop)) + 
  geom_col(aes(fill = prop)) + 
  scale_fill_gradient(low = "darkred", high = "red") +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 0.25, 0.05)) +
  text_in_plots +
  labs(
    x = "Premise Description",
    y = "Proportional Frequency", 
    title = "Premise Description Frequncy in Descending Order"
  ) + 
  theme( 
    legend.position = "none",
    panel.grid.major.y = element_blank()
  ) +
  coord_flip()

# Weapons Used
crimes |>
  filter(!is.na(weapon_desc)) |>
  count(weapon_desc) |>
  mutate(prop = n / sum(n)) |>
  slice_max(n = 20, order_by = prop) |>
  ggplot(aes(x = fct_reorder(weapon_desc, prop), y = prop)) + 
  geom_col(aes(fill = prop)) + 
  scale_fill_gradient(low = "darkred", high = "red") +
  scale_y_continuous(labels = label_percent()) +
  coord_flip() + 
  labs(
    x = "Proportional Frequency", 
    y = "Weapon Description", 
    title = "Weapon Description Proportional Frequency in Descending Order"
  ) + 
  text_in_plots +
  theme( 
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

# Crimes Distribution Geographically
head(crimes)
qmplot(lon, lat, data = crimes, maptype = "toner-lite", color = I("red"))

crimes |>
  ggplot(aes(x = lon)) + 
  geom_histogram()

crimes |>
  ggplot(aes(x = lat)) + 
  geom_histogram()

crimes |>
  filter(lon == 0 | lat == 0)

crimes <- crimes |>
  mutate(lon = ifelse(lon == 0, NA, lon)) |>
  mutate(lat = ifelse(lat == 0, NA, lat))

qmplot(lon, lat, data = crimes, maptype = "toner-lite", color = I("red"), alpha = I(0.01))

# Crimes Area Classification 
# Sufficient amount of data exists for each geographic area. 
crimes



























