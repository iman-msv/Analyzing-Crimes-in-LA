# Loading Pacman (Packages Management)
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# Loading Required Packages
p_load(tidyverse, magrittr, scales, ggmap, naniar, tidymodels)

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
crimes <- crimes |>
  mutate(area = as.factor(area))

str(crimes)

# Feature Engineering
# Dropping Unnecessary Columns
crime_areas_df <- crimes |> 
  select(-c(dr_no, area_name, rpt_dist_no, crm_cd_desc, premis_desc, 
            weapon_desc, location, cross_street))

# Time to Report Crimes
crime_areas_df <- crime_areas_df |> 
  mutate(date_rptd = as.POSIXct(date_rptd)) |> 
  mutate(time_to_rep_hrs = as.numeric(date_rptd - date_occ) / 3600) |> 
  select(-c(date_rptd))

# Integration of Time Occurance
crime_areas_df <- crime_areas_df |> 
  mutate(
    dttime_occ = make_datetime(
      year = year(date_occ),
      month = month(date_occ),
      day = day(date_occ),
      hour = hour(time_occ),
      min = minute(time_occ),
      sec = second(time_occ)
    )
  ) |> 
  select(-c(date_occ, time_occ)) |> 
  select(dttime_occ, everything()) |> 
  arrange(dttime_occ)

# Factorizing crm_cd
crime_areas_df |>
  select(crm_cd, crm_cd_1, crm_cd_2, crm_cd_3, crm_cd_4)

crime_areas_df |>
  filter(!is.na(crm_cd_3) | !is.na(crm_cd_4))

crime_areas_df <- crime_areas_df |>
  select(-c(crm_cd, crm_cd_3, crm_cd_4))

crime_areas_df |>
  count(crm_cd_2) |>
  arrange(desc(n))

crime_areas_df <- crime_areas_df |>
  mutate(crm_cd_2 = !is.na(crm_cd_2)) |>
  rename(sec_crm = crm_cd_2, crm_cd = crm_cd_1)

crime_areas_df <- crime_areas_df |>
  mutate(dttime_occ = parse_date_time(dttime_occ, "y-m-d H:M:S")) |>
  mutate(day_week = wday(dttime_occ, label = TRUE),
         hour = as.factor(hour(dttime_occ))) |>
  select(-dttime_occ)

str(crime_areas_df)

crime_areas_df <- crime_areas_df |>
  select(-c(lat, lon))

crime_areas_df |>
  summarize(age_mean = mean(vict_age, na.rm = TRUE), .by = area) |>
  ggplot(aes(x = age_mean, y = fct_reorder(area, age_mean))) + 
  geom_point(size = 2) + 
  labs(
    x = "Average of Victims Age",
    y = "Area Code",
    title = "Average Age of Victims across Areas"
  ) +
  theme_minimal() + 
  theme(text = element_text(size = 16))

crime_areas_df |>
  ggplot(aes(x = area, fill = vict_sex)) + 
  geom_bar(position = "fill", width = 0.8) + 
  labs(
    x = "Area Code",
    y = "Proportion",
    title = "Gender of Victims across Areas"
  ) + 
  theme(text = element_text(size = 14), 
        panel.grid.major.x = element_blank())

# Missing Values
crime_areas_df |>
  miss_var_summary()

# vict_age, premis_cd, and crm_cd include few missing values, which may
# suggest MCAR (Missing Completely at Random) and we can remove them
# without worrying about biasness and sample reduction.
crime_areas_df <- crime_areas_df |>
  drop_na(vict_age, premis_cd, crm_cd)

# weapon_used_cd has so many missing values, but it doesn't mean 
# there has been an issue during data collection or data entry. 
# There are cases in which no weapon has been used, and NAs represent 
# those cases. In order to prevent any confusion, a new category is 
# added to the data set showing no weapon (its code will be 0).
crime_areas_df <- crime_areas_df |>
  replace_na(list(weapon_used_cd = 0L))

# Groping by vict_sex, which contains so missing values itself.
crime_areas_df |>
  group_by(vict_sex) |> 
  miss_var_summary() |> 
  filter(n_miss != 0)
# Clearly, both vict_sex and vict_descent have direct relationship in 
# terms of missing values. 

crime_areas_df |>
  summarize(miss_cases = sum(is.na(vict_sex)), 
            n = n(),
            .by = premis_cd) |> 
  mutate(miss_perc = miss_cases / sum(miss_cases) * 100,
         n_pct = n / sum(n) * 100) |> 
  arrange(desc(miss_perc))

# Let's check how missing values varies with the type of weapon
crime_areas_df |>
  summarize(miss_cases = sum(is.na(vict_sex)), 
            n = n(),
            .by = weapon_used_cd) |> 
  mutate(miss_perc = miss_cases / sum(miss_cases) * 100,
         n_pct = n / sum(n) * 100) |> 
  arrange(desc(miss_perc))
# Clearly, those cases that the criminal had no weapon, more cases
# lack information about the sex and descent of the criminal.
crime_areas_df |>
  summarize(miss_cases = sum(is.na(vict_sex)), 
            n = n(),
            .by = day_week) |> 
  mutate(miss_perc = miss_cases / sum(miss_cases) * 100,
         n_pct = n / sum(n) * 100) |> 
  arrange(desc(miss_perc))

# The majority of missing values in vict_sex and vict_descent (75%)
# falls into premis_cd of 101. 

crime_areas_df <- crime_areas_df |>
  mutate(vict_sex = as.factor(vict_sex),
         vict_descent = as.factor(vict_descent),
         premis_cd = as.factor(premis_cd),
         weapon_used_cd = as.factor(weapon_used_cd),
         status_desc = as.factor(status_desc),
         crm_cd = as.factor(crm_cd),
         sec_crm = as.factor(sec_crm))

# Decision Tree
# Model Object
decision_mdl <- decision_tree(
  mode = "classification",
  engine = "rpart"
)

# Train-Test Split
crime_area_split <- initial_split(crime_areas_df, prop = 0.8)
crime_area_train <- training(crime_area_split)
crime_area_test <- testing(crime_area_split)

crime_area_rec <- recipe(area ~ ., data = crime_area_train) |>
  step_dummy(all_factor_predictors())

# Workflow
crime_area_wk <- workflow() |>
  add_model(decision_mdl) |>
  add_recipe(crime_area_rec)

# Fitting the Model
crime_area_wk |>
  last_fit(split = crime_area_split)





