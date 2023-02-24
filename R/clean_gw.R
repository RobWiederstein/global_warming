# split data sets into dash "-" and slash "/"
# dash first
file <-  "./data-raw/GlobalTemperatures.csv"
df_dash <- readr::read_csv(file = file, n_max = 1800)
# format date for dash
df_dash$dt <- as.Date(df_dash$dt, format = "%Y-%m-%d")
# slash second
df_slash <- readr::read_csv(file = file, skip = 1801, col_names = F)# check date format
names(df_slash) <- names(df_dash)
# format date for slash
df_slash$dt <- as.Date(df_slash$dt, format = "%m/%d/%Y")
# combine datasets now that dates are formatted
df <- dplyr::bind_rows(df_dash, df_slash)
#plot -- its a mess!
library(ggplot2)
df |>
    ggplot() +
    aes(dt, LandAverageTemperature) +
    geom_line() +
    labs(title = "Monthly Temperatures in What Unit?",
         x = "",
         y = "Avg. Temp.",
         caption = "Source: Geological Survey") +
    theme_minimal()
#convert to annual
library(dplyr)
library(tidyr)
df1 <-
    df |>
    # create year column
    mutate(year = lubridate::year(dt), .after = dt) |>
    # create groups
    group_by(year) |>
    # average monthly measurements
    summarize(
        land_avg_temp = mean(LandAverageTemperature),
        land_avg_temp_uncertainty = mean(LandAverageTemperatureUncertainty),
        land_max_temp = mean(LandMaxTemperature),
        land_max_temp_uncertainty = mean(LandMaxTemperatureUncertainty),
        land_min_temp = mean(LandMinTemperature),
        land_min_temp_uncertainty = mean(LandMinTemperatureUncertainty),
        land_ocean_avg_temp = mean(LandAndOceanAverageTemperature),
        land_ocean_avg_temp_uncertainty = mean(LandAndOceanAverageTemperatureUncertainty)
        ) |>
    # wide --> long
    pivot_longer(cols = land_avg_temp:land_ocean_avg_temp_uncertainty)

# plot annual
df1 |>
    ggplot() +
    aes(year, value) +
    geom_line() +
    facet_wrap(vars(name),
               ncol = 1,
               scales = "free_y") +
    theme_minimal() +
    labs(
        title = "My Title",
        x = "",
        y = "Temperature in Units",
        caption = "Source: Some Source."
    ) +
    geom_smooth()
