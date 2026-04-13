library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)



# Suggestions for the app ------------------------------------------------

# 2. 
# 3. 
# 4. Add another button with the feature to compare 2 locations. Copy the ui and behaviour of the single location mode/tab but add a second address input field and split the current row of the 2 plots into two rows to show the plots of the second location in the lower row. The plot settings from the sidebar affect plots of both locations.


# Deploy App -------------------------------------------------------------

# rsconnect::deployApp(
#   appDir   = ".",
#   appFiles = c("app.R", "groundwater.duckdb"),
#   appName  = "groundwater-bouquet"
# )

# Import ------------------------------------------------------------------

data_directory <- "../../git_daten/bouquet/GEMS-GER_data/dynamic/"

data_paths <- list.files(data_directory, full.names = TRUE)

# set.seed(42)
# data_paths <- 
#   data_paths |> 
#   sample(size = n_wells)

data_gems_static <- read_csv("../../git_daten/bouquet/GEMS-GER_data/static/static_features_MW_1toMW_3207.csv") |> 
  janitor::clean_names()

data_gems_static <- 
  data_gems_static |> 
  select(
    well_id = mw_id, 
    proj_id,
    coords_x = easting_epsg_3035,
    coords_y = northing_epsg_3035, 
    depth, up_filter, 
    lo_filter, 
    scr_length, 
    aquifer_med, 
    pre_state
  )

data_gems_static |> 
  readr::write_csv("data/data_gems_meta.csv")


data_gems <- 
  data_paths |>
  # sample(100) |> 
  map(\(p) fread(p) |> 
    as_tibble() |> 
    mutate(well_id = word(p, sep = "/", start = -1) |> 
    str_remove(".csv")),
  .progress = TRUE) |>
  list_rbind()


data_gems_pre <- 
  data_gems |> 
  janitor::clean_names() |> 
  mutate(id = word(path, sep = "/", start = -1) |> str_remove(".csv")) |> 
  relocate(id, path) |> 
  select(-path) |> 
  rename(date = v1) |> 
  select(id, date, gwl)
  tidyr::pivot_wider(names_from = "id", values_from = "gwl")

bouquets::make_plot_bouquet()
