####################################################
# ACCESS TO FRENCH-SPEAKING PHYSICIANS IN OTTAWA
# Targets file for reproducible analysis and reports.
#
# PLEASE NOTE! This analysis is built in R using the package 'targets', which
# is designed for reproducible scientific research. It forces you to use a
# structured approach where you 1. define all of your inputs, 2. define all of
# your functions, and then 3. use your inputs and your functions to generate
# outputs, which can then be new inputs to new functions. It keeps track of
# which outputs are up to date, and will only re-run code when something upstream
# has changed.
#
# It's a bit tricky at first but it forces you to be disciplined and organized.
# No more 'analysis1aa-FINAL.R' generating 'output2.csv' that needs to be renamed
# before you run 'analysis2bb-2020.R'....
#
# Read more here: https://books.ropensci.org/targets/

# load the two packages we need to run targets and generate Rmd files
library(targets)
library(tarchetypes)

# tell targets which packages we need to load to do the analysis
tar_option_set(packages = c("tidyverse",
                            "ggspatial",
                            "onsr",
                            "valhallr",
                            "sf",
                            "tictoc",
                            "leaflet",
                            "readxl",
                            "ggplot2"))

# load all our custom functions stored in separate .R files
source("R/valhalla.R")
source("R/publication_figures_target.R")

# define functions that point to our input files
path_to_db_startpoints <- function()  "data/db_startpoints.csv"

path_to_rural_outlines <- function()  "data/shapefiles/ruralities.shp"

path_to_rurality_classes <- function() "data/rurality_classes.xlsx"

path_to_all_physician_data <- function() "data/physicians/docs_all_2021-06-30.csv"

path_to_db_pops <- function() "data/db_pops.csv"

path_to_da_french <- function() "data/ottawa_da_french.csv"

# THIS IS THE MAIN TARGETS BODY. It's a list of tar_targets() where the first
# line is its internal name (basically a variable name), and the second line(s)
# is the function call that generates its value. tar_targets() that point to
# files also have the format = "file" flag for internal bookkeeping.

list(
  ##########################
  # LOAD INPUT FILES
  tar_target(
    db_pops_file,
    path_to_db_pops(),
    format = "file"
  ),

  tar_target(
    db_pops,
    readr::read_csv(db_pops_file, col_types = list("c","n")) %>%
      mutate(DAUID = str_trunc(DBUID, width = 8, ellipsis = ""))
  ),

  tar_target(
    da_french_file,
    path_to_da_french(),
    format = "file"
  ),

  tar_target(
    da_french,
    readr::read_csv(da_french_file, col_types = list("c","c","c","c","n","n","n")) %>%
      filter(TEXT_ID == "7002") %>%
      select(DAUID, pct_fr_only = T_DATA_DONNEE) %>%
      mutate (pct_fr_only = pct_fr_only / 100)
  ),

  tar_target(
    rural_outlines_file,
    path_to_rural_outlines(),
    format = "file"
  ),

  tar_target(
    rural_outlines,
    sf::read_sf(rural_outlines_file)
  ),

  tar_target(
    raw_physician_data_file,
    path_to_all_physician_data(),
    format = "file"
  ),
  # load the raw physician data
  tar_target(
    raw_physician_data,
    readr::read_csv(raw_physician_data_file)
  ),

  # filter it to get all FPs and French-speaking FPs
  tar_target(
    docs_all_data,
    raw_physician_data %>%
      filter(family_physician)
  ),
  tar_target(
    docs_fr_data,
    raw_physician_data %>%
      filter(family_physician & french)
  ),

  # load the DB startpoints
  tar_target(
    db_startpoints_file,
    path_to_db_startpoints(),
    format = "file"
  ),
  tar_target(
    db_startpoints,
    read_csv(db_startpoints_file, col_types = cols())
  ),
  tar_target(
    db_startpoints_shp,
    sf::st_as_sf(db_startpoints, coords = c("lon", "lat"), crs = "WGS84")
  ),

  tar_target(
    rurality_classes,
    readxl::read_xlsx(path_to_rurality_classes())
  ),

  tar_target(
    ons_data,
    onsr::get_ons_data()
  ),

  tar_target(
    ons_shp,
    onsr::get_ons_shp()
  ),

  # single-link indicators from DB/DAs to ONS neighbourhoods
  tar_target(
    db_sli,
    onsr::get_db_to_ons()
  ),

  tar_target(
    da_sli,
    onsr::get_da_to_ons()
  ),
  # tar_target(
  #   ottawa_db_shp,
  #   sf::read_sf("data/ottawa_dbs.shp")
  # ),

  #####################
  # RUN THE ANALYSIS

  # set up the physician locations as geometric point shapes
  # NOTE: i have commented out the original way, and added a new way that
  # reads from a single input csv physician data file and generates everything
  # from there. so updating the csv physician file (or pointing it to an updated
  # file) will trigger updates to the rest of the chain as required.
  tar_target(
    docs_fr_endpoints,
    docs_fr_data %>%
      select(lat,
             lon = lng,
             cpso)
  ),
  tar_target(
    docs_all_endpoints,
    docs_all_data %>%
      select(lat,
             lon = lng,
             cpso)
  ),
  tar_target(
    od_fr_auto,
    do_valhalla_od_matrix(db_startpoints,
                          docs_fr_endpoints,
                          costing = "auto",
                          population = "fr",
                          batch_size = 100)
  ),

  tar_target(
    od_fr_walk,
    do_valhalla_od_matrix(db_startpoints,
                          docs_fr_endpoints,
                          costing = "pedestrian",
                          population = "fr",
                          batch_size = 5)
  ),

  tar_target(
    od_all_auto,
    do_valhalla_od_matrix(db_startpoints,
                          docs_all_endpoints,
                          costing = "auto",
                          population = "all",
                          batch_size = 100)
  ),

  tar_target(
    od_all_walk,
    do_valhalla_od_matrix(db_startpoints,
                          docs_all_endpoints,
                          costing = "pedestrian",
                          population = "all",
                          batch_size = 5)
  ),


  tar_target(
    ons_table_auto,
    generate_weighted_table(od_all_auto, od_fr_auto, da_french, db_pops, db_sli = db_sli, da_sli = da_sli, metric = "auto") %>%
      tidyr::drop_na() %>%
      dplyr::filter(ONS_ID != 17) # remove carleton university
  ),

  tar_target(
    ons_table_walk,
    generate_weighted_table(od_all_walk, od_fr_walk, da_french, db_pops, db_sli = db_sli, da_sli = da_sli, metric = "pedestrian") %>%
      tidyr::drop_na() %>%
      dplyr::filter(ONS_ID != 17) # remove carleton university
  ),

  tar_target(
    ons_table_auto_rurality,
    generate_rurality_table(rurality_classes, ons_table_auto, db_pops, db_sli) %>%
      write_csv("outputs/neighbourhood_tables/ons_table_auto_rurality.csv")
  ),

  tar_target(
    ons_table_walk_rurality,
    generate_rurality_table(rurality_classes, ons_table_walk, db_pops, db_sli) %>%
      write_csv("outputs/neighbourhood_tables/ons_table_walk_rurality.csv")
  ),

  # get the full shapefiles
  tar_target(
    docs_all_shp,
    docs_all_data %>%
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
    #sf::read_sf(docs_all_endpoints_shp)
  ),

  tar_target(
    docs_fr_shp,
    docs_fr_data %>%
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
    #sf::read_sf(docs_fr_endpoints_shp)
  ),

  # per-capita docs
  tar_target(
    per_capita_measures,
    generate_per_capita2(docs_all_shp, docs_fr_shp, ons_data, ons_shp) %>%
      write_csv("outputs/neighbourhood_tables/per_capita_measures.csv")
  ),

  # IMAGES AND TABLES FOR PUBLICATION
  tar_target(
    publication_images,
    {
    make_publication_figures(ons_table_auto_rurality, ons_table_walk_rurality, ons_shp, rural_outlines)

      make_bivariate_choropleth(ons_table_auto_rurality, ons_shp, ons_data)

      make_scatterplot_drivetime_pctFRonly(ons_table_auto_rurality, ons_data)
    }
  ),

  tar_target(
    publication_tables,
    make_publication_tables(ons_table_auto_rurality, ons_table_walk_rurality)
  ),


  NULL
)


