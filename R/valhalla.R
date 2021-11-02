# FUNCTIONS FOR INTERFACING WITH VALHALLA AND WORKING WITH RESULTS.
#
# These functions are called from _targets.R as part of a reproducible workflow.
#
# The core of the analysis is making a call to valhallr::od_matrix() from
# dissemination blocks to physicians, and then doing some basic wrangling with
# the (very long) table that you get in return.
#
# This file includes a wrapper to easily call valhallr, and functions for
# population-weighting and generating per-capita results.


# CALL VALHALLR
# wrapper around valhallr::od_matrix() so that we can easily save to file and
# keep our targets.R cleaner
do_valhalla_od_matrix <- function(db_startpoints, doc_endpoints, costing, population, batch_size, hostname = "192.168.2.30"){
  valhallr::od_table(
    froms = db_startpoints, from_id_col = "DBUID",
    tos = doc_endpoints, to_id_col= "cpso",
    costing = costing,
    batch_size = batch_size,
    minimum_reachability = 500,
    hostname = hostname) %>%
    write_csv(paste0("outputs/od_matrices/valhalla_matrix_",costing,"_",population,"_target.csv"))

}



#############################################
# POP WEIGHTING FOR FRENCH-ONLY SPEAKERS
# Get population-weighted avg travel times and distances for French-only-speakers
# Assume FOSers are distributed uniformly across dissemination areas (DAs)
# So we use DB populations times the % of FOS in each DA, then use those pops
# for weighting, then aggregate up to the neighbourhood level.
ons_pop_weight_fr <- function(od_table, da_french, db_pops, db_sli, da_sli, verbose = TRUE){

  # get closest and shortest
  if (verbose) message ("Calculating average distance to 5 closest  physicians..")
  closest <- od_table %>%
    group_by(DBUID) %>%
    arrange(DBUID,  distance) %>%
    slice_head(n=5) %>%
    summarise(avg_dist = mean(distance, na.rm = TRUE)) %>%
    distinct() %>%
    mutate(DBUID = as.character(DBUID))

  if (verbose) message ("Calculating average time to 5 closest physicians..")
  shortest <- od_table %>%
    group_by(DBUID) %>%
    arrange(DBUID,  time) %>%
    slice_head(n=5) %>%
    summarise(avg_time = mean(time, na.rm = TRUE)/60) %>%
    distinct() %>%
    mutate(DBUID = as.character(DBUID))

  # create table with each db', adnd's avg dist and avg time to 5 closest
  closest_shortest <- left_join(closest, shortest)

  # get each DB census population, get the 2016 DA % french-only speakers,
  # multiply together to estimate # of french-only speakers in each DB
  # THEN do weighted nbhd average using # of french-only speakers
  # NOTE: ONS_IDs 51 and 65 have no French-only speakers: Hunt Club South Ind.
  # and Merivale Gardens. Confirmed both looking at the ONS data cut (CDP269b)
  # and the DAs that fall into them with onsr::da_sli(). So we'll use the regular
  # population to weight those ones, to give an idea of what it would be like
  # if a French-only speaking person moved in.
  message ("Creating population-weighted values at neighbourhood level...")
  ons_table <- db_pops %>%
    left_join(da_french) %>%
    mutate(fr_only_pop = db_pop_2016 * pct_fr_only) %>%
    arrange(desc(fr_only_pop)) %>%
    left_join(db_sli) %>%
    left_join(closest_shortest) %>%
    mutate(weighted_dist = avg_dist * fr_only_pop,
           weighted_time = avg_time * fr_only_pop) %>%
    group_by(ONS_ID) %>%
    mutate(ons_pop = sum(fr_only_pop, na.rm = TRUE)) %>%
    arrange(ONS_ID) %>%
    summarise(weighted_dist_ons = sum(weighted_dist, na.rm = TRUE)/ons_pop,
              weighted_time_ons = sum(weighted_time, na.rm = TRUE)/ons_pop) %>%
    distinct() %>%
    arrange(desc(weighted_time_ons))

  # here we do the two with no French-only-speakers
  two_nbhds_no_french <- db_pops %>%
    left_join(closest_shortest) %>%
    left_join(db_sli) %>%
    filter(ONS_ID %in% c(51, 65)) %>%
    mutate(weighted_dist = avg_dist * db_pop_2016,
           weighted_time = avg_time * db_pop_2016) %>%
    group_by(ONS_ID) %>%
    mutate(ons_pop = sum(db_pop_2016, na.rm = TRUE)) %>%
    arrange(ONS_ID) %>%
    summarise(weighted_dist_ons = sum(weighted_dist, na.rm = TRUE)/ons_pop,
              weighted_time_ons = sum(weighted_time, na.rm = TRUE)/ons_pop) %>%
    distinct() %>%
    arrange(desc(weighted_time_ons))

  ons_table <- ons_table %>%
    bind_rows(two_nbhds_no_french) %>%
    drop_na() %>%
    distinct()

  return (ons_table)
}



#############################################
# POP WEIGHTING FOR ALL RESIDENTS
# Get population-weighted avg travel times and distances for all residents.
# So we use DB populations for weighting, then aggregate up to the neighbourhood
# level.
ons_pop_weight_all <- function(od_table, da_french, db_pops, db_sli, da_sli, verbose = TRUE){

  # get closest and shortest
  if (verbose) message ("Calculating average distance to 5 closest physicians..")
  closest <- od_table %>%
    group_by(DBUID) %>%
    arrange(DBUID,  distance) %>%
    slice_head(n=5) %>%
    summarise(avg_dist = mean(distance, na.rm = TRUE)) %>%
    distinct() %>%
    mutate(DBUID = as.character(DBUID))

  if (verbose) message ("Calculating average time to 5 closest physicians..")
  shortest <- od_table %>%
    group_by(DBUID) %>%
    arrange(DBUID,  time) %>%
    slice_head(n=5) %>%
    summarise(avg_time = mean(time, na.rm = TRUE)/60) %>%
    distinct() %>%
    mutate(DBUID = as.character(DBUID))

  # create table with each db', adnd's avg dist and avg time to 5 closest
  closest_shortest <- left_join(closest, shortest)

  # get each DB census population, use the population to weight,
  # group by ONS_ID and summarise.
  message ("Creating population-weighted values at neighbourhood level...")
  ons_table <- db_pops %>%
    left_join(closest_shortest) %>%
    left_join(db_sli) %>%
    mutate(weighted_dist = avg_dist * db_pop_2016,
           weighted_time = avg_time * db_pop_2016) %>%
    group_by(ONS_ID) %>%
    mutate(ons_pop = sum(db_pop_2016, na.rm = TRUE)) %>%
    arrange(ONS_ID) %>%
    summarise(weighted_dist_ons = sum(weighted_dist, na.rm = TRUE)/ons_pop,
              weighted_time_ons = sum(weighted_time, na.rm = TRUE)/ons_pop) %>%
    distinct() %>%
    arrange(desc(weighted_time_ons))

  return (ons_table)
}




#############################################
# GENERATE WEIGHTED-AVERAGE TABLE
# Works for both walking and driving tables; provide them as input and tell it
# which metric you're using, so it saves it to the right filename.
# Combine results for all docs and French-speaking docs and get differences
generate_weighted_table <- function(od_all_input, od_fr_input, da_french, db_pops, db_sli, da_sli, metric = NA) {
  if (is.na(metric)) stop ("Please supply metric. We'll use it to name the output file.")

  filename <- paste0("outputs/ons_weighted_table_",metric,".csv")

  ons_all <- ons_pop_weight_all(od_all_input, da_french = da_french, db_pops = db_pops, db_sli = db_sli, da_sli = da_sli) %>%
    rename(dist_all = 2, time_all = 3)

  ons_fr <- ons_pop_weight_fr(od_fr_input, da_french = da_french, db_pops = db_pops, db_sli = db_sli, da_sli = da_sli) %>%
    rename(dist_fr = 2, time_fr = 3)

  ons_names <- onsr::get_ons_shp() %>%
    sf::st_set_geometry(NULL) %>%
    select(ONS_ID, Name)

  ons_table <- left_join(ons_all, ons_fr) %>%
    left_join(ons_names) %>%
    mutate(diff_d = dist_fr - dist_all,
           diff_t = time_fr - time_all) %>%
    select(ONS_ID, Name, dist_all, time_all, dist_fr, time_fr, diff_d, diff_t) %>%
    write_csv(filename)
}

#############################################
# GENERATE POPS FOR NBHDS AND RURALITY CLASSES
generate_rurality_table <- function(rurality_classes, ons_table, db_pops, db_sli){

  # get 2016 census pops of neighbourhoods based on DB SLI
  ons_pops <- left_join(db_sli, db_pops, by = "DBUID") %>%
    group_by(ONS_ID) %>%
    summarise(ons_pop = sum(db_pop_2016, na.rm = TRUE)) %>%
    tidyr::drop_na()

  # add populations to rurality classes and find total pop of rurality segments
  # filter out the 3 unpopulated neighbourhoods: two cemeteries and carleton
  rurality_classes_pops <-  rurality_classes %>%
    left_join(ons_pops, by = "ONS_ID") %>%
    filter(!ONS_ID %in% c(5, 17, 71) ) %>%
    group_by(rurality) %>%
    mutate(rurality_pop = sum(ons_pop, na.rm = TRUE)) %>%
    ungroup()

  # next we add rurality classes and neighbourhood pops to the dist/time table,
  # then we do pop-weighted neighbourhood averages for access to french docs in
  # each rurality class, then we find each neighbourhood's delta for dist & time
  # to french-speaking docs as compared to the pop-weighted rurality class avgs.
  #
  # again we filter out 3 unpopulated hoods: two cemeteries and carleton uni
  rurality_table <- ons_table %>%
    left_join(rurality_classes_pops, by = c("ONS_ID", "Name")) %>%
    filter(!ONS_ID %in% c(5, 17, 71) ) %>%
    mutate(time_fr_popwt = time_fr * ons_pop,
           dist_fr_popwt = dist_fr * ons_pop,
           time_all_popwt = time_all * ons_pop,
           dist_all_popwt = dist_all * ons_pop) %>%
    group_by(rurality) %>%
    mutate(dist_fr_rurality_avg = sum(dist_fr_popwt)/rurality_pop,
           time_fr_rurality_avg = sum(time_fr_popwt)/rurality_pop,
           dist_all_rurality_avg = sum(dist_all_popwt)/rurality_pop,
           time_all_rurality_avg = sum(time_all_popwt)/rurality_pop) %>%
    ungroup() %>%
    mutate(diff_d_rurality = dist_fr - dist_all_rurality_avg,     #dist_fr_rurality_avg,
           diff_t_rurality = time_fr - time_all_rurality_avg) %>% #time_fr_rurality_avg) %>%
    select(-rurality_pop, -time_fr_popwt, -dist_fr_popwt, -dist_all_popwt, -time_all_popwt, dist_fr_rurality_avg, time_fr_rurality_avg)

  #message(test)
  return(rurality_table)
}


################################
# PER-CAPITA PHYSICIAN MEASURES
# accepts shapefile points as inputs
generate_per_capita2 <- function(docs_all_shp, docs_fr_shp, ons_data, ons_shp){

  # get ons shape and set 50m buffer around neighbourhoods
  ons_buffer <- ons_shp %>%
    sf::st_transform(crs = 32189) %>%
    sf::st_buffer(dist = 50) %>%
    sf::st_transform(crs = "WGS84")

  ons_names <- ons_buffer %>%
    sf::st_set_geometry(NULL)


  # get neighbourhood counts for all docs and french docs
  docs_all_count <- onsr::get_pts_neighbourhood(pts = docs_all_shp,
                                                pgon = ons_buffer) %>%
    sf::st_set_geometry(NULL) %>%
    group_by(ONS_ID, Name, Name_FR) %>%
    summarise(num_en = n()) %>%
    full_join(ons_names) %>%
    mutate(num_en = if_else(is.na(num_en), 0, as.numeric(num_en))) %>%
    ungroup() %>%
    add_row(ONS_ID = 0, Name = "Ottawa", Name_FR = "Ottawa", num_en = nrow(docs_all_shp)) %>%
    arrange(ONS_ID)

  docs_fr_count <- onsr::get_pts_neighbourhood(pts = docs_fr_shp,
                                               pgon = ons_buffer) %>%
    sf::st_set_geometry(NULL) %>%
    group_by(ONS_ID, Name, Name_FR) %>%
    summarise(num_fr = n()) %>%
    full_join(ons_names) %>%
    mutate(num_fr = if_else(is.na(num_fr), 0, as.numeric(num_fr)))%>%
    ungroup() %>%
    add_row(ONS_ID = 0, Name = "Ottawa", Name_FR = "Ottawa", num_fr = nrow(docs_fr_shp)) %>%
    arrange(ONS_ID)

  # get ons pops from ons_data

  ons_pop <- ons_data %>%
    filter(polygon_attribute %in% c("pop2016", "CDP269", "CDP269a", "CDP269b", "CDP269c")) %>%
    select(ONS_ID, polygon_attribute, value) %>%
    distinct() %>%
    pivot_wider(values_from = value, names_from = polygon_attribute) %>%
    mutate(pop2016_fr_only = round(pop2016 * CDP269b / 100, digits = 0),
           pop2016_fr_only = if_else(pop2016_fr_only == 0, NA_real_, pop2016_fr_only),
           pop2016_fr = round(pop2016 * (CDP269b + CDP269c) / 100, digits = 0),
           ONS_ID = as.numeric(ONS_ID)) %>%
    select(ONS_ID,
           pop2016_en = pop2016,
           pop2016_fr,
           pop2016_fr_only)


  docs_percap <- left_join(docs_all_count, docs_fr_count) %>%
    left_join(ons_pop) %>%
    mutate(percap_en = num_en/pop2016_en,
           percap_fr = num_fr/pop2016_fr,
           percap_fr_only = num_fr/pop2016_fr_only,
           pop_per_doc_en = round(1/percap_en, digits = 0),
           pop_per_doc_fr = round(1/percap_fr, digits = 0),
           pop_per_doc_fr_only = round(1/percap_fr_only, digits = 0)) %>%
    mutate(across(where(is.numeric), function(x) if_else( (is.infinite(x) | is.nan(x)), NA_real_, x))) %>%
    filter(!is.na(ONS_ID))

  return (docs_percap)


}

