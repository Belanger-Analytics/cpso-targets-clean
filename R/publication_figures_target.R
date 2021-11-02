# CODE TO GENERATE FIGURES AND TABLES FOR PUBLICATION.
#
# These functions are called from _targets.R as part of a reproducible workflow.
#
# NOTE: images must be greyscale or CMYK, >=300dpi, and gif, jpg, or tiff

# function to take a data table, make a choropleth, and save it
# as a high-res png
save_plot <- function(data_table, fill_column, fill_label, filename, rural_outlines, output_folder){

  # not doing any diffs right now
  if (stringr::str_detect(fill_column, "diff")) return()

  # figure out which column holds the all/fr comparator for scaling fill values
  if (stringr::str_detect(fill_column, "all")) other_column <- stringr::str_replace(fill_column, "all", "fr")
  if (stringr::str_detect(fill_column, "fr")) other_column <- stringr::str_replace(fill_column, "fr", "all")

  forplot <- data_table %>%
    select(fill_column = {{fill_column}},
           other_column = {{other_column}})

  # find the limits colour scale breaks
  minval <- min(forplot$fill_column, forplot$other_column, na.rm = TRUE) %>%
    min(0)
  maxval <- max(forplot$fill_column, forplot$other_column, na.rm = TRUE)

  # construct the title based on inputs
  if (stringr::str_detect(fill_column, "all")){
    pop <- "General Population"
    dest <- "Any Family Physician"
  } else {
    pop <- "French-Only Speakers"
    dest <- "French-Speaking Family Physicians"
  }

  if (stringr::str_detect(fill_column, "dist")) {
    metric = "Distance"
  } else{
    metric = "Time"
  }

  if (stringr::str_detect(filename, "drive")){
    modality <- "Driving"
  } else{
    modality <- "Walking"
  }

  plot_title <- paste0(modality, " ",metric,": ",pop)
  plot_subtitle <- paste0(pop," to ",dest)

  # make the plot
  plot <- forplot %>%
    ggplot() +
    #ggspatial::annotation_map_tile(zoomin = 0) +
    geom_sf(aes(fill = fill_column),
            size = 0.2,
            colour = NA) +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         fill = fill_label) +
    theme_minimal() +
    # scale_fill_viridis_c(breaks = plot_breaks, labels = plot_labs)  +
    theme(legend.position = "bottom")+
    scale_fill_viridis_c(limits = c(minval, maxval))

  # we are adding rural outlines to all plots now
  plot <- plot +
    geom_sf(data = rural_outlines,
            colour = "light grey", fill = NA, size = 1) +
    ggplot2::guides(colour = FALSE)

  ggsave(plot = plot,
         filename = paste0(output_folder,filename,".png"),
         dpi = 300,
         width = 7,
         height = 7)

  return(plot)
}
#

make_publication_figures <- function(ons_table_auto_rurality, ons_table_walk_rurality, ons_shp, rural_outlines){

  # where we will put the images
  output_folder <- "outputs/pub_figures/"

  # make auto and walking shapefiles out of the tables
  ons_auto <- left_join(ons_shp, ons_table_auto_rurality)
  ons_walk <- left_join(ons_shp, ons_table_walk_rurality)

  # get the outlines for our figures (this is ab it messy)
  #rural_outlines <- sf::read_sf("data/shapefiles/ruralities.shp") #sf::read_sf("data/urban_suburban.shp")



  ## tibble of plots to save algorithmically
  auto_plots <- tibble::tribble(
    ~data_table, ~fill_column, ~fill_label, ~filename,
    "ons_auto", "time_all",  "Drive Time (min)", "drive_time_all",
    "ons_auto", "time_fr",   "Drive Time (min)", "drive_time_fr",

    "ons_walk", "dist_all",  "Walk Dist. (km)",  "walk_dist_all",
    "ons_walk", "dist_fr",   "Walk Dist. (km)",  "walk_dist_fr",
  )



  # we will save all our plots in a list
  all_plots <- list()

  # Loop to generate the choropleths
  for (i in 1:nrow (auto_plots)){
    this_plot <- auto_plots[i,]

    result <- NA

    if (this_plot$data_table == "ons_auto"){
      result <- save_plot(ons_auto, this_plot$fill_column, this_plot$fill_label, this_plot$filename, rural_outlines, output_folder)
    }
    if (this_plot$data_table == "ons_walk"){
      result <- save_plot(ons_walk, this_plot$fill_column, this_plot$fill_label, this_plot$filename, rural_outlines, output_folder)
    }

    all_plots[[i]] <- result
  }

  for (i in 1:(nrow(auto_plots)/2)) {
    message(paste0(i,"/",nrow(auto_plots)/2))

    index <- (i-1)*2 + 1

    # get filename
    filename <- auto_plots$data_table[[index]] %>%
      paste0(.,"_", auto_plots$fill_column[[index]] %>% stringr::str_remove_all("_all"), "_comp.jpg")

    # make composite image
    cowplot::plot_grid(all_plots[[index]] + theme(plot.subtitle=element_text(size=8)),
                       all_plots[[index+1]] + theme(plot.subtitle=element_text(size=8))) %>%
      ggsave(filename = paste0(output_folder, filename), dpi = 500, width = 8, height = 4)

  }

  return (TRUE)
}


### TABLES

make_publication_tables <- function(ons_table_auto_rurality, ons_table_walk_rurality){
  auto_overall <- ons_table_auto_rurality %>%
    select(time_all, time_fr, diff_t) %>%
    skimr::skim() %>% as_tibble() %>%
    mutate(rurality = "Overall")

  auto_groups <- ons_table_auto_rurality %>%
    select(time_all, time_fr, diff_t, rurality) %>%
    group_by(rurality) %>%
    skimr::skim() %>% as_tibble()#,

  auto_summary <- bind_rows(auto_overall, auto_groups) %>%
    select(rurality, everything(), -skim_type,  -n_missing, -complete_rate, -numeric.hist, -numeric.mean, -numeric.sd)


  walk_overall <- ons_table_walk_rurality %>%
    select(dist_all, dist_fr, diff_d) %>%
    skimr::skim() %>% as_tibble() %>%
    mutate(rurality = "Overall")

  walk_groups <- ons_table_walk_rurality %>%
    select(dist_all, dist_fr, diff_d, rurality) %>%
    group_by(rurality) %>%
    skimr::skim() %>% as_tibble()#,

  walk_summary <- bind_rows(walk_overall, walk_groups) %>%
    select(rurality, everything(), -skim_type,  -n_missing, -complete_rate, -numeric.hist, -numeric.mean, -numeric.sd)

  ##
  auto_table_pub <- auto_summary %>%
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    arrange(rurality) %>%
    mutate(median_iqr_text =     if_else(!stringr::str_detect(skim_variable, "diff"),
                                         paste0(numeric.p50, " (",numeric.p25,"-",numeric.p75,")"),
                                         NA_character_),
           median_min_max_text = if_else(stringr::str_detect(skim_variable, "diff"),
                                         paste0(numeric.p50, " (",numeric.p0 ,"-",numeric.p100,")"),
                                         NA_character_)
    )


  walk_table_pub <- walk_summary %>%
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    arrange(rurality) %>%
    mutate(median_iqr_text =     if_else(!stringr::str_detect(skim_variable, "diff"),
                                         paste0(numeric.p50, " (",numeric.p25,"-",numeric.p75,")"),
                                         NA_character_),
           median_min_max_text = if_else(stringr::str_detect(skim_variable, "diff"),
                                         paste0(numeric.p50, " (",numeric.p0 ,"-",numeric.p100,")"),
                                         NA_character_)
    )

  #knitr::kable(walk_table_pub)

  DT::datatable(walk_table_pub) %>%
    htmltools::save_html(file = "outputs/pub_tables/walking_table.html")

  DT::datatable(auto_table_pub) %>%
    htmltools::save_html(file = "outputs/pub_tables/driving_table.html")

  return(TRUE)
}


## stats
make_publication_stats <- function() {

  paired_test <- TRUE
  alternative_test <- "two.sided"
  alternative_test <- "less"

  drive_stat <- ons_table_auto_rurality %>%
  group_by(rurality) %>%
  summarise(p_value = wilcox.test(time_all, time_fr,
                                  paired = paired_test,
                                  alternative = alternative_test)$p.value,
            alternative = alternative_test,
            paired = paired_test) %>%
    mutate(mode = "Driving",
           metric = "Time")

  drive_overall <- ons_table_auto_rurality %>%
    summarise(p_value = wilcox.test(time_all, time_fr,
                                    paired = paired_test,
                                    alternative = alternative_test)$p.value,
              alternative = alternative_test,
              paired = paired_test) %>%
    mutate(mode = "Driving",
           rurality = "All",
           metric = "Time")

walk_stat <- ons_table_walk_rurality %>%
  group_by(rurality) %>%
  summarise(p_value = wilcox.test(dist_all, dist_fr,
                                  paired = paired_test,
                                  alternative = alternative_test)$p.value,
            alternative = alternative_test,
            paired = paired_test) %>%
  mutate(mode = "Walking",
         metric = "Distance")


walk_overall <- ons_table_walk_rurality %>%
  summarise(p_value = wilcox.test(dist_all, dist_fr,
                                  paired = paired_test,
                                  alternative = alternative_test)$p.value,
            alternative = alternative_test,
            paired = paired_test) %>%
  mutate(mode = "Walking",
         rurality = "All",
         metric = "Distance")

drive_overall %>%
  bind_rows(drive_stat) %>%
  bind_rows(walk_overall) %>%
  bind_rows(walk_stat) %>%
  select(mode, metric, rurality, p_value, paired, alternative)

}


make_bivariate_choropleth <- function(ons_table_auto_rurality, ons_shp, ons_data){

  # filter the ONS data for % french-only speakers
  data_forplot <- ons_data %>%
    filter(polygon_attribute == "CDP269b") %>%
    select(ONS_ID, value) %>%
    mutate(ONS_ID = as.numeric(ONS_ID)) %>%
    rename(pct_french_only = value) %>%
    distinct()

  # combine it all with the ons shapefile
  shp_forplot <- ons_shp %>%
    left_join(
      ons_table_auto_rurality %>%
        select(ONS_ID, Name, time_fr),
      by = c("ONS_ID", "Name")
    ) %>%
    left_join(data_forplot,
              by = "ONS_ID")


  bi_plot <- shp_forplot %>%
    bivariatechoropleths::bivariate_choropleth(var2_name = "pct_french_only",
                                               var1_name = "time_fr",
                                               var2_label = "% FR-Only Speakers",
                                               var1_label = "Drive Time to\nFR-Speaking PCP",
                                               polygon_names = "Name",
                                               interactive = FALSE,
                                               legend_xpos = 0.105,
                                               legend_ypos = 0.21,
                                               legend_size = 0.25)

  ggsave("outputs/pub_figures/Ottawa_bivariate_language_access.png",
         width = 5, height= 5)


}


make_scatterplot_drivetime_pctFRonly <- function(ons_table_auto_rurality, ons_data){
  # filter the ONS data for % french-only speakers
  data_forplot <- ons_data %>%
    filter(polygon_attribute == "CDP269b") %>%
    select(ONS_ID, value) %>%
    mutate(ONS_ID = as.numeric(ONS_ID)) %>%
    rename(pct_french_only = value) %>%
    distinct() %>%
    left_join(select(ons_table_auto_rurality,
                     ONS_ID,
                     diff_t),
              by = "ONS_ID") %>%
    drop_na()

  final_plot <- data_forplot %>%
    ggplot() +
    geom_point(aes(x = pct_french_only,
                   y = diff_t)) +
    theme_minimal() +
    labs(x = "% Who can speak French only",
         y = "Difference in drive time (FR - EN)",
         title = "Neighbourhood Differences in Drive Times and % French-Only Speakers")

  ggsave("outputs/pub_figures/Fig3-Differences_in_drive_times_and_pct_FR_only_speakers.png",
         width = 7, height = 5)
}
