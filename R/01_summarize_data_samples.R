################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2022
# 01_summarize_data_samples.R
#
# Script with manipulation functions to format data on preys and create
# a clean table suitable for publication
################################################################################


# load excel files  
load_xl <- function(pathxl) {
  readxl::read_xlsx(pathxl)
}

# clean file and summarise data on samples
summary_fish_samples <- function(fish_tab, 
                                 compo_results_fish) {
  fish_tab |>
    dplyr::filter(Code_new_format %in% compo_results_fish$Code_sample) |>
    dplyr::group_by(Family, Species, Campaign) |>
    dplyr::mutate(SL_cm = as.integer(SL_cm)) |> # generates warnings because
    # of samples with approximate length (*XX) as they were damaged
    dplyr::summarize(n = dplyr::n_distinct(Code_new_format), 
                     length_mean = mean(SL_cm, na.rm = TRUE), 
                     length_min = min(SL_cm, na.rm = TRUE), 
                     length_max = max(SL_cm, na.rm = TRUE), 
                     H20_mean = mean(Water_percent), 
                     H20_min = min(Water_percent), 
                     H20_max= max(Water_percent))
}


# clean file and summarise data on samples
summary_scat_samples <- function(scat_tab) {
  scat_tab |>
    dplyr::group_by(site, date_collecte) |>
    dplyr::mutate(HPI_0 = dplyr::case_when(index_hard_parts == 0 ~ 1,
                                                  TRUE ~ 0),
                  HPI_1 = dplyr::case_when(index_hard_parts == 1 ~ 1,
                                                  TRUE ~ 0),
                  HPI_2 = dplyr::case_when(index_hard_parts == 2 ~ 1,
                                                  TRUE ~ 0), 
                  HPI_3 = dplyr::case_when(index_hard_parts == 3 ~ 1,
                                                  TRUE ~ 0)) |>
    dplyr::summarize(n = dplyr::n_distinct(Code_sample), 
                     wweight_mean = mean(ww), 
                     wweight_min = min(ww), 
                     wweight_max = max(ww), 
                     H20_mean = mean(water_percent), 
                     H20_min = min(water_percent), 
                     H20_max= max(water_percent), 
                     percent_HPI0 = 100*(sum(HPI_0)/n), 
                     percent_HPI1 = 100*(sum(HPI_1)/n), 
                     percent_HPI2 = 100*(sum(HPI_2)/n), 
                     percent_HPI3 = 100*(sum(HPI_3)/n))
}


# clean file and summarise data on diets
summary_diet_data <- function(diet_tab, 
                              compo_results_fish) {
  diet_tab |>
    dplyr::filter(Location  == "Kerguelen Islands", 
                  # select only studies estimating %W
                  Source %in% c("Cherel et al 1997", "Jeanniard-du-Dot 2015", 
                                "Lea et al 2002")) |>
    dplyr::mutate(Source = factor(Source, 
                                  levels = c("Cherel et al 1997",  
                                             "Lea et al 2002",
                                             "Jeanniard-du-Dot 2015")), 
                  fish_analyzed = dplyr::case_when(Species %in% compo_results_fish$Species ~ 1, 
                                                   TRUE ~ 0)) |>
    dplyr::group_by(Source, Year_collection) |>
    dplyr::summarize(n_species_fish = dplyr::n_distinct(Species, na.rm = TRUE), 
                     n_species_fish_analyzed = sum(fish_analyzed), 
                     n_scats = unique(n),
                     Site = unique(Site))
}