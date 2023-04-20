################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# November 2022
# 02_identify_outliers.R
#
# Script with functions to identify analytical outliers and samples that were
# potentially contaminated when processed
################################################################################


#'
#'
#'
#'
#'
# function to display boxplots with labeled outliers
boxplot_id_outliers <- function(res_tib, 
                                sample_type # either "scat" or "fish"
) {
  
  # small function to find statistical outliers
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  if (sample_type == "fish") {
    
    res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                    sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
      dplyr::group_by(Nutrient) |>
      dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), 
                                     Code_sample, NA)) |>
      ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, 
                                   fill = Nutrient)) +
      ggplot2::geom_text(ggplot2::aes(label = outlier), na.rm=TRUE, 
                         hjust=-.1, size = 3.5) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                            "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                            "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                            "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                            "#F0D77BFF")) +
      ggplot2::facet_wrap(~ Nutrient, scale = "free") +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                     legend.position = "none")
    ggplot2::ggsave(paste0("output/outliers/boxplot_stat_outliers_",
                           sample_type, 
                           ".jpg"), 
                    scale = 1,
                    height = 12, width = 17
    )
  } else if (sample_type == "scat") {
    res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::group_by(Nutrient) |>
      dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw),
                                     Code_sample, NA)) |>
      ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, 
                                   fill = Nutrient)) +
      ggplot2::geom_text(ggplot2::aes(label = outlier), na.rm=TRUE, 
                         hjust=-.1, size = 3.5) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                            "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                            "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                            "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                            "#F0D77BFF")) +
      ggplot2::facet_wrap(~ Nutrient, scale = "free") +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                     legend.position = "none")
    ggplot2::ggsave(paste0("output/outliers/boxplot_stat_outliers_",
                           sample_type, 
                           ".jpg"), 
                    scale = 1,
                    height = 12, width = 17
    )
  }
  
}

#'
#'
#'
#'
#'
# function to show histograms and identify extreme values 
# analytical outliers 
hist_id_outliers <- function(res_tib, 
                             sample_type # either "scat" or "fish"
                             ) {
  
  
  if (sample_type == "fish") {
    res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                    sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
      dplyr::group_by(Nutrient) |>
      ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, fill = Nutrient)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                            "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                            "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                            "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                            "#F0D77BFF")) +
      ggplot2::facet_wrap(~ Nutrient, scale = "free") +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                     legend.position = "none")
    ggplot2::ggsave(paste0("output/outliers/hist_outliers_",
                           sample_type,
                           ".jpg"),
                    scale = 1,
                    height = 12, width = 17
    )
  } else if (sample_type == "scat") {
    res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::group_by(Nutrient) |>
      ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, fill = Nutrient)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                            "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                            "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                            "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                            "#F0D77BFF")) +
      ggplot2::facet_wrap(~ Nutrient, scale = "free") +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                     legend.position = "none")
    ggplot2::ggsave(paste0("output/outliers/hist_outliers_",
                           sample_type,
                           ".jpg"),
                    scale = 1,
                    height = 12, width = 17
    )
  }
  
}



#'
#'
#'
#'
#'
# function to generate tables with all samples identified as 
# statistical outliers, and 
tib_id_outliers <- function(res_tib, # tibble with results of compo of prey
                            fish_samples_tib, # tibble with data on samples 
                            sample_type, # either "scat" or "fish"
                            name_file
) {
  
  # small function to find outliers
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  options(scipen = 999)
  
  if (sample_type == "fish") {
    table <- res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::mutate(Code_sample = 
                      dplyr::case_when(Code_sample == "2005_GYMNBOL_GB6AB" ~ "2005_GYMNBOL_GB6", # name not adapted when gone to analysis
                                                   TRUE ~ Code_sample)) |>
      dplyr::group_by(Nutrient) |>
      dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), 
                                     Code_sample, NA), 
                    mean_all = mean(concentration_mg_g_dw), 
                    min_all = min(concentration_mg_g_dw), 
                    max_all = max(concentration_mg_g_dw)) |>
      dplyr::filter(!is.na(outlier)) |> # keep only samples with abnormal values 
      dplyr::left_join(fish_samples_tib |>
                         dplyr::rename(Code_sample = Code_new_format),
                       by = "Code_sample") |>
      dplyr::select(Code_sample, Nutrient, concentration_mg_g_dw, 
                    mean_all, min_all, max_all,
                    Water_percent, Prepa_operator, Comment) |>
      dplyr::arrange(Code_sample, Nutrient)
  } else if (sample_type == "scat") {
    table <- res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::group_by(Nutrient) |>
      dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw),
                                     Code_sample, NA), 
                    mean_all = mean(concentration_mg_g_dw), 
                    min_all = min(concentration_mg_g_dw), 
                    max_all = max(concentration_mg_g_dw)) |>
      dplyr::filter(!is.na(outlier)) |> # keep only samples with abnormal values 
      dplyr::select(Code_sample, Nutrient, concentration_mg_g_dw, 
                    mean_all, min_all, max_all) |>
      dplyr::arrange(Code_sample, Nutrient)
  }
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/outliers/", name_file, ".xlsx"))
  
}


#'
#'
#'
#'
#'
# function to add data to the result table with composition of prey
complete_fish_data <- function(res_fish_tib,
                               diet_tib) {
  
  # create vector with list of species identified as prey of Arctocephalus gazella in the SO
  prey_sp <- unique(diet_tib$Species)
  
  res_fish_tib |>
    dplyr::mutate(campaign = dplyr::case_when(stringr::str_starts(Code_sample, 
                                                                  "2005", 
                                                                  negate = FALSE) ~ "Isotopes 2005", 
                                              stringr::str_starts(Code_sample, 
                                                                  "2010", 
                                                                  negate = FALSE) ~ "Poker II 2010"), 
                  diet = dplyr::case_when(Species %in% c(prey_sp) ~ 1, 
                                          TRUE ~ 0))
}