################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# November 2022
# 02_characterize_compo.R
#
# Script with functions to characterize results for preys and poop
################################################################################


###################### OUTLIERS TREATMENT ######################################

#'
#'
#'
#'
#'
# function to display boxplots with labeled outliers
boxplot_id_outliers <- function(res_tib, 
                                type # either "poop" or "prey
                                ) {
  
  # small function to find statistical outliers
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  if (type == "prey") {
  
  res_tib |>
    dplyr::select(-c(Cr, Mo, V, # below detection limit
                     Ag, Pb, Cd, Sr)) |> # non essential
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), Code_sample, NA)) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_text(ggplot2::aes(label = outlier), na.rm=TRUE, hjust=-.1, size = 3.5) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF", "#E75B64FF", "#DE7862FF", "#D8AF39FF", "#E8C4A2FF", 
                                                     "#14191FFF", "#1D2645FF", "#403369FF", "#AE93BEFF", "#B4DAE5FF", "#F0D77BFF")) +
                                                       ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/boxplot_outliers_compo_",
                         type, 
                         ".jpg"), 
                  scale = 1,
                  height = 12, width = 17
  )
  } else if (type == "poop") {
    res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::group_by(Nutrient) |>
      dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), Code_sample, NA)) |>
      ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, fill = Nutrient)) +
      ggplot2::geom_text(ggplot2::aes(label = outlier), na.rm=TRUE, hjust=-.1, size = 3.5) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF", "#E75B64FF", "#DE7862FF", "#D8AF39FF", "#E8C4A2FF", 
                                                       "#14191FFF", "#1D2645FF", "#403369FF", "#AE93BEFF", "#B4DAE5FF", "#F0D77BFF")) +
                                                         ggplot2::facet_wrap(~ Nutrient, scale = "free") +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                     legend.position = "none")
    ggplot2::ggsave(paste0("output/boxplot_outliers_compo_",
                           type, 
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
# function to display boxplots with labeled outliers
hist_id_outliers <- function(res_tib, 
                             type) {
  
  # small function to find outliers
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  if (type == "prey") {
  res_tib |>
    dplyr::select(-c(Cr, Mo, V, # below detection limit
                     Ag, Pb, Cd, Sr)) |> # non essential
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), Code_sample, NA)) |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF", 
                                                     "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                                     "#E8C4A2FF", 
                                                     "#14191FFF", "#1D2645FF", "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                                     "#F0D77BFF")) +
                                                       ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/hist_outliers_compo_",
                         type,
                         ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  } else if (type == "poop") {
    res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::group_by(Nutrient) |>
      dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), Code_sample, NA)) |>
      ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, fill = Nutrient)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF", 
                                                       "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                                       "#E8C4A2FF", 
                                                       "#14191FFF", "#1D2645FF", "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                                       "#F0D77BFF")) +
                                                         ggplot2::facet_wrap(~ Nutrient, scale = "free") +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                     legend.position = "none")
    ggplot2::ggsave(paste0("output/hist_outliers_compo_",
                           type,
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
# function to display boxplot of elemental composition per campaign
tib_id_outliers <- function(res_tib, # tibble with results of compo of prey
                            prey_samples_tib, # tibble with data on samples 
                            type, 
                            name_file
) {
  
  # small function to find outliers
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  options(scipen = 999)
  
  if (type == "prey") {
    table <- res_tib |>
    dplyr::select(-c(Cr, Mo, V, # below detection limit
                     Ag, Pb, Cd, Sr)) |> # non essential
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Code_sample = dplyr::case_when(Code_sample == "2005_GYMNBOL_GB6AB" ~ "2005_GYMNBOL_GB6", # name not adapted when gone to analysis
                                                 TRUE ~ Code_sample)) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), Code_sample, NA), 
                  mean_all = mean(concentration_mg_g_dw), 
                  min_all = min(concentration_mg_g_dw), 
                  max_all = max(concentration_mg_g_dw)) |>
    dplyr::filter(!is.na(outlier)) |> # keep only samples with abnormal values 
    dplyr::left_join(prey_samples_tib |>
                       dplyr::rename(Code_sample = Code_new_format), by = "Code_sample") |>
    dplyr::select(Code_sample, Nutrient, concentration_mg_g_dw, 
                  mean_all, min_all, max_all,
                  Water_percent, Prepa_operator, Comment) |>
    dplyr::arrange(Code_sample, Nutrient)
  } else if (type == "poop") {
    table <- res_tib |>
      dplyr::select(-c(Cr, Mo, V, # below detection limit
                       Ag, Pb, Cd, Sr)) |> # non essential
      tidyr::pivot_longer(cols = c("As":"Zn"), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_g_dw") |>
      dplyr::group_by(Nutrient) |>
      dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw), Code_sample, NA), 
                    mean_all = mean(concentration_mg_g_dw), 
                    min_all = min(concentration_mg_g_dw), 
                    max_all = max(concentration_mg_g_dw)) |>
      dplyr::filter(!is.na(outlier)) |> # keep only samples with abnormal values 
      dplyr::select(Code_sample, Nutrient, concentration_mg_g_dw, 
                    mean_all, min_all, max_all) |>
      dplyr::arrange(Code_sample, Nutrient)
  }
  
  openxlsx::write.xlsx(table,
                       file =paste0("output/", name_file, ".xlsx"))
  
}


#'
#'
#'
#'
#'
# function to add data to the result table with composition of prey
complete_prey_results <- function(res_prey_tib, 
                                  diet_tib) {
  
  # create vector with list of species identified as prey of Arctocephalus gazella in the SO
  prey_sp <- unique(diet_tib$Species)
  
  res_prey_tib |>
    dplyr::mutate(campaign = dplyr::case_when(stringr::str_starts(Code_sample, "2005", negate = FALSE) ~ "Isotopes 2005", 
                                              stringr::str_starts(Code_sample, "2010", negate = FALSE) ~ "Poker II 2010"), 
                  diet = dplyr::case_when(Species %in% c(prey_sp) ~ 1, 
                                          TRUE ~ 0))
}



#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
boxplot_compo_prey_sp <- function(res_prey_tib, 
                                  nutrient) {
  
  res_prey_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Species, 
                                             concentration_mg_g_dw), 
                                 y = concentration_mg_g_dw, fill = Species)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "inferno", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/per-sp/boxplot_order_sp_",
                         nutrient,
                         ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per genus
boxplot_compo_prey_genus <- function(res_prey_tib, 
                                     nutrient) {
  
  res_prey_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Genus,
                                             concentration_mg_g_dw),
                                 y = concentration_mg_g_dw, fill = Genus)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "inferno", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
    ggplot2::xlab("Genus") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/per-genus/boxplot_order_genus_",
                         nutrient,
                         ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per family
boxplot_compo_prey_fam <- function(res_prey_tib, 
                                   nutrient) {
  
  res_prey_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Family, 
                                             concentration_mg_g_dw), 
                                 y = concentration_mg_g_dw, fill = Family)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "inferno", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
    ggplot2::xlab("Family") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/per-fam/boxplot_order_fam_",
                         nutrient,
                         ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per campaign
boxplot_compo_prey_camp <- function(res_prey_tib, 
                                    nutrient) {
  
  res_prey_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(campaign,
                                             concentration_mg_g_dw), 
                                 y = concentration_mg_g_dw, fill = campaign)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "inferno", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
    ggplot2::xlab("Campaign") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/per-camp/boxplot_order_camp_",
                         nutrient,
                         ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
}

#######################################################################################################
############################################# POOP ####################################################
#######################################################################################################


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of poops in total without the 
# statistical outlier display
boxplot_compo_poop_tot <- function(res_poop_tib) {
  
  res_poop_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF", "#E75B64FF", "#DE7862FF", "#D8AF39FF", "#E8C4A2FF", 
                                          "#14191FFF", "#1D2645FF", "#403369FF", "#AE93BEFF", "#B4DAE5FF", "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave("output/boxplot_outliers_compo_tot.jpg", 
                  scale = 1,
                  height = 12, width = 17
  )
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per site
boxplot_compo_poop_site <- function(res_poop_tib) {
  
  res_poop_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = site, y = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF", "#E75B64FF", "#DE7862FF", "#D8AF39FF", "#E8C4A2FF", 
                                          "#14191FFF", "#1D2645FF", "#403369FF", "#AE93BEFF", "#B4DAE5FF", "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave("output/boxplot_outliers_compo_sites.jpg", 
                  scale = 1,
                  height = 12, width = 17
  )
  
}


#'
#'
#'
#'
# perform centered-logratio transformation on data 

clr_transf_prey <- function(res_prey_tib) {

  
  res_prey_tib |>
    # remove outlier 2005_PROTAND_PA03
    dplyr::filter(Code_sample != "2005_PROTAND_PA03") |>
    tidyr::pivot_longer(cols = c(Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn),
                        names_to = "Element",
                        values_to = "Concentration") |> 
    dplyr::group_by(Code_sample) |>
    # perform centered log-ratio transformation
    dplyr::mutate(mean_sample = mean(Concentration),
                  product = prod(Concentration),
                  Concentration = log(Concentration/mean_sample))

}




#'
#'
#'
#'
# perform PCA on centered-logratio transformed data 

clr_PCA <- function(clr_prey_tib) {
  
  data.act <- as.data.frame(clr_prey_tib |>
                              tidyr::pivot_wider(names_from = Element,
                                                 values_from = Concentration))
  rownames(data.act) <- data.act$Code_sample
  data.act <- data.act[, 8:20] 
  
  
  res.pca <- FactoMineR::PCA(data.act, ncp = 5, graph = FALSE)
  
  # eigen values
  eig.val <- factoextra::get_eigenvalue(res.pca)
  eig.val
  
  factoextra::fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
  
  var <- factoextra::get_pca_var(res.pca)
  
  factoextra::fviz_pca_var(res.pca, col.var = "black")
  
  corrplot::corrplot(var$cos2, is.corr=FALSE)
  
  #### graph of individuals
  ind <- factoextra::get_pca_ind(res.pca)
  
  factoextra::fviz_pca_ind (res.pca,
                            axes = c(1, 2))
  
  factoextra::fviz_pca_ind (res.pca,
                            axes = c(1, 3))
  
  factoextra::fviz_pca_ind (res.pca,
                            axes = c(2, 3))
  
  
}


#' #'
#' #'
#' #'
#' #'
#' # perform PCA on composition dataset of prey
#' 
#' PCA_prey<- function(res_prey_tib) {
#'   
#'   # keep only quantitative variables 
#'   data.act <- res_prey_tib |>
#'     #dplyr::select(-c(Mo, V, Ag, Cr)) |> #pca1
#'     dplyr::select(-c(Mo, V, Ag, Cr, Pb, As, Cd)) |> #pca2
#'     # remove outlier 2005_PROTAND_PA03
#'     dplyr::filter(Code_sample != "2005_PROTAND_PA03") |>
#'     # tidyr::pivot_longer(cols = c(As, Ca, Cd, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Pb, Se, Zn), 
#'     #                     names_to = "Element", 
#'     #                     values_to = "Concentration") |> #pca1
#'     tidyr::pivot_longer(cols = c(Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
#'                         names_to = "Element", 
#'                         values_to = "Concentration") |> #pca2
#'     dplyr::group_by(Code_sample) |>
#'     # perform centered log-ratio transformation
#'     dplyr::mutate(mean_sample = mean(Concentration),
#'                   Concentration = log(Concentration/mean_sample))
#'   
#'   data.act <- as.data.frame(data.act |> 
#'                               tidyr::pivot_wider(names_from = Element, 
#'                                                  values_from = Concentration))
#'   rownames(data.act) <- data.act$Code_sample
#'   #data.act <- data.act[, 5:19]  #pca1
#'   data.act <- data.act[, 3:15] #pca2
#'   
#'   
#'   res.pca <- FactoMineR::PCA(data.act, ncp = 5, graph = FALSE) 
#'   
#'   # eigen values 
#'   eig.val <- factoextra::get_eigenvalue(res.pca)
#'   eig.val
#'   
#'   factoextra::fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#'   
#'   var <- factoextra::get_pca_var(res.pca)
#'   
#'   factoextra::fviz_pca_var(res.pca, col.var = "black")
#'   
#'   corrplot::corrplot(var$cos2, is.corr=FALSE)
#'   
#'   #### graph of individuals
#'   ind <- factoextra::get_pca_ind(res.pca)
#'   
#'   factoextra::fviz_pca_ind (res.pca, 
#'                             axes = c(1, 2))
#'   
#'   factoextra::fviz_pca_ind (res.pca, 
#'                             axes = c(1, 3))
#'   
#'   factoextra::fviz_pca_ind (res.pca, 
#'                             axes = c(2, 3))
#'   
#'   
#' }
#' 
#' 
#' #'
#' #'
#' #'
#' #'
#' # perform HCA on composition dataset of prey
#' 
#' HCA_prey<- function(res_prey_tib) {
#'   
#'   # keep only quantitative variables 
#'   data.act <- res_prey_tib |>
#'     dplyr::select(-c(Mo, V, Ag, Cr)) |> 
#'     # remove outlier 2005_PROTAND_PA03
#'     dplyr::filter(Code_sample != "2005_PROTAND_PA03")
#'   
#'   data.act <- as.data.frame(data.act)
#'   rownames(data.act) <- data.act$Code_sample
#'   data.act <- data.act[, 4:19]  
#'   
#'   data.act <- scale(data.act)
#'   
#'   #compute distance matrix
#'   d <- dist(data.act, method = "euclidean")
#'   
#'   #perform hierarchical clustering using Ward's method
#'   final_clust <- hclust(d, method = "ward.D2" )
#'   
#'   #cut the dendrogram into 4 clusters
#'   groups <- cutree(final_clust, k=4)
#'   
#'   #find number of observations in each cluster
#'   table(groups)
#'   
#'   #calculate gap statistic for each number of clusters (up to 10 clusters)
#'   gap_stat <- cluster::clusGap(data.act, FUN = factoextra::hcut, 
#'                                nstart = 25, K.max = 10, B = 50)
#'   
#'   #produce plot of clusters vs. gap statistic
#'   factoextra::fviz_gap_stat(gap_stat)
#'   
#'   #cut the dendrogram into 4 clusters
#'   groups <- cutree(final_clust, k=10)
#'   
#'   #find number of observations in each cluster
#'   table(groups)
#'   
#'   
#'   
#'   # HCPC 
#'   res.hcpc <- FactoMineR::HCPC(res.pca, graph = FALSE)
#'   
#'   
#'   factoextra::fviz_dend(res.hcpc, 
#'                         cex = 0.7,                     # Label size
#'                         palette = "jco",               # Color palette see ?ggpubr::ggpar
#'                         rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
#'                         rect_border = "jco",           # Rectangle color
#'                         labels_track_height = 0.8      # Augment the room for labels
#'   )
#'   
#'   factoextra::fviz_cluster(res.hcpc,
#'                            repel = TRUE,            # Avoid label overlapping
#'                            show.clust.cent = TRUE, # Show cluster centers
#'                            palette = "jco",         # Color palette see ?ggpubr::ggpar
#'                            ggtheme = ggplot2::theme_minimal(),
#'                            main = "Factor map"
#'   )
#'   
#'   head(res.hcpc$data.clust, 10)
#'   
#'   res.hcpc$desc.var$quanti
#'   
#'   res.hcpc$desc.axes$quanti
#'   
#'   res.hcpc$desc.ind$para
#'   
#' }
