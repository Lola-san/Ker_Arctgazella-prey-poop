################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# March (end) 2022
# 05_compair_compo.R
#
# Script with functions to compair graphically composition of preys and poop
################################################################################





#'
#'
#'
# function to pull together prey and poop data
pool_fish_scat <- function(res_fish_tib, 
                           res_scat_tib) {
  
  # add column with NAs in each table for each variable valuable for only one type of data
  # ie site for poop or species for preys
  
  res_fish_tib <- res_fish_tib |>
    dplyr::mutate(site = NA) |>
    dplyr::select("Code_sample", "As", "Ca", "Co",
                  "Cu", "Fe", "K", "Mg", "Mn",
                  "Na", "Ni", "P", "Se", "Zn",
                  "Family", "Species", "campaign", "diet", "site") |>
    dplyr::mutate(type = "fish")
  
  
  res_scat_tib <- res_scat_tib |>
    dplyr::mutate(Family = NA, 
                  Species = NA, 
                  campaign = NA, 
                  diet = NA) |>
    dplyr::select("Code_sample", "As", "Ca", "Co",
                  "Cu", "Fe", "K", "Mg", "Mn",
                  "Na", "Ni", "P", "Se", "Zn",
                  "Family", "Species", "campaign", "diet", "site")|>
    dplyr::mutate(type = "fur seal scat")
  
  # join the two tibbles by row
  
  rbind(res_fish_tib, res_scat_tib)
  
}

#'
#'
#'
# function to pull together fish and scat data but with fish 
# data pooled per species 
pool_fish_sp_means_scat <- function(res_fish_tib, 
                           res_scat_tib) {
  
  # add column with NAs in each table for each variable valuable for only one type of data
  # ie site for poop or species for preys
  
  res_fish_tib <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(diet, Family, Species, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_g_dw)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = mean_sp) |>
    dplyr::mutate(site = NA) |>
    dplyr::select("As", "Ca", "Co",
                  "Cu", "Fe", "K", "Mg", "Mn",
                  "Na", "Ni", "P", "Se", "Zn",
                  "Family", "Species", "diet", "site") |>
    dplyr::mutate(type = "fish")
  
  
  res_scat_tib <- res_scat_tib |>
    dplyr::mutate(Family = NA, 
                  Species = NA, 
                  diet = NA) |>
    dplyr::select("As", "Ca", "Co",
                  "Cu", "Fe", "K", "Mg", "Mn",
                  "Na", "Ni", "P", "Se", "Zn",
                  "Family", "Species", "diet", "site")|>
    dplyr::mutate(type = "fur seal scat")
  
  # join the two tibbles by row
  
  rbind(res_fish_tib, res_scat_tib)
  
}



#'
#'
#'
#'
#'
# function to compair composition of all fish analized
# A. gazella and poop of A. gazella
boxplot_compair_compo_full <- function(res_fish_scat_pooled
) {
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             concentration_mg_g_dw), 
                                 y = log(concentration_mg_g_dw), fill = type)) +
    #ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(" concentration (in mg/g dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave("output/boxplot_comp_fish_scat_log.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
  
}

#'
#'
#'
#'
#'
# function to compair composition of fish species identified as prey of
# A. gazella and poop of A. gazella
boxplot_compair_compo_prey <- function(res_fish_scat_pooled
) {
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(type == "fish" & diet %in% c(1, 
                                                                       NA # NA are for scat data 
    ) ~ "fur seal fish prey", 
    type == "fur seal scat" ~ "fur seal scat",
    TRUE ~ "other fish")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             concentration_mg_g_dw), 
                                 y = log(concentration_mg_g_dw), fill = type)) +
    #ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(" concentration (in mg/g dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave("output/boxplot_comp_prey_scat_log.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
}



#'
#'
#'
#'
#'
# function to compair composition of all fish analized
# A. gazella and poop of A. gazella
# but with relative compositions (i.e. relative per sample)
boxplot_compair_compo_fish_scat_relative <- function(res_fish_scat_pooled
) {
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_g_dw/sum) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(relative_concentration))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             relative_concentration), 
                                 y = log(relative_concentration), fill = type)) +
    #ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(" concentration (in mg/g dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave("output/boxplot_comp_fish_poop_relative_log.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
  
}


#'
#'
#'
#'
#'
# function to compair relative composition of 
# the mean food bowl of A. gazella across sources
# fish and scats
boxplot_compair_compo_fish_scat_bowl_relative <- function(res_fish_scat_pooled, 
                                                          conc_diet_tib
) {
  
  # join the two tibbles by row
  
  tib_to_plot <- rbind(res_fish_scat_pooled |>
                         dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                                         Mg + Mn + Na + Ni + P + Se + Zn) |>
                         tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                                             names_to = "Nutrient", 
                                             values_to = "concentration_mg_g_dw") |>
                         dplyr::mutate(relative_concentration = concentration_mg_g_dw/sum) |>
                         dplyr::select(type, Nutrient, relative_concentration), 
                       conc_diet_tib |>
                         dplyr::mutate(type = "fish ration") |>
                         dplyr::select(type, Nutrient, relative_concentration))
  
  tib_to_plot |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             relative_concentration), 
                                 y = log(relative_concentration), fill = type)) +
    #ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0("Relative concentration")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave("output/boxplot_comp_all_relative_log.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
  
}