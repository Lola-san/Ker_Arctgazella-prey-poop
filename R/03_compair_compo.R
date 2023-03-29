################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# March (end) 2022
# 03_compair_compo.R
#
# Script with functions to compair graphically composition of preys and poop
################################################################################





#'
#'
#'
# function to pull together prey and poop data
pool_prey_poop <- function(res_prey_tib, 
                            res_poop_tib) {
  
  # add column with NAs in each table for each variable valuable for only one type of data
  # ie site for poop or species for preys
  
  res_prey_tib <- res_prey_tib |>
    dplyr::mutate(site = NA) |>
    dplyr::select("Code_sample", "As", "Ca", "Co",
                  "Cu", "Fe", "K", "Mg", "Mn",
                  "Na", "Ni", "P", "Se", "Zn",
                  "Family", "Species", "campaign", "diet", "site") |>
    dplyr::mutate(type = "prey")
  
  
  res_poop_tib <- res_poop_tib |>
    dplyr::mutate(Family = NA, 
                  Species = NA, 
                  campaign = NA, 
                  diet = NA) |>
    dplyr::select("Code_sample", "As", "Ca", "Co",
                  "Cu", "Fe", "K", "Mg", "Mn",
                  "Na", "Ni", "P", "Se", "Zn",
                  "Family", "Species", "campaign", "diet", "site")|>
    dplyr::mutate(type = "poop")
  
  # join the two tibbles by row
  
  rbind(res_prey_tib, res_poop_tib)
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
boxplot_compair_compo <- function(res_prey_poop_pooled
                                  ) {
  
  res_prey_poop_pooled |>
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
  # ggplot2::ggsave(paste0("output/per-sp/boxplot_order_sp_",
  #                        nutrient,
  #                        ".jpg"),
  #                 scale = 1,
  #                 height = 12, width = 17
  # )
  
  
}