################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# March (end) 2022
# 05_compare_compo.R
#
# Script with functions to compare graphically composition of preys and poop
################################################################################



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
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(diet, Family, Species, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_kg_dw)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = mean_sp) |>
    dplyr::mutate(Code_sample = NA, 
                  site = NA) |>
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
                  "Family", "Species", "diet", "Code_sample", "site")|>
    dplyr::mutate(type = "fur seal scat")
  
  # join the two tibbles by row
  
  rbind(res_fish_tib, res_scat_tib)
  
}



#'
#'
#'
#'
#'
# function to compare composition of all fish analized
# A. gazella and poop of A. gazella
boxplot_compare_compo_full <- function(res_fish_scat_pooled, 
                                       file_name
) {
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             concentration_mg_kg_dw), 
                                 y = log(concentration_mg_kg_dw), fill = type)) +
    #ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(" concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
  
}

#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and scat of A. gazella
boxplot_compare_compo_prey <- function(res_fish_scat_pooled, 
                                       file_name
) {
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(type == "fish" & diet %in% c(1, 
                                                                       NA # NA are for scat data 
                                                                       ) ~ "fur seal fish prey", 
                                          type == "fur seal scat" ~ "fur seal scat",
                                          TRUE ~ "other fish")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             concentration_mg_kg_dw), 
                                 y = log(concentration_mg_kg_dw), fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(" concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave(paste0("output/", file_name, ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
}


#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and scat of A. gazella, in relative composition
boxplot_compare_compo_prey_relative <- function(res_fish_scat_pooled, 
                                                file_name
) {
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_kg_dw/sum) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(type == "fish" & diet %in% c(1, 
                                                                       NA # NA are for scat data 
                                                                       ) ~ "fur seal fish prey", 
                                          type == "fur seal scat" ~ "fur seal scat",
                                          TRUE ~ "other fish")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             relative_concentration), 
                                 y = log(relative_concentration), fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab("Relative concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave(paste0("output/",
                         file_name, ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
}


#'
#'
#'
#'
#'
# function to compare composition of all fish analized
# A. gazella and scat of A. gazella
# but with relative compositions (i.e. relative per sample)
boxplot_compare_compo_fish_scat_relative <- function(res_fish_scat_pooled, 
                                                     file_name
) {
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_kg_dw/sum) |>
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
    ggplot2::ylab("Relative concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 12, width = 17
  )
  
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# relative concentrations of nutrients in fish and scats 
MWtest_conc_rel_fish_scats <- function(res_fish_scat_pooled, 
                                       file_name # should specify if there is pup or no pup
) {
  
  compo_tib <- res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_kg_dw/sum)
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = relative_concentration)
    
    fish <- na.omit(table$fish)
    scats <- na.omit(table$`fur seal scat`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(fish, scats)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,]
  
  df_test <- df_test |>
    dplyr::mutate(significant = dplyr::case_when(alpha_MW <= 0.05 ~ "yes", 
                                                 TRUE ~ "no"))
  
  openxlsx::write.xlsx(df_test, 
                       file = paste0("output/Mann_Whitney_test_fish_scats_relative_",
                                     file_name, 
                                     ".xlsx"))
  
  
  
}

