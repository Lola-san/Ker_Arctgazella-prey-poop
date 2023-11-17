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
    dplyr::mutate(type = "forage fish")
  
  
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
# function to pull together fish and scat data but with fish 
# data not pooled per species 
pool_fish_full_scat <- function(res_fish_tib, 
                                    res_scat_tib) {
  
  # add column with NAs in each table for each variable valuable for only one type of data
  # ie site for poop or species for preys
  
  res_fish_tib <- res_fish_tib |>
    dplyr::mutate(site = NA) |>
    dplyr::select("As", "Ca", "Co",
                  "Cu", "Fe", "K", "Mg", "Mn",
                  "Na", "Ni", "P", "Se", "Zn",
                  "Family", "Species", "diet", "Code_sample", "site") |>
    dplyr::mutate(type = "forage fish")
  
  
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
# compute fold-change ratio between concentration in fish and in scats  
comp_ratios_fish_vs_scat <- function(res_fish_scat_pooled) {
  
  options(scipen = 999)
  
  # with mean concentrations
  table_means <- res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(med_conc = round(median(concentration_mg_kg_dw), 2)) |>
    tidyr::pivot_wider(names_from = type, 
                       values_from = med_conc) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(ratio_fish_to_scats = `forage fish`/`fur seal scat`, 
                     ratio_scats_to_fish = `fur seal scat`/`forage fish`)
    
  openxlsx::write.xlsx(table_means, 
                       file = paste0("output/comp_ratios_fish_vs_scats_means.xlsx"))

  # with median concentrations
  table_med <- res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(med_conc = round(median(concentration_mg_kg_dw), 2)) |>
    tidyr::pivot_wider(names_from = type, 
                       values_from = med_conc) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(ratio_fish_to_scats = `forage fish`/`fur seal scat`, 
                     ratio_scats_to_fish = `fur seal scat`/`forage fish`)
  
  openxlsx::write.xlsx(table_med, 
                       file = paste0("output/comp_ratios_fish_vs_scats_medians.xlsx"))
  
}

#'
#'
#'
#'
#'
# function to create table with stats of compo of both scats and fish
table_comparison_stats <- function(res_fish_scat_pooled
) {
  
  options(scipen = 999)
  
  table_comp <- res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     median = round(median(concentration_mg_kg_dw), 2), 
                     min = round(min(concentration_mg_kg_dw), 2), 
                     max = round(max(concentration_mg_kg_dw), 2), 
                     `2.5% quantile` = round(quantile(concentration_mg_kg_dw, 
                                                      probs = c(0.025)), 2), 
                     `97.5% quantile` = round(quantile(concentration_mg_kg_dw, 
                                                       probs = c(0.975)), 2),
                     IQR = round(stats::IQR(concentration_mg_kg_dw), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    tidyr::pivot_longer(cols = c(mean:cv), 
                        names_to = "Statistic",
                        values_to = "value") |>
    dplyr::mutate(Statistic = factor(Statistic, 
                                     levels = c("mean", "median", "sd", "cv",
                                                "2.5% quantile", "97.5% quantile", 
                                                "IQR", "min", "max"))) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = value) |>
    dplyr::arrange(type, Statistic)
  
  openxlsx::write.xlsx(table_comp, 
                       file = "output/comp_stats_fish_vs_scats.xlsx")
  
  
}


#'
#'
#'
#'
#'
# function to create barplot displaying CV for major and trace nutrients in
# both scats and prey
barplot_cv_comp_nut <- function(res_fish_scat_pooled
) {
  
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")), 
                  major_or_trace = dplyr::case_when(Nutrient %in% c("Ca", "P", 
                                                                    "Na", "K", 
                                                                    "Mg") ~ "Major", 
                                                    Nutrient %in% c("Fe", "Zn",
                                                                    "Cu", "Mn", 
                                                                    "Se", "As", 
                                                                    "Ni","Co") ~ "Trace"), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "A.gazella scats", 
                                          TRUE ~ type), 
                                levels = c("forage fish", "A.gazella scats"))
                  ) |>
    dplyr::group_by(major_or_trace, Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     median = round(median(concentration_mg_kg_dw), 2), 
                     IQR = round(stats::IQR(concentration_mg_kg_dw), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                       y = cv, 
                                       fill = major_or_trace), 
                          stat = "identity") +
    ggplot2::scale_fill_manual(values = c("Major" = "#DE7862FF", 
                                           "Trace" = "#1D2645FF")) +
    ggplot2::facet_wrap(~ type) +
    ggplot2::ylab("Coefficient of variation") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   strip.text = ggplot2::element_text(size = 15),
                   legend.title = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(1, "cm")
    )
  ggplot2::ggsave("output/Cv_per_nut_comp.jpg",
                  scale = 1,
                  height = 4, width = 11
  )
  

  
  
}


#'
#'
#'
#'
#'
# function to compare composition of all fish analized
# and scat of A. gazella
lineplot_compare_compo_abs <- function(res_fish_scat_pooled, 
                                       file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "Antarctic fur\n seal scats", 
                                                 type == "forage fish" ~ "forage\nfish"), 
                                levels = c("forage\nfish", "Antarctic fur\n seal scats"))) |>
    dplyr:: group_by(type, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = type), 
                            linewidth = 2, 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = median, 
                                     color = type), 
                        size = 3, 
                        position = ggplot2::position_dodge(0.5)) +
    ggplot2::scale_color_manual(values = c("Antarctic fur\n seal scats" = "#4C413FFF", 
                                          "forage\nfish" = "#278B9AFF")) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
  ggplot2::ylab("Absolute concentration\n(in mg/kg dry weight)") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.title = ggplot2::element_blank(), 
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(1.5, "cm") 
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
  
}



#'
#'
#'
#'
#'
# function to compare composition of all fish analized
# and scat of A. gazella
boxplot_compare_compo_abs <- function(res_fish_scat_pooled, 
                                       file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "Antarctic fur\n seal scats", 
                                                 type == "forage fish" ~ "forage\nfish"), 
                                levels = c("forage\nfish", "Antarctic fur\n seal scats"))) |>
    # dplyr:: group_by(type, Nutrient) |>
    # dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
    #                                         probs = c(0.025)), 
    #                  mean = mean(concentration_mg_kg_dw), 
    #                  median = median(concentration_mg_kg_dw), 
    #                  `97.5_quant` = quantile(concentration_mg_kg_dw, 
    #                                          probs = c(0.975))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, 
                             y = concentration_mg_kg_dw, 
                             fill = type)) +
    ggplot2::geom_violin(ggplot2::aes(color = type, 
                                    fill = type), 
                       position = ggplot2::position_dodge(width = 1),
                       width = 1.4, 
                       alpha = 0.5) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::scale_color_manual(values = c("Antarctic fur\n seal scats" = "#4C413FFF", 
                                           "forage\nfish" = "#278B9AFF")) +
    ggplot2::scale_fill_manual(values = c("Antarctic fur\n seal scats" = "#4C413FFF", 
                                           "forage\nfish" = "#278B9AFF")) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Absolute concentration\n(in mg/kg dry weight)") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.title = ggplot2::element_blank(), 
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(1.5, "cm") 
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
  
}




#'
#'
#'
#'
#'
# function to compare composition of fish analised with distinction of ided prey
# and scat of A. gazella
lineplot_compare_compo_prey_abs <- function(res_fish_scat_pooled, 
                                             file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "forage fish" & diet %in% c(1, 
                                                                                     NA # NA are for scat data 
                  ) ~ "fur seal\nfish prey", 
                  type == "fur seal scat" ~ "fur seal\nscat",
                  TRUE ~ "other\nfish"), 
                  levels = c("fur seal\nfish prey", 
                             "other\nfish",
                             "fur seal\nscat"))) |>
    dplyr:: group_by(type, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = type), 
                            linewidth = 2, 
                            position = ggplot2::position_dodge(0.8)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = median, 
                                     color = type), 
                        size = 3, 
                        position = ggplot2::position_dodge(0.8)) +
    ggplot2::scale_color_manual(values = c("fur seal\nscat" = "#4C413FFF", 
                                           "fur seal\nfish prey" = "#278B9AFF", 
                                           "other\nfish" = "#B4DAE5FF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::coord_flip() +
    ggplot2::ylab("Concentration (in mg/kg dry weight),\nnormalised per nutrient") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.title = ggplot2::element_blank(), 
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(1.5, "cm") 
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
  
}


#'
#'
#'
#'
#'
# function to compare composition of all fish analized
# and scat of A. gazella
lineplot_compare_compo_norm <- function(res_fish_scat_pooled, 
                                       file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "A.gazella\nscats", 
                                                 type == "forage fish" ~ "forage\nfish"), 
                                levels = c("forage\nfish", "A.gazella\nscats"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr:: group_by(type, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(conc_norm, 
                                            probs = c(0.025)), 
                     mean = mean(conc_norm), 
                     median = median(conc_norm), 
                     `97.5_quant` = quantile(conc_norm, 
                                             probs = c(0.975))) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = type), 
                            linewidth = 2, 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = median, 
                                     color = type), 
                        size = 3, 
                        position = ggplot2::position_dodge(0.5)) +
    ggplot2::scale_color_manual(values = c("A.gazella\nscats" = "#4C413FFF", 
                                           "forage\nfish" = "#278B9AFF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::coord_flip() +
    ggplot2::ylab("Concentration (in mg/kg dry weight),\nnormalised per nutrient") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.title = ggplot2::element_blank(), 
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(1.5, "cm") 
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
  
}



#'
#'
#'
#'
#'
# function to compare composition of fish analised with distinction of ided prey
# and scat of A. gazella
lineplot_compare_compo_prey_norm <- function(res_fish_scat_pooled, 
                                        file_name
) {
  options(scipen = 999)

  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "forage fish" & diet %in% c(1, 
                                                                                     NA # NA are for scat data 
                  ) ~ "fur seal\nfish prey", 
                  type == "fur seal scat" ~ "fur seal\nscat",
                  TRUE ~ "other\nfish"), 
                                levels = c("fur seal\nfish prey", 
                                           "other\nfish",
                                           "fur seal\nscat"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr:: group_by(type, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(conc_norm, 
                                            probs = c(0.025)), 
                     mean = mean(conc_norm), 
                     median = median(conc_norm), 
                     `97.5_quant` = quantile(conc_norm, 
                                             probs = c(0.975))) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = type), 
                            linewidth = 2, 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = median, 
                                     color = type), 
                        size = 3, 
                        position = ggplot2::position_dodge(0.5)) +
    ggplot2::scale_color_manual(values = c("fur seal\nscat" = "#4C413FFF", 
                                          "fur seal\nfish prey" = "#278B9AFF", 
                                          "other\nfish" = "#B4DAE5FF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::coord_flip() +
    ggplot2::ylab("Concentration (in mg/kg dry weight),\nnormalised per nutrient") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.title = ggplot2::element_blank(), 
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(1.5, "cm") 
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
  
}




#'
#'
#'
#'
#'
# function to compare composition of all fish analized
# and scat of A. gazella
boxplot_compare_compo_norm <- function(res_fish_scat_pooled, 
                                        file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "A.gazella\nscats", 
                                                 type == "forage fish" ~ "forage\nfish"), 
                                levels = c("forage\nfish", "A.gazella\nscats"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Nutrient, 
                                     y = conc_norm, 
                                     fill = type), 
                        position = ggplot2::position_dodge(0.75)) +
    ggplot2::scale_fill_manual(values = c("A.gazella\nscats" = "#4C413FFF", 
                                           "forage\nfish" = "#278B9AFF")) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.title = ggplot2::element_blank(), 
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 15), 
                   legend.key.height = ggplot2::unit(1.5, "cm") 
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
  
}



#'
#'
#'
#'
#'
# function to compare composition of all fish analized 
# density plot
densplot_compare_compo_full <- function(res_fish_scat_pooled, 
                                       file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    # normalise between 0 and 1 for each nutrient 
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(normalise_conc = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = normalise_conc,
                                 fill = type)) +
    ggplot2::geom_density(alpha = 0.8) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("fur seal scat" = "#4C413FFF", 
                                          "forage fish" = "#278B9AFF")) +
    ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)\nnormalised between 0 and 1") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   strip.text = ggplot2::element_text(size = 15, face = "bold"),
                   legend.title = ggplot2::element_blank()
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 7, width = 12
  )
  
  
}


#'
#'
#'
#'
#'
# function to compare composition of all fish analized 
# density plot normalised per type
densplot_compare_compo_full_norm_per_type <- function(res_fish_scat_pooled, 
                                        file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "A.gazella scats", 
                                                 type == "forage fish" ~ "forage fish"), 
                                levels = c("forage fish", "A.gazella scats"))) |>
    # normalise between 0 and 1 for each nutrient 
    dplyr::group_by(Nutrient, type) |>
    dplyr::mutate(normalise_conc = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = normalise_conc,
                                 fill = type)) +
    ggplot2::geom_density(alpha = 0.8) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("A.gazella scats" = "#4C413FFF", 
                                          "forage fish" = "#278B9AFF")) +
    ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)\nnormalised between 0 and 1") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 18, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   strip.text = ggplot2::element_text(size = 15, face = "bold"),
                   panel.spacing.x = ggplot2::unit(0.5, "cm"),
                   legend.position = "none"
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 7, width = 10
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
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(type == "forage fish" & diet %in% c(1, 
                                                                       NA # NA are for scat data 
                                                                       ) ~ "fur seal fish prey", 
                                          type == "fur seal scat" ~ "fur seal scat",
                                          TRUE ~ "other fish")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("fur seal scat" = "#4C413FFF", 
                                          "fur seal fish prey" = "#278B9AFF", 
                                          "other fish" = "#B4DAE5FF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
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
                  height = 8, width = 9
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
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = 100*(concentration_mg_kg_dw/sum)) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(type == "forage fish" & diet %in% c(1, 
                                                                       NA # NA are for scat data 
                                                                       ) ~ "fur seal fish prey", 
                                          type == "fur seal scat" ~ "fur seal scat",
                                          TRUE ~ "other fish")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             relative_concentration), 
                                 y = relative_concentration, fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("fur seal scat" = "#4C413FFF", 
                                          "fur seal fish prey" = "#278B9AFF", 
                                          "other fish" = "#B4DAE5FF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Relative concentration (in %)") +
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
                  height = 8, width = 9
  )
  
}


#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and scat of A. gazella, in relative composition
# but just taking the trace elements (as major are weighing too much +
# associated to different metabolisms)
lineplot_compare_compo_fish_scat_relative_trace_only <- function(res_fish_scat_pooled, 
                                                file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Co + Cu + Fe +
                    Mn + Ni + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Co, Cu, Fe, Mn, Ni, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = 100*(concentration_mg_kg_dw/sum)) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "Antarctic fur\n seal scats", 
                                                 type == "forage fish" ~ "forage\nfish"), 
                                levels = c("forage\nfish", "Antarctic fur\n seal scats"))) |>
    dplyr:: group_by(type, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(relative_concentration, 
                                            probs = c(0.025)), 
                     mean = mean(relative_concentration), 
                     median = median(relative_concentration), 
                     `97.5_quant` = quantile(relative_concentration, 
                                             probs = c(0.975))) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = type), 
                            linewidth = 2, 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = median, 
                                     color = type), 
                        size = 3, 
                        position = ggplot2::position_dodge(0.5)) +
    ggplot2::scale_color_manual(values = c("Antarctic fur\n seal scats" = "#4C413FFF", 
                                          "forage\nfish" = "#278B9AFF")) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Relative concentration (in %)") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.spacing.y = ggplot2::unit(5, "cm"),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 14),
                   legend.key.height = ggplot2::unit(1.5, "cm")
    )
  ggplot2::ggsave(paste0("output/",
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
}



#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and scat of A. gazella, in relative composition
# but just taking the trace elements (as major are weighing too much +
# associated to different metabolisms)
boxplot_compare_compo_fish_scat_relative_trace_only <- function(
    res_fish_scat_pooled, 
    file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Co + Cu + Fe +
                    Mn + Ni + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Co, Cu, Fe, Mn, Ni, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = 100*(concentration_mg_kg_dw/sum)) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca")), 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "Antarctic fur\n seal scats", 
                                                 type == "forage fish" ~ "forage\nfish"), 
                                levels = c("forage\nfish", "Antarctic fur\n seal scats"))) |>
    # dplyr::group_by(type, Nutrient) |>
    # dplyr::summarise(`2.5_quant` = quantile(relative_concentration, 
    #                                         probs = c(0.025)), 
    #                  mean = mean(relative_concentration), 
    #                  median = median(relative_concentration), 
    #                  `97.5_quant` = quantile(relative_concentration, 
    #                                          probs = c(0.975))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, 
                                 y = relative_concentration, 
                                 fill = type)) +
    ggplot2::geom_violin(ggplot2::aes(color = type, 
                                      fill = type), 
                         position = ggplot2::position_dodge(width = 1),
                         width = 1.4, 
                         alpha = 0.5) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 1)) +
    ggplot2::scale_color_manual(values = c("Antarctic fur\n seal scats" = "#4C413FFF", 
                                           "forage\nfish" = "#278B9AFF")) +
    ggplot2::scale_fill_manual(values = c("Antarctic fur\n seal scats" = "#4C413FFF", 
                                           "forage\nfish" = "#278B9AFF")) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Relative concentration (in %)") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.spacing.y = ggplot2::unit(5, "cm"),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 14),
                   legend.key.height = ggplot2::unit(1.5, "cm")
    )
  ggplot2::ggsave(paste0("output/",
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 5
  )
  
}





#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and scat of A. gazella, in relative composition
# but just taking the major elements 
boxplot_compare_compo_fish_scat_relative_major_only <- function(res_fish_scat_pooled, 
                                                                file_name
) {
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = Ca + K + Mg + Na + P) |>
    tidyr::pivot_longer(cols = c(Ca, K, Mg, Na, P), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = 100*(concentration_mg_kg_dw/sum)) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             relative_concentration), 
                                 y = relative_concentration, fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("fur seal scat" = "#4C413FFF", 
                                          "forage fish" = "#278B9AFF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Relative concentration (in %)") +
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
                  height = 8, width = 9
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
  options(scipen = 999)
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = 100*(concentration_mg_kg_dw/sum)) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(relative_concentration))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             relative_concentration), 
                                 y = relative_concentration, fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("fur seal scat" = "#4C413FFF", 
                                          "forage fish" = "#278B9AFF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab("Relative concentration (in %)") +
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
                  height = 8, width = 9
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
boxplot_compare_compo_fish_scat_relative_major_vs_trace <- function(res_fish_scat_pooled, 
                                                     file_name
) {
  options(scipen = 999)
  
  table_ratio_stats <- res_fish_scat_pooled |>
    dplyr::mutate(sum_trace = As + Co + Cu + Fe + 
                    Mn + Ni + Se + Zn, 
                  sum_major = Ca + K + Mg + Na + P,
                  sum_tot = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn, 
                  ratio_trace_vs_major = sum_trace/sum_major, 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "Antarctic fur\n seal scats", 
                                                 type == "forage fish" ~ "forage fish"), 
                                levels = c("forage fish", "Antarctic fur\n seal scats"))) |>
    dplyr::group_by(type) |>
    dplyr::summarise(`2.5_quant` = quantile(ratio_trace_vs_major, 
                                            probs = c(0.025)), 
                     mean = mean(ratio_trace_vs_major), 
                     median = median(ratio_trace_vs_major), 
                     `97.5_quant` = quantile(ratio_trace_vs_major, 
                                             probs = c(0.975)), 
                     sd = sd(ratio_trace_vs_major))
  
  openxlsx::write.xlsx(table_ratio_stats, 
                       file = paste0("output/comp_ratios_fish_vs_scats_trace_vs_major.xlsx"))
  
  res_fish_scat_pooled |>
    dplyr::mutate(sum_trace = As + Co + Cu + Fe + 
                    Mn + Ni + Se + Zn, 
                  sum_major = Ca + K + Mg + Na + P,
                  sum_tot = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn, 
                  ratio_trace_vs_major = sum_trace/sum_major, 
                  type = factor(dplyr::case_when(type == "fur seal scat" ~ "Antarctic fur\n seal scats", 
                                                 type == "forage fish" ~ "forage\nfish"), 
                                levels = c("Antarctic fur\n seal scats", "forage\nfish"))) |>
    ggplot2::ggplot(ggplot2::aes(x = type, 
                                 y = ratio_trace_vs_major, fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = c("Antarctic fur\n seal scats" = "#4C413FFF", 
                                          "forage\nfish" = "#278B9AFF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::coord_flip() +
    ggplot2::ylab("Ratio\n(trace nutrients)/(major nutrients)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20), 
                   axis.text.y = ggplot2::element_text(size = 22, face = "bold"), 
                   axis.title.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 22, face = "bold"), 
                   legend.position = "none"
    )
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 4, width = 7
  )
  
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# absolute concentrations of nutrients in fish and scats 
MWtest_conc_fish_scats <- function(res_fish_scat_pooled, 
                                       file_name # should specify if there is pup or no pup
) {
  
  compo_tib <- res_fish_scat_pooled |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = concentration_mg_kg_dw)
    
    fish <- na.omit(table$`forage fish`)
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
                       file = paste0("output/Mann_Whitney_test_fish_scats_absolute_",
                                     file_name, 
                                     ".xlsx"))
  
  
  
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
    
    fish <- na.omit(table$`forage fish`)
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

#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# relative concentrations of nutrients in fish and scats 
MWtest_conc_rel_fish_scats_trace_only <- function(res_fish_scat_pooled, 
                                       file_name # should specify if there is pup or no pup
) {
  
  compo_tib <- res_fish_scat_pooled |>
    dplyr::mutate(sum = As + Co + Cu + Fe +
                    Mn + Ni + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Co, Cu, Fe, Mn, Ni, Se, Zn), 
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
    
    fish <- na.omit(table$`forage fish`)
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
                       file = paste0("output/Mann_Whitney_test_fish_scats_relative_trace_only_",
                                     file_name, 
                                     ".xlsx"))
  
  
  
}

## covariation between nutrients 

#'
#'
#'
#'
#'
# function to display correlation plot of elemental composition of fish
corr_compo_fish <- function(res_fish_tib) {
  
  corr_mat <- robCompositions::corCoDa(
    as.data.frame(res_fish_tib |>
                    dplyr::select(c(Ca, P, Na, K, Mg, 
                                    Fe, Zn, Cu, Mn, 
                                    Se, As, Ni, Co 
                    )))) 
  
  colnames(corr_mat) <- rownames(corr_mat) <- c("Ca", "P", "Na", "K", "Mg", 
                                                "Fe", "Zn", "Cu", "Mn", "Se",
                                                "As", "Ni","Co")
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat), 
                                                    na.rm = TRUE)) 
  
  ggplot2::ggplot(data = melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                  high = "#E75B64FF", 
                                  mid = "white", 
                                  midpoint = 0, limit = c(-1,1)) +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Forage fish") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave("output/corrplot_compo_fish.jpg",
                  scale = 1,
                  height = 5, width = 5
  )
  
}



#'
#'
#'
#'
#'
# function to display correlation plot of elemental composition of fish
corr_compo_fish_prey_only <- function(res_fish_tib) {
  
  corr_mat <- robCompositions::corCoDa(
    as.data.frame(res_fish_tib |>
                    # make non-prey and prey species of fur seals distinct
                    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                                          diet == 0 ~ "not prey")) |>
                    dplyr::filter(type == "prey") |>
                    dplyr::select(c(As, Ca, Co, Cu, Fe, K,
                                    Mg, Mn, Na, Ni, P, Se, Zn)))) 
  
  colnames(corr_mat) <- rownames(corr_mat) <- c("As", "Ca", "Co", "Cu", "Fe", 
                                                "K", "Mg", "Mn", "Na", "Ni", 
                                                "P", "Se", "Zn")
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat), 
                                                    na.rm = TRUE)) 
  
  ggplot2::ggplot(data = melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                  high = "#E75B64FF", 
                                  mid = "white", 
                                  midpoint = 0, limit = c(-1,1)) +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Forage fish identified as prey of A. gazella") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/corrplot_compo_fish_prey_only.jpg",
                  scale = 1,
                  height = 5, width = 5
  )
  
}


#'
#'
#'
#'
#'
# function to display correlation plot of elemental composition of scats
corr_compo_scats <- function(res_scat_tib,
                             file_name) {
  
  corr_mat <- robCompositions::corCoDa(
    as.data.frame(res_scat_tib |>
                    dplyr::select(c(Ca, P, Na, K, Mg, 
                                    Fe, Zn, Cu, Mn, 
                                    Se, As, Ni, Co 
                    )))) 
  
  colnames(corr_mat) <- rownames(corr_mat) <- c("Ca", "P", "Na", "K", "Mg", 
                                                "Fe", "Zn", "Cu", "Mn", "Se",
                                                "As", "Ni","Co")
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat), 
                                                    na.rm = TRUE)) 
  
  ggplot2::ggplot(data = melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                  high = "#E75B64FF", 
                                  mid = "white", 
                                  midpoint = 0, limit = c(-1,1),
                                  name = "Correlation\ncoefficient") +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Antarctic fur seal scats") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "right", 
                   legend.title = ggplot2::element_text(size = 13), 
                   legend.text = ggplot2::element_text(size = 11), 
                   legend.title.align = 0)
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 5, width = 6
  )
  
}


#'
#'
#'
#'
#'
# change in nutrient covariation between fish and scats 
covar_changes_fish_scats <- function(res_fish_tib, 
                                     res_scat_tib,
                                     file_name) {
  
  corr_mat_fish <- robCompositions::corCoDa(
    as.data.frame(res_fish_tib |>
                    dplyr::select(c(Ca, P, Na, K, Mg, 
                                    Fe, Zn, Cu, Mn, 
                                    Se, As, Ni, Co 
                    )))) 
  
  colnames(corr_mat_fish) <- rownames(corr_mat_fish) <- c("Ca", "P", "Na", "K", "Mg", 
                                                            "Fe", "Zn", "Cu", "Mn", "Se",
                                                            "As", "Ni","Co")
  
  corr_mat_fish_012 <- tibble::as_tibble(corr_mat_fish) |>
    dplyr::mutate(Ca = dplyr::case_when(Ca > 0.25 ~ 2, 
                                        Ca <= 0.25 & Ca >= -0.25 ~ 1, 
                                        Ca < -0.25 ~ 0), 
                  P = dplyr::case_when(P > 0.25 ~ 2, 
                                       P <= 0.25 & P >= -0.25 ~ 1, 
                                       P < -0.25 ~ 0), 
                  Na = dplyr::case_when(Na > 0.25 ~ 2, 
                                        Na <= 0.25 & Na >= -0.25 ~ 1, 
                                        Na < -0.25 ~ 0), 
                  K = dplyr::case_when(K > 0.25 ~ 2, 
                                       K <= 0.25 & K >= -0.25 ~ 1, 
                                       K < -0.25 ~ 0), 
                  Mg = dplyr::case_when(Mg > 0.25 ~ 2, 
                                        Mg <= 0.25 & Mg >= -0.25 ~ 1, 
                                        Mg < -0.25 ~ 0), 
                  Fe = dplyr::case_when(Fe > 0.25 ~ 2, 
                                        Fe <= 0.25 & Fe >= -0.25 ~ 1, 
                                        Fe < -0.25 ~ 0), 
                  Zn = dplyr::case_when(Zn > 0.25 ~ 2, 
                                        Zn <= 0.25 & Zn >= -0.25 ~ 1, 
                                        Zn < -0.25 ~ 0), 
                  Cu = dplyr::case_when(Cu > 0.25 ~ 2, 
                                        Cu <= 0.25 & Cu >= -0.25 ~ 1, 
                                        Cu < -0.25 ~ 0), 
                  Mn = dplyr::case_when(Mn > 0.25 ~ 2, 
                                        Mn <= 0.25 & Mn >= -0.25 ~ 1, 
                                        Mn < -0.25 ~ 0), 
                  Se = dplyr::case_when(Se > 0.25 ~ 2, 
                                        Se <= 0.25 & Se >= -0.25 ~ 1, 
                                        Se < -0.25 ~ 0), 
                  As = dplyr::case_when(As > 0.25 ~ 2, 
                                        As <= 0.25 & As >= -0.25 ~ 1, 
                                        As < -0.25 ~ 0), 
                  Ni = dplyr::case_when(Ni > 0.25 ~ 2, 
                                        Ni <= 0.25 & Ni >= -0.25 ~ 1, 
                                        Ni < -0.25 ~ 0), 
                  Co = dplyr::case_when(Co > 0.25 ~ 2, 
                                        Co <= 0.25 & Co >= -0.25 ~ 1, 
                                        Co < -0.25 ~ 0), 
                  Nutrient2 = c("Ca", "P", "Na", "K", "Mg", 
                                "Fe", "Zn", "Cu", "Mn", "Se", "As", "Ni", "Co"))
  
  corr_mat_scats <- robCompositions::corCoDa(
    as.data.frame(res_scat_tib |>
                    dplyr::select(c(Ca, P, Na, K, Mg, 
                                    Fe, Zn, Cu, Mn, 
                                    Se, As, Ni, Co 
                    )))) 
  
  colnames(corr_mat_scats) <- rownames(corr_mat_scats) <- c("Ca", "P", "Na", "K", "Mg", 
                                                "Fe", "Zn", "Cu", "Mn", "Se",
                                                "As", "Ni","Co")
  
  corr_mat_scats_012 <- tibble::as_tibble(corr_mat_scats) |>
    dplyr::mutate(Ca = dplyr::case_when(Ca > 0.25 ~ 2, 
                                        Ca <= 0.25 & Ca >= -0.25 ~ 1, 
                                        Ca < -0.25 ~ 0), 
                  P = dplyr::case_when(P > 0.25 ~ 2, 
                                       P <= 0.25 & P >= -0.25 ~ 1, 
                                       P < -0.25 ~ 0), 
                  Na = dplyr::case_when(Na > 0.25 ~ 2, 
                                        Na <= 0.25 & Na >= -0.25 ~ 1, 
                                        Na < -0.25 ~ 0), 
                  K = dplyr::case_when(K > 0.25 ~ 2, 
                                       K <= 0.25 & K >= -0.25 ~ 1, 
                                       K < -0.25 ~ 0), 
                  Mg = dplyr::case_when(Mg > 0.25 ~ 2, 
                                        Mg <= 0.25 & Mg >= -0.25 ~ 1, 
                                        Mg < -0.25 ~ 0), 
                  Fe = dplyr::case_when(Fe > 0.25 ~ 2, 
                                        Fe <= 0.25 & Fe >= -0.25 ~ 1, 
                                        Fe < -0.25 ~ 0), 
                  Zn = dplyr::case_when(Zn > 0.25 ~ 2, 
                                        Zn <= 0.25 & Zn >= -0.25 ~ 1, 
                                        Zn < -0.25 ~ 0), 
                  Cu = dplyr::case_when(Cu > 0.25 ~ 2, 
                                        Cu <= 0.25 & Cu >= -0.25 ~ 1, 
                                        Cu < -0.25 ~ 0), 
                  Mn = dplyr::case_when(Mn > 0.25 ~ 2, 
                                        Mn <= 0.25 & Mn >= -0.25 ~ 1, 
                                        Mn < -0.25 ~ 0), 
                  Se = dplyr::case_when(Se > 0.25 ~ 2, 
                                        Se <= 0.25 & Se >= -0.25 ~ 1, 
                                        Se < -0.25 ~ 0), 
                  As = dplyr::case_when(As > 0.25 ~ 2, 
                                        As <= 0.25 & As >= -0.25 ~ 1, 
                                        As < -0.25 ~ 0), 
                  Ni = dplyr::case_when(Ni > 0.25 ~ 2, 
                                        Ni <= 0.25 & Ni >= -0.25 ~ 1, 
                                        Ni < -0.25 ~ 0), 
                  Co = dplyr::case_when(Co > 0.25 ~ 2, 
                                        Co <= 0.25 & Co >= -0.25 ~ 1, 
                                        Co < -0.25 ~ 0), 
                  Nutrient2 = c("Ca", "P", "Na", "K", "Mg", 
                                "Fe", "Zn", "Cu", "Mn", "Se", "As", "Ni", "Co"))

  
  # compare the two correlation matrix
  corr_mat_fish_012_long <- corr_mat_fish_012 |>
    tidyr::pivot_longer(cols = c(Ca:Co), 
                        names_to = "Nutrient1", 
                        values_to = "corr_factor") |>
    dplyr::mutate(sample = "fish")
  
  corr_mat_scats_012_long <- corr_mat_scats_012 |>
    tidyr::pivot_longer(cols = c(Ca:Co), 
                        names_to = "Nutrient1", 
                        values_to = "corr_factor") |>
    dplyr::mutate(sample = "scats") 
  
  corr_mat_012_pooled <- corr_mat_scats_012_long |>
    dplyr::bind_rows(corr_mat_fish_012_long) |>
    tidyr::pivot_wider(names_from = sample, 
                       values_from = corr_factor) |>
    dplyr::mutate(change = dplyr::case_when(fish == scats ~ 0, 
                                            TRUE ~ 1))
  
  corr_mat_change <- corr_mat_012_pooled |>
    dplyr::select(-c(fish, scats)) |>
    tidyr::pivot_wider(names_from = Nutrient1, 
                       values_from = change) |>
    as.data.frame() 
  
  rownames(corr_mat_change) <- colnames(corr_mat_change)[2:14]
  
  corr_mat_change <- corr_mat_change |>
    dplyr::select(-Nutrient2)
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat <- tibble::as_tibble(reshape2::melt(as.matrix(get_lower_tri(corr_mat_change)), 
                                                    na.rm = TRUE)) |>
    dplyr::mutate(value = factor(value))
  
  ggplot2::ggplot(data = melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_manual(values = c("lightgrey",
                                          "lightblue"), 
                               name = "Binary\nindicator") +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Change in correlation coefficient between\nforage fish and Antarctic fur seal scats") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16.5, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15.5), 
                   axis.text.y = ggplot2::element_text(size = 15.5), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "right", 
                   legend.title = ggplot2::element_text(size = 13.5), 
                   legend.text = ggplot2::element_text(size = 11.5), 
                   legend.title.align = 0)
  ggplot2::ggsave(paste0("output/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 6.5
  )
  
}






