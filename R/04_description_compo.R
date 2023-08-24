################################################################################
# Ker_Arctgazella-fish-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# November 2022
# 04_description_compo.R
#
# Script with functions to characterize results for fishs and poop
################################################################################


################################################################################
########################### FISH  #############################################
################################################################################

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
table_compo_fish_sp <- function(res_fish_tib, 
                                object_type # either "output" or "file" 
) {
  
  
  if (object_type == "file") {
    table <- res_fish_tib |>
      tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                   Mg, Mn, Na, Ni, P, Se, Zn), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_kg_dw") |>
      # remove NAs if there is still some
      dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
      dplyr::group_by(Family, Species, Nutrient) |>
      dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                       mean = round(mean(concentration_mg_kg_dw), 3),
                       sd = round(sd(concentration_mg_kg_dw), 3)) |> 
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = c(mean, sd), 
                         names_sep = "_")
    
    openxlsx::write.xlsx(table, 
                         file = paste0("output/compo fish/summary_fish_compo_sp.xlsx"))
  } else {
    res_fish_tib |>
      tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                   Mg, Mn, Na, Ni, P, Se, Zn), 
                          names_to = "Nutrient", 
                          values_to = "concentration_mg_kg_dw") |>
      # remove NAs if there is still some
      dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
      dplyr::group_by(Family, Species, Nutrient) |>
      dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                       conc_mg_kg_dw_min = round(min(concentration_mg_kg_dw), 3), 
                       conc_mg_kg_dw_low_quant = round(quantile(concentration_mg_kg_dw, 
                                                                probs = c(0.025)), 3),
                       conc_mg_kg_dw_mean = round(mean(concentration_mg_kg_dw), 3),
                       conc_mg_kg_dw_high_quant = round(quantile(concentration_mg_kg_dw, 
                                                                 probs = c(0.975)), 3),
                       conc_mg_kg_dw_max = round(max(concentration_mg_kg_dw), 3), 
                       conc_mg_kg_sd = round(sd(concentration_mg_kg_dw), 3), 
                       conc_mg_kg_cv = round(sd(concentration_mg_kg_dw)/conc_mg_kg_dw_mean, 3))
  }
  
  
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
    # prey species
    dplyr::mutate(campaign = dplyr::case_when(stringr::str_starts(Code_sample, 
                                                                  "2005", 
                                                                  negate = FALSE) ~ "Isotopes 2005", 
                                              stringr::str_starts(Code_sample, 
                                                                  "2010", 
                                                                  negate = FALSE) ~ "Poker II 2010"), 
                  diet = dplyr::case_when(Species %in% c(prey_sp) ~ 1,
                                          # some sp of Muraenolepis sp. were found in 
                                          # A. gazella scats but they don't match here because 
                                          # we did not specified species
                                          Species == "Muraenolepis sp" ~ 1,
                                          TRUE ~ 0), 
                  habitat = dplyr::case_when(Species %in% c("Gobionotothen acuta", 
                                                            "Channichthys rhinoceratus",
                                                            "Dissostichus eleginoides",
                                                            "Mancopsetta mancopsetta",
                                                            "Electrona antarctica") ~ "Demersal", 
                                             Species %in% c("Lepidonotothen squamifrons", 
                                                            "Champsocephalus gunnari",
                                                            "Lindbergichthys mizops",
                                                            "Muraenolepis sp",
                                                            "Gymnoscopelus piabilis",
                                                            "Gymnoscopelus bolini") ~ "Benthopelagic", 
                                             Species %in% c("Bathydraco antarcticus",
                                                            "Macrourus carinatus",
                                                            "Paradiplospinus gracilis",
                                                            "Echiodon cryomargarites") ~ "Bathydemersal", 
                                             Species %in% c("Krefftichthys anderssoni",
                                                            "Melanostigma gelatinosum",
                                                            "Bathylagus tenuis",
                                                            "Luciosudis normani", 
                                                            "Gymnoscopelus braueri", 
                                                            "Gymnoscopelus fraseri", 
                                                            "Gymnoscopelus nicholsi", 
                                                            "Electrona subaspera",
                                                            "Poromitra crassiceps", 
                                                            "Nansenia antarctica",
                                                            "Electrona carlsbergi", 
                                                            "Protomyctophum andriashevi",
                                                            "Protomyctophum bolini", 
                                                            "Protomyctophum choriodon",
                                                            "Stomias sp",
                                                            "Idiacanthus atlanticus",
                                                            "Arctozenus risso",
                                                            "Notolepis coatsi", 
                                                            "Protomyctophum tenisoni") ~ "Bathypelagic"))
}

#'
#'
#'
# simple function to add ecological group to each species
# ecological habitat given on Fishbase
add_eco_habitat_sp <- function(compo_tib) {
  
  compo_tib |>
    dplyr::mutate(habitat = dplyr::case_when(Species %in% c("Gobionotothen acuta", 
                                                            "Channichthys rhinoceratus",
                                                            "Dissostichus eleginoides",
                                                            "Mancopsetta mancopsetta",
                                                            "Electrona antarctica") ~ "Demersal", 
                                             Species %in% c("Lepidonotothen squamifrons", 
                                                            "Champsocephalus gunnari",
                                                            "Lindbergichthys mizops",
                                                            "Muraenolepis sp",
                                                            "Gymnoscopelus piabilis",
                                                            "Gymnoscopelus bolini") ~ "Benthopelagic", 
                                             Species %in% c("Bathydraco antarcticus",
                                                            "Macrourus carinatus",
                                                            "Paradiplospinus gracilis",
                                                            "Echiodon cryomargarites") ~ "Bathydemersal", 
                                             Species %in% c("Krefftichthys anderssoni",
                                                            "Melanostigma gelatinosum",
                                                            "Bathylagus tenuis",
                                                            "Luciosudis normani", 
                                                            "Gymnoscopelus braueri", 
                                                            "Gymnoscopelus fraseri", 
                                                            "Gymnoscopelus nicholsi", 
                                                            "Electrona subaspera",
                                                            "Poromitra crassiceps", 
                                                            "Nansenia antarctica",
                                                            "Electrona carlsbergi", 
                                                            "Protomyctophum andriashevi",
                                                            "Protomyctophum bolini", 
                                                            "Protomyctophum choriodon",
                                                            "Stomias sp",
                                                            "Idiacanthus atlanticus",
                                                            "Arctozenus risso",
                                                            "Notolepis coatsi", 
                                                            "Protomyctophum tenisoni") ~ "Bathypelagic")) 
  
}


#'
#'
#'
# simple function to create table with mean per species 
compute_means_sp <- function(compo_tib) {
  
  compo_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Species) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species, "\n(n = ", n, ")")) |>
    dplyr::group_by(Family, habitat, Speciesn, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_kg_dw)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = mean_sp) |>
    dplyr::rename(Species = Speciesn)
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
boxplot_compo_fish_sp <- function(res_fish_tib, 
                                  nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    dplyr::group_by(Species) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species, "\n(n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Speciesn, 
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = Species)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_order_sp_",
                         nutrient,
                         ".jpg"),
                  scale = 1,
                  height = 14, width = 17
  )
  
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
boxplot_compo_fish_sp_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2])) |>
    dplyr::mutate(Species_short = factor(Species_short, 
                                         levels = c(# Paralepididae
                                           "A. risso", 
                                           "N. coatsi",
                                           # Bathydraconidae
                                           "B. antarcticus",
                                           # Bathylagidae
                                           "B. tenuis",
                                           # Channichthyidae
                                           "C. gunnari",
                                           "C. rhinoceratus",
                                           # Nototheniidae
                                           "D. eleginoides",
                                           "G. acuta",
                                           "L. squamifrons",
                                           "L. mizops",
                                           # Carapidae
                                           "E. cryomargarites",
                                           # Myctophidae
                                           "E. antarctica",
                                           "E. carlsbergi", 
                                           "E. subaspera",
                                           "G. bolini",
                                           "G. braueri",
                                           "G. fraseri",
                                           "G. nicholsi", 
                                           "G. piabilis", 
                                           "K. anderssoni",
                                           "P. andriashevi",
                                           "P. bolini",
                                           "P. choriodon",
                                           "P. tenisoni",
                                           # Stomiidae
                                           "I. atlanticus", 
                                           "S. sp",
                                           # Notosudidae
                                           "L. normani",
                                           # Macrouridae
                                           "M. carinatus",
                                           # Achiropsettidae
                                           "M. mancopsetta",
                                           "M. gelatinosum",
                                           # Muraenolepididae
                                           "M. sp",
                                           # Microstomatidae
                                           "N. antarctica",
                                           # Gempylidae
                                           "P. gracilis",
                                           # Melamphaidae
                                           "P. crassiceps")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species_short, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Speciesn, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 3) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_all_nut.jpg"),
                  scale = 1,
                  height = 28, width = 22
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
# with a gradient color and only mean and quantiles 
boxplot_compo_fish_sp_all_nut_grad <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2])) |>
    dplyr::mutate(Species_short = factor(Species_short, 
                                         levels = c(# Paralepididae
                                           "A. risso", 
                                           "N. coatsi",
                                           # Bathydraconidae
                                           "B. antarcticus",
                                           # Bathylagidae
                                           "B. tenuis",
                                           # Channichthyidae
                                           "C. gunnari",
                                           "C. rhinoceratus",
                                           # Nototheniidae
                                           "D. eleginoides",
                                           "G. acuta",
                                           "L. squamifrons",
                                           "L. mizops",
                                           # Carapidae
                                           "E. cryomargarites",
                                           # Myctophidae
                                           "E. antarctica",
                                           "E. carlsbergi", 
                                           "E. subaspera",
                                           "G. bolini",
                                           "G. braueri",
                                           "G. fraseri",
                                           "G. nicholsi", 
                                           "G. piabilis", 
                                           "K. anderssoni",
                                           "P. andriashevi",
                                           "P. bolini",
                                           "P. choriodon",
                                           "P. tenisoni",
                                           # Stomiidae
                                           "I. atlanticus", 
                                           "S. sp",
                                           # Notosudidae
                                           "L. normani",
                                           # Macrouridae
                                           "M. carinatus",
                                           # Achiropsettidae
                                           "M. mancopsetta",
                                           "M. gelatinosum",
                                           # Muraenolepididae
                                           "M. sp",
                                           # Microstomatidae
                                           "N. antarctica",
                                           # Gempylidae
                                           "P. gracilis",
                                           # Melamphaidae
                                           "P. crassiceps")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species_short, " (n = ", n, ")")) |>
    dplyr:: group_by(Speciesn, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = reorder(Speciesn, 
                                                     median), 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = Speciesn), 
                            linewidth = 2) +
    ggplot2::geom_point(ggplot2::aes(x = reorder(Speciesn, 
                                                 median), 
                                     y = median, 
                                     color = Speciesn), 
                        size = 3) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "bottom")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_all_nut_grad.jpg"),
                  scale = 1,
                  height = 28, width = 22
  )
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
# with a gradient color and only mean and quantiles 
# only for one nutrient
boxplot_compo_fish_sp_one_nut_grad <- function(res_fish_tib, 
                                               nutrient
) {
  
  # calculate median for all species
  mean_med_allsp <- res_fish_tib |>
                     tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                                  Mg, Mn, Na, Ni, P, Se, Zn), 
                                         names_to = "Nutrient", 
                                         values_to = "concentration_mg_kg_dw") |>
                     dplyr::filter(Nutrient == nutrient) |>
                     dplyr::summarise(mean_allsp = mean(concentration_mg_kg_dw), 
                                      median_allsp = median(concentration_mg_kg_dw))
  
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2]),
      Nutrient = factor(Nutrient, 
                        levels = c("Ca", "P", "Na", "K", "Mg", 
                                   "Fe", "Zn", "Cu", "Mn", "Se",
                                   "As", "Ni","Co"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso (n = 2)", 
                                      "N. coatsi (n = 10)",
                                      "S. sp (n = 10)",
                                      "P. bolini (n = 10)",
                                      "P. tenisoni (n = 10)",
                                      "K. anderssoni (n = 4)",
                                      "P. andriashevi (n = 3)",
                                      "M. carinatus (n = 10)",
                                      "B. antarcticus (n = 5)",
                                      "P. choriodon (n = 8)",
                                      "M. sp (n = 10)",
                                      "L. mizops (n = 7)",
                                      "M. mancopsetta (n = 2)",
                                      "G. acuta (n = 8)",
                                      "I. atlanticus (n = 1)", 
                                      "L. normani (n = 1)",
                                      "E. carlsbergi (n = 10)", 
                                      "P. gracilis (n = 10)",
                                      "D. eleginoides (n = 2)",
                                      "G. fraseri (n = 12)",
                                      "L. squamifrons (n = 10)",
                                      "N. antarctica (n = 5)",
                                      "P. crassiceps (n = 1)",
                                      "B. tenuis (n = 11)",
                                      "E. antarctica (n = 10)",
                                      "E. subaspera (n = 10)",
                                      "G. piabilis (n = 10)", 
                                      "G. nicholsi (n = 10)", 
                                      "M. gelatinosum (n = 10)",
                                      "C. rhinoceratus (n = 10)",
                                      "E. cryomargarites (n = 10)",
                                      "G. bolini (n = 10)",
                                      "G. braueri (n = 12)",
                                      "C. gunnari (n = 10)"
                                    ))) |>
    dplyr::group_by(Speciesn, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = reorder(Speciesn, 
                                                     median), 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = Speciesn), 
                            linewidth = 2) +
    ggplot2::geom_point(ggplot2::aes(x = reorder(Speciesn, 
                                                 median), 
                                     y = median, 
                                     color = Speciesn), 
                        size = 3) +
    ggplot2::geom_hline(data = mean_med_allsp, 
                        ggplot2::aes(yintercept = median_allsp), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_med_allsp, 
                        ggplot2::aes(yintercept = mean_allsp), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_nut_grad_", 
                         nutrient, ".jpg"),
                  scale = 1,
                  height = 5, width = 5
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
# with a gradient color and only mean and quantiles 
# only for one nutrient, with sp names
boxplot_compo_fish_sp_one_nut_grad_sp <- function(res_fish_tib, 
                                                  nutrient
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2]),
      Nutrient = factor(Nutrient, 
                        levels = c("Ca", "P", "Na", "K", "Mg", 
                                   "Fe", "Zn", "Cu", "Mn", "Se",
                                   "As", "Ni","Co"))) |>
    dplyr::group_by(Species_short) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                    levels = c(# classification is that of median
                                      # for Fe concentrations
                                      "A. risso (n = 2)", 
                                      "N. coatsi (n = 10)",
                                      "S. sp (n = 10)",
                                      "P. bolini (n = 10)",
                                      "P. tenisoni (n = 10)",
                                      "K. anderssoni (n = 4)",
                                      "P. andriashevi (n = 3)",
                                      "M. carinatus (n = 10)",
                                      "B. antarcticus (n = 5)",
                                      "P. choriodon (n = 8)",
                                      "M. sp (n = 10)",
                                      "L. mizops (n = 7)",
                                      "M. mancopsetta (n = 2)",
                                      "G. acuta (n = 8)",
                                      "I. atlanticus (n = 1)", 
                                      "L. normani (n = 1)",
                                      "E. carlsbergi (n = 10)", 
                                      "P. gracilis (n = 10)",
                                      "D. eleginoides (n = 2)",
                                      "G. fraseri (n = 12)",
                                      "L. squamifrons (n = 10)",
                                      "N. antarctica (n = 5)",
                                      "P. crassiceps (n = 1)",
                                      "B. tenuis (n = 11)",
                                      "E. antarctica (n = 10)",
                                      "E. subaspera (n = 10)",
                                      "G. piabilis (n = 10)", 
                                      "G. nicholsi (n = 10)", 
                                      "M. gelatinosum (n = 10)",
                                      "C. rhinoceratus (n = 10)",
                                      "E. cryomargarites (n = 10)",
                                      "G. bolini (n = 10)",
                                      "G. braueri (n = 12)",
                                      "C. gunnari (n = 10)"
                                    ))) |>
    dplyr::group_by(Speciesn, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = reorder(Speciesn, 
                                                     median), 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = Speciesn), 
                            linewidth = 2) +
    ggplot2::geom_point(ggplot2::aes(x = reorder(Speciesn, 
                                                 median), 
                                     y = median, 
                                     color = Speciesn), 
                        size = 3) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
    #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-sp/boxplot_sp_nut_grad_spnames_", 
                         nutrient, ".jpg"),
                  scale = 1,
                  height = 5, width = 5
  )
  
}



#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
# with a gradient color and only mean and quantiles 
# only for one nutrient but just to get the legend right 
boxplot_compo_fish_sp_one_nut_legend <- function(res_fish_tib, 
                                                 nutrient
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Species_short = paste0(
      stringr::str_sub(stringr::str_split_fixed(Species, 
                                                " ", 
                                                n = 2)[,1], 
                       start = 1, 
                       end = 1), ". ", 
      stringr::str_split_fixed(Species, " ", n = 2)[,2]),
      Nutrient = factor(Nutrient, 
                    levels = c("Ca", "P", "Na", "K", "Mg", 
                               "Fe", "Zn", "Cu", "Mn", "Se",
                               "As", "Ni","Co"))) |>
  dplyr::group_by(Species_short) |>
  dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                Speciesn = factor(paste0(Species_short, " (n = ", n, ")"), 
                                  levels = c(# classification is that of median
                                    # for Fe concentrations
                                    "A. risso (n = 2)", 
                                    "N. coatsi (n = 10)",
                                    "S. sp (n = 10)",
                                    "P. bolini (n = 10)",
                                    "P. tenisoni (n = 10)",
                                    "K. anderssoni (n = 4)",
                                    "P. andriashevi (n = 3)",
                                    "M. carinatus (n = 10)",
                                    "B. antarcticus (n = 5)",
                                    "P. choriodon (n = 8)",
                                    "M. sp (n = 10)",
                                    "L. mizops (n = 7)",
                                    "M. mancopsetta (n = 2)",
                                    "G. acuta (n = 8)",
                                    "I. atlanticus (n = 1)", 
                                    "L. normani (n = 1)",
                                    "E. carlsbergi (n = 10)", 
                                    "P. gracilis (n = 10)",
                                    "D. eleginoides (n = 2)",
                                    "G. fraseri (n = 12)",
                                    "L. squamifrons (n = 10)",
                                    "N. antarctica (n = 5)",
                                    "P. crassiceps (n = 1)",
                                    "B. tenuis (n = 11)",
                                    "E. antarctica (n = 10)",
                                    "E. subaspera (n = 10)",
                                    "G. piabilis (n = 10)", 
                                    "G. nicholsi (n = 10)", 
                                    "M. gelatinosum (n = 10)",
                                    "C. rhinoceratus (n = 10)",
                                    "E. cryomargarites (n = 10)",
                                    "G. bolini (n = 10)",
                                    "G. braueri (n = 12)",
                                    "C. gunnari (n = 10)"
                                  ))) |>
  dplyr::group_by(Speciesn, Nutrient) |>
  dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                          probs = c(0.025)), 
                   mean = mean(concentration_mg_kg_dw), 
                   median = median(concentration_mg_kg_dw), 
                   `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                           probs = c(0.975))) |>
  dplyr::rename(Species = Speciesn) |>
  dplyr::filter(Nutrient == nutrient) |>
  ggplot2::ggplot() +
  ggplot2::geom_linerange(ggplot2::aes(x = reorder(Species, 
                                                   median), 
                                       ymin = `2.5_quant`, 
                                       ymax = `97.5_quant`, 
                                       color = Species), 
                          linewidth = 2) +
  ggplot2::geom_point(ggplot2::aes(x = reorder(Species, 
                                               median), 
                                   y = median, 
                                   color = Species), 
                      size = 3) +
  ggplot2::facet_wrap(~ Nutrient, scale = "free", nrow = 3) +
  #ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
  ggplot2::coord_flip() +
  viridis::scale_color_viridis(option = "magma", discrete = TRUE) +
  ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)") +
  ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                 axis.text.y = ggplot2::element_blank(), 
                 axis.title.x = ggplot2::element_text(size = 17, 
                                                      face = "bold"), 
                 axis.title.y = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_text(size = 16),
                 legend.position = "right", 
                 legend.text = ggplot2::element_text(size = 16), 
                 legend.title = ggplot2::element_text(size = 17, 
                                                      face = "bold"))
ggplot2::ggsave("output/compo fish/per-sp/boxplot_sp_nut_grad_legend.jpg",
                scale = 1,
                height = 10, width = 9)
# ggplot2::ggsave("output/compo fish/per-sp/boxplot_sp_nut_grad_legend.jpg",
#                 width = 9,
#                 height = 4,
#                 dpi = 300)

}




#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per genus
boxplot_compo_fish_genus <- function(res_fish_tib, 
                                     nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    dplyr::group_by(Genus) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Genusn = paste0(Genus, "\n(n = ", n, ")")) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Genusn,
                                             concentration_mg_kg_dw),
                                 y = concentration_mg_kg_dw, fill = Genus)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Genus") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-genus/boxplot_order_genus_",
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
boxplot_compo_fish_genus_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2], 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::mutate(Genus = factor(Genus, 
                                 levels = c("Arctozenus", 
                                            "Bathydraco",
                                            "Bathylagus",
                                            "Champsocephalus",
                                            "Channichthys",
                                            "Dissostichus",
                                            "Echiodon",
                                            "Electrona",
                                            "Gobionotothen", 
                                            "Gymnoscopelus",
                                            "Idiacanthus",
                                            "Krefftichthys",
                                            "Lepidonotothen",
                                            "Lindbergichthys",
                                            "Luciosudis", 
                                            "Macrourus", 
                                            "Mancopsetta", 
                                            "Melanostigma",
                                            "Muraenolepis",
                                            "Nansenia",
                                            "Notolepis",
                                            "Paradiplospinus",
                                            "Poromitra",
                                            "Protomyctophum",
                                            "Stomias"))) |>
    dplyr::group_by(Genus) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Genusn = paste0(Genus, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Genusn, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 3) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF"))+
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Genus") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-genus/boxplot_genus_all_nut.jpg"),
                  scale = 1,
                  height = 22, width = 22
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per family
boxplot_compo_fish_fam <- function(res_fish_tib, 
                                   nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Family) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Familyn = paste0(Family, "\n(n = ", n, ")")) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Familyn, 
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = Family)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Family") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-fam/boxplot_order_fam_",
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
boxplot_compo_fish_fam_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Family = factor(Family, 
                                  levels = c("Achiropsettidae", 
                                             "Bathydraconidae",
                                             "Bathylagidae",
                                             "Carapidae",
                                             "Channichthyidae",
                                             "Gempylidae",
                                             "Macrouridae",
                                             "Melamphaidae", 
                                             "Microstomatidae",
                                             "Muraenolepididae",
                                             "Myctophidae",
                                             "Notosudidae", 
                                             "Nototheniidae", 
                                             "Paralepididae", 
                                             "Stomiidae",
                                             "Zoarcidae")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(Family) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Familyn = paste0(Family, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Familyn, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 3) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF"))+
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Family") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/per-fam/boxplot_fam_all_nut.jpg",
                  scale = 1,
                  height = 18, width = 22
  )
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per family
boxplot_compo_fish_fam_few_nut <- function(res_fish_tib, 
                                           nutrients
) {
  
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Family) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Familyn = paste0(Family, " (n = ", n, ")"))|> 
    dplyr::mutate(Familyn = factor(Familyn, 
                                   levels = c("Channichthyidae (n = 20)",
                                              "Zoarcidae (n = 10)",
                                              "Microstomatidae (n = 5)",
                                              "Melamphaidae (n = 1)", 
                                              "Bathylagidae (n = 11)",
                                              "Notosudidae (n = 1)", 
                                              "Nototheniidae (n = 27)", 
                                              "Carapidae (n = 10)",
                                              "Myctophidae (n = 119)",
                                              "Achiropsettidae (n = 2)", 
                                              "Muraenolepididae (n = 10)",
                                              "Macrouridae (n = 10)",
                                              "Bathydraconidae (n = 5)",
                                              "Gempylidae (n = 10)",
                                              "Stomiidae (n = 11)",
                                              "Paralepididae (n = 12)")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::filter(Nutrient %in% nutrients) |>
    ggplot2::ggplot(ggplot2::aes(x = Familyn, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 2) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", discrete = TRUE) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Family") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-fam/boxplot_fam_", 
                         stringr::str_c(nutrients, collapse = ""), ".jpg"),
                  scale = 1,
                  height = 8, width = 9
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per campaign
boxplot_compo_fish_camp <- function(res_fish_tib, 
                                    nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(campaign,
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = campaign)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Campaign") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-camp/boxplot_order_camp_",
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
boxplot_compo_fish_camp_full <- function(res_fish_tib) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(campaign,
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = campaign)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Campaign") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/per-camp/boxplot_order_camp_full.jpg",
                  scale = 1,
                  height = 12, width = 21
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per habitat
boxplot_compo_fish_hab_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(habitat = factor(habitat, 
                                   levels = c("Demersal", 
                                              "Bathydemersal", 
                                              "Benthopelagic",
                                              "Bathypelagic")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(habitat) |>
    dplyr::mutate(nsample = dplyr::n_distinct(Code_sample),
                  nsp = dplyr::n_distinct(Species)) |>
    dplyr::group_by(Species, habitat, Nutrient) |>
    dplyr::summarise(habitatnsp = paste0(habitat, " (n(sp) = ", nsp, ")"), 
                     mean_conc_mg_kg_dw = mean(concentration_mg_kg_dw)) |>
    dplyr::mutate(habitatnsp = factor(habitatnsp, 
                                      levels = c("Demersal (n(sp) = 5)", 
                                                 "Bathydemersal (n(sp) = 4)", 
                                                 "Benthopelagic (n(sp) = 6)",
                                                 "Bathypelagic (n(sp) = 19)")))|>
    ggplot2::ggplot(ggplot2::aes(x = habitatnsp, 
                                 y = mean_conc_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free_x", nrow = 3) +
    ggplot2::geom_boxplot(alpha=0.9) +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::xlab("Habitat") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16), 
                   axis.text.y = ggplot2::element_text(size = 16), 
                   axis.title.x = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 16),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo fish/per-habitat/boxplot_hab_all_nut.jpg"),
                  scale = 1,
                  height = 10, width = 14
  )
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
MWtest_fish_hab <- function(res_fish_tib) {
  
  compo_tib <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")))
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::group_by(habitat, Species, Nutrient) |>
      dplyr::summarise(mean_sp_conc_mg_kg_dw = mean(concentration_mg_kg_dw)) |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = habitat, 
                         values_from = mean_sp_conc_mg_kg_dw)
    
    demersal <- na.omit(table$Demersal)
    bathypel <- na.omit(table$Bathypelagic)
    bathydem <- na.omit(table$Bathydemersal)
    benthopel <- na.omit(table$Benthopelagic)
    
    nut_test <- data.frame(Nutrient = rep(nut, 6), 
                           Habitat1 = c("Demersal", "Demersal", "Demersal",
                                        "Bathypelagic", "Bathypelagic", 
                                        "Bathydemersal"), 
                           Habitat2 = c("Bathypelagic", "Bathydemersal", "Benthopelagic",
                                        "Bathydemersal", "Benthopelagic", 
                                        "Benthopelagic"), 
                           alpha_MW = c(wilcox.test(demersal, bathypel)[[3]],
                                        wilcox.test(demersal, bathydem)[[3]],
                                        wilcox.test(demersal, benthopel)[[3]],
                                        
                                        wilcox.test(bathypel, bathydem)[[3]],
                                        wilcox.test(bathypel, benthopel)[[3]],
                                        
                                        wilcox.test(bathydem, benthopel)[[3]]))
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        Habitat1 = NA,
                        Habitat2 = NA,
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
                       file = paste0("output/compo fish/per-habitat/Mann_Whitney_test_fish_habitat.xlsx"))
  
  
  
}



#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of all fish
boxplot_compo_fish_tot <- function(res_fish_tib) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, 
                                 y = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/boxplot_compo_fish_tot.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
}


#'
#'
#'
#'
#'
# function to display densityplot of elemental composition of all fish
densplot_compo_fish_tot <- function(res_fish_tib) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_kg_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_kg_dw, fill = Nutrient)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo fish/densplot_compo_fish_tot.jpg",
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
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
table_compare_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  table <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey"))) |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     median = round(median(concentration_mg_kg_dw), 2), 
                     `2.5% quantile` = round(quantile(concentration_mg_kg_dw, 
                                                      probs = c(0.025)), 2), 
                     `97.5% quantile` = round(quantile(concentration_mg_kg_dw, 
                                                       probs = c(0.975)), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    tidyr::pivot_longer(cols = c(mean:cv), 
                        names_to = "Statistic",
                        values_to = "value") |>
    dplyr::mutate(Statistic = factor(Statistic, 
                                     levels = c("mean", "median", "sd", "cv",
                                                "2.5% quantile", "97.5% quantile"))) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = value) |>
    dplyr::arrange(type, Statistic)
  
  openxlsx::write.xlsx(table, 
                       file = "output/compo fish/stats_comp_prey_vs_not_prey.xlsx")
  
  
}


#'
#'
#'
#'
#'
# function to create barplot displaying CV for major and trace nutrients in
# both scats and prey
barplot_comp_nut_prey_not_prey <- function(res_fish_tib
) {
  
  options(scipen = 999)
  
  res_fish_tib |>
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
                  type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr::group_by(major_or_trace, Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     mean_norm = round(mean(conc_norm), 2),
                     `2.5_quant_norm` = round(quantile(conc_norm, 
                                                       probs = c(0.025)), 2), 
                     `97.5_quant_norm` = round(quantile(conc_norm, 
                                                        probs = c(0.975)), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                   y = cv, 
                                   fill = major_or_trace), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant_norm`, 
                                         ymax = `97.5_quant_norm`), 
                            linewidth = 1, 
                            color = "#B4DAE5FF", 
                            position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = Nutrient, 
                                     y = mean_norm), 
                        size = 3, 
                        color = "#B4DAE5FF", 
                        position = ggplot2::position_dodge(0.5)) +
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
  ggplot2::ggsave("output/compo fish/Cv_per_nut_comp_prey_vs_not_prey.jpg",
                  scale = 1,
                  height = 4, width = 11
  )
  
  
  
  
}




#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
lineplot_compare_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(mean = round(mean(concentration_mg_kg_dw), 2), 
                     median = round(mean(concentration_mg_kg_dw), 2),
                     `2.5_quant` = round(quantile(concentration_mg_kg_dw, 
                                                       probs = c(0.025)), 2), 
                     `97.5_quant` = round(quantile(concentration_mg_kg_dw, 
                                                        probs = c(0.975)), 2),
                     mean_norm = round(mean(conc_norm), 2),
                     median_norm = round(mean(conc_norm), 2),
                     `2.5_quant_norm` = round(quantile(conc_norm, 
                                                       probs = c(0.025)), 2), 
                     `97.5_quant_norm` = round(quantile(conc_norm, 
                                                        probs = c(0.975)), 2),
                     sd = round(sd(concentration_mg_kg_dw), 2), 
                     cv = round(sd/mean, 3)) |>
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
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                           "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/lineplot_compo_prey_vs_not_prey_abs.jpg",
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
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
lineplot_compare_compo_prey_not_prey_mean_sp_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    dplyr::group_by(type, Species, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_kg_dw)) |>
    dplyr::group_by(Nutrient, type) |>
    dplyr::summarise(mean = round(mean(mean_sp), 2), 
                     median = round(mean(mean_sp), 2),
                     `2.5_quant` = round(quantile(mean_sp, 
                                                  probs = c(0.025)), 2), 
                     `97.5_quant` = round(quantile(mean_sp, 
                                                   probs = c(0.975)), 2)) |>
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
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_color_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                           "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/lineplot_compo_prey_vs_not_prey_mean_sp_abs.jpg",
                  scale = 1,
                  height = 5, width = 5
  )
  
}




#'
#'
#'
#'
#'
# function to compare composition of fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
boxplot_compare_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Co", "Ni", "As", "Se", "Mn",
                                               "Cu", "Zn", "Fe", "Mg", "K",
                                               "Na", "P", "Ca"))) |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_norm = (concentration_mg_kg_dw - min(concentration_mg_kg_dw))/
                    (max(concentration_mg_kg_dw) - min(concentration_mg_kg_dw))) |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Nutrient, 
                                       y = conc_norm, 
                                       color = type), 
                          size = 3, 
                          position = ggplot2::position_dodge(0.5)) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                           "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/boxplot_compo_prey_vs_not_prey_abs.jpg",
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
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
boxplot_compare_compo_prey_not_prey_rel <- function(res_fish_tib) {
  options(scipen = 999)
  
  res_fish_tib |>
    dplyr::mutate(sum = As + Co + Cu + Fe + 
                    Mn + Ni + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Co, Cu, Fe, Mn, Ni, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(type = factor(dplyr::case_when(diet == 1 ~ "fish species identied as fur seal prey", 
                                                 diet == 0 ~ "fish species never identied as fur seal prey"), 
                                levels = c("fish species never identied as fur seal prey", 
                                           "fish species identied as fur seal prey"))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Nutrient, 
                                             concentration_mg_kg_dw), 
                                 y = concentration_mg_kg_dw, fill = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("fish species identied as fur seal prey" = "#278B9AFF", 
                                          "fish species never identied as fur seal prey" = "#B4DAE5FF")) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::ylab(paste0("Concentration (in mg/kg dry weight)")) +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15)
    )
  ggplot2::ggsave("output/compo fish/compo_prey_vs_not_prey_rel_trace_only.jpg",
                  scale = 1,
                  height = 8, width = 9
  )
  
}




#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# absolute concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = concentration_mg_kg_dw)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_absolute.xlsx")
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# absolute concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_mean_sp_abs <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) |>
    dplyr::group_by(type, Species, Nutrient) |>
    dplyr::summarise(mean_sp = mean(concentration_mg_kg_dw))
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = mean_sp)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_absolute_mean_sp.xlsx")
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# relative concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_rel_trace_only <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    dplyr::mutate(sum = As + Co + Cu + Fe + 
                    Mn + Ni + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Co, Cu, Fe, Mn, Ni, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_kg_dw/sum) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = relative_concentration)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_rel_trace_only.xlsx")
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# relative concentrations of nutrients in fish species identified as prey of
# A. gazella and fish never identified as prey of A. gazella around Kerguelen
MWtest_compo_prey_not_prey_rel_all_nut <- function(res_fish_tib) {
  options(scipen = 999)
  
  compo_tib <- res_fish_tib |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(relative_concentration = concentration_mg_kg_dw/sum) |>
    # make non-prey and prey species of fur seals distinct
    dplyr::mutate(type = dplyr::case_when(diet == 1 ~ "prey",
                                          diet == 0 ~ "not prey")) 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = type, 
                         values_from = relative_concentration)
    
    prey <- na.omit(table$`prey`)
    not_prey <- na.omit(table$`not prey`)
    
    nut_test <- data.frame(Nutrient = nut, 
                           alpha_MW = wilcox.test(prey, not_prey)[[3]])
    
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
                       file = "output/compo fish/Mann_Whitney_test_fish_prey_not_prey_rel_all_nut.xlsx")
  
}



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
  ggplot2::ggsave("output/compo fish/corrplot_compo_fish.jpg",
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


################################################################################
############################################# SCATS ############################
################################################################################


#'
#'
#'
#'
#'
# function to produce table with summary of elemental analysis
table_compo_scats_tot <- function(res_scat_tib, 
                                  object_type # either "output" or "file" 
) {
  
  table <- res_scat_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(conc_mg_kg_dw_min = round(min(concentration_mg_kg_dw), 3), 
                     conc_mg_kg_dw_low_quant = round(quantile(concentration_mg_kg_dw, 
                                                              probs = c(0.025)), 3),
                     conc_mg_kg_dw_mean = round(mean(concentration_mg_kg_dw), 3),
                     conc_mg_kg_dw_median = round(median(concentration_mg_kg_dw), 3),
                     conc_mg_kg_dw_high_quant = round(quantile(concentration_mg_kg_dw, 
                                                               probs = c(0.975)), 3),
                     conc_mg_kg_dw_max = round(max(concentration_mg_kg_dw), 3), 
                     conc_mg_kg_dw_sd = round(sd(concentration_mg_kg_dw), 3), 
                     conc_mg_kg_dw_cv = round(sd(concentration_mg_kg_dw)/conc_mg_kg_dw_mean, 3), )
  
  
  if (object_type == "file") {
    openxlsx::write.xlsx(table, 
                         file = paste0("output/compo scats/summary_scats_compo_tot.xlsx"))
  } else {
    table
  }
  
}


#'
#'
#'
#'
#'
# function to produce table with summary of elemental analysis
table_compo_scats_site <- function(res_scat_tib, 
                                   object_type # either "output" or "file" 
) {
  
  table <- res_scat_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::group_by(site, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     conc_mg_kg_dw_min = round(min(concentration_mg_kg_dw), 3), 
                     conc_mg_kg_dw_low_quant = round(quantile(concentration_mg_kg_dw, 
                                                              probs = c(0.025)), 3),
                     conc_mg_kg_dw_mean = round(mean(concentration_mg_kg_dw), 3),
                     conc_mg_kg_dw_high_quant = round(quantile(concentration_mg_kg_dw, 
                                                               probs = c(0.975)), 3),
                     conc_mg_kg_dw_max = round(max(concentration_mg_kg_dw), 3), 
                     conc_mg_kg_dw_sd = round(sd(concentration_mg_kg_dw), 3), 
                     conc_mg_kg_dw_cv = round(sd(concentration_mg_kg_dw)/conc_mg_kg_dw_mean, 3), )
  
  
  if (object_type == "file") {
    openxlsx::write.xlsx(table, 
                         file = paste0("output/compo scats/summary_scats_compo_site.xlsx"))
  } else {
    table
  }
  
}



#'
#'
#'
#'
#'
# function to bind composition data and data on samples (hard part index HPI and 
# eventually suspicion of pup scat)
complete_compo_scats <- function(res_scat_tib, 
                                 data_scat_tib
) {
  
  res_scat_tib |>
    dplyr::left_join(data_scat_tib |> 
                       dplyr::select(Code_sample, index_hard_parts, pup_suspicion), 
                     by = "Code_sample") |>
    dplyr::rename(HPI = index_hard_parts) |>
    dplyr::mutate(pup_suspicion = factor(pup_suspicion),
                  HPI01 = dplyr::case_when(HPI != 0 ~ "1",  # positive
                                           TRUE ~ "0")) # negative
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of scats in total without  
# statistical outliers displayed
boxplot_compo_scats_tot <- function(res_scat_tib,
                                    file_name) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_kg_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/", 
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
# function to display boxplot of elemental composition of poops in total without the 
# statistical outlier display
densplot_compo_scats_tot <- function(res_scat_tib, 
                                     file_name) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_kg_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_density() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::xlab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/", 
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
# function to display boxplot of elemental composition per site
boxplot_compo_scats_site <- function(res_scat_tib, 
                                     file_name) {
  
  mean_median_tib <- res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by( Nutrient) |>
    dplyr::summarise(mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw))
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(site = dplyr::case_when(site == "Cap Noir" ~ "Cap\nNoir", 
                                          site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::mutate(site = factor(site, 
                                levels = c("Cap\nNoir", 
                                           "Pointe\nSuzanne"))) |>
    ggplot2::ggplot(ggplot2::aes(x = site, y = concentration_mg_kg_dw, 
                                 fill = site)) +
    ggplot2::geom_violin(ggplot2::aes(color = site),
                         width = 1.4, alpha = 0.5) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_fill_manual(values = c( "#14191FFF", 
                                           "#AE93BEFF")) +
    ggplot2::scale_color_manual(values = c( "#14191FFF", 
                                           "#AE93BEFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme_linedraw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold", 
                                                        color = "black"), 
                   strip.background = ggplot2::element_rect(fill = "lightgrey"),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 9, width = 10
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per site
linerangeplot_compo_scats_site <- function(res_scat_tib, 
                                     file_name) {
  
  
  mean_median_tib <- res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by( Nutrient) |>
    dplyr::summarise(mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw))
    
    
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(site = dplyr::case_when(site == "Cap Noir" ~ "Cap\nNoir", 
                                          site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::mutate(site = factor(site, 
                                levels = c("Cap\nNoir", 
                                           "Pointe\nSuzanne"))) |>
    dplyr::group_by(site, Nutrient) |>
    dplyr::summarise(`2.5_quant` = quantile(concentration_mg_kg_dw, 
                                            probs = c(0.025)), 
                     mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw), 
                     `97.5_quant` = quantile(concentration_mg_kg_dw, 
                                             probs = c(0.975))) |>
    ggplot2::ggplot() +
    ggplot2::geom_linerange(ggplot2::aes(x = site, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`, 
                                         color = site), 
                            linewidth = 2) +
    ggplot2::geom_point(ggplot2::aes(x = site, 
                                     y = median, 
                                     color = site), 
                        size = 3) +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_color_manual(values = c( "#14191FFF", 
                                           "#AE93BEFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/linerange_plot_", 
                         file_name, "_sites.jpg"),
                  scale = 1,
                  height = 9, width = 10
  )
  
}



#'
#'
#'
#'
#'
# function to display boxplot of elemental composition
boxplot_compo_scats_pups <- function(res_scat_tib, 
                                     file_name) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(pup_suspicion = dplyr::case_when(pup_suspicion == "1" ~ "probable\nnursed\npup scat", 
                                                   pup_suspicion == "0" ~ "scat of\nnon-nursed\nindividual")) |>
    dplyr::mutate(pup_suspicion = factor(pup_suspicion, 
                                         levels = c("probable\nnursed\npup scat", 
                                                    "scat of\nnon-nursed\nindividual"))) |>
    ggplot2::ggplot(ggplot2::aes(x = pup_suspicion, y = concentration_mg_kg_dw, 
                                 fill = pup_suspicion)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.2) +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF",
                                          "#278B9AFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 12, width = 15
  )
  
}



#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of scats per 
# hard part index HPI 0, 1, 2, 3
boxplot_compo_scats_HPI <- function(res_scat_tib,
                                    file_name) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = factor(HPI), 
                                 y = concentration_mg_kg_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.2) +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::xlab("Hard-parts index") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/", 
                         file_name, ".jpg"), 
                  scale = 1,
                  height = 12, width = 22
  )
  
}


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of scats per 
# hard part index HPI 0/1
boxplot_compo_scats_HPI01 <- function(res_scat_tib,
                                      file_name) {
  
  mean_median_tib <- res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by( Nutrient) |>
    dplyr::summarise(mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw))
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(HPI01 = factor(dplyr::case_when(HPI01 == 0 ~ "HPI=0", 
                                                  HPI01 == 1 ~ "HPI=1"), 
                                 levels = c("HPI=0", 
                                            "HPI=1")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    ggplot2::ggplot(ggplot2::aes(x = HPI01, 
                                 y = concentration_mg_kg_dw, 
                                 fill = HPI01)) +
    ggplot2::geom_violin(ggplot2::aes(color = HPI01),
                         width = 1.4, alpha = 0.5) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", 
                                          "#E75B64FF")) +
    ggplot2::scale_color_manual(values = c("#278B9AFF", 
                                          "#E75B64FF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::xlab("Hard-parts index") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   panel.spacing.y = ggplot2::unit(0.4, "cm"),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/", 
                         file_name, ".jpg"), 
                  scale = 1,
                  height = 9, width = 10
  )
  
}




#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of scats per 
# hard part index HPI 0/1 and showing that of pups separately
boxplot_compo_scats_HPI01_pups <- function(res_scat_tib,
                                           file_name) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(HPI_pup_nopup = dplyr::case_when(HPI01 == "0" & pup_suspicion == "1" ~ "probable\nnursed\npup",
                                                   HPI01 == "0" & pup_suspicion == "0" ~ "non-\nnursed\nHPI=0",
                                                   HPI01 == "1" & pup_suspicion == "0" ~ "non-\nnursed\nHPI=1")) |>
    dplyr::mutate(HPI_pup_nopup = factor(HPI_pup_nopup, 
                                         levels = c("probable\nnursed\npup",
                                                    "non-\nnursed\nHPI=0", 
                                                    "non-\nnursed\nHPI=1" 
                                         )), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    ggplot2::ggplot(ggplot2::aes(x = HPI_pup_nopup, 
                                 y = concentration_mg_kg_dw, 
                                 fill = HPI_pup_nopup)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    #ggplot2::coord_flip() +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.2) +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", 
                                          "#278B9AFF", 
                                          "#E75B64FF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::xlab("Hard-parts index") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/compo scats/", 
                         file_name, ".jpg"), 
                  scale = 1,
                  height = 10, width = 13
  )
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentrations in scats between sites 
MWtest_scats_sites <- function(res_scat_tib,
                               pup_no_pup # either "pup" or "no_pup" 
) {
  
  compo_tib <- res_scat_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")))
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = site, 
                         values_from = concentration_mg_kg_dw)
    
    CapNo <- na.omit(table$`Cap Noir`)
    PSuz <- na.omit(table$`Pointe Suzanne`)
    
    nut_test <- data.frame(Nutrient = nut,  
                           alpha_MW = wilcox.test(CapNo, PSuz)[[3]])
    
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
                       file = paste0("output/compo scats/Mann_Whitney_test_scats_sites_",
                                     pup_no_pup,
                                     ".xlsx"))
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentrations in scats between that suspected to be of pups and 
# the other ones
MWtest_scats_pup_nopup <- function(res_scat_tib) {
  
  compo_tib <- res_scat_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")))
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = pup_suspicion, 
                         values_from = concentration_mg_kg_dw)
    
    Nopupsus <- na.omit(table$`0`)
    pupsus <- na.omit(table$`1`)
    
    nut_test <- data.frame(Nutrient = nut,  
                           alpha_MW = wilcox.test(Nopupsus, pupsus)[[3]])
    
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
                       file = "output/compo scats/Mann_Whitney_test_scats_pup_nopup.xlsx")
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentrations in scats between presence and absence of hard parts
MWtest_scats_HPI <- function(res_scat_tib, 
                             pup_no_pup) {
  
  compo_tib <- res_scat_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")))
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = HPI01, 
                         values_from = concentration_mg_kg_dw)
    
    HPI0 <- na.omit(table$`0`)
    HPI1 <- na.omit(table$`1`)
    
    nut_test <- data.frame(Nutrient = nut,  
                           alpha_MW = wilcox.test(HPI0, HPI1)[[3]])
    
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
                       file = paste0("output/compo scats/Mann_Whitney_test_scats_HPI_",
                                     pup_no_pup,
                                     ".xlsx"))
  
}

#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentrations in scats between presence and absence of hard parts
MWtest_scats_HPI01_pup <- function(res_scat_tib) {
  
  compo_tib <- res_scat_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(HPI_pup_nopup = dplyr::case_when(HPI01 == "0" & pup_suspicion == "1" ~ "pup",
                                                   HPI01 == "0" & pup_suspicion == "0" ~ "HPI0",
                                                   HPI01 == "1" & pup_suspicion == "0" ~ "HPI1")) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) 
  
  nut_vec <- unique(compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = HPI_pup_nopup, 
                         values_from = concentration_mg_kg_dw)
    
    pup <- na.omit(table$`pup`)
    HPI0 <- na.omit(table$HPI0)
    HPI1 <- na.omit(table$HPI1)
    
    nut_test <- data.frame(Nutrient = nut,  
                           "Sample type 1" = c("scat of non-nursed individual with hard parts", 
                                               "scat of non-nursed individual with hard parts",
                                               "scat of non-nursed individual with no hard parts"), 
                           "Sample type 2" = c("scat of non-nursed individual with no hard parts",
                                               "probable pup scat",
                                               "probable pup scat"), 
                           alpha_MW = c(wilcox.test(HPI1, HPI0)[[3]],
                                        wilcox.test(HPI1, pup)[[3]], 
                                        wilcox.test(HPI0, pup)[[3]]))
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        "Sample type 1" = NA,
                        "Sample type 2" = NA,
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
                       file = "output/compo scats/Mann_Whitney_test_scats_HPI0_pup.xlsx")
  
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
  ggplot2::ggsave(paste0("output/compo scats/", 
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
# scatter plot % of water and nutrient content
scatterplot_compo_water_scats <- function(res_scat_tib, 
                                 scat_tab) {
  
  res_scat_tib |>
    dplyr::left_join(scat_tab |>
                       dplyr::select(Code_sample, water_percent), 
                     by = "Code_sample") |>
    tidyr::pivot_longer(cols = c(As:Se), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    ggplot2::ggplot(ggplot2::aes(x = water_percent, y = conc_mg_kg_dw)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("A. gazella scats") +
    ggplot2::ylab("Concentration in mg per kg dry weight") +
    ggplot2::xlab("Water %") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 17, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"))
  ggplot2::ggsave("output/compo scats/scatter_plot_water_nut_conc_scats.jpg",
                  scale = 1,
                  height = 6, width = 10
  )

  
}


################################################################################
#################################### DIET COMPOSITION ####################
################################################################################

#'
#'
#'
#'
#'
# plot concentration in diet in the different sources 
boxplot_compo_diets_sources <- function(conc_diet_tib) {
  
  conc_diet_tib |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg",  
                                               "Fe", "Zn", "Cu", "Mn",
                                               "Se","Ni", "As", "Co"))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient,
                                 y = log(concentration_in_fish_ration_ww), 
                                 color = Id_source))+
    ggplot2::geom_point(size = 3, 
                        position = ggplot2::position_dodge(width = 0.8)) +
    viridis::scale_color_viridis(option = "magma", 
                                 discrete = TRUE) +
    #ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::ylab(paste0("Nutrient concentration in fish\nration (in mg/kg wet weight)")) +
    ggplot2::xlab("Source") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   axis.title.x = ggplot2::element_text(size = 15, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 15, face = "bold"), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_blank()
    )
  ggplot2::ggsave("output/boxplot_compo_diets_sources_log.jpg",
                  scale = 1,
                  height = 5, width = 9
  )
}
