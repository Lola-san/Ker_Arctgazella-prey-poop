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
  
  table <- res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::group_by(Species, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     conc_mg_g_dw_min = min(concentration_mg_g_dw), 
                     conc_mg_g_dw_low_quant = quantile(concentration_mg_g_dw, 
                                                       probs = c(0.025)),
                     conc_mg_g_dw_mean = mean(concentration_mg_g_dw),
                     conc_mg_g_dw_high_quant = quantile(concentration_mg_g_dw, 
                                                        probs = c(0.975)),
                     conc_mg_g_dw_max = max(concentration_mg_g_dw))
  
  
  if (object_type == "file") {
    openxlsx::write.xlsx(table, 
                         file = paste0("output/compo fish/summary_fish_compo_sp.xlsx"))
  } else {
    table
  }
  
  
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
                        values_to = "concentration_mg_g_dw") |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    dplyr::group_by(Species) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species, "\n(n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Speciesn, 
                                             concentration_mg_g_dw), 
                                 y = concentration_mg_g_dw, fill = Species)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
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
# function to display boxplot of elemental composition per family
boxplot_compo_fish_sp_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Species = factor(Species, 
                                  levels = c(# Paralepididae
                                             "Arctozenus risso", 
                                             "Notolepsis coatsi",
                                             # Bathydraconidae
                                             "Bathydraco antarcticus",
                                             # Bathylagidae
                                             "Bathylagus tenuis",
                                             # Channichthyidae
                                             "Champsocephalus gunnari",
                                             "Channichthys rhinoceratus",
                                             # Nototheniidae
                                             "Dissostichus eleginoides",
                                             "Gobionotothen acuta",
                                             "Lepidonotothen squamifrons",
                                             "Lindbergichthys mizops",
                                             # Carapidae
                                             "Echiodon cryomargarites",
                                             # Myctophidae
                                             "Electrona antarctica",
                                             "Electrona carlsbergi", 
                                             "Electrona subaspera",
                                             "Gymnoscopelus bolini",
                                             "Gymnoscopelus braueri",
                                             "Gymnoscopelus fraseri",
                                             "Gymnoscopelus nicholsi", 
                                             "Gymnoscopelus piabilis", 
                                             "Krefftichthys anderssoni",
                                             "Protomyctophum andriashevi",
                                             "Protomyctophum bolini",
                                             "Protomyctophum choriodon",
                                             "Protomyctophum tenisoni",
                                             # Stomiidae
                                             "Idiacanthus atlanticus", 
                                             "Stomias sp",
                                             # Notosudidae
                                             "Luciosudis normani",
                                             # Macrouridae
                                             "Macrourus carinatus",
                                             # Achiropsettidae
                                             "Mancopsetta mancopsetta",
                                             "Melanostigma gelatinosum",
                                             # Muraenolepididae
                                             "Muraenolepsis sp",
                                             # Microstomatidae
                                             "Nansenia antarctica",
                                             # Gempylidae
                                             "Paradiplospinus gracilis",
                                             # Melamphaidae
                                             "Poromitra crassiceps")), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co"))) |>
    dplyr::group_by(Species) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Speciesn = paste0(Species, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Speciesn, 
                                 y = concentration_mg_g_dw, fill = Nutrient)) +
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
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
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
# function to display boxplot of elemental composition per genus
boxplot_compo_fish_genus <- function(res_fish_tib, 
                                     nutrient) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    dplyr::group_by(Genus) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Genusn = paste0(Genus, "\n(n = ", n, ")")) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Genusn,
                                             concentration_mg_g_dw),
                                 y = concentration_mg_g_dw, fill = Genus)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
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
# function to display boxplot of elemental composition per family
boxplot_compo_fish_genus_all_nut <- function(res_fish_tib 
) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K,
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
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
                                             "Muraenolepsis",
                                             "Nansenia",
                                             "Notolepsis",
                                             "Paradiplospinus",
                                             "Poromitra",
                                             "Protomyctophum",
                                             "Stomias"))) |>
    dplyr::group_by(Genus) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Genusn = paste0(Genus, " (n = ", n, ")")) |>
    ggplot2::ggplot(ggplot2::aes(x = Genusn, 
                                 y = concentration_mg_g_dw, fill = Nutrient)) +
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
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
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
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(Family) |>
    dplyr::mutate(n = dplyr::n_distinct(Code_sample),
                  Familyn = paste0(Family, "\n(n = ", n, ")")) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    dplyr::filter(Nutrient == nutrient) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Familyn, 
                                             concentration_mg_g_dw), 
                                 y = concentration_mg_g_dw, fill = Family)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.5) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
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
                        values_to = "concentration_mg_g_dw") |>
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
                                 y = concentration_mg_g_dw, fill = Nutrient)) +
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
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
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
  ggplot2::ggsave(paste0("output/compo fish/per-fam/boxplot_fam_all_nut.jpg"),
                  scale = 1,
                  height = 18, width = 22
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
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab(paste0(nutrient, " concentration (in mg/g dry weight)")) +
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
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1], 
                  sp = stringr::str_split_fixed(Species, " ", 2)[,2]) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(campaign,
                                             concentration_mg_g_dw), 
                                 y = concentration_mg_g_dw, fill = campaign)) +
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
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
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
# function to display boxplot of elemental composition of all fish
boxplot_compo_fish_tot <- function(res_fish_tib) {
  
  res_fish_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Ca", "P", "Na", "K", "Mg", 
                                                 "Fe", "Zn", "Cu", "Mn", "Se",
                                                 "As", "Ni","Co"))) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, 
                                 y = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
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
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Ca", "P", "Na", "K", "Mg", 
                                                 "Fe", "Zn", "Cu", "Mn", "Se",
                                                 "As", "Ni","Co"))) |>
    # remove NAs if there is still some
    dplyr::filter(!(is.na(concentration_mg_g_dw))) |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::xlab("Nutrient concentration (in mg/g dry weight)") +
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
# function to display correlation plot of elemental composition of fish
corr_compo_fish <- function(res_fish_tib) {
  
  corr_mat <- robCompositions::corCoDa(
    as.data.frame(res_fish_tib |>
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
                                  midpoint = 0, limit = c(-1,1),
                         name = "Correlation coefficient") +
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_text(size = 13), 
                   legend.text = ggplot2::element_text(size = 11), 
                   legend.title.align = 0)
  ggplot2::ggsave("output/compo fish/corrplot_compo_fish.jpg",
                  scale = 1,
                  height = 5, width = 6
  )
  
}


################################################################################
############################################# SCAT ############################
################################################################################


#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per species
table_compo_scats <- function(res_scat_tib, 
                              object_type # either "output" or "file" 
) {
  
  table <- res_scat_tib |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                                 Mg, Mn, Na, Ni, P, Se, Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(site, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     conc_mg_g_dw_min = min(concentration_mg_g_dw), 
                     conc_mg_g_dw_low_quant = quantile(concentration_mg_g_dw, 
                                                       probs = c(0.025)),
                     conc_mg_g_dw_mean = mean(concentration_mg_g_dw),
                     conc_mg_g_dw_high_quant = quantile(concentration_mg_g_dw, 
                                                        probs = c(0.975)),
                     conc_mg_g_dw_max = max(concentration_mg_g_dw))
  
  
  if (object_type == "file") {
    openxlsx::write.xlsx(table, 
                         file = paste0("output/compo scats/summary_scats_compo.xlsx"))
  } else {
    table
  }
  
  
}

#'
#'
#'
#'
#'
# function to display boxplot of elemental composition of poops in total without the 
# statistical outlier display
boxplot_compo_scats_tot <- function(res_scat_tib) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo scats/boxplot_compo_tot.jpg", 
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
densplot_compo_scats_tot <- function(res_scat_tib) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_density() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::xlab("Nutrient concentration (in mg/g dry weight)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/compo scats/densplot_compo_tot.jpg", 
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
boxplot_compo_scats_site <- function(res_scat_tib) {
  
  res_scat_tib |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = site, y = concentration_mg_g_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.2) +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                          "#E8C4A2FF", "#14191FFF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
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
  ggplot2::ggsave("output/compo scats/boxplot_compo_sites.jpg", 
                  scale = 1,
                  height = 12, width = 17
  )
  
}



#'
#'
#'
#'
#'
# function to display correlation plot of elemental composition of fish
corr_compo_scats <- function(res_scat_tib) {
  
  corr_mat <- robCompositions::corCoDa(
    as.data.frame(res_scat_tib |>
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
                                  midpoint = 0, limit = c(-1,1),
                                  name = "Correlation coefficient") +
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_text(size = 13), 
                   legend.text = ggplot2::element_text(size = 11), 
                   legend.title.align = 0)
  ggplot2::ggsave("output/compo scats/corrplot_compo_scats.jpg",
                  scale = 1,
                  height = 5, width = 6
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
                                 y = log(concentration_in_fish_ration_ww), fill = Id_source))+
    ggplot2::geom_boxplot() +
    viridis::scale_fill_viridis(option = "magma", 
                                discrete = TRUE) +
    #ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::ylab(paste0("Nutrient concentration in fish\nration (in mg/g wet weight)")) +
    ggplot2::xlab("Source") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   axis.title.x = ggplot2::element_text(size = 15, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 15, face = "bold"), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_blank()
    )
  ggplot2::ggsave("output/boxplot_compo_diets_sources.jpg",
                  scale = 1,
                  height = 5, width = 9
  )
}







#'
#'
#'
#'
# perform centered-logratio transformation on data 

clr_transf_fish <- function(res_fish_tib) {
  
  
  res_fish_tib |>
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

clr_PCA <- function(clr_fish_tib) {
  
  data.act <- as.data.frame(clr_fish_tib |>
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
#' # perform PCA on composition dataset of fish
#' 
#' PCA_fish<- function(res_fish_tib) {
#'   
#'   # keep only quantitative variables 
#'   data.act <- res_fish_tib |>
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
#' # perform HCA on composition dataset of fish
#' 
#' HCA_fish<- function(res_fish_tib) {
#'   
#'   # keep only quantitative variables 
#'   data.act <- res_fish_tib |>
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
