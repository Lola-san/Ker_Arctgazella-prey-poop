################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# March (end) 2022
# 05_diet_Agazella.R
#
# Script with functions to compute relative composition of A. gazella 
# in Kerguelen based on the composition of its prey 
################################################################################



#'
#'
#'
#'
#'
# function to compute means of concentrations of preys at different taxonomic levels
dw_to_ww_prey <- function(res_prey_tib, 
                          data_samples_tib) {
  res_prey_tib |>
    dplyr::left_join(data_samples_tib |>
                       dplyr::select(-c("Code_Sample")) |>
                       dplyr::rename(Code_sample = "Code_new_format"),
                     by = c("Code_sample", "Family", "Species"), keep = FALSE) |>
    dplyr::mutate(As = As*(1-(Water_percent/100)), 
                  Ca = Ca*(1-(Water_percent/100)),
                  Co = Co*(1-(Water_percent/100)),
                  Cu = Cu*(1-(Water_percent/100)),
                  Fe = Fe*(1-(Water_percent/100)),
                  K = K*(1-(Water_percent/100)),
                  Mg = Mg*(1-(Water_percent/100)),
                  Mn = Mn*(1-(Water_percent/100)),
                  Na = Na*(1-(Water_percent/100)),
                  Ni = Ni*(1-(Water_percent/100)),
                  P = P*(1-(Water_percent/100)),
                  Se = Se*(1-(Water_percent/100)),
                  Zn = Zn*(1-(Water_percent/100))) |>
    dplyr::select(c("Code_sample", "Family", "Species",
                    "As", "Ca", "Co", "Cu", "Fe", "K",
                    "Mg", "Mn", "Na", "Ni", "P", "Se", "Zn"))
  
  
}


#'
#'
#'
#'
#'
# function to compute %W in diet * means 
compute_concentrations_in_diet <- function(diet_tib, 
                                           ww_prey_tib) {
  
  ww_prey_tib <- ww_prey_tib |> 
    dplyr::mutate(Genus = stringr::str_split_fixed(Species, " ", 2)[,1],
                  Order = dplyr::case_when(Family == "Myctophidae" ~ "Myctophiformes", 
                                           Family == "Macrouridae" ~ "Gadiformes", 
                                           Family == "Carapidae" ~ "Ophidiiformes",
                                           Family == "Bathydraconidae" ~ "Perciformes",
                                           Family == "Paralepididae" ~ "Aulopiformes",
                                           Family == "Nototheniidae" ~ "Perciformes",
                                           Family == "Bathylagidae" ~ "Argentiniformes",
                                           Family == "Stomiidae" ~ "Stomiiformes",
                                           Family == "Melamphaidae" ~ "Beryciformes",
                                           Family == "Notosudidae" ~ "Aulopiformes",
                                           Family == "Achiropsettidae" ~ "Pleuronectiformes",
                                           Family == "Zoarcidae" ~ "Perciformes",
                                           Family == "Channichthyidae" ~ "Perciformes",
                                           Family == "Gempylidae" ~ "Scombriformes",
                                           Family == "Muraenolepididae" ~ "Gadiformes",
                                           Family == "Microstomatidae" ~ "Argentiniformes")
    )
  
  # get means at different levels with intermediary tables 
  sp_means <- ww_prey_tib |> 
    dplyr::group_by(Species) |>
    dplyr::summarise(meanAs_sp = mean(As),
                     meanCa_sp = mean(Ca),
                     meanCo_sp = mean(Co),
                     meanCu_sp = mean(Cu),
                     meanFe_sp = mean(Fe),
                     meanK_sp = mean(K),
                     meanMg_sp = mean(Mg),
                     meanMn_sp = mean(Mn),
                     meanNa_sp = mean(Na),
                     meanNi_sp = mean(Ni),
                     meanP_sp = mean(P),
                     meanSe_sp = mean(Se),
                     meanZn_sp = mean(Zn)
    ) |>
    dplyr::filter(!(is.na(Species)))
  
  genus_means <- ww_prey_tib |> 
    dplyr::group_by(Genus) |>
    dplyr::summarise(meanAs_ge = mean(As),
                     meanCa_ge = mean(Ca),
                     meanCo_ge = mean(Co),
                     meanCu_ge = mean(Cu),
                     meanFe_ge = mean(Fe),
                     meanK_ge = mean(K),
                     meanMg_ge = mean(Mg),
                     meanMn_ge = mean(Mn),
                     meanNa_ge = mean(Na),
                     meanNi_ge = mean(Ni),
                     meanP_ge = mean(P),
                     meanSe_ge = mean(Se),
                     meanZn_ge = mean(Zn)
    ) |>
    dplyr::filter(!(is.na(Genus)))
  
  family_means <- ww_prey_tib |> 
    dplyr::group_by(Family) |>
    dplyr::summarise(meanAs_fam = mean(As),
                     meanCa_fam = mean(Ca),
                     meanCo_fam = mean(Co),
                     meanCu_fam = mean(Cu),
                     meanFe_fam = mean(Fe),
                     meanK_fam = mean(K),
                     meanMg_fam = mean(Mg),
                     meanMn_fam = mean(Mn),
                     meanNa_fam = mean(Na),
                     meanNi_fam = mean(Ni),
                     meanP_fam = mean(P),
                     meanSe_fam = mean(Se),
                     meanZn_fam = mean(Zn)
    )
  
  order_means <- ww_prey_tib |> 
    dplyr::group_by(Order) |>
    dplyr::summarise(meanAs_or = mean(As),
                     meanCa_or = mean(Ca),
                     meanCo_or = mean(Co),
                     meanCu_or = mean(Cu),
                     meanFe_or = mean(Fe),
                     meanK_or = mean(K),
                     meanMg_or = mean(Mg),
                     meanMn_or = mean(Mn),
                     meanNa_or = mean(Na),
                     meanNi_or = mean(Ni),
                     meanP_or = mean(P),
                     meanSe_or = mean(Se),
                     meanZn_or = mean(Zn)
    ) 
  
  
  fish_means <- ww_prey_tib |> 
    dplyr::mutate(Taxa = "Fish") |>
    dplyr::group_by(Taxa) |>
    dplyr::summarise(meanAs_fish = mean(As),
                  meanCa_fish = mean(Ca),
                  meanCo_fish = mean(Co),
                  meanCu_fish = mean(Cu),
                  meanFe_fish = mean(Fe),
                  meanK_fish = mean(K),
                  meanMg_fish = mean(Mg),
                  meanMn_fish = mean(Mn),
                  meanNa_fish = mean(Na),
                  meanNi_fish = mean(Ni),
                  meanP_fish = mean(P),
                  meanSe_fish = mean(Se),
                  meanZn_fish = mean(Zn) 
                  
    )
  
  
  diet_tib |>
    # select only - diet of animals around Kerguelen + studies with %W + fish species
    dplyr::filter(Location  == "Kerguelen Islands", 
                  Taxa == "Fish") |>
    dplyr::mutate(`%W` = as.numeric(`%W`)) |>
    dplyr::rename(W = `%W`) |>
    dplyr::filter(!(is.na(W))) |>
    # join the tables
    dplyr::left_join(sp_means, by = "Species", keep = FALSE) |>
    dplyr::left_join(genus_means, by = "Genus", keep = FALSE) |>
    dplyr::left_join(family_means, by = "Family", keep = FALSE) |>
    dplyr::left_join(order_means, by = "Order", keep = FALSE) |>
    dplyr::left_join(fish_means, by = "Taxa", keep = FALSE) |>
    dplyr::select(c("Source", "n", "W", 
                    "Species", "Genus", "Family", "Order", "Taxa", 
                    "meanAs_sp", "meanCa_sp", "meanCo_sp", "meanCu_sp", 
                    "meanFe_sp", "meanK_sp", "meanMg_sp", "meanMn_sp", 
                    "meanNa_sp", "meanNi_sp", "meanP_sp", "meanSe_sp", "meanZn_sp", 
                    "meanAs_ge", "meanCa_ge", "meanCo_ge", "meanCu_ge", 
                    "meanFe_ge", "meanK_ge", "meanMg_ge", "meanMn_ge", 
                    "meanNa_ge", "meanNi_ge", "meanP_ge", "meanSe_ge", "meanZn_ge",
                    "meanAs_fam", "meanCa_fam", "meanCo_fam", "meanCu_fam",
                    "meanFe_fam", "meanK_fam", "meanMg_fam", "meanMn_fam",
                    "meanNa_fam", "meanNi_fam", "meanP_fam", "meanSe_fam", "meanZn_fam",
                    "meanAs_or", "meanCa_or", "meanCo_or", "meanCu_or",  
                    "meanFe_or", "meanK_or", "meanMg_or", "meanMn_or",
                    "meanNa_or", "meanNi_or", "meanP_or", "meanSe_or", "meanZn_or",
                    "meanAs_fish", "meanCa_fish", "meanCo_fish", "meanCu_fish",
                    "meanFe_fish", "meanK_fish", "meanMg_fish", "meanMn_fish",
                    "meanNa_fish", "meanNi_fish", "meanP_fish", "meanSe_fish", "meanZn_fish")) |>
    dplyr::distinct() |>
    dplyr::mutate(Id_source = dplyr::case_when(Source == "Cherel et al 1997" ~ "Cherel et al 1997", 
                                               Source == "Jeanniard-du-Dot 2015" ~ "Jeanniard-du-Dot 2015",
                                               Source == "Lea et al. 2002" & n == 60 ~ "Lea et al. 2002 (data 1998)",
                                               Source == "Lea et al. 2002" & n == 24 ~ "Lea et al. 2002 (data 1999)",
                                               Source == "Lea et al. 2002" & n == 47 ~ "Lea et al. 2002 (data 2000)")) |>
    # delete duplicated lines due to the joints
    dplyr::mutate(# get values at the finest level available (species if not genus, if not family, if not order and then taxa)
      As = (W/100)*dplyr::case_when(is.na(meanAs_or) ~ meanAs_fish,
                            is.na(meanAs_fam) ~ meanAs_or,
                            is.na(meanAs_ge) ~ meanAs_fam,
                            is.na(meanAs_sp) ~ meanAs_ge, 
                            TRUE ~ meanAs_sp), 
      Ca = (W/100)*dplyr::case_when(is.na(meanCa_or) ~ meanCa_fish,
                             is.na(meanCa_fam) ~ meanCa_or,
                             is.na(meanCa_ge) ~ meanCa_fam,
                             is.na(meanCa_sp) ~ meanCa_ge, 
                             TRUE ~ meanCa_sp),
      Co = (W/100)*dplyr::case_when(is.na(meanCo_or) ~ meanCo_fish,
                             is.na(meanCo_fam) ~ meanCo_or,
                             is.na(meanCo_ge) ~ meanCo_fam,
                             is.na(meanCo_sp) ~ meanCo_ge, 
                             TRUE ~ meanCo_sp),
      Cu = (W/100)*dplyr::case_when(is.na(meanCu_or) ~ meanCu_fish,
                             is.na(meanCu_fam) ~ meanCu_or,
                             is.na(meanCu_ge) ~ meanCu_fam,
                             is.na(meanCu_sp) ~ meanCu_ge,
                             TRUE ~ meanCu_sp),
      Fe = (W/100)*dplyr::case_when(is.na(meanFe_or) ~ meanFe_fish,
                             is.na(meanFe_fam) ~ meanFe_or,
                             is.na(meanFe_ge) ~ meanFe_fam,
                             is.na(meanFe_sp) ~ meanFe_ge, 
                             TRUE ~ meanFe_sp),
      K = (W/100)*dplyr::case_when(is.na(meanK_or) ~ meanK_fish,
                            is.na(meanK_fam) ~ meanK_or,
                            is.na(meanK_ge) ~ meanK_fam,
                            is.na(meanK_sp) ~ meanK_ge, 
                            TRUE ~ meanK_sp),
      Mg = (W/100)*dplyr::case_when(is.na(meanMg_or) ~ meanMg_fish,
                             is.na(meanMg_fam) ~ meanMg_or,
                             is.na(meanMg_ge) ~ meanMg_fam,
                             is.na(meanMg_sp) ~ meanMg_ge, 
                             TRUE ~ meanMg_sp),
      Mn = (W/100)*dplyr::case_when(is.na(meanMn_or) ~ meanMn_fish,
                             is.na(meanMn_fam) ~ meanMn_or,
                             is.na(meanMn_ge) ~ meanMn_fam,
                             is.na(meanMn_sp) ~ meanMn_ge, 
                             TRUE ~ meanMn_sp),
      Na = (W/100)*dplyr::case_when(is.na(meanNa_or) ~ meanNa_fish,
                             is.na(meanNa_fam) ~ meanNa_or,
                             is.na(meanNa_ge) ~ meanNa_fam,
                             is.na(meanNa_sp) ~ meanNa_ge, 
                             TRUE ~ meanNa_sp),
      Ni = (W/100)*dplyr::case_when(is.na(meanNi_or) ~ meanNi_fish,
                             is.na(meanNi_fam) ~ meanNi_or,
                             is.na(meanNi_ge) ~ meanNi_fam,
                             is.na(meanNi_sp) ~ meanNi_ge, 
                             TRUE ~ meanNi_sp),
      P = (W/100)*dplyr::case_when(is.na(meanP_or) ~ meanP_fish,
                            is.na(meanP_fam) ~ meanP_or,
                            is.na(meanP_ge) ~ meanP_fam,
                            is.na(meanP_sp) ~ meanP_ge, 
                            TRUE ~ meanP_sp),
      Se = (W/100)*dplyr::case_when(is.na(meanSe_or) ~ meanSe_fish,
                             is.na(meanSe_fam) ~ meanSe_or,
                             is.na(meanSe_ge) ~ meanSe_fam,
                             is.na(meanSe_sp) ~ meanSe_ge, 
                             TRUE ~ meanSe_sp),
      Zn = (W/100)*dplyr::case_when(is.na(meanZn_or) ~ meanZn_fish,
                             is.na(meanZn_fam) ~ meanZn_or,
                             is.na(meanZn_ge) ~ meanZn_fam,
                             is.na(meanZn_sp) ~ meanZn_ge, 
                             TRUE ~ meanZn_sp),
      # keep track of the taxonomic level used
      Level_used = dplyr::case_when(is.na(meanAs_or) ~ "Taxa", # any element would work
                             is.na(meanAs_fam) ~ "Order",
                             is.na(meanAs_ge) ~ "Family",
                             is.na(meanAs_sp) ~ "Genus", 
                             TRUE ~ "Species")
    ) |>
    dplyr::select(c(Source, n, Id_source, W, Species, Genus, Family, Order, Taxa,
             "As", "Ca", "Co", "Cu", "Fe", "K",
             "Mg", "Mn", "Na", "Ni", "P", "Se", "Zn", Level_used)) |>
    dplyr::group_by(Id_source) |>
    dplyr::summarise(As = sum(As), 
                     Ca = sum(Ca), 
                     Co = sum(Co),
                     Cu = sum(Cu),
                     Fe = sum(Fe),
                     K = sum(K),
                     Mg = sum(Mg),
                     Mn = sum(Mn),
                     Na = sum(Na),
                     Ni = sum(Ni),
                     P = sum(P),
                     Se = sum(Se),
                     Zn = sum(Zn)) |>
    dplyr::mutate(sum = As + Ca + Co + Cu + Fe + K +
                    Mg + Mn + Na + Ni + P + Se + Zn) |>
    tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, Mg, Mn, Na, Ni, P, Se, Zn),
                 names_to = "Nutrient",
                 values_to = "concentration_in_fish_ration_ww") |>
    dplyr::mutate(relative_concentration = concentration_in_fish_ration_ww/sum) 
}


#'
#'
#'
#'
#'
# plot concentration in diet in the different sources 
compair_diets_results <- function(conc_diet_tib) {
  conc_in_diet |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Id_source,
                                             concentration_in_fish_ration_ww),
                                 y = concentration_in_fish_ration_ww, color = Id_source))+
    ggplot2::geom_point(size = 3) +
    #ggplot2::coord_flip() +
    viridis::scale_color_viridis(option = "magma", 
                                discrete = TRUE) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::ylab(paste0("concentration in fish ration (in mg/g wet weight)")) +
    ggplot2::xlab("Source") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
  ggplot2::ggsave("output/boxplot_comp_diets_compo_sources.jpg",
                  scale = 1,
                  height = 12, width = 17
  )
  
  
  conc_in_diet |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg",  
                                               "Fe", "Zn", "Cu", "Mn",
                                               "Se","Ni", "As", "Co"))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient,
                                 y = log(concentration_in_fish_ration_ww), fill = Nutrient))+
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(option = "magma", 
                                 discrete = TRUE) +
    #ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::ylab(paste0("concentration in fish ration (in mg/g wet weight)")) +
    ggplot2::xlab("Source") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"), 
                   #legend.position = "none"
    )
}
