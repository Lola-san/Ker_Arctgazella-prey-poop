################################################################################
# Ker_Arctgazella-prey-scats project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2022
# _targets.R
#
# Script decomposing with all steps of the analysis with target
################################################################################

library("targets")


# Source all functions contained in all files in the R directory
lapply(list.files(here::here("R"),
                  recursive = TRUE, full.names = T),
       source)


list(
  #################### DATA #########################################
  ##### 1 - on prey samples 
  # define and load data on samples of prey
  tar_target(data_prey_file,
             "data/Data_prey_YC.xlsx", 
             format = "file"),
  tar_target(data_prey_samples, load_xl(data_prey_file)),
  # define and load results of composition of prey samples
  tar_target(res_compo_prey_file,
             "data/Compo_preys.xlsx", 
             format = "file"),
  tar_target(res_compo_prey, load_xl(res_compo_prey_file)),
  # summarize data on samples 
  tar_target(data_prey_summary, summary_prey_samples(data_prey_samples, 
                                                     res_compo_prey)),
  
  ##### 2 - on scats
  # define and load data on samples of scats
  tar_target(data_scat_file,
             "data/Scat_data.xlsx", 
             format = "file"),
  tar_target(data_scat_samples, load_xl(data_scat_file)),
  # summarize data on samples 
  tar_target(data_scat_summary, summary_scat_samples(data_scat_samples)),
  
  ##### 3 - on diet of Arctocephalus gazella
  tar_target(data_diet_file, 
             "data/Diet_Agazella.xlsx", 
             format = "file"), 
  tar_target(data_diet, load_xl(data_diet_file)),
  # summarize data on diets 
  tar_target(data_diet_summary, summary_diet_data(data_diet, 
                                                  res_compo_prey)),
  
  ################### COMPOSITION OF PREY ###################
  
  # identify outliers
  # statistical outliers
  tar_target(boxplot_outliers_prey_stats, boxplot_id_outliers(res_compo_prey, "prey")), # boxplot
  tar_target(tib_outliers_prey_stats, tib_id_outliers(res_compo_prey, data_prey_samples, "prey", "prey_id_outliers")), # tibble
  # technical outliers 
  tar_target(hist_outliers_prey_tech, hist_id_outliers(res_compo_prey, "prey")), # histograms
  
  ###### samples identified with adnormal values must be treated independently with several possibilities: 
  # - if they display adnormal values for a majority of nutrients (more than 7), there must have been a contamination
  #   during sample preparation ----> THESE ARE REMOVED 
  # - if not, then analysis can be rerun for same samples, then two possible outcomes 
  #             - value is back into "normal" range --> REPLACE ODD VALUE WITH NEW ONE
  #             - values is still "abnormal" --> KEEP SAMPLE FOR CHARACTERIZATION OF COMPOSITION OF SAMPLE 
  #                                             AND FOR STATISTICAL ANALYSIS REPLACE VALUE WITH VALUE OF HIGHER QUANTILE 
  #                                             (97.5 %) OF OTHER SAMPLES FOR SAME ELEMENT
  
  # format by adding campaign data and identify sp of prey included in diet of Agazella
  # & remove sample contaminated (2005_PROTAND_PA03 & 2010PII_ARCTRIS_CHA94_AR01)
  # & remove unwanted elements
  tar_target(full_res_compo_prey, complete_prey_results(res_compo_prey, data_diet) |> 
               dplyr::filter(!(Code_sample %in% c("2005_PROTAND_PA03",
                                                  "2010PII_ARCTRIS_CHA94_AR01"))) |>
               dplyr::select(-c(Mo, V, Ag, Cr, Pb, Cd, Sr)) ),
  
  # boxplots per species and per nutrient
  tar_target(boxplot_prey_sp_As, boxplot_compo_prey_sp(full_res_compo_prey, "As")),
  tar_target(boxplot_prey_sp_Ca, boxplot_compo_prey_sp(full_res_compo_prey, "Ca")),
  tar_target(boxplot_prey_sp_Co, boxplot_compo_prey_sp(full_res_compo_prey, "Co")),
  tar_target(boxplot_prey_sp_Cu, boxplot_compo_prey_sp(full_res_compo_prey, "Cu")),
  tar_target(boxplot_prey_sp_Fe, boxplot_compo_prey_sp(full_res_compo_prey, "Fe")),
  tar_target(boxplot_prey_sp_K, boxplot_compo_prey_sp(full_res_compo_prey, "K")),
  tar_target(boxplot_prey_sp_Mg, boxplot_compo_prey_sp(full_res_compo_prey, "Mg")),
  tar_target(boxplot_prey_sp_Mn, boxplot_compo_prey_sp(full_res_compo_prey, "Mn")),
  tar_target(boxplot_prey_sp_Na, boxplot_compo_prey_sp(full_res_compo_prey, "Na")),
  tar_target(boxplot_prey_sp_Ni, boxplot_compo_prey_sp(full_res_compo_prey, "Ni")),
  tar_target(boxplot_prey_sp_P, boxplot_compo_prey_sp(full_res_compo_prey, "P")),
  tar_target(boxplot_prey_sp_Se, boxplot_compo_prey_sp(full_res_compo_prey, "Se")),
  tar_target(boxplot_prey_sp_Zn, boxplot_compo_prey_sp(full_res_compo_prey, "Zn")),
  
  
  tar_target(boxplot_prey_genus_As, boxplot_compo_prey_genus(full_res_compo_prey, "As")),
  tar_target(boxplot_prey_genus_Ca, boxplot_compo_prey_genus(full_res_compo_prey, "Ca")),
  tar_target(boxplot_prey_genus_Co, boxplot_compo_prey_genus(full_res_compo_prey, "Co")),
  tar_target(boxplot_prey_genus_Cu, boxplot_compo_prey_genus(full_res_compo_prey, "Cu")),
  tar_target(boxplot_prey_genus_Fe, boxplot_compo_prey_genus(full_res_compo_prey, "Fe")),
  tar_target(boxplot_prey_genus_K, boxplot_compo_prey_genus(full_res_compo_prey, "K")),
  tar_target(boxplot_prey_genus_Mg, boxplot_compo_prey_genus(full_res_compo_prey, "Mg")),
  tar_target(boxplot_prey_genus_Mn, boxplot_compo_prey_genus(full_res_compo_prey, "Mn")),
  tar_target(boxplot_prey_genus_Na, boxplot_compo_prey_genus(full_res_compo_prey, "Na")),
  tar_target(boxplot_prey_genus_Ni, boxplot_compo_prey_genus(full_res_compo_prey, "Ni")),
  tar_target(boxplot_prey_genus_P, boxplot_compo_prey_genus(full_res_compo_prey, "P")),
  tar_target(boxplot_prey_genus_Se, boxplot_compo_prey_genus(full_res_compo_prey, "Se")),
  tar_target(boxplot_prey_genus_Zn, boxplot_compo_prey_genus(full_res_compo_prey, "Zn")),
  
  
  tar_target(boxplot_prey_fam_As, boxplot_compo_prey_fam(full_res_compo_prey, "As")),
  tar_target(boxplot_prey_fam_Ca, boxplot_compo_prey_fam(full_res_compo_prey, "Ca")),
  tar_target(boxplot_prey_fam_Co, boxplot_compo_prey_fam(full_res_compo_prey, "Co")),
  tar_target(boxplot_prey_fam_Cu, boxplot_compo_prey_fam(full_res_compo_prey, "Cu")),
  tar_target(boxplot_prey_fam_Fe, boxplot_compo_prey_fam(full_res_compo_prey, "Fe")),
  tar_target(boxplot_prey_fam_K, boxplot_compo_prey_fam(full_res_compo_prey, "K")),
  tar_target(boxplot_prey_fam_Mg, boxplot_compo_prey_fam(full_res_compo_prey, "Mg")),
  tar_target(boxplot_prey_fam_Mn, boxplot_compo_prey_fam(full_res_compo_prey, "Mn")),
  tar_target(boxplot_prey_fam_Na, boxplot_compo_prey_fam(full_res_compo_prey, "Na")),
  tar_target(boxplot_prey_fam_Ni, boxplot_compo_prey_fam(full_res_compo_prey, "Ni")),
  tar_target(boxplot_prey_fam_P, boxplot_compo_prey_fam(full_res_compo_prey, "P")),
  tar_target(boxplot_prey_fam_Se, boxplot_compo_prey_fam(full_res_compo_prey, "Se")),
  tar_target(boxplot_prey_fam_Zn, boxplot_compo_prey_fam(full_res_compo_prey, "Zn")),
  
  
  tar_target(boxplot_prey_camp_As, boxplot_compo_prey_camp(full_res_compo_prey, "As")),
  tar_target(boxplot_prey_camp_Ca, boxplot_compo_prey_camp(full_res_compo_prey, "Ca")),
  tar_target(boxplot_prey_camp_Co, boxplot_compo_prey_camp(full_res_compo_prey, "Co")),
  tar_target(boxplot_prey_camp_Cu, boxplot_compo_prey_camp(full_res_compo_prey, "Cu")),
  tar_target(boxplot_prey_camp_Fe, boxplot_compo_prey_camp(full_res_compo_prey, "Fe")),
  tar_target(boxplot_prey_camp_K, boxplot_compo_prey_camp(full_res_compo_prey, "K")),
  tar_target(boxplot_prey_camp_Mg, boxplot_compo_prey_camp(full_res_compo_prey, "Mg")),
  tar_target(boxplot_prey_camp_Mn, boxplot_compo_prey_camp(full_res_compo_prey, "Mn")),
  tar_target(boxplot_prey_camp_Na, boxplot_compo_prey_camp(full_res_compo_prey, "Na")),
  tar_target(boxplot_prey_camp_Ni, boxplot_compo_prey_camp(full_res_compo_prey, "Ni")),
  tar_target(boxplot_prey_camp_P, boxplot_compo_prey_camp(full_res_compo_prey, "P")),
  tar_target(boxplot_prey_camp_Se, boxplot_compo_prey_camp(full_res_compo_prey, "Se")),
  tar_target(boxplot_prey_camp_Zn, boxplot_compo_prey_camp(full_res_compo_prey, "Zn")),
  
  
  
  tar_target(boxplot_prey_tot, boxplot_compo_prey_tot(full_res_compo_prey)),
  
  
  ################### COMPOSITION OF scats ###################
  # load data with composition of scats 
  tar_target(res_compo_scats_file,
             "data/Compo_poop.xlsx", 
             format = "file"),
  tar_target(res_compo_scats, load_xl(res_compo_scats_file)), 
  
  
  # statistical outliers
  tar_target(boxplot_outliers_scats_stats, boxplot_id_outliers(res_compo_scats, "poop")), # boxplot
  tar_target(tib_outliers_scats_stats, tib_id_outliers(res_compo_scats, data_samples, "poop", "scats_id_outliers")), # tibble
  # technical outliers 
  tar_target(hist_outliers_scats_tech, hist_id_outliers(res_compo_scats, "poop")), # histogram
  
  # remove contaminated samples (CN12 & CN02) and unwanted elements
  tar_target(full_res_compo_scats, res_compo_scats |>
               dplyr::select(-c(Mo, V, Ag, Cr, Pb, Cd, Sr)) |>
               dplyr::mutate(site = dplyr::case_when(stringr::str_starts(Code_sample, "CN") ~ "Cap Noir", 
                                                     stringr::str_starts(Code_sample, "PS") ~ "Pointe Suzanne")) |>
               dplyr::filter(!(Code_sample %in% c("CN12", "CN02")))),
  
  # composition : total without outliers
  tar_target(boxplot_scats_tot, boxplot_compo_poop_tot(full_res_compo_scats)), 
  tar_target(boxplot_scats_sites, boxplot_compo_poop_site(full_res_compo_scats)),
  
  
  ##################### SEE COMPOSITION OF DIET OF A.GAZELLA ############
  tar_target(compo_prey_ww, dw_to_ww_prey(full_res_compo_prey, 
                                          data_prey_samples)),
  tar_target(conc_in_diet, compute_concentrations_in_diet(data_diet,
                                                          compo_prey_ww)),
  
  ##################### COMPAIR COMPOSITION OF PREY, DIET AND SCATS ###########
  # bind the data
  tar_target(prey_and_scats, pool_prey_poop(full_res_compo_prey, 
                                           full_res_compo_scats)),
  
  # boxplot comparison between composition of fish and scats 
  tar_target(boxplot_prey_and_scats_full, boxplot_compair_compo_full(prey_and_scats)),
  # in relative, standardized per sample
  tar_target(boxplot_prey_and_scats_relative, boxplot_compair_compo_fish_scat_relative(prey_and_scats)),
  # boxplot comparison between composition of fish and scats with only fish identified
  # as prey species
  tar_target(boxplot_prey_and_scats_prey, boxplot_compair_compo_prey(prey_and_scats)),
  
  tar_target(boxplot_prey_scats_bowl_relative, boxplot_compair_compo_fish_scats_bowl_relative(prey_and_scats, 
                                                                                             conc_in_diet)),
  
  ##################### TRANSFORM & PERFORM PCA ON PREY DATA ############
  tar_target(PCA_preys, PCA_prey_Rob(full_res_compo_prey))
  
  
  
  
)