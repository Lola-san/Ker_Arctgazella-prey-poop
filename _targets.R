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
  #################### DATA : SAMPLES & DIET ###################################
  # script 01_summarize_data_samples.R
  ##### 1 - on prey samples 
  # define and load data on samples of prey
  tar_target(data_fish_file,
             "data/data_fish_YC.xlsx", 
             format = "file"),
  tar_target(data_fish_samples, load_xl(data_fish_file)),
  # summarize data on samples 
  tar_target(data_fish_summary, summary_fish_samples(data_fish_samples, 
                                                     res_compo_fish)),
  
  ##### 2 - on scats
  # define and load data on samples of scats
  tar_target(data_scat_file,
             "data/data_scats.xlsx", 
             format = "file"),
  tar_target(data_scat_samples, load_xl(data_scat_file)),
  # summarize data on samples 
  tar_target(data_scat_summary, summary_scat_samples(data_scat_samples)),
  
  ##### 3 - on diet of Arctocephalus gazella
  tar_target(data_diet_file, 
             "data/data_diet_Agazella.xlsx", 
             format = "file"), 
  tar_target(data_diet, load_xl(data_diet_file)),
  # summarize data on diets 
  tar_target(data_diet_summary, summary_diet_data(data_diet, 
                                                  res_compo_fish)),
  
  
  #################### RESULTS : SAMPLE COMPOSITION ############################
  # define and load results of composition of fish samples
  tar_target(res_compo_fish_file,
             "data/res_compo_fish.xlsx", 
             format = "file"),
  tar_target(res_compo_fish, # add campaign data
             load_xl(res_compo_fish_file)),
  
  # define and load results of composition of scats 
  tar_target(res_compo_scats_file,
             "data/res_compo_scats.xlsx", 
             format = "file"),
  tar_target(res_compo_scats, load_xl(res_compo_scats_file)), 
  
  ####### IDENTIFY ANALYTICAL OUTLIERS + POTENTIAL CONTAMINATION ###############
  ###### samples identified with adnormal values must be treated independently 
  # with several possibilities: 
  # - if they display adnormal values for a majority of nutrients (more than 7), 
  #   there must have been a contamination during sample preparation ----> 
  #   THESE ARE REMOVED 
  # - if not, then analysis can be rerun for same samples, 2 possible outcomes 
  #       - value is back into "normal" range --> REPLACE ODD VALUE WITH NEW ONE
  #       - values is still "abnormal" --> KEEP SAMPLE FOR CHARACTERIZATION OF 
  #                                       COMPOSITION OF SAMPLE AND FOR 
  #                                       STATISTICAL ANALYSIS REPLACE VALUE 
  #                                       WITH VALUE OF HIGHER QUANTILE (97.5 %) 
  #                                       OF OTHER SAMPLES FOR SAME ELEMENT
  # script 02_identify_outliers.R
  
  ####### fish
  # identify outliers
  # statistical outliers
  tar_target(boxplot_outliers_fish_stats, 
             boxplot_id_outliers_stats(res_compo_fish, 
                                       "fish")), # boxplot
  tar_target(tib_outliers_fish_stats, 
             tib_id_outliers(res_compo_fish, 
                             data_fish_samples, 
                             "fish", "fish_id_outliers")), # tibble
  # technical outliers 
  tar_target(hist_outliers_fish_tech, 
             hist_id_outliers(res_compo_fish, "fish")), # histograms
  ### two samples identified as potentially contaminated during sample preparation
  ### other samples identified with high values for just a few elements were rerun for analysis
  ### and values were confirmed: they are kept as is, eventually to modify for statistical analysis
  # get rid of technical outliers + add campaign data 
  # format by adding campaign data and identify sp of prey included in diet of Agazella
  # & remove contaminated samples (2005_PROTAND_PA03 & 2010PII_ARCTRIS_CHA94_AR01)
  # & remove unwanted elements
  tar_target(full_res_compo_fish, 
             complete_fish_data(res_compo_fish, data_diet) |> 
               dplyr::filter(!(Code_sample %in% c("2005_PROTAND_PA03",
                                                  "2010PII_ARCTRIS_CHA94_AR01"))) |>
               dplyr::select(-c(Mo, V, Ag, Cr, Pb, Cd, Sr)) ),
  
  # scats
  # statistical outliers
  tar_target(boxplot_outliers_scats_stats, 
             boxplot_id_outliers_stats(res_compo_scats, 
                                       "scat")), # boxplot
  tar_target(tib_outliers_scats_stats, 
             tib_id_outliers(res_compo_scats,
                             data_samples, 
                             "scat", "scats_id_outliers")), # tibble
  # technical outliers 
  tar_target(hist_outliers_scats_tech, 
             hist_id_outliers(res_compo_scats, "scat")), # histogram
  # remove contaminated samples (CN12 & CN02) and unwanted elements
  tar_target(full_res_compo_scats, res_compo_scats |>
               dplyr::select(-c(Mo, V, Ag, Cr, Pb, Cd, Sr)) |>
               dplyr::mutate(site = dplyr::case_when(stringr::str_starts(Code_sample, 
                                                                         "CN") ~ "Cap Noir", 
                                                     stringr::str_starts(Code_sample, 
                                                                         "PS") ~ "Pointe Suzanne")) |>
               dplyr::filter(!(Code_sample %in% c("CN12", "CN02")))),
  
  
  ################### DESCRIBE COMPOSITION ######################################
  
  ####### OF FISH ######
  tar_target(table_summary_compo_fish_sp_output, 
             table_compo_fish_sp(full_res_compo_fish, 
                                 "output")),
  tar_target(table_summary_compo_fish_sp_file, 
             table_compo_fish_sp(full_res_compo_fish, 
                                 "file")),
  
  # all fish samples together 
  tar_target(boxplot_fish_tot, boxplot_compo_fish_tot(full_res_compo_fish)),
  tar_target(densplot_fish_tot, densplot_compo_fish_tot(full_res_compo_fish)), 
  
  # boxplots per species and per nutrient
  tar_target(boxplot_fish_sp_As, 
             boxplot_compo_fish_sp(full_res_compo_fish, "As")),
  tar_target(boxplot_fish_sp_Ca, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Ca")),
  tar_target(boxplot_fish_sp_Co, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Co")),
  tar_target(boxplot_fish_sp_Cu, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Cu")),
  tar_target(boxplot_fish_sp_Fe, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Fe")),
  tar_target(boxplot_fish_sp_K, 
             boxplot_compo_fish_sp(full_res_compo_fish, "K")),
  tar_target(boxplot_fish_sp_Mg, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Mg")),
  tar_target(boxplot_fish_sp_Mn, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Mn")),
  tar_target(boxplot_fish_sp_Na, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Na")),
  tar_target(boxplot_fish_sp_Ni, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Ni")),
  tar_target(boxplot_fish_sp_P, 
             boxplot_compo_fish_sp(full_res_compo_fish, "P")),
  tar_target(boxplot_fish_sp_Se, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Se")),
  tar_target(boxplot_fish_sp_Zn, 
             boxplot_compo_fish_sp(full_res_compo_fish, "Zn")),
  
  # per species for all nutrients
  tar_target(boxplot_fish_sp_all_nut, 
             boxplot_compo_fish_sp_all_nut(full_res_compo_fish)),
  
  # boxplots per genus and per nutrient
  tar_target(boxplot_fish_genus_As, 
             boxplot_compo_fish_genus(full_res_compo_fish, "As")),
  tar_target(boxplot_fish_genus_Ca, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Ca")),
  tar_target(boxplot_fish_genus_Co, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Co")),
  tar_target(boxplot_fish_genus_Cu,
             boxplot_compo_fish_genus(full_res_compo_fish, "Cu")),
  tar_target(boxplot_fish_genus_Fe, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Fe")),
  tar_target(boxplot_fish_genus_K, 
             boxplot_compo_fish_genus(full_res_compo_fish, "K")),
  tar_target(boxplot_fish_genus_Mg, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Mg")),
  tar_target(boxplot_fish_genus_Mn,
             boxplot_compo_fish_genus(full_res_compo_fish, "Mn")),
  tar_target(boxplot_fish_genus_Na, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Na")),
  tar_target(boxplot_fish_genus_Ni, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Ni")),
  tar_target(boxplot_fish_genus_P, 
             boxplot_compo_fish_genus(full_res_compo_fish, "P")),
  tar_target(boxplot_fish_genus_Se, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Se")),
  tar_target(boxplot_fish_genus_Zn, 
             boxplot_compo_fish_genus(full_res_compo_fish, "Zn")),
  
  # per genus for all nutrients
  tar_target(boxplot_fish_genus_all_nut, 
             boxplot_compo_fish_genus_all_nut(full_res_compo_fish)),
  
  # boxplots per family and per nutrient
  tar_target(boxplot_fish_fam_As, 
             boxplot_compo_fish_fam(full_res_compo_fish, "As")),
  tar_target(boxplot_fish_fam_Ca, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Ca")),
  tar_target(boxplot_fish_fam_Co, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Co")),
  tar_target(boxplot_fish_fam_Cu, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Cu")),
  tar_target(boxplot_fish_fam_Fe, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Fe")),
  tar_target(boxplot_fish_fam_K, 
             boxplot_compo_fish_fam(full_res_compo_fish, "K")),
  tar_target(boxplot_fish_fam_Mg, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Mg")),
  tar_target(boxplot_fish_fam_Mn, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Mn")),
  tar_target(boxplot_fish_fam_Na, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Na")),
  tar_target(boxplot_fish_fam_Ni, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Ni")),
  tar_target(boxplot_fish_fam_P, 
             boxplot_compo_fish_fam(full_res_compo_fish, "P")),
  tar_target(boxplot_fish_fam_Se, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Se")),
  tar_target(boxplot_fish_fam_Zn, 
             boxplot_compo_fish_fam(full_res_compo_fish, "Zn")),
  
  # per family for all nutrients
  tar_target(boxplot_fish_fam_all_nut, 
             boxplot_compo_fish_fam_all_nut(full_res_compo_fish)),
  
  # # boxplots per campaign and per nutrient
  # fish samples were collected during two different fishing campaign
  # however, care must be taken when comparing differences between two campaigns
  # because species collected were different on the two : fish collected during
  # the first one (Isotopes 2005) are mainly Myctophids while there is much more
  # variability in the ecology of fish collected during the second campaign 
  # Poker II - and there are differences between families - see above
  tar_target(boxplot_fish_camp_As, 
             boxplot_compo_fish_camp(full_res_compo_fish, "As")),
  tar_target(boxplot_fish_camp_Ca, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Ca")),
  tar_target(boxplot_fish_camp_Co, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Co")),
  tar_target(boxplot_fish_camp_Cu, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Cu")),
  tar_target(boxplot_fish_camp_Fe, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Fe")),
  tar_target(boxplot_fish_camp_K, 
             boxplot_compo_fish_camp(full_res_compo_fish, "K")),
  tar_target(boxplot_fish_camp_Mg, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Mg")),
  tar_target(boxplot_fish_camp_Mn, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Mn")),
  tar_target(boxplot_fish_camp_Na, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Na")),
  tar_target(boxplot_fish_camp_Ni, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Ni")),
  tar_target(boxplot_fish_camp_P, 
             boxplot_compo_fish_camp(full_res_compo_fish, "P")),
  tar_target(boxplot_fish_camp_Se, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Se")),
  tar_target(boxplot_fish_camp_Zn, 
             boxplot_compo_fish_camp(full_res_compo_fish, "Zn")),
  
  # correlation matrix plot
  tar_target(corrplot_fish, corr_compo_fish(full_res_compo_fish)),
  
  
  ######## OF SCATS ######
  tar_target(table_summary_compo_scats_output, 
             table_compo_scats(full_res_compo_scats, 
                               "output")),
  tar_target(table_summary_compo_scats_file, 
             table_compo_scats(full_res_compo_scats, 
                               "file")),
  # composition : total without outliers
  tar_target(boxplot_scats_tot, boxplot_compo_scats_tot(full_res_compo_scats)), 
  tar_target(densplot_scats_tot, densplot_compo_scats_tot(full_res_compo_scats)), 
  # per site
  tar_target(boxplot_scats_sites, boxplot_compo_scats_site(full_res_compo_scats)),
  
  # correlation matrix plot
  tar_target(corrplot_scats, corr_compo_scats(full_res_compo_scats)),
  
  ######## OF DIET OF A.GAZELLA #######
  tar_target(compo_prey_ww, dw_to_ww_prey(full_res_compo_fish, 
                                          data_fish_samples)),
  tar_target(conc_in_diet, compute_concentrations_in_diet(data_diet,
                                                          compo_prey_ww)), 
  tar_target(boxplot_compo_diets, boxplot_compo_diets_sources(conc_in_diet)),
  
  ##################### COMPAIR COMPOSITIONS ###################################
  # bind the data
  tar_target(fish_and_scats, pool_fish_scat(full_res_compo_fish, 
                                            full_res_compo_scats)),
  # with just means per species for fish
  tar_target(fish_sp_means_and_scats, pool_fish_sp_means_scat(full_res_compo_fish, 
                                            full_res_compo_scats)),
  
  # boxplot comparison between composition of fish and scats 
  tar_target(boxplot_fish_and_scats_full, 
             boxplot_compair_compo_full(fish_and_scats)),
  # in relative, standardized per sample
  tar_target(boxplot_fish_and_scats_relative, 
             boxplot_compair_compo_fish_scat_relative(fish_and_scats)),
  # boxplot comparison between composition of fish and scats with only fish 
  # identified as prey species
  tar_target(boxplot_fish_and_scats_prey, 
             boxplot_compair_compo_prey(fish_and_scats)),
  
  tar_target(boxplot_prey_scats_bowl_relative, 
             boxplot_compair_compo_fish_scat_bowl_relative(fish_and_scats, 
                                                           conc_in_diet)),
  
  ##################### REPLACE STATISTICAL OUTLIERS VALUES BEFORE #############
  ############################## STATISTICAL ANALYSIS ##########################
  
  tar_target(compo_fish_clean, replace_outliers_conc(full_res_compo_fish)),
  
  ##################### PCA ####################################################
  
  ########## fish
  # with all samples and robust CoDa PCA analysis 
  tar_target(PCA_fish_total_coda_rob, pca_coda(full_res_compo_fish, 
                                               "fish")),
  tar_target(biplot_PCA_fish_total_coda_rob, 
             biplot_pca_coda(PCA_fish_total_coda_rob, 
                             full_res_compo_fish, 
                             "fish_tot_coda_rob",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  tar_target(biplot_PCA_fish_total_coda_rob_ell, 
             biplot_pca_coda(PCA_fish_total_coda_rob, 
                             full_res_compo_fish, 
                             "fish_tot_coda_rob_ell",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  tar_target(biplot_PCA_fish_total_coda_rob_ell_PC13, 
             biplot_pca_coda(PCA_fish_total_coda_rob, 
                             full_res_compo_fish, 
                             "fish_tot_coda_rob_ell_PC13",
                             pcomp = c(1, 3), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  # with all samples but classical (non-robust) coda PCA analysis
  tar_target(PCA_fish_total_coda_norob, pca_coda_norob(full_res_compo_fish, 
                                                       "fish")),
  tar_target(biplot_PCA_fish_total_coda_norob, 
             biplot_pca_coda(PCA_fish_total_coda_norob, 
                             full_res_compo_fish, 
                             "fish_tot_coda_norob",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5, 
                             ellipse = FALSE)),
  tar_target(biplot_PCA_fish_total_coda_norob_ell, 
             biplot_pca_coda(PCA_fish_total_coda_norob, 
                             full_res_compo_fish, 
                             "fish_tot_coda_norob_ell",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5, 
                             ellipse = TRUE)),
  # with all samples but classical PCA analysis
  tar_target(PCA_fish_total_nocoda, pca_nocoda(full_res_compo_fish, 
                                               "fish")),
  tar_target(biplot_PCA_fish_total_nocoda, 
             biplot_pca_nocoda(PCA_fish_total_nocoda, 
                               full_res_compo_fish, 
                               groups = "Family", 
                               "fish_total")),
  # without what could be statistical outliers & robust CoDa PCA
  tar_target(PCA_fish_clean_coda_rob, pca_coda(compo_fish_clean, 
                                               "fish")),
  tar_target(biplot_PCA_fish_clean_coda_rob, 
             biplot_pca_coda(PCA_fish_clean_coda_rob, 
                             full_res_compo_fish,
                             "fish_clean_coda_rob",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  tar_target(biplot_PCA_fish_clean_coda_rob_ell, 
             biplot_pca_coda(PCA_fish_clean_coda_rob, 
                             full_res_compo_fish,
                             "fish_clean_coda_rob_ell",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  # without what could be statistical outliers but classical coda PCA analysis
  tar_target(PCA_fish_clean_coda_norob, pca_coda_norob(compo_fish_clean, 
                                                       "fish")),
  tar_target(biplot_PCA_fish_clean_coda_norob, 
             biplot_pca_coda(PCA_fish_clean_coda_norob, 
                             full_res_compo_fish,
                             "fish_clean_coda_norob", 
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  tar_target(biplot_PCA_fish_clean_coda_norob_ell, 
             biplot_pca_coda(PCA_fish_clean_coda_norob, 
                             full_res_compo_fish,
                             "fish_clean_coda_norob_ell", 
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  # without what could be statistical outliers but classical PCA
  tar_target(PCA_fish_clean_nocoda, pca_nocoda(compo_fish_clean, 
                                               "fish")),
  tar_target(biplot_PCA_fish_clean_nocoda, 
             biplot_pca_nocoda(PCA_fish_clean_nocoda, 
                               full_res_compo_fish, 
                               groups = "Family", 
                               "fish_clean")),
  
  ### PCA on fish, but with means per species! 
  # compute mean per species first
  tar_target(fish_mean_sp, compute_means_sp(full_res_compo_fish)),
  tar_target(PCA_fish_tot_means_sp_coda_rob, pca_coda(fish_mean_sp, 
                                                      "means_fish")),
  tar_target(biplot_PCA_fish_tot_means_sp_coda_rob, 
             biplot_pca_coda(PCA_fish_tot_means_sp_coda_rob, 
                             fish_mean_sp, 
                             "fish_tot_means_sp_coda_rob",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  tar_target(biplot_PCA_fish_tot_means_sp_coda_rob_ell, 
             biplot_pca_coda(PCA_fish_tot_means_sp_coda_rob, 
                             fish_mean_sp, 
                             "fish_tot_means_sp_coda_rob_ell",
                             pcomp = c(1:2), 
                             groups = "Family", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  
  
  
  ########## scats
  # with all samples and robust CoDa PCA analysis 
  tar_target(PCA_scats_total_coda_rob, pca_coda(full_res_compo_scats, 
                                                "scats")),
  tar_target(biplot_PCA_scats_total_coda_rob, 
             biplot_pca_coda(PCA_scats_total_coda_rob, 
                             full_res_compo_scats, 
                             "scats_tot_coda_rob",
                             pcomp = c(1:2), 
                             groups = "site", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  tar_target(biplot_PCA_scats_total_coda_rob_ell, 
             biplot_pca_coda(PCA_scats_total_coda_rob, 
                             full_res_compo_scats, 
                             "scats_tot_coda_rob_ell",
                             pcomp = c(1:2), 
                             groups = "site", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  # with all samples and non-robust CoDa PCA analysis 
  tar_target(PCA_scats_total_coda_norob, pca_coda_norob(full_res_compo_scats, 
                                                        "scats")),
  tar_target(biplot_PCA_scats_total_coda_norob, 
             biplot_pca_coda(PCA_scats_total_coda_norob, 
                             full_res_compo_scats, 
                             "scats_tot_coda_norob",
                             pcomp = c(1:2), 
                             groups = "site", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)), 
  # with all samples but classical PCA analysis
  tar_target(PCA_scats_total_nocoda, pca_nocoda(full_res_compo_scats, 
                                                "scats")),
  tar_target(biplot_PCA_scats_total_nocoda, 
             biplot_pca_nocoda(PCA_scats_total_nocoda, 
                               full_res_compo_scats, 
                               groups = "site", 
                               "scats_total")),
  
  
  ##### with both fish and scats samples together
  # with all samples and robust CoDa PCA analysis 
  tar_target(PCA_fish_scats_coda_rob, pca_coda(fish_and_scats, 
                                               "both")),
  tar_target(biplot_PCA_fish_scats_coda_rob, 
             biplot_pca_coda(PCA_fish_scats_coda_rob, 
                             fish_and_scats, 
                             "fish_scats_tot_coda_rob",
                             pcomp = c(1:2), 
                             groups = "type", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  tar_target(biplot_PCA_fish_scats_coda_rob_ell, 
             biplot_pca_coda(PCA_fish_scats_coda_rob, 
                             fish_and_scats, 
                             "fish_scats_tot_coda_rob_ell",
                             pcomp = c(1:2), 
                             groups = "type", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  # with all samples and non-robust CoDa PCA analysis 
  tar_target(PCA_fish_scats_coda_norob, pca_coda_norob(fish_and_scats, 
                                                       "both")),
  tar_target(biplot_PCA_fish_scats_coda_norob, 
             biplot_pca_coda(PCA_fish_scats_coda_norob, 
                             fish_and_scats, 
                             "fish_scats_tot_coda_norob",
                             pcomp = c(1:2), 
                             groups = "type", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  # with all samples but classical PCA analysis
  tar_target(PCA_fish_scats_total_nocoda, pca_nocoda(fish_and_scats, 
                                                     "both")),
  tar_target(biplot_PCA_fish_scats_total_nocoda, 
             biplot_pca_nocoda(PCA_fish_scats_total_nocoda, 
                               fish_and_scats, 
                               groups = "type", 
                               "fish_scats_total")), 
  
  # with both but only means per sp for fish 
  tar_target(PCA_fish_sp_means_scats_coda_rob, pca_coda(fish_sp_means_and_scats, 
                                                      "both_means")),
  tar_target(biplot_PCA_fish_sp_means_scats_coda_rob, 
             biplot_pca_coda(PCA_fish_sp_means_scats_coda_rob, 
                             fish_sp_means_and_scats, 
                             "fish_sp_means_scats_code_rob",
                             pcomp = c(1:2), 
                             groups = "type", 
                             var.add.scaling = 2.5,
                             ellipse = FALSE)),
  tar_target(biplot_PCA_fish_sp_means_scats_coda_rob_ell, 
             biplot_pca_coda(PCA_fish_sp_means_scats_coda_rob, 
                             fish_sp_means_and_scats, 
                             "fish_sp_means_scats_code_rob_ell",
                             pcomp = c(1:2), 
                             groups = "type", 
                             var.add.scaling = 2.5,
                             ellipse = TRUE)),
  
  ##################### CLUSTERING #############################################
  
  ########## fish
  tar_target(findk_ward_fish_output, clust_find_k_table(full_res_compo_fish, 
                                                        type = "fish", 
                                                        k = c(2:10), 
                                                        method = "ward.D2", 
                                                        object_type = "output")), 
  tar_target(findk_ward_fish_file, clust_find_k_table(full_res_compo_fish, 
                                                      type = "fish", 
                                                      k = c(2:10), 
                                                      method = "ward.D2", 
                                                      object_type = "file")), 
  tar_target(findk_ward_boxplot_fish, boxplot_clust_find_k_val(findk_ward_fish_output, 
                                                               "fish")),
  tar_target(findk_ward_means_fish, means_clust_find_k_val(findk_ward_fish_output, 
                                                           "fish")),
  # show outputs for a few k still
  tar_target(clust_fish_k3_ward, clust_compo_full_tib(full_res_compo_fish, 
                                                      "fish", 
                                                      k = 3, 
                                                      method = "ward.D2")), 
  tar_target(clust_fish_k3_complete, clust_compo_full_tib(full_res_compo_fish, 
                                                          "fish", 
                                                          k = 3, 
                                                          method = "complete")), 
  tar_target(boxplot_clust_fish_k3_ward, boxplot_compo_clust(clust_fish_k3_ward, 
                                                             full_res_compo_fish, 
                                                             "fish_k3_ward")), 
  tar_target(barplot_clust_fish_k3_ward, barplot_fam_clust(clust_fish_k3_ward, 
                                                           full_res_compo_fish, 
                                                           "fish_k3_ward")),
  tar_target(biplot_clust_k3_fish, 
             biplot_after_clust(PCA_fish_total_coda_rob, 
                                clust_fish_k3_ward,
                                full_res_compo_fish, 
                                "fish_tot_coda_rob_k3_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)),
  tar_target(clust_fish_k4_ward, clust_compo_full_tib(full_res_compo_fish, 
                                                      "fish", 
                                                      k = 4, 
                                                      method = "ward.D2")), 
  tar_target(boxplot_clust_fish_k4_ward, boxplot_compo_clust(clust_fish_k4_ward, 
                                                             full_res_compo_fish, 
                                                             "fish_k4_ward")), 
  tar_target(barplot_clust_fish_k4_ward, barplot_fam_clust(clust_fish_k4_ward, 
                                                           full_res_compo_fish, 
                                                           "fish_k4_ward")),
  tar_target(biplot_clust_k4_fish, 
             biplot_after_clust(PCA_fish_total_coda_rob, 
                                clust_fish_k4_ward,
                                full_res_compo_fish, 
                                "fish_tot_coda_rob_k4_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)),
  tar_target(clust_fish_k2_ward, clust_compo_full_tib(full_res_compo_fish, 
                                                      "fish", 
                                                      k = 2, 
                                                      method = "ward.D2")), 
  tar_target(boxplot_clust_fish_k2_ward, boxplot_compo_clust(clust_fish_k2_ward, 
                                                             full_res_compo_fish, 
                                                             "fish_k2_ward")), 
  tar_target(barplot_clust_fish_k2_ward, barplot_fam_clust(clust_fish_k2_ward, 
                                                           full_res_compo_fish, 
                                                           "fish_k2_ward")), 
  tar_target(biplot_clust_k2_fish, 
             biplot_after_clust(PCA_fish_total_coda_rob, 
                                clust_fish_k2_ward,
                                full_res_compo_fish, 
                                "fish_tot_coda_rob_k2_ward",
                                pcomp = c(1:2),  
                                var.add.scaling = 2.5)), 
  
  #### fish but using PCs instead
  tar_target(clust_fish_PCs_k3_ward, clust_compo_PCs(PCA_fish_total_coda_rob, 
                                                     type = "fish", 
                                                     k = 3, 
                                                     method = "ward.D2", 
                                                     "fish_tot_coda_rob_k3_ward")),
  tar_target(boxplot_clust_fish_PCs_k3_ward, boxplot_compo_clust(clust_fish_PCs_k3_ward, 
                                                                 full_res_compo_fish, 
                                                                 "fish_PCs_k3_ward")), 
  tar_target(barplot_clust_fish_PCs_k3_ward, barplot_fam_clust(clust_fish_PCs_k3_ward, 
                                                               full_res_compo_fish, 
                                                               "fish_PCs_k3_ward")),
  tar_target(biplot_clust_PCs_k3_fish, 
             biplot_after_clust(PCA_fish_total_coda_rob, 
                                clust_fish_PCs_k3_ward,
                                full_res_compo_fish, 
                                "fish_PCs_k3_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)),
  
  
  #### fish but using PCs on mean per species
  tar_target(clust_fish_PCs_tot_means_sp_dendro, clust_compo_PCs_dendro(PCA_fish_tot_means_sp_coda_rob, 
                                                                        fish_mean_sp,  
                                                                        type = "fish",  
                                                                        method = "ward.D2", 
                                                                        "fish_tot_means_sp_coda_rob_ward")),
  tar_target(clust_fish_PCs_tot_means_sp_k4_ward, clust_compo_PCs(PCA_fish_tot_means_sp_coda_rob, 
                                                                  type = "fish", 
                                                                  k = 4, 
                                                                  method = "ward.D2", 
                                                                  "fish_tot_means_sp_coda_rob_k4_ward")),
  tar_target(boxplot_clust_fish_PCs_tot_means_sp_k4_ward, boxplot_compo_clust(clust_fish_PCs_tot_means_sp_k4_ward, 
                                                                              fish_mean_sp, 
                                                                              "fish_PCs_tot_means_sp_k4_ward")), 
  tar_target(barplot_clust_fish_PCs_tot_means_sp_k4_ward, barplot_fam_clust(clust_fish_PCs_tot_means_sp_k4_ward, 
                                                                            fish_mean_sp, 
                                                                            "fish_PCs_tot_means_sp_k4_ward")),
  tar_target(biplot_clust_PCs_tot_means_sp_k4_fish, 
             biplot_after_clust(PCA_fish_tot_means_sp_coda_rob, 
                                clust_fish_PCs_tot_means_sp_k4_ward,
                                fish_mean_sp, 
                                "fish_PCs_tot_means_sp_k4_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)),
  
  ########## scats
  tar_target(findk_ward_scats_output, clust_find_k_table(full_res_compo_scats, 
                                                         type = "scats", 
                                                         k = c(2:10), 
                                                         method = "ward.D2", 
                                                         object_type = "output")), 
  tar_target(findk_ward_scats_file, clust_find_k_table(full_res_compo_scats, 
                                                       type = "scats", 
                                                       k = c(2:10), 
                                                       method = "ward.D2", 
                                                       object_type = "file")), 
  tar_target(findk_ward_boxplot_scats, boxplot_clust_find_k_val(findk_ward_scats_output, 
                                                                "scats")),
  tar_target(findk_ward_means_scats, means_clust_find_k_val(findk_ward_scats_output, 
                                                            "scats")),
  # try with a few k still 
  tar_target(clust_scats_k2_ward, clust_compo_full_tib(full_res_compo_scats, 
                                                       "scats", 
                                                       k = 2, 
                                                       method = "ward.D2")), 
  tar_target(boxplot_clust_scats_k2_ward, boxplot_compo_clust(clust_scats_k2_ward, 
                                                              full_res_compo_scats, 
                                                              "scats_k2_ward")), 
  tar_target(biplot_clust_k2_scats, 
             biplot_after_clust(PCA_scats_total_coda_rob, 
                                clust_scats_k2_ward,
                                full_res_compo_scats, 
                                "scat_tot_coda_rob_k2_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)), 
  tar_target(clust_scats_k3_ward, clust_compo_full_tib(full_res_compo_scats, 
                                                       "scats", 
                                                       k = 3, 
                                                       method = "ward.D2")), 
  tar_target(boxplot_clust_scats_k3_ward, boxplot_compo_clust(clust_scats_k3_ward, 
                                                              full_res_compo_scats, 
                                                              "scats_k3_ward")), 
  tar_target(biplot_clust_k3_scats, 
             biplot_after_clust(PCA_scats_total_coda_rob, 
                                clust_scats_k3_ward,
                                full_res_compo_scats, 
                                "scat_tot_coda_rob_k3_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)), 
  #### scats but using PCs instead
  tar_target(clust_scats_PCs_k2_ward, clust_compo_PCs(PCA_scats_total_coda_rob, 
                                                      type = "scats", 
                                                      k = 2, 
                                                      method = "ward.D2", 
                                                      "scats_tot_coda_rob_k2_ward")),
  tar_target(boxplot_clust_scats_PCs_k2_ward, boxplot_compo_clust(clust_scats_PCs_k2_ward, 
                                                                  full_res_compo_scats, 
                                                                  "scats_PCs_k2_ward")), 
  tar_target(biplot_clust_PCs_k2_scats, 
             biplot_after_clust(PCA_scats_total_coda_rob, 
                                clust_scats_PCs_k2_ward,
                                full_res_compo_scats, 
                                "scats_PCs_k2_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)),
  
  ########## fish and scats
  tar_target(findk_ward_fish_and_scats_output, clust_find_k_table(fish_and_scats, 
                                                                  type = "both", 
                                                                  k = c(2:10), 
                                                                  method = "ward.D2", 
                                                                  object_type = "output")), 
  tar_target(findk_ward_fish_and_scats_file, clust_find_k_table(fish_and_scats, 
                                                                type = "both", 
                                                                k = c(2:10), 
                                                                method = "ward.D2", 
                                                                object_type = "file")), 
  tar_target(findk_ward_boxplot_fish_and_scats, boxplot_clust_find_k_val(findk_ward_fish_and_scats_output, 
                                                                         "both")),
  tar_target(findk_ward_means_fish_and_scats, means_clust_find_k_val(findk_ward_fish_and_scats_output, 
                                                                     "both")),
  tar_target(clust_fish_and_scats_k3_ward, clust_compo_full_tib(fish_and_scats, 
                                                                "both", 
                                                                k = 3, 
                                                                method = "ward.D2")), 
  tar_target(boxplot_clust_fish_and_scats_k3_ward, boxplot_compo_clust(clust_fish_and_scats_k3_ward, 
                                                                       fish_and_scats, 
                                                                       "fish_and_scats_k3_ward")), 
  tar_target(biplot_clust_k3_fish_and_scats, 
             biplot_after_clust(PCA_fish_scats_coda_rob, 
                                clust_fish_and_scats_k3_ward,
                                fish_and_scats, 
                                "fish_and_scats_tot_coda_rob_k3_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5)), 
  #### fish and scats but using PCs instead
  tar_target(clust_fish_and_scats_PCs_k2_ward, clust_compo_PCs(PCA_fish_scats_coda_rob, 
                                                               type = "scats", 
                                                               k = 2, 
                                                               method = "ward.D2", 
                                                               "both_tot_coda_rob_k2_ward")),
  tar_target(boxplot_clust_fish_and_scats_PCs_k2_ward, boxplot_compo_clust(clust_fish_and_scats_PCs_k2_ward, 
                                                                           fish_and_scats, 
                                                                           "both_PCs_k2_ward")),
  tar_target(biplot_clust_PCs_k2_fish_and_scats, 
             biplot_after_clust(PCA_fish_scats_coda_rob, 
                                clust_fish_and_scats_PCs_k2_ward,
                                fish_and_scats, 
                                "both_PCs_k2_ward",
                                pcomp = c(1:2), 
                                var.add.scaling = 2.5))
)