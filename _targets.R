################################################################################
# Ker_Arctgazella-prey-poop project
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
  ##### 1 - on prey
  # define and load data on samples of prey
  tar_target(data_prey_file,
             "data/Data_prey_YC.xlsx", 
             format = "file"),
  tar_target(data_samples, load_xl(data_prey_file)),
  # summarize data on samples 
  tar_target(data_prey_summary, summary_prey_samples(data_samples)),
  
  ##### 2 - on diet of Arctocephalus gazella
  tar_target(data_diet_file, 
             "data/Diet_Agazella.xlsx", 
             format = "file"), 
  tar_target(data_diet, load_xl(data_diet_file)),
  
  ################### COMPOSITION OF PREY ###################
  # load data with composition of prey 
  tar_target(res_compo_prey_file,
             "data/Compo_preys.xlsx", 
             format = "file"),
  tar_target(res_compo_prey, load_xl(res_compo_prey_file)), 
  
  # identify outliers
  # statistical outliers
  tar_target(boxplot_outliers_prey_stats, boxplot_id_outliers(res_compo_prey, "prey")), # boxplot
  tar_target(tib_outliers_prey_stats, tib_id_outliers(res_compo_prey, data_samples, "prey")), # tibble
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
  tar_target(full_res_prey, complete_prey_results(res_compo_prey, data_diet)),
  tar_target(boxplot_prey_sp, boxplot_compo_prey_sp(full_res_prey)),
  tar_target(boxplot_prey_genus, boxplot_compo_prey_genus(full_res_prey)),
  tar_target(boxplot_prey_fam, boxplot_compo_prey_fam(full_res_prey)),
  tar_target(boxplot_prey_camp, boxplot_compo_prey_camp(full_res_prey)),
  

  ################### COMPOSITION OF POOP ###################
  # load data with composition of poop 
  tar_target(res_compo_poop_file,
             "data/Compo_poop.xlsx", 
             format = "file"),
  tar_target(res_compo_poop, load_xl(res_compo_poop_file)), 
  
  
  # statistical outliers
  tar_target(boxplot_outliers_poop_stats, boxplot_id_outliers(res_compo_poop, "poop")), # boxplot
  tar_target(tib_outliers_poop_stats, tib_id_outliers(res_compo_poop, data_samples, "poop")), # tibble
  # technical outliers 
  tar_target(hist_outliers_poop_tech, hist_id_outliers(res_compo_poop, "poop")) # histogram

)