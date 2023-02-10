################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2022
# 00_simulate_results.R
#
# Script with functions to simulate results waiting for the true results to 
# come
################################################################################

#'
#'
#'
# function to sample from the composition of prey dataset to create 
# either file of results for preys or for poops

sample_tab <- function(tab_to_sample_from) {
  
  tab_to_sample_from |>
    dplyr::slice_sample(prop = .5, 
                        replace = TRUE)
  
}

#'
#'
#'
# function to format sampled tables for poop

format_poop_results <- function(sampled_poop) {
  
  sampled_poop |>
    dplyr::select(-c(Species, Family, Code_sample)) |>
    dplyr::mutate(no_sample = c(1:nrow(sampled_poop)), 
                  site = dplyr::case_when(no_sample > nrow(sampled_poop)/2 ~ "Pointe Suzanne", 
                                          no_sample <= nrow(sampled_poop)/2 ~ "Cap Noir"))
  
}