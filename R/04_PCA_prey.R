################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2023
# 04_PCA_prey.R
#
# Script with functions to compute PCA on prey data 
################################################################################



#'
#'
#'
#'
#'
# function to add data to the result table with composition of prey
PCA_prey_Rob <- function(res_prey_tib) {
  
  data.act.prey <- as.data.frame(res_prey_tib)
  
  rownames(data.act.prey) <- data.act.prey$Code_sample
  data.act.prey <- data.act.prey[, 4:16] 
  
  
  ## robust estimation (default):
  res.rob.prey <- robCompositions::pcaCoDa(data.act.prey)
  res.rob.prey
}