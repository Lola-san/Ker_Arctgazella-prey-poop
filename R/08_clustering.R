################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2023
# 08_clustering.R
#
# Script with functions to compute clustering on compositional data 
################################################################################

#'
#'
#'
#'
# function to perform clustering directly on compositional dataset
clust_compo <- function(compo_tib) {
  compo_tib <- targets::tar_read(full_res_compo_fish)
  
  rr <- robCompositions::clustCoDa(as.data.frame(compo_tib[4:16]),
                  k=6, scale = "robust", method = "complete")
}