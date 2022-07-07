################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2022
# 01_table_data_prey.R
#
# Script with manipulation functions to format data on preys and create
# a clean table suitable for publication
################################################################################


############################# load data ########################################

# load excel files  
load_xl <- function(pathxl) {
  readxl::read_xlsx(pathxl, 
                    col_names = TRUE, 
                    sheet = 1)
}

#  
clean_prey_tab <- function(prey_tab) {
  prey_tab |>
    dplyr::group_by(Family, Species, Campaign) |>
    dplyr::mutate(SL_cm = as.integer(SL_cm)) |> # generates warnings because
    # of samples with approximate length (*XX) as they were damaged
    dplyr::summarize(n = dplyr::n_distinct(Code_new_format), 
                     length_mean = mean(SL_cm, na.rm = TRUE), 
                     length_min = min(SL_cm, na.rm = TRUE), 
                     length_max = max(SL_cm, na.rm = TRUE), 
                     H20_mean = mean(Water_percent), 
                     H20_min = min(Water_percent), 
                     H20_max= max(Water_percent))
}