################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2023
# 06_id_stat_outliers.R
#
# Script with functions to handle statistical outliers identified after 
# description of compositions and handle them before conducting statistical 
# analysis
################################################################################


# statistical outliers were identified as outliers per species and per element
# ie boxplot of elemental concentrations per species and element were 
# examined to detect any adnormal value - as compared to other samples
# of the same species but also to samples of other species

# files used for this identifications: "boxplot_sp_all_nut.jpg" and verification
# of those also identified as statistical outliers using function of 
# "02_id_technical_outliers.R"

# Were identified as adnormal values that could mess up with statistical 
# methods results: 
###### 2010PII_MACRCAR_CHA103_MC07 for Ni
###### 2005_GYMNFRA_GF11 for Zn
###### 2005_STOMSP_SS09 for Fe
###### 2005_NOTOCOA_NC03 for Fe
###### 2005_PARAGRA_PG06 for Fe 

# for statistical analysis, these values will be replaced by the highest 
# quantile of all samples for the nutrient

#'
#'
#'
#'
#'
# function to replace IDed adnormal values per highest quantiles 
replace_outliers_conc <- function(compo_tib) {
  
  # calculate quantiles 
  all_quant <- compo_tib |>
    # exclude unwanted samples 
    dplyr::filter(!(Code_sample %in% c("2010PII_MACRCAR_CHA103_MC07",
                                       "2005_GYMNFRA_GF11",
                                       "2005_STOMSP_SS09",
                                       "2005_NOTOCOA_NC03",
                                       "2005_PARAGRA_PG06"))) |>
    # get rid of unneeded nutrient
    dplyr::select(c(Code_sample, Fe, Zn, Ni)) |> 
    tidyr::pivot_longer(cols = c("Fe":"Ni"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(high_quant = quantile(concentration_mg_g_dw, 0.975)) |> 
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = high_quant)
  
  quant_zn <- all_quant$Zn
  quant_ni <- all_quant$Ni
  quant_fe <- all_quant$Fe
  
  compo_tib |>
    dplyr::mutate(Fe = dplyr::case_when(Code_sample %in% c("2005_STOMSP_SS09",
                                                           "2005_NOTOCOA_NC03",
                                                           "2005_PARAGRA_PG06") ~ quant_fe, 
                                        TRUE ~ Fe),
                  Ni = dplyr::case_when(Code_sample == "2010PII_MACRCAR_CHA103_MC07" ~ quant_ni,
                                        TRUE ~ Ni), 
                  Zn = dplyr::case_when(Code_sample == "2005_GYMNFRA_GF11" ~ quant_zn,
                                        TRUE ~ Zn))
}