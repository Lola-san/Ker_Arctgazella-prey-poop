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
# using the full dataset (as in comparison to using only Principal 
# components estimated by PCA)
clust_compo_full_tib <- function(compo_tib, 
                                 type, # either fish or scats or both
                                 k,
                                 scale = "robust", # other option is "classical"
                                 method) {
  
  if (type == "fish") {
    data.act <- as.data.frame(compo_tib[, 4:16])
  } else if (type %in% c("scats", "both")) {
    data.act <- as.data.frame(compo_tib[, 2:14])
  }
  
  robCompositions::clustCoDa(data.act,
                             k = k, 
                             scale = scale, 
                             method = method)
}

#'
#'
#'
#'
# function to perform clustering with different cluster nb and plot 
# different validating values of the outputs
clust_find_k_table <- function(compo_tib, 
                                 type, # either fish or scats or both
                                 k_range = c(2:10),
                                 scale = "robust", # other option is "classical"
                                 method, 
                                 object_type # either "output" or "file" 
                                 ) {
  
  if (type == "fish") {
    data.act <- as.data.frame(compo_tib[, 4:16])
  } else if (type %in% c("scats", "both")) {
    data.act <- as.data.frame(compo_tib[, 2:14])
  }
  
  list_outputs <- list()
  
  for (i in k_range) {
    ki <- robCompositions::clustCoDa(data.act,
                                     k = i, 
                                     scale = scale, 
                                     method = method)
    ki_df <- data.frame(k = i, 
                        method = method, 
                        size = as.data.frame(ki$size)$Freq,
                        separation = round(ki$separation, 3),
                        average.distance = round(ki$average.distance, 3), 
                        median.distance = round(ki$median.distance, 3),
                        avg.silwidth = round(as.data.frame(ki$silwidth)[,1], 3), 
                        average.toother = round(ki$average.toother, 3)) |>
      dplyr::group_by(k, method) |>
      dplyr::mutate(min.size = min(size))
    
    list_outputs <- append(list_outputs, list(ki_df))
    
  }
  
  df0 <- data.frame(k = NA, 
                    method = NA,
                    size = NA,
                    separation = NA,
                    average.distance = NA, 
                    median.distance = NA,
                    avg.silwidth = NA, 
                    average.toother = NA, 
                    min.size = NA)
  
  for (i in 1:length(k_range)) {
    df0 <- rbind(df0, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df.to.plot <- df0[-1,]
  
  if (object_type == "file") {
    openxlsx::write.xlsx(df.to.plot, 
                         file = paste0("output/Clustering/find_k_validity_measures_", 
                         type, 
                         ".xlsx"))
  } else {
    df.to.plot
  }
  
}


#'
#'
#'
#'
# function to perform clustering with different cluster nb and plot 
# different validating values of the outputs
boxplot_clust_find_k <- function(find_k_output, 
                                 type # either fish or scats or both
                                 ) {
  
  
  # set color palette 
  if (length(unique(find_k_output$k)) >= 7) {
    diff <- length(unique(find_k_output$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
  }
  
  find_k_output |>
    dplyr::mutate(k = as.factor(k)) |>
    tidyr::pivot_longer(cols = c("separation":"min.size"), 
                        names_to = "validity.variable", 
                        values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = k, y = value, group = k, fill = k)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~validity.variable, scale = "free") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 15), 
                   legend.position = "none")
  # save plot 
  ggplot2::ggsave(paste0("output/Clustering/validity_measures_boxplot_",
                         type,
                         ".jpg"),
                  scale = 1,
                  height = 6, width = 8)
  
  
}

#'
#'
#'
#'
# function to perform clustering with different cluster nb and plot 
# different validating values of the outputs
means_clust_find_k <- function(find_k_output, 
                               type) {
  
  # set color palette 
  if (length(unique(find_k_output$k)) >= 7) {
    diff <- length(unique(find_k_output$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
  }
  
  find_k_output |>
    dplyr::mutate(k = as.factor(k)) |>
    tidyr::pivot_longer(cols = c("separation":"min.size"), 
                        names_to = "validity.variable", 
                        values_to = "value") |>
    dplyr::group_by(k, validity.variable) |>
    dplyr::summarize(mean = mean(value)) |>
    ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~validity.variable, scale = "free") +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 15), 
                   legend.position = "none")
  # save plot 
  ggplot2::ggsave(paste0("output/Clustering/validity_measures_means_",
                         type,
                         ".jpg"),
                  scale = 1,
                  height = 6, width = 8)
  
}


#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
boxplot_compo_clust <- function(clust_output,
                                compo_tib,
                                file_name
) {
  
  # assign each sample to its cluster
  clust_vec <- clust_output$cluster
  
  
  compo_tib |> 
    dplyr::mutate(cluster = as.factor(clust_vec)) |>
    tidyr::pivot_longer(cols = c("As":"Zn"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    ggplot2::ggplot(ggplot2::aes(x = cluster, y = concentration_mg_g_dw, 
                                 fill = cluster)) +
    ggplot2::geom_violin(width=1.4) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab("Nutrient concentration (in mg/g dry weight)") +
    ggplot2::geom_jitter(color="darkgrey", size=0.7, alpha=0.2) +
    ghibli::scale_fill_ghibli_d("YesterdayMedium", direction = -1) +
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
  # save plot 
  ggplot2::ggsave(paste0("output/Clustering/clust_boxplot_",
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = 8, width = 10)
}


#'
#'
#'
#'
# function to show families of samples from the different clusters
barplot_fam_clust <- function(clust_output,
                              compo_tib,
                              file_name
) {
  
  # assign each sample to its cluster
  clust_vec <- clust_output$cluster
  
  compo_tib |> 
    dplyr::mutate(cluster = as.factor(clust_vec)) |>
    ggplot2::ggplot(ggplot2::aes(x = Family, 
                                 fill = cluster)) +
    ggplot2::geom_bar() +
    ghibli::scale_fill_ghibli_d("YesterdayMedium", direction = -1) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 15, angle = 20, 
                                                       hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "bottom")
  # save plot 
  ggplot2::ggsave(paste0("output/Clustering/clust_barplot_",
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = 8, width = 10)
}


biplot_after_clust <- function(res_pca, 
                               res_clust,
                               compo_tib,
                               file_name, # should be explicit regarding dataset (fish
                               # or scats), method (coda or nocoda), "cleanness" of 
                               # dataset (with ou without stat outliers), ellipse
                               # or not and PC chosen (if other than 1,2)
                               pcomp = c(1:2), # choices of the PC to plot (2 in total), 
                               # 1 and 2 by default
                               circle.prob = 0.69, # not sure yet why this value by default
                               var.add.scaling = 2, # constant to multiply coordinates
                               # of variables by so that they show on a similar scale as 
                               # that of observations # 2 seems to fit ok but could be changed 
                               ellipse.prob = 0.68 # size of the ellipse in Normal probability
                               # not sure yet why this value by default
) {
  
  # function constructed based on ggbiplot function on github 
  # https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
  # and was adapted to work for an pca CoDa object
  
  ###### biplot settings
  # common practice as explained in ?biplot() : 
  # variables are scaled by lambda^scale and observations are scaled by
  # lambda^(1-scale) where lambda are singular values as computed by PCA
  # i.e d below
  scale <- 0
  obs.scale <- 1 - scale
  var.scale <- scale
  
  ##### recover the single value decomposition SVD
  nobs.factor <- sqrt(nrow(res_pca$scores) - 1) # not sure what this is 
  # and what is it for
  
  # standard deviation of the PCs #lambda in ?biplot()
  d <- sqrt(res_pca$eigenvalues)
  
  u <- sweep(res_pca$scores, 2, 1 / (d * nobs.factor), FUN = '*')
  v <- res_pca$loadings
  
  
  #####
  # compute scores 
  # ie coordinates of individuals (observations) on each principal component (PC)
  # pcomp <- pmin(pcomp, ncol(u)) # not sure what is the purpose of this
  df.u <- as.data.frame(sweep(u[,pcomp], 2, d[pcomp]^obs.scale, FUN='*'))
  # scale observations by lambda^(1-scale)
  
  # compute directions 
  # ie coordinates of the variables ie loadings * sdev of PCs
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, pcomp])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  df.u <- df.u * nobs.factor # so we are back to the original scores - res_pca$scores
  # ie the coordinates of the individuals on the PCs
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores (as done with ggbiplot)
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4) 
  
  # scale directions
  # v^2 = cos2 = quality of representation of variables on each PC 
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  # multiply then by another constant to get arrows on the same scale as observations 
  # coordinates
  # as mentioned in 
  # https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
  # "it might be necessary to scale arrows by some arbitrary constant factor so 
  # that both arrows and data points appear roughly on the same scale "
  df.v <- var.add.scaling * df.v
  
  # scale scores 
  # as done by 
  # https://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
  # with r <- 1 
  # r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
  # df.u=.99*df.u/r.scale
  # this version was set aside as we are more interested in comparing individuals 
  # and not structuring variables, so we went for an additional scaling 
  # of variables coordinates instead - see above
  
  # Append the proportion of explained variance to the axis labels
  if (res_pca$method == "robust") {
    u.axis.labs <- paste('PC', pcomp,' (clr - robust)', sep='')  
  } else if (res_pca$method == "classical") {
    u.axis.labs <- paste('PC', pcomp, ' (clr - classical)', sep='') 
    
  }
  # add explained variance
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * res_pca$eigenvalues[pcomp]/sum(res_pca$eigenvalues)))
  
  # Score Labels (labels of the observations)
  df.u$labels <- compo_tib$Code_sample
  
  # define groups
  # grouping per cluster
  df.u$groups <- as.factor(res_clust$cluster)
  
  
  # Variable Names
  df.v$varname <- rownames(v)
  
  # Variables for text label placement
  varname.adjust <- 1.2 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
  var.axes <- TRUE # draw arrow for the variable
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # draw circle 
  # theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  # circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
  
  ############## draw biplot
  g <- 
    
    # Base plot
    ggplot2::ggplot(data = df.u, ggplot2::aes(x = xvar, y = yvar)) + 
    ggplot2::xlab(u.axis.labs[1]) + 
    ggplot2::ylab(u.axis.labs[2]) + 
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    # # draw circle 
    # ggplot2::geom_path(data = circle, color = 'black', 
    #           size = 1/2, alpha = 1/3) +
    # Draw directions
    ggplot2::geom_segment(data = df.v,
                          ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                          arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 
                                                                        'picas')), 
                          color = 'darkred') +
    # Draw either labels or points
    ggplot2::geom_point(ggplot2::aes(color = groups), 
                        size = 1.5,
                        alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
    ) + 
    # Label the variable axes
    ggplot2::geom_text(data = df.v, 
                       ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                    angle = angle, hjust = hjust), 
                       color = 'darkred', size = 5) +
    viridis::scale_color_viridis(option = "cividis", 
                                 discrete = TRUE, 
                                 name = "Cluster") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   legend.text = ggplot2::element_text(size = 15))
  
  # Overlay a concentration ellipse of clusters
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  
  ell <- plyr::ddply(df.u, 'groups', function(x) {
    if(nrow(x) <= 2) {
      return(NULL)
    }
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(ellipse.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
               groups = x$groups[1])
  })
  names(ell)[1:2] <- c('xvar', 'yvar')
  
  g <- g + ggplot2::geom_path(data = ell, ggplot2::aes(color = groups, group = groups))
  
  g
  
  # save plot 
  ggplot2::ggsave(paste0("output/Clustering/clust_biplot_",
                         file_name,
                         ".jpg"),
                  scale = 1,
                  height = 8, width = 10)
}
