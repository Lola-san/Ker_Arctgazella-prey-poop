################################################################################
# Ker_Arctgazella-prey-poop project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# April 2023
# 06_PCA.R
#
# Script with functions to compute PCA on prey data 
################################################################################



#'
#'
#'
#'
#'
# function to perform Principal Component Analysis using robust method 
# for composition data with package robCompositions
pca_fish_coda <- function(res_fish_tib) {
  
  ## robust estimation (default):
  res.rob.fish <- robCompositions::pcaCoDa(as.data.frame(res_fish_tib[, 4:16]))
  res.rob.fish
}


#'
#'
#'
#'
#'
# function to perform Principal Component Analysis using classical method 
# with package robCompositions
biplot_pca <- function(res_pca, 
                       compo_tib,
                       pcomp = c(1:2), # choices of the PC to plot (2 in total), 
                       # 1 and 2 by default
                       groups, # either "species", "family"
                       # if on fish data 
                       # or "site" if on scat data
                       circle = FALSE, # weither to draw correlation circle or not 
                       circle.prob = 0.69, # not sure yet why this value by default
                       ellipse = FALSE, # logical weither to draw ellipse around groups
                       # of points or not 
                       ellipse.prob = 0.68 # size of the ellipse in Normal probability
                       # not sure yet why this value by default
) {
  
  # function constructed based on ggbiplot function on github 
  # https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
  # and was adapted to work for an pca CoDa object
  
  ###### biplot settings
  scale <- 1
  obs.scale <- 1 - scale
  var.scale <- scale
  
  ##### recover the single value decomposition SVD
  
  nobs.factor <- sqrt(nrow(res_pca$scores) - 1) # not sure what this is 
  # and what is it for
  
  # standard deviation of the PCs
  d <- sqrt(res_pca$eigenvalues) 
  
  u <- sweep(res_pca$scores, 2, 1 / (d * nobs.factor), FUN = '*')
  # u <- res_pca$scores
  v <- res_pca$loadings
  
  # compute scores 
  # ie coordinates of individuals (observations) on each principal component (PC)
  # 
  #choices <- pmin(choices, ncol(u)) # not sure what is the purpose of this
  df.u <- as.data.frame(sweep(u[,pcomp], 2, d[pcomp]^obs.scale, FUN='*'))
  # df.u <- as.data.frame(u[,pcomp])
  
  
  # Directions 
  # compute coordinates of the variables ie loadings * standard dev of PCs
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, pcomp])
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # scale directions
  # v^2 = cos2 = quality of representation of variables on each PC 
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  df.u <- df.u * nobs.factor
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste('Robust clr PC', pcomp, sep='') # as obs.scale == 0 
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * res_pca$eigenvalues[pcomp]/sum(res_pca$eigenvalues)))
  
  # Score Labels (labels of the observations)
  df.u$labels <- compo_tib$Code_sample
  
  if (groups == "species") {
    # grouping per species (fish composition)
    df.u$groups <- compo_tib$Species
  } else if (groups == "Family") {
    # grouping per Family (fish composition)
    df.u$groups <- compo_tib$Family
  } else if (groups == "site") {
    # grouping per site (scat composition)
    df.u$groups <- compo_tib$site
  }  
  
  # Variable Names
  df.v$varname <- rownames(v)
  
  # Variables for text label placement
  varname.adjust <- 1.5 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
  var.axes <- TRUE # draw arrow for the variable
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  
  ############## draw biplot
  g <- 
    # Base plot
    ggplot2::ggplot(data = df.u, ggplot2::aes(x = xvar, y = yvar)) + 
    ggplot2::xlab(u.axis.labs[1]) + 
    ggplot2::ylab(u.axis.labs[2]) + 
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    # Draw directions
    ggplot2::geom_segment(data = df.v,
                          ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                          arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 'picas')), 
                          color = 'darkred') +
    # Label the variable axes
    ggplot2::geom_text(data = df.v, 
                       ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                    angle = angle, hjust = hjust), 
                       color = 'darkred', size = 3) +
    # Draw either labels or points
    ggplot2::geom_point(ggplot2::aes(color = groups), 
                        alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
    ) 
  
  if (ellipse == TRUE) { # Overlay a concentration ellipse if there are groups
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
    
  } else {
    g
  }
  
  # draw circle 
  # theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  # circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
  # g <- g + ggplot2::geom_path(data = circle, color = scales::muted('white'), 
  #                             linewidth = 1/2, alpha = 1/3)

  
  
  
  # # compute coord, cos2 and contrib for variables and individuals 
  # 
  # 
  # #### variables 
  # # coordinates
  # # helper function 
  # var_coord_func <- function(loadings, comp.sdev){
  #   loadings*comp.sdev
  # }
  # 
  # loadings <- res_pca$loadings
  # sdev <- sqrt(res_pca$eigenvalues)
  # var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
  # head(var.coord[, 1:4])
  # 
  # # contrib
  # contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
  
  
}


#'
#'
#'
#'
#'
# function to perform Principal Component Analysis using classical method 
# with package robCompositions
pca_fish_nocoda <- function(res_fish_tib) {
  
  data.act.fish <- as.data.frame(res_fish_tib)
  
  rownames(data.act.fish) <- data.act.fish$Code_sample
  data.act.fish <- data.act.fish[, 4:16] 
  
  ## robust estimation (default):
  res.fish <- robCompositions::pcaCoDa(data.act.fish, method="classical")
  res.fish
}