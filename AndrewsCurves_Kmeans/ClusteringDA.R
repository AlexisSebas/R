#########################
# Cluster analysis and descriptive analysis
# Author: Sebastian Alexis Flores Sanchez
#########################

#########################
# Required libraries                                            
library(cluster)            # Version 2.1.8.2
library(ggplot2)            # Version 4.0.2
library(ggthemes)           # Version 5.2.0
library(factoextra)         # Version 2.0.0
library(TeachingDemos)      # Version 2.13
library(ggdendro)           # Version 0.2.0
library(dendextend)         # Version 1.19.1
library(here)               # Version 1.0.2
#########################

#########################
# Iris data
iris_scaled = scale(iris[,-5])
head(iris_scaled)

#Andrews Curves
andrewsCurves <- function(x, groups = NULL,
                          xlab = "", ylab = "", title = "",
                          legend.title = NULL) {
  
  x <- as.matrix(x)
  
  t <- seq(-pi, pi, length.out = 200)
  n <- nrow(x)
  p <- ncol(x)
  
  # Build Fourier basis
  f <- matrix(0, length(t), p)
  f[,1] <- 1/sqrt(2)
  
  for(i in 2:p){
    if(i %% 2 == 0){
      f[,i] <- sin((i/2)*t)
    } else {
      f[,i] <- cos(((i-1)/2)*t)
    }
  }
  
  # Vectorized transformation
  res <- x %*% t(f)
  
  # Long format
  df <- data.frame(
    t = rep(t, each = n),
    value = as.vector(res),
    id = rep(1:n, times = length(t))
  )
  
  if(!is.null(groups)){
    df$group <- rep(groups, times = length(t))
  }
  
  # Base plot
  if(!is.null(groups)){
    p <- ggplot(df, aes(t, value, group = id, colour = group)) +
      geom_line()
    
    # Apply legend title if provided
    if(!is.null(legend.title)){
      p <- p + labs(colour = legend.title)
    }
    
  } else {
    p <- ggplot(df, aes(t, value, group = id)) +
      geom_line() +
      theme(legend.position = "none")
  }
  
  p <- p +
    theme_minimal() +
    labs(x = xlab, y = ylab, title = title)
  
  return(p)
}

#Andrews Curves for Iris Data
AC <- andrewsCurves(iris_scaled, groups = iris$Species, xlab = "", ylab = "", legend.title = "Andrews Curves")
AC
#########################

#########################
#K-means clustering
set.seed(123)

km <- kmeans(
  iris_scaled,
  centers = 3,
  nstart = 25
)

km
 
#vizualizaiton
fviz_cluster(km, data = iris_scaled)

#silhoutte
library(cluster)

sil <- silhouette(km$cluster, dist(iris_scaled))
summary(sil)

library(factoextra)

fviz_silhouette(sil)
#########################