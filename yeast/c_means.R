#####################################################################
## load required libraries ##########################################
#####################################################################

## install.packages("e1071")
## install.packages("ggplot2)
## install.packages("grid)
library(e1071)
library(ggplot2)
library(grid)

#####################################################################
## set working directory and load data ##############################
#####################################################################

## Data Reference: Eisen et al.: http://www.pnas.org/content/95/25/14863.full
## Accessed 5/12/15: http://genome-www.stanford.edu/clustering/
## data not committed to git, but can be accessed from link above as of 5/12/15

setwd("~/Documents/Website/yeast")
yeast <- read.csv("yeast.csv")

#####################################################################
## process data #####################################################
#####################################################################

## only include complete data, format numeric data
yeast_complete_i <- yeast[which(apply(yeast, 1, function(x) !any(is.na(x)))),]
yeast_complete <- yeast_complete_i
yeast_complete[,-c(1,2)] <- apply(yeast_complete_i[,-c(1,2)], 2, function(x) as.numeric(as.character(x)))

## scale data (center (relative to mean), scale (relative to sd))
yeast_complete_scaled <- yeast_complete 
yeast_complete_scaled[,-c(1,2)] <- t(scale(t(yeast_complete_scaled[,-c(1,2)])))

#####################################################################
## estimate number of clusters ######################################
#####################################################################

withinerror_k <- rep(NA, 24)

for(i in 2:25){
c_clust <- cmeans(yeast_complete_scaled[,-c(1,2)],
                  centers = i,
                  dist = "euclidean",
                  iter.max = 200,
                  verbose = TRUE,
                  method="cmeans",
                  m = 1.17)

withinerror_k[i-1] <- c_clust$withinerror
}

jpeg(filename = "number_of_clusters.jpeg", width = 652, height = 452, type = "quartz")
plot(2:25,
     withinerror_k,
     xlab = "Number of Clusters",
     ylab = "Sum of Weighted Distances (objective function)",
     type = "l",
     lwd = 2,
     lty = 2,
     main = "Cluster 'Compactness' (objective function) vs. Number of Clusters \n Fuzzy C-Means (FCM)")
dev.off()

#####################################################################
## estimate fuzziness factor (m) ####################################
#####################################################################

m <- c(1.05, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2:10)
withinerror_m <- rep(NA, length(m))

for(i in 1:length(m)){
  c_clust <- cmeans(yeast_complete_scaled[,-c(1,2)],
                    centers = 16,
                    iter.max = 200,
                    verbose = TRUE,
                    method="cmeans",
                    m = m[i])
  
  withinerror_m[i] <- c_clust$withinerror

}


jpeg(filename = "m.jpeg", width = 652, height = 452, type = "quartz")
plot(m,
     withinerror_m,
     xlab = "Fuzziness Parameter (m)",
     ylab = "Sum of Weighted Distances (objective function)",
     type = "l",
     lwd = 2,
     lty = 2,
     main = "Cluster 'Compactness' (objective function) vs. Fuzziness Parameter \n Fuzzy C-Means (FCM)")
dev.off()

#####################################################################
## run clustering with chosen parameters ############################
#####################################################################

c_clust <- cmeans(yeast_complete_scaled[,-c(1,2)],
                  centers = 16,
                  iter.max = 500,
                  verbose = TRUE,
                  method="cmeans",
                  m = 1.17)

#####################################################################
## visualize data: cluster centers ##################################
#####################################################################

pca <- prcomp(yeast_complete_scaled[,-c(1,2)], scale = FALSE)
centers_pca <- predict(pca, c_clust$centers)

jpeg(filename = "cluster_centers.jpeg", width = 1000, height = 800, type = "quartz")
ggplot(data.frame(centers_pca), aes(PC1, PC2)) + 
  geom_point(aes(color = as.factor(1:nrow(centers_pca))), size = 6) +
  ggtitle("Patterns of S. cerevisiae Gene Expression:\nFuzzy C-means Clustering of Yeast Microarray Data") +
  xlab ("Projection onto First Principal Component") +
  ylab("Projection onto Second Principal Component") +
  labs(colour = "Cluster ID") +
  theme(axis.title = element_text(size = 25),
        title = element_text(size = 30),
        legend.text = element_text(size = 20))
dev.off()

#####################################################################
## explore data: specific centers ###################################
#####################################################################

## look at gene names in specific clusterings (membership score >= 0.6)
as.character(yeast_complete[which(c_clust$membership[,4] >= 0.6),][,2])
as.character(yeast_complete[which(c_clust$membership[,7] >= 0.6),][,2])

#####################################################################
## visualize data: specific centers (4) #############################
#####################################################################

jpeg(filename = "cluster_4.jpeg", width = 1200, height = 800, type = "quartz")
ggplot () + geom_point(data = data.frame(centers_pca),
            aes(x = PC1, y = PC2, show_guide = FALSE), 
            size = c(rep(4, 3), 8, rep(4, 12)),
            color = c(rep("dark blue", 3), "red", rep("dark blue", 12))) +
  
            geom_point(data = data.frame(pca$x),
            aes(x = PC1, y = PC2, colour = c_clust$membership[,4])) +
            ggtitle("S. cerevisiae Gene Expression:\n Clusters of Genes based on Activation Patterns") +
            xlab ("Projection onto First Principal Component") +
            ylab("Projection onto Second Principal Component") +
            labs(colour = "Cluster '4' \n Cluster Membership Score\n") +
            scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.25, limits = c(0, 1)) +
            theme(axis.title = element_text(size = 25),
                  title = element_text(size = 30),
                  legend.text = element_text(size = 20),
                  legend.key.size = unit(1, "cm")) 
dev.off()

#####################################################################
## visualize data: specific centers (7) #############################
#####################################################################

jpeg(filename = "cluster_7.jpeg", width = 1200, height = 800, type = "quartz")
ggplot () + geom_point(data = data.frame(centers_pca),
                       aes(x = PC1, y = PC2, show_guide = FALSE), 
                       size = c(rep(4, 6), 8, rep(4, 9)) ,
                       color = c(rep("dark blue", 6), "red", rep("dark blue", 9))) +
  
  geom_point(data = data.frame(pca$x),
             aes(x = PC1, y = PC2, colour = c_clust$membership[,7])) +
  ggtitle("S. cerevisiae Gene Expression:\n Clusters of Genes based on Activation Patterns") +
  xlab ("Projection onto First Principal Component") +
  ylab("Projection onto Second Principal Component") +
  labs(colour = "Cluster '7' \n Cluster Membership Score") +
  scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.25) +
  theme(axis.title = element_text(size = 25),
        title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, "cm")) 
dev.off()
save.image(file = "final_results.RData")
