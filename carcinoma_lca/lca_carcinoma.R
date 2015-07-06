#####################################################################
## load required libraries ##########################################
#####################################################################

#install.packages("poLCA")
library(poLCA)

#####################################################################
## set working directory and load data ##############################
#####################################################################

## Methodology Reference: Drew Linzer and Jeffrey Lewis:
## poLCA: An R Package for Polytomous Variable Latent Class Analysis
## (Journal of Statistical Software)
## Accessed online: http://www.sscnet.ucla.edu/polisci/faculty/lewis/pdf/poLCA-JSS-final.pdf
## example based on suggested application by Linzer and Lewis (see reference above)

## Data Reference:
## Agresti, Alan. 2002. Categorical Data Analysis, second edition. 
## Hoboken: John Wiley & Sons.
## See also: http://www.ncbi.nlm.nih.gov/pubmed/843571
setwd("~/Documents/Website/carcinoma_lca")
data(carcinoma)

#####################################################################
## function to identify ideal number of clusters ####################
#####################################################################

lca_cluster_count <- function(data, formula, cluster_range = 2:10,
                              maxiter = 5000){
  
  fit_results <- cbind.data.frame(nclust = cluster_range,
                                  AIC = rep(NA, length(cluster_range)),
                                  BIC = rep(NA, length(cluster_range)))
  
  for(i in 1:length(cluster_range)){
  
    ## run LCA
    lca_results <- poLCA(formula, data, nclass = cluster_range[i], maxiter = maxiter, verbose = FALSE)

    ## save fit results
    fit_results[i, ]$AIC <- lca_results$aic
    fit_results[i, ]$BIC <- lca_results$bic}
  
  ## generate plot
  plot(x = fit_results$nclust, y = fit_results$AIC, type = "l", col = "dark green",
       ylim = c(min(c(fit_results$AIC, fit_results$BIC)), 
                max(c(fit_results$AIC, fit_results$BIC))),
       xlab = "Number of Classes",
       ylab = "AIC/BIC",
       main = "Fit Results by Number of Classes",
       lwd = 2)
  
  lines(x = fit_results$nclust, y = fit_results$BIC, type = "l", col = "blue", lwd = 2)
  legend("topleft",
         c("AIC", "BIC"),
         lty = 1, lwd = 1.5,
         col = c("dark green", "blue"),
         bg = "gray90")
  
  return(fit_results)
  print(fit_results)

}

#####################################################################
## identify ideal number of clusters ################################
#####################################################################

cluster_count <- lca_cluster_count(data = carcinoma,
                  formula = cbind(A,B,C,D,E,F,G)~1,
                  cluster_range = 2:10)

#####################################################################
## identify ideal number of clusters ################################
#####################################################################

set.seed(1202)
lca_carcinoma <- poLCA(data = carcinoma,
                       formula = cbind(A,B,C,D,E,F,G)~1,
                       maxiter = 5000,
                       nclass = 3)

#jpeg(filename = "rater_scores.jpeg", width = 850, height = 400, type = "quartz")
par(mar=c(5.1, 4.1, 4.1, 16), xpd=TRUE)
plot(sapply(names(lca_carcinoma$probs),function(x) unlist(lca_carcinoma$probs[[x]][ 1 , 1])),
     ylab = "Cancer 'Rating'",
     main = "Cancer 'Ratings' Across Pathologists \n (How certain are pathologists than a given tissue sample reflects the presence of carcinoma?)",
     xlab = "Pathologist ID",
     type = "l",
     ylim = c(0, 1),
     lty = 1,
     lwd = 2, 
     xaxt = 'n')
axis(1, at = 1:7, labels = names(lca_carcinoma$probs))
lines(sapply(names(lca_carcinoma$probs),function(x) unlist(lca_carcinoma$probs[[x]][ 2 , 1])), 
      lty = 2, lwd = 2)
lines(sapply(names(lca_carcinoma$probs),function(x) unlist(lca_carcinoma$probs[[x]][ 3 , 1])), 
      lty = 3, lwd = 2)
legend("topleft",
       lwd = 2, lty = rev(1:3),
       legend = c(paste("Class A: \n Likely Cancerous \n(n = ", as.numeric(table(lca_carcinoma$predclass)[1]), " samples)\n", sep = ''),
                  paste("Class B: \n Unclear Pathology \n(n = ", as.numeric(table(lca_carcinoma$predclass)[2]), " samples)\n", sep = ''),
                  paste("Class C: \n Unlikely to be Cancerous \n(n = ", as.numeric(table(lca_carcinoma$predclass)[3]), " samples)\n", sep = '')),
       inset=c(1.05,0))
#dev.off()
