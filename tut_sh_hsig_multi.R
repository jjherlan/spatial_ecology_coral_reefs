require(tidyverse)
require(vegan)
require(knitr)
require(fpc)
require(cluster)
require(purrr)
require(factoextra)
require(pander)

am_sam_tut_hsig.mat <- read_csv("am_sam_tut_hsig.csv") %>%
  column_to_rownames(var = "site") %>%
  as.matrix()

tut_sh.table <- as.table(am_sam_tut_hsig.mat)
#rpn_2016.tbl <- as_tibble(rpn_2016.table)

# Compute distance matrix with different distance measures
# Using distance measures including Euclidean, Manhattan, Bray-Curtis and Jaccard to calculate distance matrix.

dist_meth = c("euclidean", "manhattan", "bray", "jaccard")
dist_m = map(as.list(dist_meth), vegdist, x = tut_sh.table)

# Determine the optimal number of clusters to use
# Define function find_k for plotting scree plot to show how within groups sum of square (WSS) and average Silhouette width (Si) change with different number of clusters used.

find_k <- function(data, plot_title = ""){
  #create data for within group sum of square
  wss = numeric(15)
  for (k in 1:15) wss[k] <- kmeans(data, k, nstart = 20)$tot.withinss
  
  #create data for average silhouette distance
  asw <- numeric(15)
  for (k in 2:15) asw[k] <- pam(data, k)$silinfo$avg.width
  
  #create s cree plot
  par(mar=c(5, 4, 4, 6))
  plot(1:15, wss, type = "b", main = plot_title, xlab = "Number of Clusters", ylab = "Within groups sum of squares")
  par(new = T)
  plot(1:15, asw, type = "l", lty = 2, col = "red", axes = F, xlab = NA, ylab = NA)
  axis(side = 4)
  mtext("Average Silhouette width", side = 4, line = 3)
  legend("topright", legend = c("WSS", "Si"), lty = c(1,2), col = c("black", "red"))
}

# Scree plots
# Define plot titles.

dist_meas = c("Euclidean", "Manhattan", "Bray-Curtis", "Jaccard")

# Create scree plots to determine the number of clusters to use (k). Click on the tabs to show results for different distance methods.

for(i in 1:4){
  cat("\n")
  cat("#### ", dist_meas[i], "\n")
  find_k(dist_m[[i]], plot_title = dist_meas[i])
  cat("\n")
}

# Perform k-means clustering to analyze the clusters of stations
#conduct k-means to find clusters
km = map(dist_m, pam, k = 2)

# extract cluster data
km_result = transpose(km)
km_cluster = as.data.frame(km_result$cluster)
colnames(km_cluster) <- dist_meas

# Silhoulette plots: results for k-means clustering
# Plot the Silhouette plots for k-means clustering. Click on the tabs to see results for different distance methods.

for(i in 1:4){
  cat("\n")
  cat("#### ", dist_meas[i], "\n")
  plot(silhouette(km[[i]], dist_m[[i]]), main = paste("Silhoulette plot using ", dist_meas[i], " distance") , col = 1:2, border = NA, cex = 0.6)
  cat("\n")
}


# Clusters stability: k-means
# Using the bootstrap method, assess the stability of clusters when using k-means. Click on different tabs to see results for different distance methods.

for(i in 1:4){
  cat("\n")
  cat("#### ", dist_meas[i], "\n")
  stab = clusterboot(dist_m[[i]], B = 1000, bootmethod = "boot", clustermethod = claraCBI, k = 2, count = F)
  stab_results = cbind(stab$bootmean, stab$bootbrd, stab$bootrecover)
  print(kable(stab_results, col.names = c("Clusterwise Jaccard bootstrap mean", "dissolved", "recovered"), caption = "Cluster stability assessment for k-means"))
  cat("\n")
}

# Perform hierarchical clustering to analyze the clusters of stations
# perform hierarchical clustering on different distance measures and linkage methods
hc_agg = list()
m1 = list(method = list("single", "complete", "average", "ward"))
m2 = list(method = list("single", "complete", "average", "ward.D"))
for(i in 1:4){
  hc_agg[[i]] = pmap(m1, agnes, x = dist_m[[i]])
}

# Dendrogram: results for hierarchical clustering
# Define plot titles

link_meth = c("Single", "Complete", "Average",  "Ward's")

# Plot the dendrograms for hierarchical clustering. 
# Click on different tabs to see results for different distance methods with different linkage methods.

for(i in 1:4){
  cat("#### ", dist_meas[i], "{.tabset results='asis'}", "\n")
  for(j in 1:4){
    cat("\n")
    cat("##### ", link_meth[j], "\n")
    print(fviz_dend(hc_agg[[i]][[j]], cex = 0.5, k = 2, lwd = 0.8, main = paste(link_meth[j], "linkage Dendrogram"), horiz = T, ylab = paste("Height", "\n", "\n", "Agglomerative coefficient=", round(hc_agg[[i]][[j]]$ac, 2), "\n", "Cophenetic correlation=", round(cor(dist_m[[i]], cophenetic(hc_agg[[i]][[j]])), 2))))
    cat("\n")
  }
  cat("\n")
}

# Clusters stability: hierarchical clustering
# Using the bootstrap method, assess the stability of clusters when using hierarchical clustering. 
# Click on different tabs to see results for different distance methods with different linkage methods.

for(i in 1:4){
  cat("#### ", dist_meas[i], "{.tabset results='asis'}", "\n")
  for(j in 1:4){
    cat("\n")
    cat("##### ", link_meth[j], "\n")
    stab = clusterboot(dist_m[[i]], B = 1000, bootmethod = "boot", clustermethod = hclustCBI, k = 2, method = m2[[1]][[j]], count = F)
    stab_results = cbind(stab$bootmean, stab$bootbrd, stab$bootrecover)
    print(kable(stab_results, col.names = c("Clusterwise Jaccard bootstrap mean", "dissolved", "recovered"), caption = "Cluster stability assessment for hierarchical clustering"))
    cat("\n")
  }
  cat("\n")
}

# Cluster analysis result

# Report stations in different group
# extract indice for cluster from result of heirarchical clustering using Bray distance and Single 
# linkage method

## The following does not work!!

clust = cutree(hc_agg[[4]][[4]], 2)

#subset station data according to the clusters
g = list()
clust_name = c()
for(i in 1:2){
  g[[i]] = subset(tut_sh.table, clust == i)
  clust_name[i] = paste(rownames(g[[i]]), collapse = " ")
}

#display the clusters
clust_name = data.frame(clust_name)
colnames(clust_name) <- "Stations"
rownames(clust_name) <- c("Group 1", "Group 2")
pander(clust_name, split.cell = 35)

# Compare copepod composition in the 3 clusters
# compute average composition percentage for every copepod for each group

g_mean = map(g, apply, MARGIN = 2, FUN = mean)

kable(data.frame(g_mean), col.names = c("Group 1", "Group 2"))

#dom_species_name <- rownames(data.frame(g_mean))
#dom_g1 = dom_species_name[g_mean[[1]] > g_mean[[2]] + g_mean[[3]]]
#dom_g2 = dom_species_name[g_mean[[2]] > g_mean[[1]] + g_mean[[3]]]
#dom_g3 = dom_species_name[g_mean[[3]] > g_mean[[1]] + g_mean[[2]]]

# K-means = https://uc-r.github.io/kmeans_clustering#kmeans
# Hierarchical = https://uc-r.github.io/hc_clustering

# More *k*-means 

# Clustering Analysis, https://sites.google.com/view/biostats/lessons/multivariate-methods?authuser=0&pli=1
# Use matrix

fviz_nbclust(tut_sh.table, kmeans, method = "wss")

# Silhouette
fviz_nbclust(tut_sh.table, kmeans, method = "silhouette")

# gap stat

gap_stat <- clusGap(rpn_2016.mat1, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

fviz_gap_stat(gap_stat)

# fit final model

final <- kmeans(rpn_2016.mat1, 2, nstart = 25)
print(final)

fviz_cluster(final, data = rpn_2016.mat1)



# Husson (2011): Exploratory Multivariate Analysis by Example Using R, Clustering

rpn_2016.multi3 <-
  rpn_2016.multi1 %>%
  mutate_at(vars(site, location, depth), factor) %>%
  select(-c(location, depth, hard_coral_other, non_reef)) %>%
  relocate(site, .before = abiotic) 

#%>%
#  column_to_rownames(var = "site")

# rpn_2016.table <- read.csv("rpn_2016.multi2.csv", stringsAsFactors = F)

rpn.pca <- PCA(rpn_2016.multi3, scale.unit = TRUE, ncp = Inf, graph = FALSE, quali.sup = 1)

rpn.hcpc <- HCPC(rpn.pca)

rpn.hcpc <- HCPC(rpn.pca, t.levels = "all")
