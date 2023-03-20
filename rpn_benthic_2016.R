require(tidyverse)
require(clustsig)
require(vegan)
require(FactoMineR)
require(factoextra)

setwd('C:/esmoi')
getwd()

rpn_benthic <- read.csv('rpn_benthic_comp_2016.csv', header = T)
rpn_benthic

rpn_benthic_sites <-
  rpn_benthic %>%
  rename(site =  X) %>%
    select(site)

rpn_benthic.mat <-
  column_to_rownames(rpn_benthic, var = "X")

rpn_benthic.mat2 <- data.matrix(rpn_benthic.mat)

is.matrix(rpn_benthic.mat2)

rpn_benthic.simprof <- simprof(rpn_benthic.mat2,
                                    num.expected = 1000, num.simulated = 999,
                                    method.distance = "braycurtis",
                                    method.transform = "squareroot", alpha = 0.05,
                                    sample.orientation = "row", const = 0,
                                    silent = FALSE, increment = 100, 
                                    undef.zero = TRUE, warn.braycurtis = FALSE)


simprof.plot(rpn_benthic.simprof)

rpn_benthic.simprof2 <- simprof(rpn_benthic.mat2,
                               num.expected = 1000, num.simulated = 999,
                               method.distance = "braycurtis",
                               method.transform = "squareroot", alpha = 0.001,
                               sample.orientation = "row", const = 0,
                               silent = FALSE, increment = 100, 
                               undef.zero = TRUE, warn.braycurtis = FALSE)


simprof.plot(rpn_benthic.simprof2)

rpn_benthic.mat3 = as.matrix(rpn_benthic.mat2[,2:9])
#rpn_benthic.mat4 = as.matrix(rpn_benthic.mat2[1])

is.matrix(rpn_benthic.mat3)
#is.matrix(rpn_benthic.mat4)

#Save data.frame in tidyverse
#vignette("tibble")
as_tibble(rpn_benthic.mat3)

rpn_benthic.mat2.dist <- vegdist(rpn_benthic.mat2, 
                                         method = "bray")
rpn_benthic.mat2.dist

rpn_benthic.mat3.sqrt_y <- sqrt(rpn_benthic.mat3)

as_tibble(rpn_benthic.mat3.sqrt_y)

rpn_benthic_pca1 <- PCA(rpn_benthic.mat3.sqrt_y, scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(rpn_benthic_pca1)

rpn_benthic_pca2 <- prcomp(rpn_benthic.mat3.sqrt_y,  scale = TRUE)
summary(rpn_benthic_pca2)
rpn_benthic_pca2
plot(rpn_benthic_pca2)
str(rpn_benthic_pca2)
head(rpn_benthic_pca2$x)

rpn_benthic_pca3 <- rda(rpn_benthic.mat3.sqrt_y)
summary(rpn_benthic_pca3)

rpn_benthic.mat3.sqrt_y2 <- bind_cols(rpn_benthic_sites, 
                                            rpn_benthic.mat3.sqrt_y)

fviz_pca_ind(rpn_benthic_pca2) #label = "none", #habillage = rpn_benthic.mat3.sqrt_y2$site,
             #addEllipses = TRUE) #ellipse.level = 0.95) + 
#scale_color_brewer(palette = "Set1") +
#theme_minimal()

#fviz_pca_ind(rpn_benthic_pca2, label = "none", #habillage = rpn_benthic.mat3.sqrt_y2$site,
#             addEllipses = TRUE) #ellipse.level = 0.95) + 
#  #scale_color_brewer(palette = "Set1") +
#  theme_minimal()

#fviz_pca_biplot(tut_benthic_pca2, label = "none", habillage = tut_benthic_sh_species.sqrt_y4$coast,
#                addEllipses = FALSE, ellipse.level=0.95) + 
#  scale_color_brewer(palette = "Set1") +
#  theme_minimal()

fviz_pca_var(rpn_benthic_pca2)
fviz_pca_biplot(rpn_benthic_pca2)

#The input for PERMANOVA is a dissimilarity matrix (Bray-Curtis dissimilarity in this case), 
#and corresponding environmental data. A resultant p-value < 0.05 indicates that centroid position and/or dispersion 
#differs between the groups in the model.

#As PERMANOVA is affected by both centroid position and disperison, we perform a homogeneity of dispersions 
#analysis using betadisper to establish whether dispersion is homogeneous between groups (in this case, silt 
#and sand sediment associated communities). We will then perform PERMANOVA analysis using the adonis function in 
#the R package vegan.

rpn_benthic.mat3.sqrt_y3 <-
  rpn_benthic.mat3.sqrt_y2 %>%
  mutate(
    depth = case_when(
      site == "ana_sh" ~ "shallow",
      site == "ana_md" ~ "medium",
      site == "ana_dp" ~ "deep",
      site == "mtt_sh" ~ "shallow",
      site == "mtt_md" ~ "medium",
      site == "mtt_dp" ~ "deep",
      site == "man_sh" ~ "shallow",
      site == "man_md" ~ "medium",
      site == "man_dp" ~ "deep",
      site == "se_sh" ~ "shallow",
      site == "se_md" ~ "medium",
      site == "se_dp" ~ "deep"),
    coast = case_when(
      site == "ana_sh" ~ "north",
      site == "ana_md" ~ "north",
      site == "ana_dp" ~ "north",
      site == "mtt_sh" ~ "northwest",
      site == "mtt_md" ~ "northwest",
      site == "mtt_dp" ~ "northwest",
      site == "man_sh" ~ "west",
      site == "man_md" ~ "west",
      site == "man_dp" ~ "west",
      site == "se_sh" ~ "southeast",
      site == "se_md" ~ "southeast",
      site == "se_dp" ~ "southeast"
    
    )
  )

rpn_benthic_env <-
  rpn_benthic.mat3.sqrt_y3 %>%
  select(depth, coast)

# Homogeneity of dispersion test
permutest(betadisper(rpn_benthic.mat2.dist, 
                     rpn_benthic_env$coast)
          )

permutest(betadisper(rpn_benthic.mat2.dist, 
                     rpn_benthic_env$depth)
          )

# PERMANOVA analysis

adonis(rpn_benthic.mat2 ~ coast, data = rpn_benthic_env, permutations = 999, 
       method = "bray")

fviz_pca_ind(rpn_benthic_pca2, label = "none", habillage = rpn_benthic_env$coast,
            addEllipses = TRUE) #ellipse.level = 0.95) + 
  #scale_color_brewer(palette = "Set1") +
  theme_minimal()

#fviz_pca_biplot(tut_benthic_pca2, label = "none", habillage = tut_benthic_sh_species.sqrt_y4$coast,
#                addEllipses = FALSE, ellipse.level=0.95) + 
#  scale_color_brewer(palette = "Set1") +
#  theme_minimal()


# From 'rpn_benthic_pca2' above
  
rpn_benthic_pca_data <- as.data.frame(
  rpn_benthic_pca2$x[, 1:2]
                                      ) # extract first two columns and convert to data frame

rpn_benthic_pca_data2 <- cbind(rpn_benthic_pca_data, rpn_benthic_env$coast, rpn_benthic_env$depth) # bind by columns

colnames(rpn_benthic_pca_data2) <- c("PC1", "PC2", "coast", "depth") # change column names
  
ggplot(rpn_benthic_pca_data2) +
    aes(PC1, PC2, color = coast, shape = depth) + # define plot area
    geom_point(size = 4) # adding data points

# PCA Analysis
# The PCA code for farm stress data is shown below. Generally, any PCA’s first step is to scale the data so that each feature 
# is comparable, followed by mean centering, shifting the data on their axes without altering any stochastic properties. 
# In this specific case, but generally, not so, scaling isn’t necessary since each column is measured using the same Likert-type scale.

# We use the prcomp() function to do the PCA and subsequently extract the loadings, scores matrix, and feature importance as 
# measured by how much each of the variability each PC axis explains in the observed data.

# Make the features comparable by centering each column around its mean.  Since the data are already on the same scale, we do not need to also divide by their respective variance (e.g., scale = FALSE).
rpn_benthic.mat3.sqrt_y <- scale(rpn_benthic.mat3.sqrt_y, center = TRUE, scale = FALSE)

# This function does all of the math to create the PC matrix factorization.
rpn_benthic.mat3.sqrt_y_pca <- prcomp(rpn_benthic.mat3.sqrt_y, center = FALSE, scale. = FALSE, retx = TRUE)

# Pieces from the PCA used in the biplot.
rpn_benthic.mat3.sqrt_y_pca_loadings <- rpn_benthic.mat3.sqrt_y_pca$rotation # Loadings Matrix
rpn_benthic.mat3.sqrt_y_pca_scores <- predict(rpn_benthic.mat3.sqrt_y_pca) # Scores Matris
rpn_benthic.mat3.sqrt_y_pca_importance <- summary(rpn_benthic.mat3.sqrt_y_pca)$importance # Explained Variance

# The plot data includes occupation and the first two PC components.
plotDataScores <- cbind(coast = rpn_benthic_env$coast, depth = rpn_benthic_env$depth, 
                        as.data.frame(rpn_benthic.mat3.sqrt_y_pca_scores[, c("PC1", "PC2")]
                                      )
                        )

# We want to use min-max feature scaling on the scores so they are between zero and one, the same as the loadings.
normalize <- function(x) return ((x - min(x)) / (max(x) - min(x)))

plotDataScores[, "PC1"] <- scale(normalize(plotDataScores[, "PC1"]), center = TRUE, scale = FALSE)
plotDataScores[, "PC2"] <- scale(normalize(plotDataScores[, "PC2"]), center = TRUE, scale = FALSE)

plotDataLoadings <- as.data.frame(rpn_benthic.mat3.sqrt_y_pca_loadings)

cols <- c("north" = "red", "northwest" = "blue", "west" = "darkgreen", "southeast" = "orange")

p1 <- ggplot() + 
  geom_point(data = plotDataScores, mapping = aes(x = PC1, y = PC2, 
                                                  color = coast, shape = depth),
             size = 4
             ) + 
  scale_colour_manual(values = cols)

p1

p2 <- p1 + 
  geom_segment(data = plotDataLoadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2, colour = "darkblue") + 
  
  geom_text(data = plotDataLoadings, mapping = aes(x = PC1, y = PC2, 
                                                   label = rownames(plotDataLoadings)), 
            hjust = 1, vjust = -0.2, colour = "darkred", size = 4, check_overlap = TRUE)

p2


p3 <- p2 + 
  xlab(paste("PC1 (", round(rpn_benthic.mat3.sqrt_y_pca_importance["Proportion of Variance", "PC1"]*100, 1),"%)", sep = "")) + 
  ylab(paste("PC2 (", round(rpn_benthic.mat3.sqrt_y_pca_importance["Proportion of Variance", "PC2"]*100, 1),"%)", sep = ""))

p3

p4 <- p3 + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 50, hjust = 1),
        axis.title = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "right",
        panel.grid = element_line(color = "lightgray"),
        panel.background = element_rect(fill = "white", colour = "white"))

p4
