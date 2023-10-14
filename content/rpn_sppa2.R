require(tidyverse)
require(spatstat)
require(rgdal)
require(sf)
require(stars)
require(terra)
require(unmarked)
require(AHMbook)
require(png) #bring png images into R platform
require(raster)
require(landscapemetrics)
require(gapminder)
require(wesanderson)
require(ggridges)
require(psych)
require(DHARMa)

## Quadrat Counts

### Southeast - Vaihu

se_csv <- read.csv("vhu_centroids.csv")

se_csv2 <- as_tibble(se_csv)

se_csv2 

### West - Manavai

west_csv <- read.csv("man_centroids.csv")

west_csv2 <- as_tibble(west_csv)

west_csv2 

### North - Anakena

north_csv <- read.csv("ana_centroids.csv")

north_csv2 <- as_tibble(north_csv)

north_csv2 

# Southeast Define window for spatial pattern analyses based on the extent defined above*

se_window <- owin(c(0, 10), c(0, 25))

attach(se_window)

se_corals <- ppp(se_csv2$x, se_csv2$y, window = se_window)

#Summary information

summary(se_corals)

#This summary shows there are 3131 points (pocilloporid coral colony) and provides the observed, 
#lambda. The density plot can be a helpful visualization of intensity of points across the plot. 
#By plotting the spatial intensity this way, spatial trends in the point occurrences that may 
#violate the assumption of homogeneous point process.

#West Define window for spatial pattern analyses based on the extent defined above

west_window <- owin(c(0, 10), c(0, 25))

attach(west_window)

west_corals <- ppp(west_csv2$x, west_csv2$y, window = west_window)

#Summary information

summary(west_corals)

#This summary shows there are XXX points (pocilloporid coral colony) and provides the observed, 
#lambda. The density plot can be a helpful visualization of intensity of points across the plot. 
#By plotting the spatial intensity this way, spatial trends in the point occurrences that may 
#violate the assumption of homogeneous point process.

#North Define window for spatial pattern analyses based on the extent defined above*

north_window <- owin(c(0, 10), c(0, 25))

attach(north_window)

north_corals <- ppp(north_csv2$x, north_csv2$y, window = north_window)

#Summary information

summary(north_corals)

#This summary shows there are XXXX points (pocilloporid coral colony) and provides the observed, 
#lambda. The density plot can be a helpful visualization of intensity of points across the plot. 
#By plotting the spatial intensity this way, spatial trends in the point occurrences that may 
#violate the assumption of homogeneous point process.

## Density Plots

plot(
  density(se_corals)
)


plot(
  density(west_corals)
)


plot(
  density(north_corals)
)

#Alter smoothing parameter

plot(
  density(se_corals, 1)
)  

plot(
  density(west_corals, 1)
)  

plot(
  density(north_corals, 1)
)  

# **Contour plot**

contour(
  density(se_corals, 1)
)

contour(
  density(west_corals, 1)
)

contour(
  density(north_corals, 1)
)


# We can also make tallies of counts of point locations based on quadrats overlaid on the plot. 
# To determine whether these quadrat counts conform to CSR (i.e., a homogeneous Poisson process), 
# use a simple Chi-Square test statistic.

# **Quadrat counts**

#Counts in 10 x 25 m quadrats

se_Q <- quadratcount(se_corals, nx = 10, ny = 25)

#Plot

plot(se_corals, cex = 1, main = 'Southeast')

plot(se_Q, add = TRUE, cex = 1)

plot(se_corals, cex = 1.0, pch = 19, main = 'Southeast')

#Chi-sq test for complete spatial randomness, CSR

quadrat.test(se_corals, nx = 10, ny = 25, method = "Chisq")

#Quadrat counts

#Counts in 10 x 25 m quadrats

west_Q <- quadratcount(west_corals, nx = 10, ny = 25)

#Plot

west_quad <- plot(west_corals, cex = 1, main = 'West')
plot(west_Q, add = TRUE, cex = 1)

#Chi-sq test for complete spatial randomness, CSR

quadrat.test(west_corals, nx = 10, ny = 25, method = "Chisq")

# The test statistic suggests highly a non-random point pattern at the scale of the quadrat defined. 
# Note that this test is more akin to a first-order point pattern analysis because it is based on the 
# dispersion of points among sampling quadrats.

#Quadrat counts

#Counts in 10 x 25 m quadrats

north_Q <- quadratcount(north_corals, nx = 10, ny = 25)

#Plot

north_quad <- plot(north_corals, cex = 1, main = 'North')
plot(north_Q, add = TRUE, cex = 1)

#Plot all three

par(mfrow = c(1, 3))
plot(north_corals, cex = 1, main = 'North')
plot(north_Q, add = TRUE, cex = 1)
plot(west_corals, cex = 1, main = 'West')
plot(west_Q, add = TRUE, cex = 1, main = 'West')
plot(se_corals, cex = 1, main = 'Southeast')
plot(se_Q, add = TRUE, cex = 1, main = 'Southeast')

#Chi-sq test for complete spatial randomness, CSR

quadrat.test(north_corals, nx = 10, ny = 25, method = "Chisq")

#The test statistic suggests highly a non-random point pattern at the scale of the quadrat defined. 
#Note that this test is more akin to a first-order point pattern analysis because it is based on the 
#dispersion of points among sampling quadrats.

## Ripley's K function:

#-   Second-order point pattern analyses can readily be implemented in 'spatstat'.
#-   Ripley's K and the standard L functions
#-   Ignore edge effects with '(correction = "none")'

### Southeast - Vaihu

K_none_se <- Kest(se_corals, rmax = 5.0, correction = "none")

#-   Plot K

se_K <- plot(K_none_se, legend = F, main = "Southeast: Ripley's K")

#-   Plot L with 1:1 expectation

L_none_se <- Lest(se_corals, rmax = 5.0, correction = "none")

plot(L_none_se, legend = F, main = "Southeast: standardized L function (standardized 1:1)")

#-   Plot L with 0 expectation

plot(L_none_se, . - r ~ r, legend = F, main = "Southeast: standardized L function (standardized 0)")

### West - Manavai

K_none_west <- Kest(west_corals, rmax = 5.0, correction = "none")

#-   Plot K

west_K <- plot(K_none_west, legend = F, main = "West: Ripley's K")

#-   Plot L with 1:1 expectation

L_none_west <- Lest(west_corals, rmax = 5.0, correction = "none")

plot(L_none_west, legend = F, main = "West: standardized L function (standardized 1:1)")

#-   Plot L with 0 expectation

plot(L_none_west, . - r ~ r, legend = F, main = "West: standardized L function (standardized 0)")

### North - Anakena

K_none_north <- Kest(north_corals, rmax = 5.0, correction = "none")

#-   Plot K

north_K <- plot(K_none_north, legend = F, main = "North: Ripley's K")

#-   Plot L with 1:1 expectation

L_none_north <- Lest(north_corals, rmax = 5.0, correction = "none")

plot(L_none_north, legend = F, main = "North: standardized L function (standardized 1:1)")

#-   Plot L with 0 expectation

plot(L_none_north, . - r ~ r, legend = F, main = "North: standardized L function (standardized 0)")

#The above analysis ignores the problem of edge effects. `spatstat` provides a variety of edge 
#corrections. Contrast an (1) isotropic and (2) translate correction for adjusting for boundary 
#effects. The isotropic correction uses a simple weighting for the area sampled near the plot 
#boundary (Ripley 1988), the translate correction uses a toroidal shift. We adjust for potential 
#boundary effects by typing:

### Southeast -- Vaihu

#-   Isotropic edge correction

L_iso_se <- Lest(se_corals, rmax = 5.0, correction = "isotropic")

plot(L_iso_se, . - r ~ r, legend = F, main = "Southeast: standardzied L (isotropic correction)")

#-   Translate (toroidal) edge correction

L_trans_se <- Lest(se_corals, rmax = 5.0, correction = "trans")

plot(L_trans_se, . - r ~ r, legend = F, main = "Southeast: standardzied L (translate correction)")

## Monte Carlo simulations to calculate a global and pointwise confidence envelope under CSR

L_csr_se <- envelope(se_corals, rmax = 5.0, Lest, nsim = 99, rank = 1, correction = "trans", global = F)

L_csr.g_se <- envelope(se_corals, rmax = 5.0, Lest, nsim = 99, rank = 1, correction = "trans", global = T)

#-   Plot point-wise envelope

plot(L_csr_se, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "Southeast: stand. L (Monte Carlo, CSR env)")

#-   Plot global envelope

plot(L_csr.g_se, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "Southeast: stand. L (Monte Carlo, CSR env)")

### West - Manavai

#-   Isotropic edge correction

L_iso_west <- Lest(west_corals, rmax = 5.0, correction = "isotropic")

plot(L_iso_west, . - r ~ r, legend = F, main = "West: standardzied L (isotropic correction)")

#-   Translate (toroidal) edge correction

L_trans_west <- Lest(west_corals, rmax = 5.0, correction = "trans")

plot(L_trans_west, . - r ~ r, legend = F, main = "West: standardzied L (translate correction)")

## Monte Carlo simulations to calculate a global and pointwise confidence envelope under CSR

L_csr_west <- envelope(west_corals, rmax = 5.0, Lest, nsim = 99, rank = 1, correction = "trans", global = F)

L_csr.g_west <- envelope(west_corals, rmax = 5.0, Lest, nsim = 99, rank = 1, correction = "trans", global = T)

#-   Plot point-wise envelope

plot(L_csr_west, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "Southeast: stand. L (Monte Carlo, CSR env)")

#-   Plot global envelope

plot(L_csr.g_west, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "Southeast: stand. L (Monte Carlo, CSR env)")

### North - Anakena

#-   Isotropic edge correction

L_iso_north <- Lest(north_corals, rmax = 5.0, correction = "isotropic")

plot(L_iso_north, . - r ~ r, legend = F, main = "North: standardzied L (isotropic correction)")

#-   Translate (toroidal) edge correction

L_trans_north <- Lest(north_corals, rmax = 5.0, correction = "trans")

plot(L_trans_north, . - r ~ r, legend = F, main = "North: standardzied L (translate correction)")

## Monte Carlo simulations to calculate a global and pointwise confidence envelope under CSR

L_csr_north <- envelope(north_corals, rmax = 5.0, Lest, nsim = 99, rank = 1, correction = "trans", global = F)

L_csr.g_north <- envelope(north_corals, rmax = 5.0, Lest, nsim = 99, rank = 1, correction = "trans", global = T)

#-   Plot point-wise envelope

plot(L_csr_north, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "North: stand. L (Monte Carlo, CSR env)")

#-   Plot global envelope

plot(L_csr.g_north, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "North: stand. L (Monte Carlo, CSR env)")

# Plot K-function

scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178"))

par(mfrow = c(3, 1))
se_north <- plot(K_none_north, legend = F, main = "North: Ripley's K", col = c("#FFC74E", "#FF0000"))
se_west <- plot(K_none_west, legend = F, main = "West: Ripley's K", col = c("#ABC178", "#FF0000"))
se_se <- plot(K_none_se, legend = F, main = "Southeast: Ripley's K", col = c("#82A5C0", "#FF0000"))

par(mfrow = c(3, 1))
se_north <- plot(K_none_north, legend = F, main = "North", col = c("#FFC74E", "#FF0000"))
se_west <- plot(K_none_west, legend = F, main = "West", col = c("#ABC178", "#FF0000"))
se_se <- plot(K_none_se, legend = F, main = "Southeast", col = c("#82A5C0", "#FF0000"))

# Plot standardized L-function

par(mfrow = c(3, 1))
plot(L_csr_north, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "North: stand. L (Monte Carlo, CSR env)")
plot(L_csr_west, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "West: stand. L (Monte Carlo, CSR env)")
plot(L_csr_se, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "Southeast: stand. L (Monte Carlo, CSR env)")

# For the functions above, two lines are drawn. The $L_{pois}$ line is a dashed line that represents 
# the expected (theoretical) value based on a Poisson process (CSR). The way that `spatstat` calculates 
# $L$ is to linearize $K$ such that the expected value is $r$ (or the radius). The other solid line 
# represents the estimated $L$ (linearized $K$), when the edges are ignored.

# When comparing the $L$ function that ignores boundaries to those above that account for boundaries, 
# notice that patterns change at larger distances - we expect that the $L$ function at larger distances 
# should potentially be more biased than at smaller distances because larger radii will naturally overlap 
# more with the boundary of the study area.

# When edge effects are ignored, the effect in the of counting fewer points within the radius $r$ near 
# the boundary, so the observed value for $L$ or $K$ should have an artifact of decreasing as $r$ increases.

# The analyses so far are exploratory. While the observed statistics ($K$, $L$) appear different than the 
# expectation, it is unclear if these are substantially (or significantly) different.

# To conduct formal inference regarding if the point pattern follows CSR, we can use Monte Carlo 
# simulations ro calculate a confidence envelope under CSR with the `envelope` function.

# In the `envelope` function, rank specifies the alpha for the simulations. For a *rank = 1*, the max an 
# min are used as the envelopes, such that for **99 simulations, alpha = 0.01** while for **19 simulations, 
# alpha = 0.05**.

# Also not that we used `global = FALSE`. This means that these are *pointwise envelopes*.

# These envelopes work better for $L$ than $K$ because of variance stabilizing properties.

# Plots of pointwise envelopes show the stated upper and lower quantiles of simulated patterns for any 
# distance r. Because such analyses are calculating envelopes for vhuy distances, pointwise envelopes 
# with a specified alpha should not be used to reject a null model at that level (because of the multiple 
# tests). Consequently, there are alternative global tests that can be used in this way. While global 
# tests are under active development (Baddeley et al. 2014; Wiegand et al. 2016), `spatstat` does provide 
# one option for a global test (using global = T).

# This approach estimates the maximum deviation from the Poisson point process across all 
# r (i.e., $D = max|K_{(r)} - K_{pois(r)}|)$. This approach is referred to as a simultaneous 
# envelope (or critical band) rather than a pointwise envelope.

# If the observed line falls outside the simultaneous envelope at any point on $r$, we would 
# reject the null hypothesis.

### Southeast -- Vaihu

Ptrans_se <- pcf(se_corals, r = NULL, correction = "translate")

plot(Ptrans_se)

plot(Ptrans_se$r, Ptrans_se$pcf, type = "l", xlab = "r", ylab = "g(r)", main = "pair correlation")
abline(h=1, lty=1)

Penv_se <- envelope(se_corals, r = NULL, pcf, nsim = 99, rank = 1, correction =
                       "translate", global = FALSE)

plot(Penv_se, shade = c("hi", "lo"), legend = FALSE, main = "Southeast: pair correlation function, g")

### West - Manavai

Ptrans_west <- pcf(west_corals, r = NULL, correction = "translate")

plot(Ptrans_west)

plot(Ptrans_west$r, Ptrans_west$pcf, type = "l", xlab = "r", ylab = "g(r)", main = "pair correlation")
abline(h=1, lty=1)

Penv_west <- envelope(west_corals, r = NULL, pcf, nsim = 99, rank = 1, correction =
                       "translate", global = FALSE)

plot(Penv_west, shade = c("hi", "lo"), legend = FALSE, main = "Southeast: pair correlation function, g")

### North - Anakena

Ptrans_north <- pcf(north_corals, r = NULL, correction = "translate")

plot(Ptrans_north)

plot(Ptrans_north$r, Ptrans_north$pcf, type = "l", xlab = "r", ylab = "g(r)", main = "pair correlation")
abline(h = 1, lty = 1)

Penv_north <- envelope(north_corals, r = NULL, pcf, nsim = 99, rank = 1, correction =
                       "translate", global = FALSE)

plot(Penv_north, shade = c("hi", "lo"), legend = FALSE, main = "North: pair correlation function, g")

par(mfrow = c(3, 1))
plot(Penv_north, shade = c("hi", "lo"), legend = FALSE, main = "North: pair correlation function, g")
plot(Penv_west, shade = c("hi", "lo"), legend = FALSE, main = "West: pair correlation function, g")
plot(Penv_se, shade = c("hi", "lo"), legend = FALSE, main = "Southeast: pair correlation function, g")

# The `pcf` function uses a smoothing kernel such that distance bins are not needed. 

# The default bandwidth coefficient (related to sigma in a Gaussian kernel) for the smoothing kernel is 
# set to 0.15 (Stoyan and Stoyan 1994).

# We can adjust the smoothing on the pair correlation function using the `stoyan` comvhud in the `pcf` 
# function. 

# Increasing the value of the bandwidth coefficient (e.g., stoyan = 0.4) results in a less wiggly *g* 
# function.

# Finally, we can use similar arguments for the G-function to estimate the probability of finding a 
# nearest neighbor as a function of distance. 

# `spatstat` uses a similar approach as above with the `Gest` function.

# Note that for `Gest`, there are subtly different ways to account for edge effects relative to above. 
# Below we use `rs`, the reduced sample correction. 

# We can check the observed G-function calculated by `spatstat` to the cumulative distribution function 
# of the empirical data (with the `ecdf` function):

plot(Penv_se, shade = c("hi", "lo"), legend = FALSE, main = "Southeast: pair correlation function, g")

### Southeast -- Vaihu

Gtrans_se <- Gest(se_corals, r = NULL, correction = "rs")

plot(Gtrans_se, legend = F)

Genv_se <- envelope(se_corals, r = NULL, Gest, nsim = 99, rank = 1, correction = "rs", global = FALSE)

plot(Genv_se, shade = c("hi", "lo"), legend = FALSE, main = "Southeast: G-function")


### West - Manavai

Gtrans_west <- Gest(west_corals, r = NULL, correction = "rs")

plot(Gtrans_west, legend = F)

Genv_west <- envelope(west_corals, r = NULL, Gest, nsim = 99, rank = 1, correction = "rs", global = FALSE)

plot(Genv_west, shade = c("hi", "lo"), legend = FALSE, main = "West: G-function")

### North - Anakena

Gtrans_north <- Gest(north_corals, r = NULL, correction = "rs")

plot(Gtrans_north, legend = F)

Genv_north <- envelope(north_corals, r = NULL, Gest, nsim = 99, rank = 1, correction = "rs", global = FALSE)

par(mfrow = c(3, 1))
plot(Genv_north, shade = c("hi", "lo"), legend = FALSE, main = "North: G-function")
plot(Genv_west, shade = c("hi", "lo"), legend = FALSE, main = "West: G-function")
plot(Genv_se, shade = c("hi", "lo"), legend = FALSE, main = "Southeast: G-function")

### Alternative Null Models

# While CSR is a useful starting point as a null model, in some situations we may be interested 
# in using alternative null models. Some null models can be derived from a Poisson cluster process. 
# Two common Poisson cluster processes considered in ecology are **Matern cluster** processes and 
# *Thomas cluster** processes (Velazquez et al. 2016). In a Matérn cluster process, there are two 
# types of points. The first are *parent* points, which have a Poisson distribution. 
# Second, for each parent point, there are *offspring* points, which are independently 
# and uniformly distributed around the parent points within a radius r. Consequently, 
# these *offspring* points generate an underlying aggregated pattern. Similarly, with a Thomas process, 
# *offspring* points are generated with parents but with an isotropic Gaussian distribution 
# (similar to a Gaussian kernel). Such a process could reflect biological phenomena such as seed 
# dispersal from parent plants. We can use these alternative null models in `spatstat`, with the above 
# functions ($K$, $L$, *pair correlation g*, etc.). For example, a `K` function with a Thomas process 
# as a null model can be quantified as:

### Southeast -- Vaihu

Kthomas_se <- kppm(se_corals, ~ 1, "Thomas")

Kthomas_se

summary(Kthomas_se)

Kthomas.env_se <- envelope(Kthomas_se, rmax = 5, Lest, nsim = 99, rank = 1, global = F)

plot(Kthomas.env_se, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "Southeast: Thomas Model")

### West - Manavai

Kthomas_west <- kppm(west_corals, ~ 1, "Thomas")

Kthomas_west

summary(Kthomas_west)

Kthomas.env_west <- envelope(Kthomas_west, rmax = 5, Lest, nsim = 99, rank = 1, global = F)

plot(Kthomas.env_west, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "West: Thomas Model")

### North - Anakena

Kthomas_north <- kppm(north_corals, ~ 1, "Thomas")

Kthomas_north

summary(Kthomas_north)

Kthomas.env_north <- envelope(Kthomas_north, rmax = 5, Lest, nsim = 99, rank = 1, global = F)

plot(Kthomas.env_north, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "North: Thomas Model")

