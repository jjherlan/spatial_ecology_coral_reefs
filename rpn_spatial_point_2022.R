# Packages
require(spatstat)
require(rgdal)
require(sf)
require(stars)
require(terra)
require(unmarked)
require(AHMbook)

#Define points base spatstat format

vhu_corals <- ppp(vhu_centroids$x, vhu_centroids$y, window = vhu_window)

# Plot
plot(vhu_corals)

#summary information

summary(vhu_corals)

#density plots

plot(density(vhu_corals)
)

#alter smoothing parameter

plot(density(vhu_corals, 1)
  )  

#contour plot

contour(density(vhu_corals, 1)
)

#quadrat counts

vhu_Q <- quadratcount(vhu_corals, nx = 10, ny = 25) #counts in 10 x 25 m quadrats

#plot

plot(vhu_corals, cex = 1)

plot(vhu_Q, add = TRUE, cex = 1)

#chi-sq test for complete spatial randomness, CSR

quadrat.test(vhu_corals, nx = 10, ny = 25, method = "Chisq")

##############################################
#Univariate point patterns
##############################################

#-----------------------#
#Ripley's K function:
#-----------------------#

K_none_vhu <- Kest(vhu_corals, correction = "none")

#plot K

plot(K_none_vhu, legend = F)

#plot L with 1:1 expectation

L_none_vhu <- Lest(vhu_corals, correction = "none")

plot(L_none_vhu, legend = F)

#plot L with 0 expectation

plot(L_none_vhu, . - r ~ r, legend = F)

#isotropic edge correction

L_iso_vhu <- Lest(vhu_corals, correction = "isotropic")

plot(L_iso_vhu, . - r ~ r, legend = F)

#translate (toroidal) edge correction

L_trans_vhu <- Lest(vhu_corals, correction = "trans")

plot(L_trans_vhu, . - r ~ r, legend = F)

#Monte Carlo simulations to calculate a global and pointwise confidence envelope under CSR

L_csr_vhu <- envelope(vhu_corals, Lest, nsim = 99, rank = 1, correction = "trans", global = F)

L_csr.g_vhu <- envelope(vhu_corals, Lest, nsim = 99, rank = 1, correction = "trans", global = T)

#plot point-wise envelope

plot(L_csr_vhu, . - r ~ r, shade = c("hi", "lo"), legend = F)

#plot global envelope

plot(L_csr.g_vhu, . - r ~ r, shade = c("hi", "lo"), legend = F)

# AHMbook (version 0.2.3) ######################################################################################

# sim.fn: Simulate a Poisson point process

# Simulates animal or plant locations in space according to a homogenous Poisson process. 
# This process is characterized by the intensity, which is the average number of points per 
# (very small)unit area. The resulting point pattern is then discretized to obtain abundance 
# data and presence/absence (or occurrence) data. The discretization of space is achieved by 
# choosing the cell size. It is used in AHM1 Section 1.1 to help to understand the relationship 
# between point patterns, abundance data and occurrence data (also called presence/absence or distribution data).

# Simulate cell size = 15 (x 15)

vhu_poci_simulate1 <- sim.fn(quad.size = 15, cell.size = 1, intensity = 12.524)

# Simulate cell size = 16 (x 16)

vhu_poci_simulate2 <- sim.fn(quad.size = 16, cell.size = 1, intensity = 12.524)

# AHMbook (version 0.2.3)
# simPPe: Simulate a spatial point pattern in a heterogeneous landscape

# simPPe(lscape.size = 150, buffer.width = 25, variance.X = 1, theta.X = 10,
# M = 250, beta = 1, quads.along.side = 6, show.plots = TRUE)

#----------------------------------------#
#Pair correlation function, g
#----------------------------------------#
################ PAGE 119 ################

# Estimate the distance at which spatial patterns arise, 
# such that using a "ring" rather than a circle (as in Ripley's K)
# is more appropriate. To estimate the pair correlation 
# coefficient, g, most of the arguments are similar above.   
  

P_trans_vhu <- pcf(vhu_corals, correction = "trans")

# Plot

plot(P_trans_vhu, legend = FALSE, xlim = c(0.5 , 2.5)
     )

#Monte Carlo simulations for confidence envelope

P_env_vhu <- envelope(vhu_corals, pcf, nsim = 99, rank = 1, stoyan = 0.15, correction = "trans", global = F) #stoyan = bandwidth; set to default

# Plot pcf with envelope

plot(P_env_vhu, shade=c("hi", "lo"), legend = FALSE, xlim = c(0.5, 2.5)
     )

### BELOW NOT ACCOMPLISHED - 4/8/2022 ###

#alter bandwidth on pcf
Penv.coarse <- envelope(ppp.cactus, pcf, nsim=99, rank=1, stoyan=0.3, correction="trans", global=F)

#plot
plot(Penv.coarse, shade=c("hi", "lo"), legend=F, xlim=c(0.5,14))


# Manavai

#Define points base spatstat format

man_corals <- ppp(man_centroids$x, man_centroids$y, window = man_window)

# Several exploratory graphs and summary statistics that 'spatstat' can provide

plot(man_corals)

# summary information

summary(man_corals)

# This summary shows there are 372 points (pocilloporid coral colony) and provides 
# the observed, lambda. The density plot can be a helpful visualization of intensity 
# of points across the plot. By plotting the spatial intensity this way, spatial trends
# in the point occurrences that may violate the assumption of homogeneous point process.

# density plots

plot(
  density(man_corals)
                      )

#alter smoothing parameter

plot(
  density(man_corals, 1)
                        )  

#contour plot

contour(
  density(man_corals, 1)
                        )

# We can also make tallies of counts of point locations based on quadrats overlaid
# on the plot. To determine whether these quadrat counts conform to CSR
# (i.e., a homogeneous Poisson process), use a simple Chi-Square test statistic.

# quadrat counts

man_Q <- quadratcount(man_corals, nx = 10, ny = 25) #counts in 10 x 25 m quadrats

#plot

plot(man_corals, cex = 1)

plot(man_Q, add = TRUE, cex = 1)

#chi-sq test for complete spatial randomness, CSR

quadrat.test(man_corals, nx = 10, ny = 25, method = "Chisq")

# The test statistic suggests highly a non-random point pattern at the
# scale of the quadrat defined. Note that this test is more akin to a first-order
# point pattern analysis because it is based on the dispersion of points among 
# sampling quadrats.

##############################################
#Univariate point patterns
##############################################

#-----------------------#
#Ripley's K function:
#-----------------------#

# Second-order point pattern analyses can readily be implemented in 'spatstat'.
# Ripley's K and the standard L functions
# Ignore edge effects with '(correction = "none")'

K_none_man <- Kest(man_corals, correction = "none")

#plot K

plot(K_none_man, legend = F)

#plot L with 1:1 expectation

L_none_man <- Lest(man_corals, correction = "none")

plot(L_none_man, legend = F)

#plot L with 0 expectation

plot(L_none_man, . - r ~ r, legend = F)

# For the functions above, two lines are drawn. The 'Lpois' line
# is a dashed line that represents the expected (theoretical) 
# value based on a Poisson process (CSR). The way that 'spatstat' calculates L is to 
# linearize K such that the expected value is r (or the radius). The other solid line
# represents the estimated L (linearized K), when the edges are ignored.

# The above analysis ignores the problem of edge effects. 'spatstat' provides a variety of
# edge corrections. Contrast an (1) isotropic and (2) translate correction for 
# adjusting for boundary effects. The isotropic correction uses a simple weighting for 
# the area sampled near the plot boundary (Ripley 1988), the translate correction uses 
# a toroidal shift. We adjust for potential boundary effects
# by typing:

#isotropic edge correction

L_iso_man <- Lest(man_corals, correction = "isotropic")

plot(L_iso_man, . - r ~ r, legend = F)

#translate (toroidal) edge correction

L_trans_man <- Lest(man_corals, correction = "trans")

plot(L_trans_man, . - r ~ r, legend = F)

# When comparing the 'L' function that ignores boundaries to those above that account
# for boundaries, notice that patterns change at larger distances - we expect that the 'L' 
# function at larger distances should potentially be more biased than at smaller distances 
# because larger radii will naturally overlap more with the boundary of the study area. 
# When edge effects are ignored, the effect in the of counting fewer points within the 
# radius 'r' near the boundary, so the observed value for 'L' or 'K' should have an artifact of 
# decreasing as 'r' increases.

# The analyses so far are exploratory. While the observed statistics (K, L) appear different 
# than the expectation, it is unclear if these are substantially (or significantly) different.
# To conduct formal inference regarding if the point pattern follows CSR, we can use Monte Carlo
# simulations ro calculate a confidence envelope under CSR with the 'envelope' function.

# Monte Carlo simulations to calculate a global and pointwise confidence envelope under CSR

L_csr_man <- envelope(man_corals, Lest, nsim = 99, rank = 1, correction = "trans", global = F)

L_csr.g_man <- envelope(man_corals, Lest, nsim = 99, rank = 1, correction = "trans", global = T)

#plot point-wise envelope

plot(L_csr_man, . - r ~ r, shade = c("hi", "lo"), legend = F)

#plot global envelope

plot(L_csr.g_man, . - r ~ r, shade = c("hi", "lo"), legend = F)

# In the 'envelope' function, rank specifies the alpha for the simulations.
# For a 'rank = 1', the max an min are used as the envelopes, such that for 99 simulations,
# alpha = 0.01 while for 19 simulations, alpha = 0.05. 

# Also not that we used 'global = FALSE'. This means that these are 'pointwise envelopes'.
# These envelopes work better for 'L' than 'K' because of variance stabilizing properties.

# Plots of pointwise envelopes show the stated upper and lower quantiles of
# simulated patterns for any distance r. Because such analyses are calculating
# envelopes for many distances, pointwise envelopes with a specified alpha should not
# be used to reject a null model at that level (because of the multiple tests). Consequently,
# there are alternative global tests that can be used in this way. While global
# tests are under active development (Baddeley et al. 2014; Wiegand et al. 2016),
# 'spatstat' does provide one option for a global test (using global = T). 

# This approach estimates the maximum deviation from the Poisson
# point process across all r (i.e., D = max|K(r) - Kpois(r)|). This approach is referred
# to as a simultaneous envelope (or critical band) rather than a pointwise envelope. 
# If the observed line falls outside the simultaneous envelope at any point
# on r, we would reject the null hypothesis.

# Now, say we are more interested in estimating the distance at which spatial
# patterns arise, such that using a "ring" rather than a circle (as in Ripley’s K) is
# more appropriate. To estimate the pair correlation function, g, most of the arguments
# are similar to above. The main exception is that instead of calling 'Lest', we call 'pcf'
# (pair correlation function).

Ptrans_man <- pcf(man_corals, correction = "translate")
plot(Ptrans_man)

Penv_man <- envelope(man_corals, pcf, nsim = 99, rank = 1, correction =
                     "translate", global = FALSE)

plot(Penv_man, shade = c("hi", "lo"), legend = FALSE)

# The 'pcf' function uses a smoothing kernel such that distance bins are not needed.
# The default bandwidth coefficient (related to sigma in a Gaussian kernel)
# for the smoothing kernel is set to 0.15 (Stoyan and Stoyan 1994). We
# can adjust the smoothing on the pair correlation function using the 'stoyan' command
# in the 'pcf' function. Increasing the value of the bandwidth coefficient (e.g.,
# stoyan = 0.4) results in a less wiggly g function.

# Finally, we can use similar arguments for the G-function to estimate the probability
# of finding a nearest neighbor as a function of distance. "spatstat"
# uses a similar approach as above with the 'Gest' function. Note that for Gest, there
# are subtly different ways to account for edge effects relative to above. Below we use
# 'rs', the reduced sample correction. We can check the observed G-function calculated
# by 'spatstat' to the cumulative distribution function of the empirical data
# (with the 'ecdf' function):

Gtrans_man <- Gest(man_corals, correction = "rs")

plot(Gtrans_man, legend = F)

Genv_man <- envelope(man_corals, Gest, nsim = 99, rank = 1,
                   correction = "rs", global = FALSE)

plot(Genv_man, shade = c("hi", "lo"), legend = FALSE)

#nearest neighbor distance for each point

nn.dist_man <- nndist(man_corals)

plot(ecdf(nn.dist_man), add = T)

# AHMbook (version 0.2.3) ######################################################################################

# sim.fn: Simulate a Poisson point process

# Simulates animal or plant locations in space according to a homogenous Poisson process. 
# This process is characterized by the intensity, which is the average number of points per 
# (very small)unit area. The resulting point pattern is then discretized to obtain abundance 
# data and presence/absence (or occurrence) data. The discretization of space is achieved by 
# choosing the cell size. It is used in AHM1 Section 1.1 to help to understand the relationship 
# between point patterns, abundance data and occurrence data (also called presence/absence or distribution data).

# Simulate cell size = 15 (x 15)

vhu_poci_simulate1 <- sim.fn(quad.size = 15, cell.size = 1, intensity = 12.524)

# Simulate cell size = 16 (x 16)

vhu_poci_simulate2 <- sim.fn(quad.size = 16, cell.size = 1, intensity = 12.524)

# AHMbook (version 0.2.3)
# simPPe: Simulate a spatial point pattern in a heterogeneous landscape

# simPPe(lscape.size = 150, buffer.width = 25, variance.X = 1, theta.X = 10,
# M = 250, beta = 1, quads.along.side = 6, show.plots = TRUE)

#----------------------------------------#
# Alternative Null Models
#----------------------------------------#
################ PAGE 125 ################

# While CSR is a useful starting point as a null model, in some situations we may be
# interested in using alternative null models. Some null models can be derived from a
# Poisson cluster process. Two common Poisson cluster processes considered in
# ecology are Matern cluster processes and Thomas cluster processes (Velazquez
# et al. 2016). In a Matérn cluster process, there are two types of points. The first are
# "parent" points, which have a Poisson distribution. Second, for each parent point,
# there are “offspring” points, which are independently and uniformly distributed
# around the parent points within a radius r. Consequently, these "offspring"
# points generate an underlying aggregated pattern. Similarly, with a Thomas
# process, “offspring” points are generated with parents but with an isotropic Gaussian
# distribution (similar to a Gaussian kernel). Such a process could
# reflect biological phenomena such as seed dispersal from parent plants.
# We can use these alternative null models in spatstat, with the above functions
# (K, L, pair correlation g, etc.). For example, a K function with a Thomas process as a
# null model can be quantified as:

Kthomas_man <- kppm(man_corals, ~ 1, "Thomas")

# To interpret this model, summary(Kthomas) provides a wealth of information
# regarding the fitted model. For example, it provides an estimate of the mean cluster
# size (4.6 points), as well as the best fit scale for that size (3.7 m). Note that in the
# above kppm function, we can also account for covariates (the "~ 1" states to not
# consider covariates and only include an intercept in the model; ~ polynom(x, y, 2) 
# would account for the trend shown above) We can use the envelope function here as well to 
# interpret the point pattern.

Kthomas.env_man <- envelope(Kthomas_man, Lest, nsim = 99, rank = 1, global = F)

plot(Kthomas.env_man, . - r ~ r, shade = c("hi", "lo"), legend = F)
















