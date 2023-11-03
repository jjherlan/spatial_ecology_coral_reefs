require(tidyverse)
require(spatstat)
##################
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

ripK_north <- plot(K_none_north, legend = F, 
                 main = NULL, 
                 col = c("#FFC74E", "#FF0000")) #FFC74E

ripK_west <- plot(K_none_west, 
                  legend = F, 
                  main = NULL, 
                  col = c("#ABC178", "#FF0000"))


ripK_se <- plot(K_none_se, 
                legend = F, 
                main = NULL, 
                col = c("#82A5C0", "#FF0000"))

plot(north_corals, cex = 1.0, pch = 20, main = 'North')

plot(west_corals, cex = 1.0, pch = 20, main = 'West')

plot(se_corals, cex = 1.0, pch = 20, main = 'Southeast')

plot(Penv_se, shade = c("hi", "lo"), legend = FALSE, main = "Southeast: pair correlation function, g")

plot(Penv_west, shade = c("hi", "lo"), legend = FALSE, main = "West: pair correlation function, g")

plot(L_csr_north, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "North: stand. L (Monte Carlo, CSR env)")

plot(L_csr_west, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "West: stand. L (Monte Carlo, CSR env)")

plot(L_csr_se, . - r ~ r, shade = c("hi", "lo"), legend = F, main = "Southeast: stand. L (Monte Carlo, CSR env)")

L_csr.g_se <- envelope(se_corals, rmax = 5.0, Lest, nsim = 99, rank = 1, correction = "trans", global = T)

n_rescale <- rescale(north_corals)

E <- envelope(north_corals, Lest, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

# plot(L_none_north, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

plot(E, legend = F, main = "North: standardized L function (standardized 1:1)")

plot(E, legend = F, main = NULL)

E <- envelope(north_corals, Lest, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

# plot(L_none_north, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

plot(E, legend = F, main = "North: standardized L function (standardized 1:1)")

plot(E, legend = F, main = NULL)

warnings()

envelope(swp, Kest, nsim=19, rank=1, global=TRUE)

attach(swedishpines)
help(swedishpines)
summary(north_corals)
summary(swedishpines)

# North

E <- envelope(north_corals, Lest, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

# plot(L_none_north, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

plot(E, legend = F, main = "North: standardized L function (standardized 1:1)")

plot(E, legend = F, main = NULL)

# West

west_envelope <- envelope(west_corals, Lest, nsim = 99, rank = 1, legend = F, global = TRUE, main = "West: standardized L function (standardized 1:1)")

# plot(L_none_north, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

plot(west_envelope, legend = F, main = "West: standardized L function (standardized 1:1)")

plot(west_envelope, legend = F, main = NULL)


# Southeast

se_envelope <- envelope(se_corals, Lest, nsim = 99, rank = 1, legend = F, global = TRUE, main = "Southeast: standardized L function (standardized 1:1)")

# plot(L_none_north, nsim = 99, rank = 1, legend = F, global = TRUE, main = "North: standardized L function (standardized 1:1)")

plot(se_envelope, legend = F, main = "Southeast: standardized L function (standardized 1:1)")

plot(se_envelope, . - r ~ r, legend = F, main = NULL)

plot(se_envelope, legend = F, main = NULL)

pp <- rmpoispp(100, types=c("a"))
pp

Xcsr <- rpoispp(100)
summary(Xcsr)

Xreg <- rSSI(0.05, 100)
plot(Xreg)

Xsssi <- rSSI(0.05, 100, win = owin(c(0,1), c(0,1)))

XsmaternI <- rMaternI(100, 0.05, win = owin(c(0,1), c(0,1)))

plot(Xsssi)

plot(XsmaternI)

Xclus <- rMatClust(kappa = 10, r = 0.1, mu = 10)

# par(mfrow = c(3,1))

# plot(Xcsr, main = "CSR")

#csr_window <- owin(c(0, 1), c(0, 1))

# attach(csr_window)

#Xcsr_points <- ppp(Xcsr$x, Xcsr$y, window = csr_window)
#summary(Xcsr_points)

# Random Pattern 

plot(Xcsr, pch = 20, main = NULL)

Xcsr_ripleyK <- Kest(Xcsr, correction = "none")

Xcsr_ripleyK_plot <- plot(Xcsr_ripleyK, legend = F, main = "Xcsr: Ripley's K")

Xcsr_ripleyK_plot <- plot(Xcsr_ripleyK, legend = F, main = NULL)

Xcsr_L <- Lest(Xcsr, correction = "none")

Xcsr_L_plot <- plot(Xcsr_L, legend = F, main = "Xcsr: L")

Xcsr_L_plot <- plot(Xcsr_L, legend = F, main = NULL)


Xcsr_g <- pcf(Xcsr, correction = "none")

Xcsr_g_plot <- plot(Xcsr_g, legend = F, main = "Xcsr: g")

Xcsr_g_plot <- plot(Xcsr_g, legend = F, main = NULL)


# Regular Pattern Simulation

plot(Xreg, pch = 20, main = NULL)

Xreg_ripleyK <- Kest(Xreg, correction = "none")

Xreg_ripleyK_plot <- plot(Xreg_ripleyK, legend = F, main = "Xreg: Ripley's K")

Xreg_ripleyK_plot <- plot(Xreg_ripleyK, legend = F, main = NULL)


Xsssi <- rSSI(0.07, 30, win = owin(c(0,1), c(0,1)))

plot(Xsssi)

Xsssi_ripleyK <- Kest(Xsssi, correction = 'none') 

Xsssi_ripleyK_plot <- plot(Xsssi_ripleyK, legend = F, main = "Xsssi: Ripley's K")


XsmaternI_ripleyK <- Kest(XsmaternI, correction = 'none') 

XsmaternI_ripleyK_plot <- plot(XsmaternI_ripleyK, legend = F, main = "Xsssi: Ripley's K")


Xreg_L <- Lest(Xreg, correction = "none")

Xreg_L_plot <- plot(Xreg_L, legend = F, main = "Xreg: L")

Xreg_L_plot <- plot(Xreg_L, legend = F, main = NULL)


Xreg_g <- pcf(Xreg, correction = "none")

Xreg_g_plot <- plot(Xreg_g, legend = F, main = "Xreg: g")

Xreg_g_plot <- plot(Xreg_g, legend = F, main = NULL)


# Clustered Pattern Simulation

plot(Xclus, pch = 20, main = NULL)

Xclus_ripleyK <- Kest(Xclus, correction = "none")

Xclus_ripleyK_plot <- plot(Xclus_ripleyK, legend = F, main = "Xclus: Ripley's K")

Xclus_ripleyK_plot <- plot(Xclus_ripleyK, legend = F, main = NULL)


Xclus_L <- Lest(Xclus, correction = "none")

Xclus_L_plot <- plot(Xclus_L, legend = F, main = "Xclus: L")

Xclus_L_plot <- plot(Xclus_L, legend = F, main = NULL)


Xclus_g <- pcf(Xclus, correction = "none")

Xclus_g_plot <- plot(Xclus_g, legend = F, main = "Xclus: g")

Xclus_g_plot <- plot(Xclus_g, legend = F, main = NULL)








