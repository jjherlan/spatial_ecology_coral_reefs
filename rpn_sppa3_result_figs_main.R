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



