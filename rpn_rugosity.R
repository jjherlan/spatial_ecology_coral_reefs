require(tidyverse)

require(scatterplot3d)

ana_sh_A <- read.csv('rpn_ana_sh_r0_tran_A.csv')

ana_sh_A_ <-
as_tibble(ana_sh_A)

ana_sh_A_

# Basic scatter plot

ggplot(ana_sh_A_, aes(x = point, y = z)) + 
  geom_point() %>%
  

# Change the point size, and shape

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

ana_sh_AB <- read.csv('rpn_ana_sh_r0_A_B.csv')

ana_sh_A_B_ <-
  as_tibble(ana_sh_AB) %>%
  mutate_at(vars(transect), factor)
  

ana_sh_A_B_

# Basic scatter plot

ggplot(ana_sh_A_B_, aes(x = point, y = z)) + 
  geom_point(aes(colour = transect)) +
  scale_colour_manual(values = c("blue", "red"))
  
ana_sh <- read.csv('rpn_ana_sh_r0_transects_main.csv')

ana_sh.main <-
  as_tibble(ana_sh) %>%
  mutate_at(vars(transect), factor)


ana_sh.main

# Basic scatter plot

ggplot(ana_sh.main, aes(x = c, y = z)) + 
  geom_point(aes(colour = transect)) +
#  scale_colour_manual(values = c("blue", "red"))
scale_colour_discrete()

# res <- 0
# for(i in 1:length(x)){
#   res <- x[i] + res
# }

# cols <- c("blue", "darkblue", "lightblue",
#           "orange", 
#           "darkgreen")

with(ana_sh.main, 
     scatterplot3d(x,
                   y, 
                   z, 
                   main = "North Shallow Rugosity",
                   xlab = "x",
                   ylab = "y",
                   zlab = "rugosity",
                   pch = 16 #,
                   #cex = 1
                   ) 
)
                   
                   #color=cols[as.numeric(dat$Race)]))

# legend("right", legend = levels(dat$Race),
#        col =  c("darkblue", "orange", "lightgreen"), pch = 16)

# ggplot(ana_sh.main, aes(x = x, y = y, z = z)) + 
# #  geom_point(aes(colour = transect)) +
# geom_contour()


















  