require(tidyverse)
require(ggfortify)
require(multcomp)

rpn_benthic.df <- rpn_benthic.mat3

rpn_benthic.df_logit <- logit(rpn_benthic.df)

rpn_benthic.pca_res <- prcomp(rpn_benthic.df_logit, scale. = TRUE)

autoplot(rpn_benthic.pca_res)

autoplot(rpn_benthic.pca_res, data = rpn_benthic_2016.matrix, colour = 'coast', shape = 'depth')

autoplot(rpn_benthic.pca_res, data = rpn_benthic_2016.matrix, 
         colour = 'coast', shape = 'depth', label = TRUE, label.size = 3)


autoplot(rpn_benthic.pca_res, data = rpn_benthic_2016.matrix, 
          colour = 'coast', shape = 'depth', loadings = TRUE,
          loadings.colour = 'blue',
          loadings.label = TRUE, loadings.label.size = 3)

rpn_point_counts <- read.csv("rpn_point_counts.csv")          

rpn_plob <- rpn_point_counts %>%
  filter(group == 'plob')

rpn_plob

rpn_plob.glm <- glm(cbind(successes, failures) ~ location * depth, 
                    family = binomial(link = "logit"), data = rpn_plob)

summary(rpn_plob.glm)

par(mfrow = c(2, 2))
plot(rpn_plob.glm)

# `Anova` function from the *car* package
Anova(rpn_plob.glm, type = 'III') # Type III because... 

post_hoc.model_1.lm <- glht(rpn_plob.glm, linfct = mcp(location*depth = 'Tukey'))



