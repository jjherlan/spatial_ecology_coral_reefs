require(tidyverse)
require(car)
require(nlme)
require(multcomp)
require(multcompView)
require(lsmeans)
require(TukeyC)

require(plyr)
require(betareg)
require(lmtest)
require(glmmTMB)
require(boot)
require(emmeans)
require(brms)
require(mvtnorm)
require(nlme)
require(rstan)

require(VGAM)

# Successes, failures by photos  

ana_sh4 <- read.csv('ana_sh_2016.csv', header = T) %>%
  mutate_at(vars(transect, photo), factor) %>%
  #group_by(transect) %>%
  #filter(Tdir >= 90 & Tdir < 250) %>%
  pivot_longer(cols = pt1:pt100,
               names_to = "point",
               values_to = "taxa",
               values_drop_na = FALSE
  ) %>%
  group_by(transect, photo, taxa) %>%
  dplyr::summarize(n = n()
  ) %>%
  #select(all_of(-c(X, X.1))) %>%
  mutate(group = case_when(
    taxa == "PLOB" ~ "plob",
    taxa == "POCI" ~ "poci",
    taxa == "PVER" ~ "poci",
    taxa == "CALC" ~ "macro",
    taxa == "CPOC" ~ "macro",
    taxa == "DC" ~ "plob",
    taxa == "DAUS" ~ "macro", 
    taxa == "DCRE" ~ "macro",
    taxa == "EAM" ~ "turf",
    taxa == "HALI" ~ "macro",
    taxa == "LVAR" ~ "macro",
    taxa == "MACRO_UNK" ~ "macro",
    taxa == "O" ~ "biotic",
    taxa == "PLIG" ~ "poci",
    taxa == "SHAD" ~ "non-reef",
    taxa == "SOBT" ~ "macro",
    taxa == "SPR" ~ "abiotic",
    taxa == "TAPE" ~ "non-reef",
    taxa == "TURF" ~ "turf",
    taxa == "UNK" ~ "non-reef",
    taxa == "WAND" ~ "non-reef"
  )
    ) %>%
  group_by(photo, group) %>%
  mutate_at(vars(group), factor) %>%
  dplyr::summarize(successes = sum(n)
  ) %>%
  mutate(
    failures = 100 - successes)

ana_sh.glm2 <- glm(cbind(successes, failures) ~ group, 
                  family = binomial(link = "logit"), data = ana_sh4)

summary(ana_sh.glm2)

# `Anova` function from the *car* package
Anova(ana_sh.glm2, type = "III") # Type III because... 

ana_sh.glm2_qu <- glm(cbind(successes, failures) ~ group, 
                   family = quasibinomial(link = "logit"), data = ana_sh4)

summary(ana_sh.glm2_qu)

# `Anova` function from the *car* package
Anova(ana_sh.glm2_qu, type = "III") # Type III because... 

# Try regular `lm` function and run an ANOVA to see if there is a difference

# Successes, failures by photos  

ana_sh5 <- read.csv('ana_sh_2016.csv', header = T) %>%
  mutate_at(vars(transect, photo), factor) %>%
  #group_by(transect) %>%
  #filter(Tdir >= 90 & Tdir < 250) %>%
  pivot_longer(cols = pt1:pt100,
               names_to = "point",
               values_to = "taxa",
               values_drop_na = FALSE
  ) %>%
  group_by(transect, photo, taxa) %>%
  dplyr::summarize(n = n()
  ) %>%
  #select(all_of(-c(X, X.1))) %>%
  mutate(group = case_when(
    taxa == "PLOB" ~ "plob",
    taxa == "POCI" ~ "poci",
    taxa == "PVER" ~ "poci",
    taxa == "CALC" ~ "macro",
    taxa == "CPOC" ~ "macro",
    taxa == "DC" ~ "plob",
    taxa == "DAUS" ~ "macro", 
    taxa == "DCRE" ~ "macro",
    taxa == "EAM" ~ "turf",
    taxa == "HALI" ~ "macro",
    taxa == "LVAR" ~ "macro",
    taxa == "MACRO_UNK" ~ "macro",
    taxa == "O" ~ "biotic",
    taxa == "PLIG" ~ "poci",
    taxa == "SHAD" ~ "non-reef",
    taxa == "SOBT" ~ "macro",
    taxa == "SPR" ~ "abiotic",
    taxa == "TAPE" ~ "non-reef",
    taxa == "TURF" ~ "turf",
    taxa == "UNK" ~ "non-reef",
    taxa == "WAND" ~ "non-reef"
      )
  ) %>%
  group_by(photo, group) %>%
  mutate_at(vars(group), factor) %>%
  dplyr::summarize(successes = sum(n)
  ) %>%
  mutate(
    failures = 100 - successes,
    cover = successes/100)

ana_sh5

ana_sh.lm <- lm(cover ~ group, data = ana_sh5)

par(mfrow = c(2, 2))
plot(ana_sh.lm)

summary(ana_sh.lm)

# `Anova` function from the *car* package
Anova(ana_sh.lm, type = "III") # Type III because... 

# https://rcompanion.org/rcompanion/d_07.html

# Nested ANOVA

ana_sh5 <- read.csv('ana_sh_2016.csv', header = T) %>%
  mutate_at(vars(transect, photo), factor) %>%
  #group_by(transect) %>%
  #filter(Tdir >= 90 & Tdir < 250) %>%
  pivot_longer(cols = pt1:pt100,
               names_to = "point",
               values_to = "taxa",
               values_drop_na = FALSE
  ) %>%
  group_by(transect, photo, taxa) %>%
  dplyr::summarize(n = n()
  ) %>%
  #select(all_of(-c(X, X.1))) %>%
  mutate(group = case_when(
    taxa == "PLOB" ~ "plob",
    taxa == "POCI" ~ "poci",
    taxa == "PVER" ~ "poci",
    taxa == "CALC" ~ "macro",
    taxa == "CPOC" ~ "macro",
    taxa == "DC" ~ "plob",
    taxa == "DAUS" ~ "macro", 
    taxa == "DCRE" ~ "macro",
    taxa == "EAM" ~ "turf",
    taxa == "HALI" ~ "macro",
    taxa == "LVAR" ~ "macro",
    taxa == "MACRO_UNK" ~ "macro",
    taxa == "O" ~ "biotic",
    taxa == "PLIG" ~ "poci",
    taxa == "SHAD" ~ "non-reef",
    taxa == "SOBT" ~ "macro",
    taxa == "SPR" ~ "abiotic",
    taxa == "TAPE" ~ "non-reef",
    taxa == "TURF" ~ "turf",
    taxa == "UNK" ~ "non-reef",
    taxa == "WAND" ~ "non-reef"
  )
  ) %>%
  group_by(photo, group) %>%
  mutate_at(vars(group), factor) %>%
  dplyr::summarize(successes = sum(n)
  ) %>%
  mutate(
    failures = 100 - successes,
    cover = successes/100)

ana_sh5

ana_sh.nlme <- lme(cover ~ group, random = ~1|photo,
            data = ana_sh5,
            method = "REML")

summary(ana_sh.nlme)

anova.lme(ana_sh.nlme,
          type = "sequential",
          adjustSigma = FALSE)

# Post-hoc comparison of means
# Note that “Tukey” here instructs the glht function to compare all means, not to perform a Tukey adjustment of multiple comparisons.

ana_sh6 <- read.csv('ana_sh_2016.csv', header = T) %>%
  mutate_at(vars(transect, photo), factor) %>%
  #group_by(transect) %>%
  #filter(Tdir >= 90 & Tdir < 250) %>%
  pivot_longer(cols = pt1:pt100,
               names_to = "point",
               values_to = "taxa",
               values_drop_na = FALSE
  ) %>%
  group_by(transect, photo, taxa) %>%
  dplyr::summarize(n = n()
  ) %>%
  #select(all_of(-c(X, X.1))) %>%
  mutate(group = case_when(
    taxa == "PLOB" ~ "plob",
    taxa == "POCI" ~ "poci",
    taxa == "PVER" ~ "poci",
    taxa == "CALC" ~ "macro",
    taxa == "CPOC" ~ "macro",
    taxa == "DC" ~ "plob",
    taxa == "DAUS" ~ "macro", 
    taxa == "DCRE" ~ "macro",
    taxa == "EAM" ~ "turf",
    taxa == "HALI" ~ "macro",
    taxa == "LVAR" ~ "macro",
    taxa == "MACRO_UNK" ~ "macro",
    taxa == "O" ~ "biotic",
    taxa == "PLIG" ~ "poci",
    taxa == "SHAD" ~ "non-reef",
    taxa == "SOBT" ~ "macro",
    taxa == "SPR" ~ "abiotic",
    taxa == "TAPE" ~ "non-reef",
    taxa == "TURF" ~ "turf",
    taxa == "UNK" ~ "non-reef",
    taxa == "WAND" ~ "non-reef"
  )
  ) %>%
  group_by(transect, photo, group) %>%
  mutate_at(vars(group), factor) %>%
  dplyr::summarize(successes = sum(n)
  ) %>%
  mutate(
    failures = 100 - successes,
    cover = successes/100)

ana_sh6

ana_sh.nlme2 <- lme(cover ~ group, random = ~1|photo,
                   data = ana_sh6,
                   method = "REML")

summary(ana_sh.nlme2)

anova.lme(ana_sh.nlme2,
          type = "sequential",
          adjustSigma = FALSE)

ana_sh.nlme_posthoc = glht(ana_sh.nlme2, linfct = mcp(group = "Tukey"))

mcs = summary(ana_sh.nlme_posthoc,
              test = adjusted("single-step"))

mcs

### Adjustment options: "none", "single-step", "Shaffer",
###                     "Westfall", "free", "holm", "hochberg",
###                     "hommel", "bonferroni", "BH", "BY", "fdr"


cld(mcs,
    level = 0.05,
    decreasing = TRUE)

leastsquare = lsmeans(ana_sh.nlme2,
                      pairwise ~ group,
                      adjust = "tukey")       ###  Tukey-adjusted comparisons

leastsquare

cld(leastsquare,
    alpha = 0.05,
    Letters = letters,      ### Use lower-case letters for .group
    adjust = "tukey")       ### Tukey-adjusted comparisons

### Means sharing a letter are not significantly different

# Test the significance of the random effect in the mixed effects model

# In order to the test the significance of the random effect from our model (photo), 
# we can fit a new model with only the fixed effects from the model.  
# For this we use the gls function in the nlme package.  
# We then compare the two models with the anova fuction.  
# Note that the p-value does not agree with p-value from the Handbook, 
# because the technique is different, though in this case the conclusion is the same.  
# As a general precaution, if your models are fit with “REML” 
# (restricted maximum likelihood) estimation, then you should compare only models 
# with the same fixed effects.  If you need to compare models with different fixed 
# effects, use “ML” as the estimation method for all models.

ana_sh.model_fixed = gls(cover ~ group,
                  data = ana_sh6,
                  method = "REML")

anova(ana_sh.nlme2,
      ana_sh.model_fixed)

# Checking assumptions of the model

hist(residuals(ana_sh.nlme2),
     col = "darkgray")

plot(fitted(ana_sh.nlme2),
     residuals(ana_sh.nlme2))

plot(ana_sh.nlme2)

# Using the aov function for a nested anova

# The aov function in the native stats package allows you to specify an error 
# component to the model.  When formulating this model in R, the correct error is Rat, not Tech/Rat (Rat within Tech) as used in the SAS example.  The SAS model will tolerate Rat or Rat(Tech).

# The summary of the aov will produce the correct test for Tech.  
# The test for Rat can be performed by manually calculating the p-value 
# for the F-test using the output for Error:Rat and Error:Within.

# See the rattlesnake example in the Two-way anova chapter for designating an 
# error term in a repeated-measures model.

ana_sh.fit = aov(cover ~ group + Error(photo), data = ana_sh6)
summary(ana_sh.fit)

# Using Mean Sq and Df values to get p-value for H = Tech and Error = Rat

pf(q = 0.07093/0.00000,
   df1 = 6,
   df2 = 53,
   lower.tail = FALSE)

### Note: This is same test as summary(ana_sh.fit)

# Using Mean Sq and Df values to get p-value for H = photo and Error = Within

pf(q = 0.00000/0.013,
   df1 = 53,
   df2 = 191,
   lower.tail = F)

# Post-hoc comparison of means with Tukey
# The aov function with an Error component produces an object of aovlist type, 
# which unfortunately isn’t handled by many post-hoc testing functions.  
# However, in the TukeyC package, you can specify a model and error term.  
# For unbalanced data, the dispersion parameter may need to be modified.

tuk = TukeyC(ana_sh6,
             model = 'cover  ~ group + Error(photo)',
        http://127.0.0.1:31501/graphics/plot_zoom_png?width=2048&height=1102     error = 'photo',
             which = 'group',
             fl1 = 1,
             sig.level = 0.05)

summary(tuk)

# for comparison for pin-point data, a logistic regression for binomial counts is fit using
# `glm` function in Base R.

ana_sh_mod.glm <- glm(cover ~ group, family = binomial(link = logit), data = ana_sh6)

summary(ana_sh_mod.glm)

#built-in diagnostic plots for glm models
par(mfrow = c(2,2), pty = 'm')
plot(ana_sh_mod.glm)

# Another approach is to use logit-transformation and then apply a standard linear regression model.


# A very similar analysis can be conducted using the library `nlme` for 
# mixed effects modeling.

ana_sh.lme_1 <- lme(cover ~ group + depth, random = ~ 1 | photo, data = ana_sh6)

anova(plob_2016_lme.1)

# **Did I do this right?**
  
# plob_2016_lme.null <- lme(plob ~ 1, random = ~ 1 | tran, data = plob_2016)
# lrtest(plob_2016_lme.1, plob_2016_lme.2, plob_2016_lme.null)

## Beta regression with pooled observations

# We now turn to the beta regression model, that models the response variable as being 
# generated from a beta distribution (i.e. that is bounded at 0 and 1).

# Given that the observations of percent (coral) cover are based on replicate photos 
# (i.e., similar to quadrats) nested within depths (i.e., similar to patches), 
# we simplify the initial analysis by averaging the observations within each depth 
# (i.e., like a patch).

# We begin by attempting to fit a beta regression model, with `plob` as the response, 
# and `loc` as the categorical predictor.

ana_sh.betareg_1 <- betareg(cover ~ group, data = ana_sh6)

summary(ana_sh.betareg_1)

#As is evident from the error message, `betareg` will not accept values of 0 and 1 in the 
# response variable.

# There are two possible solutions here, rescaling the data to remove 0s and 1s, or fitting 
# zero-inflated models. We start here with the rescaling solution. A suggested rescaling 
# equation is:
  
#  $$ x^*_{i} = \frac{x_i(n-1)+0.5}{n} $$
  
# Where $x^*_i$ is the transformation of $x_i$ and $n$ is the total number of observations 
# in the dataset.

# For convenience we define this as a custom function `tranform01` and apply it to the 
# dataset:

transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

ans_sh.scaled <- transform01(ana_sh6$cover)

ana_sh7 <-
  ana_sh6 %>%
  mutate(cover.scaled = transform01(cover))

# From Damgaard (2019) - supplementary material 0002.
# This paper assumed that recorded data represents the number of times a pin hit the 
# species of interest with a known number of pins that arise with...

# The `VGAM` package uses **rho** as the spatial aggregation parameter we refer to as
# $\delta$ . A logistic regression for binomial counts is an option, but a check for 
# over-dispersion should be done as empirical plant
# cover data are subject to spatial aggregation that may manifest in overdispersed 
# binomial counts (object named: mod.glm).
# Alternatively, a logit-transformed response can be used in linear regression options 
# (object named: mod.lm). For some
# situations, the assumptions for the linear regression models are violated and they 
# should be checked as we show below. Data
# may display non-constant variance, outliers, etc and just using a transformation is 
# insufficient.

vhu_sh.mod_betabinom <- vglm(cover ~ group, betabinomial(zero = "rho"),
                      data = ana_sh7, trace = TRUE)

# With this scaled data we can now successfully fit the model. 
# And test its significance relative to a null model that assumes no effect of the location 
# treatment. For reference we also fit a classical ANOVA model assuming normally distributed 
# errors using `lm`.


ana_sh.bm1 <- betareg(cover.scaled ~ group, data = ana_sh7)
ana_sh.bmnull <- betareg(cover.scaled  ~ 1, data = ana_sh7)
ana_sh.lm1 <- lm(cover.scaled  ~ group, data = ana_sh7)

summary(ana_sh.bm1)
lrtest(ana_sh.bm1, ana_sh.bmnull)
AIC(ana_sh.lm1, ana_sh.lm1, ana_sh.bmnull)

ana_sh.bmnull <- betareg(cover.scaled  ~ 1, data = ana_sh7)
#plob_2016.bmnull <- betareg(PLOB.scaled ~ 1, data = plob_2016)

ana_sh.bm1 <- betareg(cover.scaled ~ group, data = ana_sh7)
#plob_2016.bm1 <- betareg(PLOB.scaled ~ loc + depth, data = plob_2016)
#plob_2016.bm2 <- betareg(PLOB.scaled ~ loc * depth, data = plob_2016)

ana_sh.lm1 <- lm(cover.scaled  ~ group, data = ana_sh7)
#plob_2016.lm1 <- lm(PLOB.scaled ~ loc + depth, data = plob_2016)
#plob_2016.lm2 <- lm(PLOB.scaled ~ loc * depth, data = plob_2016)

summary(ana_sh.bm1)
summary(ana_sh.bmnull)
summary(ana_sh.lm1)

lrtest(ana_sh.bm1, ana_sh.bmnull, ana_sh.lm1)

AIC(ana_sh.bm1, ana_sh.bmnull, ana_sh.lm1)
AIC(ana_sh.bm1, ana_sh.lm1)

# According to the likelihood-ratio test there is a significant difference between the 
# null model and the treatment model. 

# The AIC analysis supports this conclusion, but also highlights the improved model fit with 
# beta regression relative to normal ANOVA (`lm1`). 
# From this initial analysis we would tentatively conclude that using beta regression 
# improves our ability to model the cover.

# It is useful to plot the predictions derived from the model and compare them to the observed 
# data. 

# First we define two new functions to allow us to use the `dbeta` and `rbeta` 
# functions with the $\mu$ and $\phi$ parameterization.

dbeta2 <- function(X, mu, phi, ...) {
  dbeta(X, shape1 = mu * phi, shape2 = (1 - mu) * phi, ...)
}

rbeta2 <- function(N, mu, phi, ...) {
  rbeta(N, shape1 = mu * phi, shape2 = (1 - mu) * phi, ...)
}

  
# With this function we can plot the distributions corresponding to the MLE parameters for 
# each location (i.e., treatment) and depth :
  
# extract coefficients of beta regression model
ana_sh_coefs.bm1 <- coef(ana_sh.bm1)
ana_sh_coefs.bm1

# create vector spanning the transformed 0-1 interval

ana_sh_n.bm1 <- length(fitted(ana_sh.bm1))
ana_sh_x.range <- seq(0.5/ana_sh_n.bm1 , 1-0.5/ana_sh_n.bm1, length.out = 200)
ana_sh_x.range.bt <- (ana_sh_x.range * ana_sh_n.bm1 - 0.5)/(ana_sh_n.bm1-1)

# 
plot(ana_sh_x.range.bt, dbeta2(ana_sh_x.range, inv.logit(ana_sh_coefs.bm1["(Intercept)"] 
                                                         + ana_sh_coefs.bm1[2]), ana_sh_coefs.bm1["(phi)"]),
     type = "l", 
     lty = 1,
     ylab = "Probability density", xlab = "Proportion cover",
     ylim = c(0, 10), 
     lwd = 2, 
     col = "red")

# northwest
# lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, inv.logit(plob_2016_coefs.bm2["(Intercept)"] + plob_2016_coefs.bm2[3]), plob_2016_coefs.bm2["(phi)"]), col = "blue", lwd = 2)

# west
# lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, inv.logit(plob_2016_coefs.bm2["(Intercept)"] + plob_2016_coefs.bm2[4]), plob_2016_coefs.bm2["(phi)"]), lwd = 2, col = "green")

# southeast
# lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, inv.logit(coefs.bm2["(Intercept)"] + plob_2016_coefs.bm2[5]), plob_2016_coefs.bm2["(phi)"]), lwd = 2, col = "yellow")

rug(ana_sh7$plob[ana_sh7$group == "plob"], col = "red", lwd = 1.5, pos = 10)
rug(ana_sh7$poci[ana_sh7$group == "poci"], col = "blue", pos = 9.75, side = 3,lwd = 1.5)
#rug(plob_2016$PLOB[plob_2016$loc == "w"], col = "green", pos = 9.5, side = 3, lwd = 1.5)
#rug(plob_2016$PLOB[plob_2016$loc == "se"], pos = 9.25, col = "yellow", side = 3, lwd = 1.5)

legend("topright", lwd = 2, lty = c(2, 1, 1, 1), col = c("red", "blue", "green", "yellow"), legend = c("n", "nw", "w", "se"), bty = "n")

# We have added the original observations (patch-level means) as ticks of the appropriate color, 
# and back-transformed the densities to allow fair visual comparisons between the fitted distributions and the original data using:

# $$ x_{i} = \frac{x^*_in-0.5}{(n-1)} $$
  
# Note that the vertical positioning of the dots is merely to prevent overplotting.

# From this plot we can see that the model does a reasonable job of fitting beta distributions to each of the treatment levels, but that perhaps the variance of the [control group - not this study] is somewhat overestimated by the model. And that likewise, the variance of the [other three groups - not this study] appears to me somewhat underestimated. This can be confirmed with a residual plot, using residuals calculated relative to their predicted variance.

plot(resid(ana_sh.bm1) ~ fitted(ana_sh.bm1))

#The spread of the standardized residuals is strongly related to the fitted values, 
#suggesting that variance is not being adequately modeled. 
#This observation suggests the possible utility of allowing for the precision 
#parameter $\phi$ to vary between treatment groups. The following section will 
#show extension of the beta regression model to allow for this.

### Variable precision $\phi$

We can repeat the above analysis using a model that allows $\phi$ to vary with predictors. 

This is achieved by adding a second part to the right hand side of the formula, separated with the `|` symbol. 

All covariates to the right of this `|` symbol will be used to model $\phi$. 

Note that they do not have to be the same covariates used to model $\mu$ (specified to the left of the `|`).

plob_2016.bm3 <- betareg(PLOB.scaled ~ loc * depth | loc * depth, data = plob_2016)
summary(plob_2016.bm3)

From the `Coefficients` table we see that the estimate for $\mu$ are the following: 
  
  `inv.logit(...)` = `...` = ...\% plob cover 

Moreover, the estimates of $\mu$ for the other 3 treatments are each significantly higher.

From the `Phi coefficients` table we can see that the maximum likelihood estimate [**Estimate**] of the precision is highest at the **North** location (i.e., treatment) and is reduced significantly relative to this baseline in each of other locations (i.e., treatments).

In other words, the model fit confirms our impression from the previous two graphs that a fixed $\phi$ model overestimates variance in the **North** location, and underestimates it in the other three locations -> **IS THIS TRUE?**.

We can use likelihood-ratio tests to compare the new model to the fixed - $\phi$ and null models.

```{r}
lrtest(plob_2016.bm2, plob_2016.bm3)
lrtest(plob_2016.bmnull, plob_2016.bm2, plob_2016.bm3)
```

The likelihood ratio tests indicate that the model with varying $\phi$ is significantly better than both the previous fixed $\phi$ model, and the null model. 

In this case the conclusion is that including the model for $\phi$ led to a better fitting model than both the fixed $\phi$ model and the null model.

It is possible to apply post-hoc tests to identify which pairwise contrasts of locations (i.e., treatments) and depth levels are significant.

```{r}
test(
  pairs
  (
    emmeans(
      plob_2016.bm3, ~ loc * depth, mode = "link"
    )
  )
)
```

We conclude from this analysis that all locations (i.e., treatments) are significantly different from each other.

Note that more accurate modeling of the treatment effect on variance led to altered inferences about the effect of treatment on means. 

This is an important point: **correct modeling of $\phi$ can often be important for accurate inference on $\mu$.**
  
  Residual plots confirm our conclusion that `bm3` provides a better fit to the observed data than `bm2`. This is seen by the more even spread of residuals in the second plot below.

<div class="fold s">
  ```{r fig.height=10}
par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 0.2, 0.2))
plot(residuals(plob_2016.bm2) ~ fitted(plob_2016.bm2))
plot(residuals(plob_2016.bm3) ~ fitted(plob_2016.bm3))
```
</div>
  
  As above we can plot the MLE distributions for each of the treatments, based on the variable $\phi$ model:
  
  # Coral
  
  <div class="fold s">
  ```{r}

# plot distributions
plob_2016_muphi.bm3 <- unique(data.frame(
  mu = fitted(plob_2016.bm3),
  phi = predict(plob_2016.bm3, type = "precision"),
  treatment = plob_2016$loc
))

plot(plob_2016_x.range.bt , dbeta2(plob_2016_x.range, plob_2016_muphi.bm3[1, 1], plob_2016_muphi.bm3[1, 2]),
     type="l",
     xlab = "Proportion cover", ylab = "Probability density",
     lty = 2, lwd = 2, col = 'red')

for (i in 2:4) {
  lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, plob_2016_muphi.bm3[i, 1], plob_2016_muphi.bm3[i, 2]), col = c("red", "blue", "green", "yellow ")[i], lty = 1, lwd = 2)
}
legend("topright", lwd = 2, lty = c(2, 1, 1, 1), col = c("red", "blue", "green", "yellow"), legend = c("n", "nw", "w", "se"), bty = "n")
```
</div>
  
  Due to the much narrower variance of the Control treatment group in this model, the probability density plots of the other treatments are rather distorted. 

The graph below rescales the Y axis for comparison to the fixed $\phi$ model above.

<div class="fold s">
  ```{r}
plot(x.range.bt , dbeta2(plob_2016_x.range, plob_2016_muphi.bm3[1, "mu"], plob_2016_muphi.bm3[1, "phi"]),
     type="l",
     xlab = "Proportion cover", ylab = "Probability density",
     lty = 2, lwd = 2, ylim=c(0,10), col = 'red')

for (i in 2:4) {
  lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, plob_2016_muphi.bm3[i, "mu"], plob_2016_muphi.bm3[i, "phi"]), col = c("red", "blue", "green", "yellow")[i], lty = 1, lwd = 2)
}

legend("topright", lwd = 2, lty = c(2, 1, 1, 1), col = c("red", "blue", "green", "yellow"), legend = c("n", "nw", "w", "se"), bty = "n")

rug(plob_2016$PLOB[plob_2016$loc=="n"], col = "red", lwd=1.5,pos=10)
rug(plob_2016$PLOB[plob_2016$loc=="nw"],col="blue", pos=9.75, side = 3,lwd=1.5)
rug(plob_2016$PLOB[plob_2016$loc=="w"], col="green", pos = 9.5, side = 3, lwd=1.5)
rug(plob_2016$PLOB[plob_2016$loc=="se"], pos=9.25, col="yellow", side = 3, lwd=1.5)
```
</div>
  
  These plots support the conclusion from the likelihood ratio tests above. The best-fit distributions from the variable $\phi$ model better match the observed differences in dispersion between the different groups.

## Bias correction

As discussed in the main article, a common issue is that the estimators of the beta regression model may be biased, particularly when the sample size is small, here n = 12 here when using patch-level (i.e., depth) observations.

Bias correction can be done in two ways in the `betareg` package, by specifying either 'bias correction' (`BC`) or 'bias reduction' (`BR`). See Gr&uuml;n _et al._ (2012) for further details.

In the code below we refit `bm2`, `bm3` and `bmnull` (see above) using each of the bias adjustment options.

<div class="fold o">
  ```{r, eval=T,collapse=T,message=FALSE}

plob_2016_bm2.bc <- betareg(plob.scaled ~ loc, data = plob_2016, type = "BC")
plob_2016_bmnull.bc <- betareg(plob.scaled ~ 1, data = plob_2016, type = "BC")
plob_2016_bm3.bc <- update(plob_2016.bm2, . ~ . | loc, type = "BC")

plob_2016_bm2.br <- betareg(plob.scaled ~ loc, data = plob_2016, type = "BR")
plob_2016_bmnull.br <- betareg(plob.scaled ~ 1, data = plob_2016, type = "BR")
plob_2016_bm3.br <- update(plob_2016.bm2, . ~ . | loc, type = "BR")

confint(plob_2016.bm3)
confint(plob_2016_bm3.bc)
confint(plob_2016_bm3.br)

AIC(plob_2016_bmnull.br, plob_2016_bm2.br, plob_2016_bm3.br)
AIC(plob_2016_bmnull.bc, plob_2016_bm2.bc, plob_2016_bm3.bc)
AIC(plob_2016.bmnull, plob_2016.bm2, plob_2016.bm3)

lrtest(plob_2016_bmnull.br, plob_2016_bm2.br, plob_2016_bm3.br)
lrtest(plob_2016_bmnull.bc, plob_2016_bm2.bc, plob_2016_bm3.bc)
lrtest(plob_2016.bmnull, plob_2016.bm2, plob_2016.bm3)
```
</div>
  
  From the 95% confidence intervals, we can see that the parameter estimates and their associated uncertainties are only slightly changed in this case.

From the AIC comparison and the likelihood ratio test we can conclude that the model that allows for a different mean per treatment a non-constant precision $\phi$ among the treatments is preferred above models with either constant precision and variable treatment means, or a model with constant precision and constant treatment mean. The bias reduction does not change this conclusion.

### Posterior prediction plots

The plots of `bm2` and `bm3` above use the best fit parameters to generate the modeled distributions. However these distributions do not account for uncertainty in the maximum likelihood estimates. Using Monte Carlo simulations from the best-fit parameters and their associated variance-covariance matrix we can generate graphs that also allow for this uncertainty.

First we define a convenience function `simulate_beta_fit` that takes a model fit, and uses the maximum likelihood estimates and associated variance-covariance matrix to generate `n` new sets of coefficients. The spread of these coefficients therefore reflect both the uncertainty in the parameter estimates, and the correlation between parameter estimates.

These new parameters sets are then used to generate `NN` (default 1000) observations from a Beta distribution paramaterized for each unique set of predictors (in this case corresponding to the 4 treatment levels).

Note that generating posterior predictions and associated distributions is much more straightforward when using MCMC-based model fitting procedures such as those in `rstan` or `brms` packages (see below).

<div class="fold o">
  ```{r}
simulate_beta_fit <- function(model, n = 1000, NN = 1000) {
  num.coefs.mean <- length(coef(model, model = "mean"))
  num.coefs.phi <- length(coef(model, model = "precision"))
  
  new.coefs <- rmvnorm(n = n, coef(model), sigma = vcov(model))
  
  mm.mean <- model.matrix(model, model = "mean")
  mm.prec <- model.matrix(model, model = "precision")
  
  lin.predict.mu <- mm.mean %*% t(new.coefs[, 1:num.coefs.mean])
  lin.predict.phi <- mm.prec %*% t(new.coefs[, (1 + num.coefs.mean):(num.coefs.mean + num.coefs.phi)])
  
  pams <- list(mu = inv.logit(lin.predict.mu), precision = exp(lin.predict.phi))
  upams <- list(mu = unique(pams[[1]]), phi = pams[[2]][row.names(unique(pams[[1]])), ])
  
  sims <- matrix(NA, ncol = dim(upams$mu)[1], nrow = n * NN)
  for (i in 1:ncol(sims)) {
    sims[, i] <- rbeta2(
      n = nrow(sims),
      mu = rep(upams$mu[i, ], rep(NN, n)),
      phi = rep(upams$phi[i, ], rep(NN, n))
    )
  }
  return(sims)
}
```
</div>
  
  Now that this function is defined we can use it to generate posterior predictive distributions based on the model `bm2` (lines) and compare them to the observed data (points).

<div class="fold s">
  ```{r}
plob_2016_ppp.bm2 <- data.frame(simulate_beta_fit(plob_2016.bm2))

names(plob_2016_ppp.bm2) <- levels(plob_2016$loc)[c(1, 3, 2, 4)]

plot(density(plob_2016_ppp.bm2$n, bw = 0.04, from = 0, to = 1), xlim = c(0, 1), main = "", xlab = "Proportion cover", ylab = "Probability density", lty = 2, ylim = c(0,10), col = "red")
lines(density(plob_2016_ppp.bm2$nw, bw = 0.04, from = 0, to = 1), col = "blue")
lines(density(plob_2016_ppp.bm2$w, bw = 0.04, from = 0, to = 1), col = "green")
lines(density(plob_2016_ppp.bm2$se, bw = 0.04, from = 0, to = 1), col = "yellow")

rug(plob_2016$plob[plob_2016$loc=="n"], col = "red", lwd=1.5,pos=10)
rug(plob_2016$plob[plob_2016$loc=="nw"],col="blue", pos=9.75, side = 3,lwd=1.5)
rug(plob_2016$plob[plob_2016$loc=="w"], col="green", pos = 9.5, side = 3, lwd=1.5)
rug(plob_2016$plob[plob_2016$loc=="se"], pos=9.25, col="yellow", side = 3, lwd=1.5)

legend("top", lwd = 2, lty = c(2, 1, 1, 1), col = c("red", "blue", "green", "yellow"), legend = c("n", "nw", "w", "se"), bty = "n")
```
</div>
  
  And another based on `bm3`:
  
  <div class="fold s">
  ```{r}
plob_2016_ppp.bm3 <- data.frame(simulate_beta_fit(plob_2016.bm3))

names(plob_2016_ppp.bm3) <- levels(plob_2016$loc)[c(1, 3, 2, 4)]

plot(density(plob_2016_ppp.bm3$n, bw = 0.04, from = 0, to = 1), xlim = c(0, 1), main = "", xlab = "Proportion cover", ylab = "Probability density", lty = 2, ylim = c(0,10), col = "red")
lines(density(plob_2016_ppp.bm2$nw, bw = 0.04, from = 0, to = 1), col = "blue")
lines(density(plob_2016_ppp.bm2$w, bw = 0.04, from = 0, to = 1), col = "green")
lines(density(plob_2016_ppp.bm2$se, bw = 0.04, from = 0, to = 1), col = "yellow")

rug(plob_2016$plob[plob_2016$loc=="n"], col = "red", lwd=1.5,pos=10)
rug(plob_2016$plob[plob_2016$loc=="nw"],col="blue", pos=9.75, side = 3,lwd=1.5)
rug(plob_2016$plob[plob_2016$loc=="w"], col="green", pos = 9.5, side = 3, lwd=1.5)
rug(plob_2016$plob[plob_2016$loc=="se"], pos=9.25, col="yellow", side = 3, lwd=1.5)

legend("top", lwd = 2, lty = c(2, 1, 1, 1), col = c("red", "blue", "green", "yellow"), legend = c("n", "nw", "w", "se"), bty = "n")
```
</div>
  
  The comparison between the model with constant precision among locations (i.e., treatments) (`bm2`) and the one with varying precision (`bm3`) shows that the latter better matches the distribution of the observations. Again, this supports our earlier finding that a model with variable precision is better able to describe the differences between locations (i.e., treatments).

---
  
  ### Incorporating nested structure with random effects
  
  The models above have pooled the observations in each patch. However we can also used mixed-effects models to allow for the nested structure of observations (`QUADRAT` within `PATCH`), or `photo` within `depth`.

The `betareg` package does not currently allow fitting of mixed-effects beta regression models. 

Such models can be specified and fit using the `glmmTMB` package (https://github.com/glmmTMB). 

The syntax and output are broadly similar to the `glm` function in base R.

First, to ensure we can use and interpret the new function correctly, we try to reproduce the model fit from above with the pooled data:
  
  <div class="fold o">
  
  #Coral
  
  <div class="fold o">
  ```{r}
plob_2016_glm.1 <- glmmTMB(PLOB.scaled ~ loc, data = plob_2016, family = list(family = "beta", link = "logit"))
summary(plob_2016_glm.1)
```
</div>
  
  The parameter estimates and associated significance tests are the same as for `bm2` fitted above using the `betareg` package.

Cover of *Porites lobata* was measured by percent cover in 60 photos (i.e., each with 100 random points) located within the `depths` (i.e., patches). 

To account for the fact that those quadrats were taken from within the same patch, a mixed effect model is appropriate. A mixed effect model allows us to account for a hierarchical structure in the data.

The syntax for specifying a mixed effect model is similar to `betareg` except that the random effect needs to be specified, i.e. the grouping variable. This can be done by adding `(1|depth)`.

#Coral

<div class="fold o">
  ```{r}
# random effect
plob_2016_glm.2 <- glmmTMB(PLOB.scaled ~ loc + (1 | depth), data = plob_2016, family = list(family = "beta", link = "logit"))
summary(plob_2016_glm.2)
```
</div>
  
  The output of the mixed effect model contains three important parts: the random effects estimates, the fixed effects estimates and the estimated dispersion. 

The first part shows that variance in proportion cover among patches is 0.06654 units on the logit scale. 

The fixed parts shows the effect of treatments on  mean proportion cover. 

The dispersion ($\phi$) for the beta model is 2.26 and in this model specification is assumed to be the same for all treatments.

To relax this last assumption we can incorporate a covariate model for the dispersion parameter, equivalent to model `bm3` above.

<div class="fold o">
  ```{r}
plob_2016_glm.3 <- update(plob_2016_glm.2, dispformula = ~loc)
summary(plob_2016_glm.3)

bbmle::AICtab(plob_2016_glm.2, plob_2016_glm.3)
```
</div>
  
  By allowing a covariate model for the dispersion parameter, the summary output changes slightly. 

A summary table on the dispersion parameter ($\phi$) is added for the different locations (i.e., treatments) (including their standard error and significance). 

A comparison of both models by AIC shows that the model allowing for non-constant precision for the treatments is preferred (delta AIC > 2). This corroborates the outcome of the analyses on the pooled observations.

We can compare the fitted distributions between the pooled and mixed-effects versions. 

First we define a convenience function to calculate the parameter estimates for each of the treatments (`coef_to_mean`):
  
  #Coral
  
  <div class="fold o">
  ```{r}
coef_to_mean <- function(model) {
  vals <- fixef(model)$cond
  out <- c(vals[1], vals[2:length(vals)] + vals[1])
  return(out)
}
```
</div>
  
  
  <div class="fold s">
  ```{r fig.height = 10, fig.width=10}
old <- par(mfrow = c(2, 2))

plob_2016_muphi_glm1 <- data.frame(plob_2016_mu = inv.logit(coef_to_mean(plob_2016_glm.1)), precision = sigma(plob_2016_glm.1), treat = levels(plob_2016$loc))

curve(dbeta2(x, plob_2016_muphi_glm1[1, 1], plob_2016_muphi_glm1[1, 2]),
      xlab = "proportion", ylab = "probability density",
      main = "Pooled-data model, fixed phi"
)
abline(v = plob_2016_muphi_glm1[1, 1], lty = 2)

for (i in 2:4) {
  curve(dbeta2(x, plob_2016_muphi_glm1[i, 1], plob_2016_muphi_glm1[i, 2]), add = T, col = i)
  abline(v = plob_2016_muphi_glm1[i, 1], col = i, lty = 2)
}
legend("topright", legend = levels(plob_2016$loc), col = c(1, 2, 3, 4), lty = 1)

plob_2016_muphi_glm2 <- data.frame(mu = inv.logit(coef_to_mean(plob_2016_glm.2)), precision = sigma(plob_2016_glm.2), treat = levels(plob_2016$loc))


curve(dbeta2(x, plob_2016_muphi_glm2[1, 1], plob_2016_muphi_glm2[1, 2]),
      xlab = "proportion", ylab = "probability density",
      main = "Hierarchical model, fixed phi"
)
abline(v = plob_2016_muphi_glm2[1, 1], lty = 2)

for (i in 2:4) {
  curve(dbeta2(x, plob_2016_muphi_glm2[i, 1], plob_2016_muphi_glm2[i, 2]), add = T, col = i)
  abline(v = plob_2016_muphi_glm2[i, 1], col = i, lty = 2)
}
legend("topright", legend = levels(plob_2016$loc), col = c(1, 2, 3, 4), lty = 1)

plob_2016_muphi_glm3 <- data.frame(inv.logit(coef_to_mean(plob_2016_glm.3)), precision = sigma(plob_2016_glm.3), treat = levels(plob_2016$loc))

curve(dbeta2(x, plob_2016_muphi_glm3[1, 1], plob_2016_muphi_glm3[1, 2]),
      xlab = "proportion", ylab = "probability density",
      main = "Hierarchical model, variable phi",
      ylim = c(0,4)
)
abline(v = plob_2016_muphi_glm3[1, 1], lty = 2)
for (i in 2:4) {
  curve(dbeta2(x, plob_2016_muphi_glm3[i, 1], plob_2016_muphi_glm3[i, 2]), add = T, col = i)
  abline(v = plob_2016_muphi_glm3[i, 1], col = i, lty = 2)
}
legend("topright", legend = unique(plob_2016$loc), col = c(1, 2, 3, 4), lty = 1)

par(old)
```

Note that in the above plots, the vertical dashed lines indicate the best-fit mean for each group.

### Posthoc test
The analysis below shows that the control differs from the other treatments. This confirms the findings of earlier fitted  model in which we averaged over patches within treatments.

<div class="fold o">
  ```{r, eval=T,collapse=T,message=FALSE  }

source("https://raw.githubusercontent.com/glmmTMB/glmmTMB/78fbf246e88ae7f23c1ffcc48136278eb982a60a/misc/lsmeans.R")
lsmeans(plob_2016_glm.3, pairwise ~ loc) ## Tukey
plob_2016_f.lsm <- lsmeans(plob_2016_glm.3, "loc")
(plob_2016_f_contr.lsm <- contrast(plob_2016_f.lsm, "trt.vs.ctrl", ref = 1))
```
</div>
  
  The advantage of the mixed effect model compared to the pooled model, is that we get a cleaner partitioning of the variation in *Porites lobata* cover among `depths` (i.e., patches) and the precision parameters. 

In addition, in the mixed effect model we do inference on the observed algae cover values and not on the averaged algae cover values. In the pooled analysis, we observe smaller standard errors in the estimates of treatment effects on mean cover compared to the mixed effect model.

## Using zero-augmented beta regression models to model algae cover.

## Using zero-augmented beta regression models to model coral cover of *Porites lobata*.

Up until now we have used transformations to remove observations of 0 and 1 from the response variable.

However, as mentioned in the main article, it is possible to extend the beta regression model to also handle these kinds of observations.

The package `brms` allows the possibility to specify and fit zero- or one-augmented beta regression models.
We will now use this package  to analyse the original % cover data without applying the transformation used above to "contract" the observed values away from the boundary values of 0 and 1.

First we make a new variable rescaling the original [0-100] observations to [0-1], but leaving them un-transformed.

```{r}

andrew$pALGAE <- andrew$ALGAE / 100
```

We first use fit a model equivalent to the `glm.3` mixed effects above, that is, a model for both the mean and dispersion of algal cover is specified, in both cases as a function of removal treatment. In addition we specify that the observations of individual quadrats are nested within experimental patches.

The syntax for specifying the model formula in the `brm` function is slightly more elaborate than what has been seen in previous examples. A full explanation is available at `?brm` and `?brmsformula`. The specification of the formula should be contained within a call to `brmsformula`. Separate formulas can be specified for the mean model, the phi model, and the zero-augmentation model. Grouping variables can be specified with the `+ (1|GROUP)` syntax.

For the first model `brm.mm`, we do not yet specify a zero or one inflation, so the output is expected to be similar to the random effects beta regression with varying $\phi$ fit above (`glm.3`). We still need to use the transformed response data `ALGAE.scaled`.

```{r second_slow_chunk, cache=TRUE, results="hide", warning=FALSE, message=FALSE}
# beta regression model without zero inflation
brm.mm <- brm(brmsformula(ALGAE.scaled ~ TREAT + (1|PATCH), phi ~ TREAT, family = Beta()), data=andrew, iter=5000)
```

```{r CORAL second_slow_chunk, cache=TRUE, results="hide", warning=FALSE, message=FALSE}
# beta regression model without zero inflation
plob_2016_brm.mm <- brm(brmsformula(PLOB.scaled ~ loc + (1|depth), phi ~ loc, family = Beta()), data = plob_2016, iter = 5000)
```

Note that the distributions of the parameters of the model are estimated using Markov Chain Monte Carlo (MCMC) algorithms. This step therefore takes several minutes to complete.
Note also that the estimate of the coefficients are based on samples from the MCMC chains, and the reliability of interpretations of these estimates depends on the degree to which the MCMC chains have converged on the target density. Convergence criteria for MCMC chains is a very large and technical topic, but here we limit our analysis to a visual inspection the traces of the chains. They should appear stationary (no clear trend) and well mixed (no difference between the two independent chains). We here show the MCMC traces for the first 4 parameters as an illustration.

```{r fig.height=8}

traceplot(brm.mm$fit, pars=c("b_Intercept", "b_phi_Intercept", "b_TREATrem", "b_TREATt0.33"))
```

As a first  check of the results of this new modeling procedure, we compare the posterior means for each coefficient with the MLE estimates obtained in `glm.3`

```{r}
# need to change order of parameters to line up with glmmTMB output
sample.1 <- summary(brm.mm)$fixed[c(1,3,4,5,2,6,7,8),"Estimate"]

plot(sample.1 ~ I(1:8),
     ylim = c(-6, 4), pch = 16, cex = 1.5,
     ylab = "Parameter estimate",
     xlab = "Parameter No."
)

points(summary(glm.3)$coefficients$cond[, 1] ~ I((1:4) + 0.1), pch = 16, col = "red", cex = 1.5)
points(summary(glm.3)$coefficients$disp[, 1] ~ I((5:8) + 0.1), pch = 16, col = "red", cex = 1.5)
legend("topright", legend = c("brm.mm", "glm.3"), pch = 16, pt.cex = 1.5, col = 1:2, bty = "n")
```

From the above graph we conclude that the parameter estimates between the two methods are very similar. Although this is merely an informal comparison, it gives us some confidence in our use of the function. We now proceed to incorporate zero-inflation into the model specification.

As described in the main article, we are now modeling the observations as coming from a two-step process: each observation is first modeled as being generated by a binomial process (determining the probability of a zero observation) and then all non-zero observations are modeled as a Beta process.

This specification is achieved by adding a third component to the  model formula. The code: `zi ~ TREAT` fits a model for which the mean response of the beta process, the associated precision parameter, and the probability of a zero observation are each modeled as a function of treatment (`TREAT`). The argument `family=zero_inflated_beta()` is necessary to signify that a zero-inflation model is desired. Note that for this model we use the raw, untransformed data `pALGAE` that contains observations of zero.


```{r third_slow_chunk, cache=TRUE, results="hide", message=FALSE,warning=FALSE}
# code with zero inflation
brm.zoib <- brm(brmsformula(pALGAE ~ TREAT + (1|PATCH), phi ~ TREAT, zi ~ TREAT, family=zero_inflated_beta()), data=andrew, iter=5000)
```

Because the two models use different response variables (that for `brm.mm` is transformed) we cannot directly compare the models using standard model selection criteria such as AIC or likelihood ratio tests.

For visual comparison we produce a variation of the posterior prediction plots used above. This time with boxplots for each patch and red points showing the observed data.

In the code below we aggregate the posterior fitted values predictions per patch, plot them as boxplots, and then overlay the corresponding observations. This is repeated for both `brm.mm` (random effects only) and `brm.zoib` (random effects plus zero-augmentation model).

```{r fig.height=10}
## compare the treatment distns between brm.mm and brm.zoib
# first zoib.1
andrew$PATCHnum <- as.numeric(as.character(andrew$PATCH))
pred.brm.mm <- fitted(brm.mm, summary=FALSE, subset = 1:1000)

ch1 <- lapply(unique(andrew$PATCH),
              FUN = function(x) as.numeric(pred.brm.mm[,andrew$PATCH == x])
)

par(mfrow = c(2, 1))
boxplot(ch1, border = "gray", ylim = c(0, 1), main = "Random effects")
points(pALGAE ~ jitter(PATCHnum, factor = 0.4),
       data = andrew,
       pch = 16, col = "red"
)

abline(v = c(4.5, 8.5, 12.5), lty = 2, col = "blue")

# now zoib.0
pred.brm.zoib <- fitted(brm.zoib, summary=FALSE, subset = 1:1000)

ch1.0 <- lapply(unique(andrew$PATCH),
                FUN = function(x) as.numeric(pred.brm.zoib[,andrew$PATCH == x])
)

boxplot(ch1.0, border = "gray", ylim = c(0, 1), main = "Zero-augmented random effects")
points(pALGAE ~ jitter(PATCHnum, factor = 0.4),
       data = andrew,
       pch = 16, col = "red"
)

abline(v = c(4.5, 8.5, 12.5), lty = 2, col = "blue")
```

Comparing the two plots reveals that the two model specifications give broadly similar predictions (grey boxplots). The main difference is in the Control treatment (left-most patches in both plots) - for a random effects model fit on transformed data, the model predicts very low, non-zero algal cover with low diversity. Altering the model to allow a zero-generating process that is separated from the process determining algal cover (lower plot) leads to a somewhat broader range of predicted values in these quadrats.

The comparison in the model fits can be further visualized by extracting the coefficients and their associated quantiles from the `brms` model objects.

```{r, warnings=FALSE}
# extract coefficients
summ <- summary(brm.mm)$fixed[c(1,3,4,5,2,6,7,8),c("Estimate", "l-95% CI", "u-95% CI")]

summ0 <- summary(brm.zoib)$fixed[c(1,4,5,6,2,7,8,9,3,10,11,12),c("Estimate", "l-95% CI", "u-95% CI")]

rownames(summ)[1] <- rownames(summ0)[1] <- "TREAT_Intercept"

old <- par(mar = c(3,8,4,2))
plot(summ0[,1], (1:12)-0.15, xlim=c(-7,7), pch=16, yaxt="n", ylab="", mar=c(3,6,2,2))
segments(x0 = summ0[,2], x1 = summ0[,3], y0 = (1:12)-0.15)

points(summ[,1], (1:8)+0.15, col="red")
segments(x0 = summ[,2], x1 = summ[,3], y0 = (1:8)+0.15, col="red")
axis(side = 2, at = 1:12,
     labels = rownames(summ0), las = 1)

abline(v = 0, lty=2)
legend("topright", pch=16, col=c("black", "red"), lty=1,legend = c("Zero augmented","Mixed-effects only"))    

par(old)
```

The parameters and their credible intervals are then plotted alongside each other.


The plot shows that by adding a model zero-augmentation, the estimates for mu in the control treatment have shrunk (i.e. have become less negative). This make sense because the zeros are now accounted for by the zero inflation term. The beta model now only has to fit the non-zero observations.

Since the parameter `TREAT_Intercept`, corresponding to the estimate of the mean in the Control treatment, has slightly increased, the  offsets for each of the other treatments (`TREATrem`, `TREAtt0.33`, `TREATt0.66`) have accordingly slightly decreased.

As expected from inspecting the data, the probability of a zero is modeled to be highest in the control treatment `zi_Intercept` and is considerably reduced for the other three treatments.


Despite these small differences between the models, we conclude that the zero-inflation model does not change our conclusions about the effect of grazer removal on algal cover: only the control treatment differed from all other treatments, and the other treatments did not differ from each other. In this particular case, it could be argued that it was not worthwhile to fit and run a more complicated zero-inflated model, relative to the techniques used above. But this cannot be a general conclusion, and there will certainly be cases where there is value in fitting models that specify a separate zero (or one) generating process, in addition to the beta process producing the non-zero (or one) values.






