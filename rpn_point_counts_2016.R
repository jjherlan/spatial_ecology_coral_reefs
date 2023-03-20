require(tidyverse)
require(car)

ana_sh <- read.csv('ana_sh_2016.csv', header = T) %>%
  as_tibble() %>% 
  mutate_at(vars(transect), factor) %>%
  #group_by(transect) %>%
  #filter(Tdir >= 90 & Tdir < 250) %>%
  pivot_longer(cols = pt1:pt100,
               names_to = "point",
               values_to = "taxa",
               values_drop_na = FALSE
  ) %>%
  group_by(transect, taxa) %>%
  dplyr::summarize(n = n()
  ) %>%
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
  group_by(transect, group) %>%
  dplyr::summarize(successes = sum(n)
  ) %>%
  mutate(
    failures = 1500 - successes)

write.csv(ana_sh, 'ana_sh.csv')

ana_sh2 <-
  ana_sh %>%
  mutate_at(vars(group), factor) %>%
    filter(transect == "four") %>%
      slice(-3) %>%
    drop_na() %>%
    subset(group != "na")

ana_sh2 

ana_sh.glm <- glm(cbind(successes, failures) ~ group, 
                  family = binomial(link = "logit"), data = ana_sh2)
  
summary(ana_sh.glm)

# `Anova` function from the *car* package
Anova(ana_sh.glm, type = "III") # Type III because... 
  

ana_sh3 <-
  ana_sh2 %>%
  mutate(
    cover = successes/1500
  )
  
ana_sh3  

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

write.csv(ana_sh4, 'ana_sh.csv')

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















