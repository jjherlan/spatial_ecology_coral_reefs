require(tidyverse)
require(car)

require(clustsig)
require(vegan)
require(FactoMineR)
require(factoextra)

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

# North Anakena
# Mid Depth

rpn_ana_md <- read.csv('ana_md_2016.csv', header = T) %>%
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

rpn_ana_md %>%
  complete(group)

rpn_ana_md <-
  rpn_ana_md %>%
  complete(group)

write.csv(rpn_ana_md, 'rpn_ana_md.csv')

# West Motu Tautara

rpn_mtt_sh <- read.csv('mtt_sh_2016.csv', header = T) %>%
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

rpn_mtt_sh %>%
  complete(group)

rpn_mtt_sh2 <-
rpn_mtt_sh %>%
  complete(group)

write.csv(rpn_mtt_sh, 'rpn_mtt_sh.csv')
write.csv(rpn_mtt_sh2, 'rpn_mtt_sh2.csv')

# West Motu Tautara Mid Depth

rpn_mtt_md <- read.csv('mtt_md_2016.csv', header = T) %>%
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

rpn_mtt_md %>%
  complete(group)

rpn_mtt_md <-
  rpn_mtt_md %>%
  complete(group)

write.csv(rpn_mtt_md, 'rpn_mtt_md.csv')

# West Motu Tautara Deep Depth

rpn_mtt_dp <- read.csv('mtt_dp_2016.csv', header = T) %>%
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

rpn_mtt_dp %>%
  complete(group)

rpn_mtt_dp <-
  rpn_mtt_dp %>%
  complete(group)

write.csv(rpn_mtt_dp, 'rpn_mtt_dp.csv')

write.csv(rpn_mtt_dp, 'rpn_mtt_dp.csv')

rpn_ana_sh <- read.csv('rpn_ana_sh.csv')

# West Manavai Shallow Depth

rpn_man_sh <- read.csv('man_sh_2016.csv', header = T) %>%
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

rpn_man_sh %>%
  complete(group)

rpn_man_sh <-
  rpn_man_sh %>%
  complete(group)

write.csv(rpn_man_sh, 'rpn_man_sh.csv')

# West Manavai Medium Depth

rpn_man_md <- read.csv('man_md_2016.csv', header = T) %>%
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

# Check
rpn_man_md %>%
  complete(group)

# Save
rpn_man_md <-
  rpn_man_md %>%
  complete(group)

# Save file
write.csv(rpn_man_md, 'rpn_man_md.csv')

# West Manavai Deep Depth

rpn_man_dp <- read.csv('man_dp_2016.csv', header = T) %>%
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

# Check
rpn_man_dp %>%
  complete(group)

# Save
rpn_man_dp <-
  rpn_man_dp %>%
  complete(group)

# Save file
write.csv(rpn_man_dp, 'rpn_man_dp.csv')

# Southeast Shallow Depth

rpn_se_sh <- read.csv('se_sh_2016.csv', header = T) %>%
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

# Check
rpn_se_sh %>%
  complete(group)

# Save
rpn_se_sh <-
  rpn_se_sh %>%
  complete(group)

# Save file
write.csv(rpn_se_sh, 'rpn_se_sh.csv')

# Southeast Mid Depth

rpn_se_md <- read.csv('se_md_2016.csv', header = T) %>%
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

# Check
rpn_se_md %>%
  complete(group)

# Save
rpn_se_md <-
  rpn_se_md %>%
  complete(group)

# Save file
write.csv(rpn_se_md, 'rpn_se_md.csv')

# Southeast Deep Depth

rpn_se_dp <- read.csv('se_dp_2016.csv', header = T) %>%
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

# Check
rpn_se_dp %>%
  complete(group)

# Save
rpn_se_dp <-
  rpn_se_dp %>%
  complete(group)

# Save file
write.csv(rpn_se_dp, 'rpn_se_dp.csv')

# Read files and combine

rpn_ana_sh <- read.csv('rpn_ana_sh.csv')
rpn_ana_md <- read.csv('rpn_ana_md.csv')
rpn_ana_dp <- read.csv('rpn_ana_dp.csv')

rpn_mtt_sh <- read.csv('rpn_mtt_sh.csv')
rpn_mtt_md <- read.csv('rpn_mtt_md.csv')
rpn_mtt_dp <- read.csv('rpn_mtt_dp.csv')

rpn_man_sh <- read.csv('rpn_man_sh.csv')
rpn_man_md <- read.csv('rpn_man_md.csv')
rpn_man_dp <- read.csv('rpn_man_dp.csv')

rpn_se_sh <- read.csv('rpn_se_sh.csv')
rpn_se_md <- read.csv('rpn_se_md.csv')
rpn_se_dp <- read.csv('rpn_se_dp.csv')

rpn_ana_sh
rpn_ana_md
rpn_ana_dp

rpn_mtt_sh
rpn_mtt_md
rpn_mtt_dp

rpn_man_sh
rpn_man_md
rpn_man_dp

rpn_se_sh
rpn_se_md
rpn_se_dp

rpn_point_counts <-
  bind_rows(rpn_ana_sh,
            rpn_ana_md,
            rpn_ana_dp,
            
            rpn_mtt_sh,
            rpn_mtt_md,
            rpn_mtt_dp,
            
            rpn_man_sh,
            rpn_man_md,
            rpn_man_dp,
            
            rpn_se_sh,
            rpn_se_md,
            rpn_se_dp)

write.csv(rpn_point_counts, "rpn_point_counts.csv")

rpn_plob <- read.csv("rpn_point_counts.csv", header = T) %>%
  filter(group == 'plob') %>%
  select(-X)
  
rpn_plob

rpn_plob.glm <- glm(cbind(successes, failures) ~ location * depth, 
                   family = binomial(link = "logit"), data = rpn_plob)

summary(rpn_plob.glm)

par(mfrow = c(2, 2))
plot(rpn_plob.glm)

# `Anova` function from the *car* package
Anova(rpn_plob.glm, type = 'III') # Type III because... 

# Quasi-binomial

rpn_plob.glm_qu <- glm(cbind(successes, failures) ~ location * depth, 
                      family = quasibinomial(link = "logit"), data = rpn_plob)

summary(rpn_plob.glm_qu)

par(mfrow = c(2, 2))
plot(rpn_plob.glm_qu)

# `Anova` function from the *car* package
Anova(rpn_plob.glm_qu, type = "III") # Type III because... 

rpn_benthic_2016_main <- read.csv("rpn_point_counts.csv", header = T) %>%
  mutate(cover = successes/100) %>%
  mutate(coast = case_when(
    reef == "ana" ~ "north",
    reef == "mtt" ~ "northwest",
    reef == "man" ~ "west",
    reef == "mnu" ~ "se",
    reef == "vhu" ~ "se")
  ) %>%
  group_by(coast, group, depth) %>%
  dplyr::summarize(mean = mean(cover), 
                   sd = sd(cover), 
                   n = n(),
                   se = sd/sqrt(n)
  )
    
#write.csv(rpn_benthic_2016_main, "rpn_benthic_2016_main.csv")
  
rpn_benthic_2016_main

rpn_benthic_2016.multi <- read.csv("rpn_point_counts.csv", header = T) %>%
  mutate(cover = successes/100) %>%
  mutate(coast = case_when(
    reef == "ana" ~ "north",
    reef == "mtt" ~ "northwest",
    reef == "man" ~ "west",
    reef == "mnu" ~ "se",
    reef == "vhu" ~ "se")
  ) %>%
  group_by(coast, group, depth) %>%
  mutate_at(vars(coast, group, depth), factor) %>%
  dplyr::summarize(mean = mean(cover)
  )

#write.csv(rpn_benthic_2016_main, "rpn_benthic_2016_main.csv")

rpn_benthic_2016.multi

rpn_benthic_2016.matrix <-
  rpn_benthic_2016.multi %>%
  pivot_wider(names_from = group, values_from = mean) %>%
  select(-'non-reef') %>%
  add_column(site = 1:12) %>%
  mutate_at(vars(site), as.character) %>%
  relocate(site, .before = coast)

rpn_benthic.mat <-
  column_to_rownames(rpn_benthic_2016.matrix, var = 'site')

rpn_benthic.mat2 <- data.matrix(rpn_benthic.mat)

is.matrix(rpn_benthic.mat2)

rpn_benthic.mat3 = as.matrix(rpn_benthic.mat2[,3:8])

rpn_benthic.mat2.dist <- vegdist(rpn_benthic.mat2, method = "bray")

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

location <- 
  rep(c("ana",
        "mtt",
        "man",
        "se"), 
      each = 3)

depth <-
  rep(c("sh",
        "md",
        "dp"),
      times = 4)

wave <- 
  rep(c("low", "med", "high"), times = c(3, 6, 3)
  )

#Environmental factors

rpn_2016.env <-
  rpn_benthic_2016.matrix %>%
  select(coast, depth)

fviz_pca_ind(rpn_benthic_pca2, label = "none", habillage = rpn_2016.env$depth,
             addEllipses = TRUE) + #ellipse.level = 0.95) + 
  #scale_color_brewer(palette = "Set1") +
  theme_minimal()

fviz_pca_ind(rpn_benthic_pca2, label = "none", habillage = rpn_2016.env$coast,
             addEllipses = TRUE) + #ellipse.level = 0.95) + 
  #scale_color_brewer(palette = "Set1") +
  theme_minimal()

rpn_benthic_pca_data <- as.data.frame(
  rpn_benthic_pca2$x[, 1:2]
) # extract first two columns and convert to data frame

rpn_benthic_pca_data2 <- cbind(rpn_benthic_pca_data, rpn_2016.env$coast, rpn_2016.env$depth) # bind by columns

colnames(rpn_benthic_pca_data2) <- c("PC1", "PC2", "coast", "depth") # change column names

ggplot(rpn_benthic_pca_data2) +
  aes(PC1, PC2, color = coast, shape = depth) + # define plot area
  geom_point(size = 4) # adding data points

rpn_benthic.mat3.sqrt_y2

#choose site "column"
rpn_2016_main_location.multivar2 <-
  rpn_2016_main_location.multivar %>%
  add_column(location = c(
    "ana_sh",
    "ana_md",
    "ana_dp",
    "mtt_sh",
    "mtt_md",
    "mtt_dp",
    "man_sh",
    "man_md",
    "man_dp",
    "se_sh",
    "se_md",
    "se_dp"
  )
  ) %>%
  column_to_rownames(var = "location")

rpn_benthic_scale <- scale(rpn_benthic.mat)

rpn_benthic.multi_scaled <- scale(rpn_benthic.mat2)

# Dissimilarity matrix
rpn_benthic.multi_scaled.dm <- dist(rpn_benthic.multi_scaled, method = "euclidean")

# Hierarchical clustering using Complete Linkage
rpn_2016.multivar_scale.hc1 <- hclust(rpn_2016.multivar_scale.dm, method = "complete" )

# Plot the obtained dendrogram
plot(rpn_2016.multivar_scale.hc1, cex = 0.6, hang = -1)

# Agglomerative coefficient, which measures the amount of clustering structure found 
# (values closer to 1 suggest strong clustering structure).

rpn_2016.multivar_scale.hc2 <- agnes(rpn_2016.multivar_scale, method = "complete")

rpn_2016.multivar_scale.hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(rpn_2016.multivar_scale, method = x)$ac
}

map_dbl(m, ac)
##     average    single  complete      ward 
##    0.6535065 0.4587722 0.7560230 0.8196457

rpn_2016.multivar_scale.hc3 <- agnes(rpn_2016.multivar_scale, method = "ward")
pltree(rpn_2016.multivar_scale.hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# Convert to matrix
rpn_2016_main_wide.multivar.mat = as.matrix(rpn_2016_main_wide.multivar)

#Is matrix?
is.matrix(rpn_2016_main_wide.multivar.mat) #yes

#am_sam_tut_2018i.mat <-
#  column_to_rownames(am_sam_tut_2018i.mat, var = "X")

#am_sam_tut_2018i.mat <- data.matrix(am_sam_tut_2018i.mat)

#is.matrix(am_sam_tut_2018i.mat)

# p = 0.05

rpn_2016_main.simprof <- simprof(rpn_2016_main_wide.multivar.mat,
                                 num.expected = 1000, num.simulated = 999,
                                 method.distance = "braycurtis",
                                 method.transform = "squareroot", alpha = 0.05,
                                 sample.orientation = "row", const = 0,
                                 silent = FALSE, increment = 100, 
                                 undef.zero = TRUE, warn.braycurtis = FALSE)

#perform logit [not square root] transformation
rpn_benthic.mat.logit <- logit(rpn_benthic.mat3)

#choose site "column"
rpn_2016_main_wide.multivar.mat.logit2 <-
  as_tibble(rpn_2016_main_wide.multivar.mat.logit) %>%
  add_column(location = c(
    "ana_sh",
    "ana_md",
    "ana_dp",
    "mtt_sh",
    "mtt_md",
    "mtt_dp",
    "man_sh",
    "man_md",
    "man_dp",
    "se_sh",
    "se_md",
    "se_dp"
  )
  ) %>%
  column_to_rownames(var = "location")

# Convert to matrix
rpn_2016_main_wide.multivar.mat.logit3 = as.matrix(rpn_2016_main_wide.multivar.mat.logit2)

# perform logit transformation
# am_sam_tut_2010.sqrt_y <- sqrt(am_sam_tut_2010)

#Need to skip the "...h"
#am_sam_tut_simprof1_2018h <- bind_cols(am_sam_tut_simprof1_2018_env)

#am_sam_tut_2010.sqrt_y2 <- bind_cols(am_sam_tut_2010_env, 
#                                              am_sam_tut_2010.sqrt_y)

#am_sam_tut_2010.mat1 <-
#  column_to_rownames(am_sam_tut_2010.sqrt_y2, var = "site")

#am_sam_tut_2010.mat2 = data.matrix(am_sam_tut_2010.mat1)

#is.matrix(am_sam_tut_2010.mat2)

#write.csv(am_sam_tut_2010.mat2, "am_sam_tut_2010.mat2.csv")

#convert com to a matrix

#com_simrof1_2018 = am_sam_tut_simprof1_2018i[,4:15] # <- Do I use this?
#env_simprof1_2018 = am_sam_tut_simprof1_2018i[,2:3] # <- Do I use this?

#rownames(distmat) <- am_sam_tut_simprof1_2018_env$site


# USE rpn_2016_main_wide.multivar.mat.logit3

rpn_2016.dist <- vegdist(rpn_benthic.mat3, method = "bray")
rpn_2016.dist


#Should I do this?
rpn_2016.jac <- vegdist(rpn_2016_main_wide.multivar.mat.logit3, 
                        method = "jaccard", binary = T)
rpn_2016.jac

# Do not need this
#distmat <- as.matrix(am_sam_tut_simprof1_2018.dist)

#Do I need this?
#distmat2 <- cbind(distmat, am_sam_tut_simprof1_2018_env)

#We can use the simper function in the R package vegan to calculate the contribution 
#of each species to Bray-Curtis dissimilarity between sites in the Plymouth Sound. 
#The simper function will automatically perform pairwise comparisons between sites, 
#and return cumulative contribution scores up to a maximum of 0.7 (70% of total dissimilarity). 

#Can't read output -- what does this do?
#Need further examination
am_sam_tut_simprof1_2018.simper <- simper(am_sam_tut_simprof1_2018g, 
                                          am_sam_tut_simprof1_2018_env$site, permutations = 999)

#PERMANOVA
#PERMANOVA (permutational multivariate analysis of variance; Anderson 2001) is non-parametric multivariate 
#statistical test used to quantify the impact of both continuous and categorical variables on dissimilarity 
#between communities. While it is valid to construct a PERMANOVA model which includes continuous variables, 
#in this instance we will use a simple PERMANOVA model to test the effect of sediment type on meiofaunal 
#community composition. The input for PERMANOVA is a dissimilarity matrix (Bray-Curtis dissimilarity in this case), 
#and corresponding environmental data. A resultant p-value < 0.05 indicates that centroid position and/or dispersion 
#differs between the groups in the model.

#As PERMANOVA is affected by both centroid position and disperison, we perform a homogeneity of dispersions 
#analysis using betadisper to establish whether dispersion is homogeneous between groups (in this case, silt 
#and sand sediment associated communities). We will then perform PERMANOVA analysis using the adonis function in 
#the R package vegan.

#choose site "column"
rpn_2016_main_location.multivar2 <-
  rpn_2016_main_location.multivar %>%
  add_column(location = c(
    "ana_sh",
    "ana_md",
    "ana_dp",
    "mtt_sh",
    "mtt_md",
    "mtt_dp",
    "man_sh",
    "man_md",
    "man_dp",
    "se_sh",
    "se_md",
    "se_dp"
  )
  ) %>%
  column_to_rownames(var = "location")

location <- 
  rep(c("ana",
        "mtt",
        "man",
        "se"), 
      each = 3)

depth <-
  rep(c("sh",
        "md",
        "dp"),
      times = 4)

wave <- 
  
  rep(c("low", "med", "high"), times = c(3, 6, 3)
  )

#choose site "column"
#rpn_2016_main_location.multiva3 <-
#  rpn_2016_main_location.multivar %>%
#  
#  add_column(
#    
#    location = 
#      rep(c("ana",
#            "mtt",
#            "man",
#            "se"), 
#          each = 3),
#    
#    depth =
#      rep(c("sh",
#            "md",
#            "dp"),
#          times = 4),
#      
#      wave =
#        rep(c("low"), each = 3),
#        rep(c("med"), each = 6),
#        rep(c("high"), each = 3)
#          )
#%>%
#  column_to_rownames(var = "location")

#choose site "column"
rpn_2016_main_location.multivar3 <-
  rpn_2016_main_location.multivar %>%
  mutate(  
    
    location = 
      
      rep(c("ana",
            "mtt",
            "man",
            "se"), 
          each = 3), 
    
    depth =
      
      rep(c("sh",
            "md",
            "dp"),
          times = 4), 
    
    wave =
      
      rep(c("low", "med", "high"), times = c(3, 6, 3)
          
      )
  ) %>%
  
  mutate_at(vars(location, depth, wave), factor)

#%>%
#  column_to_rownames(var = "location")

#Environmental factors

rpn_2016.env <-
  rpn_2016_main_location.multivar3 %>%
  select(location, depth, wave)

# Benthic coverage data

rpn_2016_main_location.multivar

# as matrix

# Convert to matrix
rpn_2016.mat = as.matrix(rpn_2016_main_location.multivar)

#perform square root transformation
rpn_2016.mat.sqrt <- sqrt(rpn_2016.mat)

rpn_2016.dist <- vegdist(rpn_2016.mat, 
                         method = "bray")

rpn_2016.dist

# Homogeneity of dispersion test
permutest(betadisper(rpn_2016.dist, rpn_2016.env$wave))

# PERMANOVA analysis

adonis2(rpn_2016.mat.sqrt ~ wave, data = rpn_2016.env, permutations = 999, 
        method = "bray")

rpn_benthic.pca1 <- PCA(rpn_2016.mat.sqrt, scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(rpn_benthic.pca1)

rpn_benthic.pca2 <- prcomp(rpn_2016.mat.sqrt,  scale = TRUE)
summary(rpn_benthic.pca2)
rpn_benthic.pca2
plot(rpn_benthic.pca2)
str(rpn_benthic.pca2)
head(rpn_benthic.pca2$x)

rpn_benthic.pca3 <- rda(rpn_2016.mat.sqrt)
summary(rpn_benthic.pca3)

fviz_pca_ind(rpn_benthic.pca2)

fviz_pca_var(rpn_benthic.pca2)
fviz_pca_biplot(rpn_benthic.pca2)

fviz_pca_ind(rpn_benthic.pca2, label = "none", habillage = rpn_2016.env$depth,
             addEllipses = TRUE) + #ellipse.level = 0.95) + 
  #scale_color_brewer(palette = "Set1") +
  theme_minimal()

fviz_pca_ind(rpn_benthic.pca2, label = "none", habillage = rpn_2016.env$wave,
             addEllipses = TRUE) + #ellipse.level = 0.95) + 
  #scale_color_brewer(palette = "Set1") +
  theme_minimal()

rpn_benthic_pca_data <- as.data.frame(
  rpn_benthic.pca2$x[, 1:2]
) # extract first two columns and convert to data frame

rpn_benthic_pca_data2 <- cbind(rpn_benthic_pca_data, rpn_2016.env$wave, rpn_2016.env$depth) # bind by columns

colnames(rpn_benthic_pca_data2) <- c("PC1", "PC2", "wave", "depth") # change column names

ggplot(rpn_benthic_pca_data2) +
  aes(PC1, PC2, color = wave, shape = depth) + # define plot area
  geom_point(size = 4) # adding data points

#From Husson et al. (2011) Chapter 1

# To perform this analysis, we use the PCA function of the FactoMineR package.
# Its main input parameters are: the dataset, whether or not the variables
# are standardised, the position of the quantitative supplementary variables
# in the dataset, and the position of the categorical variables in the dataset
# (supplementary by definition). By default, all of the variables are standardised
# (scale.unit=TRUE, a parameter that does not need to be defined), and none
# of the variables are supplementary (quanti.sup=NULL and quali.sup=NULL
# in other words, all the variables are both quantitative and active).

#pca1 <- PCA(am_sam_tut_simprof1_2018.sqrt_y2[,5:60], scale.unit = TRUE, ncp = 5, graph = TRUE)

#pca2 <- PCA(am_sam_tut_simprof1_2018.sqrt_y2[,5:60], quali.sup = c(2,4),
#            scale.unit = TRUE, ncp = 5, graph = TRUE)

rpn_benthic.rda <- rda(rpn_benthic.mat.logit)

rpn_benthic.logit.df <- as.data.frame(rpn_benthic.mat.logit)

ev <- envfit(rpn_benthic.rda ~ ., data = rpn_benthic.logit.df,
             choices = 1:2,
             scaling = "symmetric",
             permutations = 999)

# PCA Analysis
# The PCA code for farm stress data is shown below. Generally, any PCA’s first step is to scale the data so that each feature 
# is comparable, followed by mean centering, shifting the data on their axes without altering any stochastic properties. 
# In this specific case, but generally, not so, scaling isn’t necessary since each column is measured using the same Likert-type scale.

# We use the prcomp() function to do the PCA and subsequently extract the loadings, scores matrix, and feature importance as 
# measured by how much each of the variability each PC axis explains in the observed data.

# Make the features comparable by centering each column around its mean.  Since the data are already on the same scale, we do not need to also divide by their respective variance (e.g., scale = FALSE).
# rpn_benthic.mat3.sqrt_y <- scale(rpn_benthic.mat3.sqrt_y, center = TRUE, scale = FALSE)

# Make the features comparable by centering each column around its mean.  Since the data are already on the same scale, we do not need to also divide by their respective variance (e.g., scale = FALSE).
rpn_2016.scale <- scale(rpn_benthic.mat.logit, center = TRUE, scale = FALSE)

# This function does all of the math to create the PC matrix factorization.
# rpn_benthic.mat3.sqrt_y_pca <- prcomp(rpn_benthic.mat3.sqrt_y, center = FALSE, scale. = FALSE, retx = TRUE)

# This function does all of the math to create the PC matrix factorization.
rpn_2016.scale_PC <- prcomp(rpn_benthic.mat.logit, center = FALSE, scale. = FALSE, retx = TRUE)

# Pieces from the PCA used in the biplot.
# rpn_benthic.mat3.sqrt_y_pca_loadings <- rpn_benthic.mat3.sqrt_y_pca$rotation # Loadings Matrix
# rpn_benthic.mat3.sqrt_y_pca_scores <- predict(rpn_benthic.mat3.sqrt_y_pca) # Scores Matris
# rpn_benthic.mat3.sqrt_y_pca_importance <- summary(rpn_benthic.mat3.sqrt_y_pca)$importance # Explained Variance

# Pieces from the PCA used in the biplot.
rpn_2016.scale_PC_loadings <- rpn_2016.scale_PC$rotation # Loadings Matrix
rpn_2016.scale_PC_scores <- predict(rpn_2016.scale_PC) # Scores Matris
rpn_2016.scale_PC_importance <- summary(rpn_2016.scale_PC)$importance # Explained Variance


# The plot data includes occupation and the first two PC components.
# plotDataScores <- cbind(coast = rpn_benthic_env$coast, depth = rpn_benthic_env$depth, 
#                        as.data.frame(rpn_benthic.mat3.sqrt_y_pca_scores[, c("PC1", "PC2")]
#                        )
#)

# The plot data includes occupation and the first two PC components.
plot_data_scores <- cbind(wave = rpn_2016.env$coast, depth = rpn_2016.env$depth, 
                          as.data.frame(rpn_2016.scale_PC_scores[, c("PC1", "PC2")]
                          )
)


# We want to use min-max feature scaling on the scores so they are between zero and one, the same as the loadings.
normalize <- function(x) return ((x - min(x)) / (max(x) - min(x)))

plot_data_scores[, "PC1"] <- scale(normalize(plot_data_scores[, "PC1"]), center = TRUE, scale = FALSE)
plot_data_scores[, "PC2"] <- scale(normalize(plot_data_scores[, "PC2"]), center = TRUE, scale = FALSE)

plot_data_loadings <- as.data.frame(rpn_2016.scale_PC_loadings)

cols <- c("high" = "red", "med" = "blue", "low" = "darkgreen")

p1 <- ggplot() + 
  geom_point(data = plot_data_scores, mapping = aes(x = PC1, y = PC2, 
                                                    color = wave, shape = depth),
             size = 4
  ) + 
  scale_colour_manual(values = cols)

p1

p2 <- p1 + 
  geom_segment(data = plot_data_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2, colour = "darkblue") + 
  
  geom_text(data = plot_data_loadings, mapping = aes(x = PC1, y = PC2, 
                                                     label = rownames(plot_data_loadings)), 
            hjust = 1, vjust = -0.2, colour = "darkred", size = 4, check_overlap = TRUE)

p2


p3 <- p2 + 
  xlab(paste("PC1 (", round(rpn_2016.scale_PC_importance["Proportion of Variance", "PC1"]*100, 1),"%)", sep = "")) + 
  ylab(paste("PC2 (", round(rpn_2016.scale_PC_importance["Proportion of Variance", "PC2"]*100, 1),"%)", sep = ""))

p3

p4 <- p3 + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 50, hjust = 1),
        axis.title = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "right",
        panel.grid = element_line(color = "lightgray"),
        panel.background = element_rect(fill = "white", colour = "white"))

p4

##################################################################
## DEPTH-- Now let's do the same comparing by depth
##################################################################

am_sam_tut_coral_nmds7

am_sam_tut_10_18_high <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010" | obs_year == "2018", exposure == "high")

#But use tidyverse! -- Very sloppy

am_sam_tut_10_18_site_high <-
  am_sam_tut_10_18_high %>%
  select(site)

am_sam_tut_10_18_high_env <-
  am_sam_tut_10_18_high %>%
  select(site, depth_bin, obs_year, exposure)

write.csv(am_sam_tut_10_18_high_env, "am_sam_tut_10_18_high_env.csv")

is.data.frame(am_sam_tut_10_18_high_env)

# Need to change anything
am_sam_tut_10_18_high_main <- am_sam_tut_10_18_high[, sapply(am_sam_tut_10_18_high, 
                                                             function(col) is.numeric(col) && any(col >= 0.01)), 
                                                    drop = FALSE]

#perform square root transformation
am_sam_tut_10_18_high.sqrt_y <- sqrt(am_sam_tut_10_18_high_main)

#Need to skip the "...h"
#am_sam_tut_simprof1_2018h <- bind_cols(am_sam_tut_simprof1_2018_env)

am_sam_tut_10_18_high.sqrt_y2 <- bind_cols(am_sam_tut_10_18_high_env, 
                                           am_sam_tut_10_18_high.sqrt_y)

am_sam_tut_10_18_high.mat <-
  column_to_rownames(am_sam_tut_10_18_high.sqrt_y2, var = "site")

am_sam_tut_10_18_high.mat2 = data.matrix(am_sam_tut_10_18_high.mat)

is.matrix(am_sam_tut_10_18_high.mat2)

write.csv(am_sam_tut_10_18_high.mat2, "am_sam_tut_10_18_high.mat2.csv")

#convert com to a matrix

#com_simrof1_2018 = am_sam_tut_simprof1_2018i[,4:15] # <- Do I use this?
#env_simprof1_2018 = am_sam_tut_simprof1_2018i[,2:3] # <- Do I use this?

#rownames(distmat) <- am_sam_tut_simprof1_2018_env$site

am_sam_tut_10_18_high.mat2.dist <- vegdist(am_sam_tut_10_18_high.mat2, 
                                           method = "bray")
am_sam_tut_10_18_high.mat2.dist


#Should I do this?
am_sam_tut_simprof1_2018.jac <- vegdist(am_sam_tut_simprof1_2018i.mat, 
                                        method = "jaccard", binary = T)
am_sam_tut_simprof1_2018.jac

# Do not need this
#distmat <- as.matrix(am_sam_tut_simprof1_2018.dist)

#Do I need this?
#distmat2 <- cbind(distmat, am_sam_tut_simprof1_2018_env)

#We can use the simper function in the R package vegan to calculate the contribution 
#of each species to Bray-Curtis dissimilarity between sites in the Plymouth Sound. 
#The simper function will automatically perform pairwise comparisons between sites, 
#and return cumulative contribution scores up to a maximum of 0.7 (70% of total dissimilarity). 

#Can't read output -- what does this do?
#Need further examination
am_sam_tut_simprof1_2018.simper <- simper(am_sam_tut_simprof1_2018g, 
                                          am_sam_tut_simprof1_2018_env$site, permutations = 999)

#PERMANOVA
#PERMANOVA (permutational multivariate analysis of variance; Anderson 2001) is non-parametric multivariate 
#statistical test used to quantify the impact of both continuous and categorical variables on dissimilarity 
#between communities. While it is valid to construct a PERMANOVA model which includes continuous variables, 
#in this instance we will use a simple PERMANOVA model to test the effect of sediment type on meiofaunal 
#community composition. The input for PERMANOVA is a dissimilarity matrix (Bray-Curtis dissimilarity in this case), 
#and corresponding environmental data. A resultant p-value < 0.05 indicates that centroid position and/or dispersion 
#differs between the groups in the model.

#As PERMANOVA is affected by both centroid position and disperison, we perform a homogeneity of dispersions 
#analysis using betadisper to establish whether dispersion is homogeneous between groups (in this case, silt 
#and sand sediment associated communities). We will then perform PERMANOVA analysis using the adonis function in 
#the R package vegan.

# Homogeneity of dispersion test
permutest(betadisper(am_sam_tut_10_18_high.mat2.dist, am_sam_tut_10_18_high_env$obs_year))

# PERMANOVA analysis

adonis(am_sam_tut_10_18_high.mat2 ~ obs_year, data = am_sam_tut_10_18_high_env, permutations = 999, 
       method = "bray")

###############################################################################
## LOW WAVE EXPOSURE SITES -- Now let's do the same comparing 2010 and 2018 ##
###############################################################################

am_sam_tut_coral_nmds7

am_sam_tut_10_18_low <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010" | obs_year == "2018", exposure == "low")

#But use tidyverse! -- Very sloppy

am_sam_tut_10_18_site_low <-
  am_sam_tut_10_18_low %>%
  select(site)

am_sam_tut_10_18_low_env <-
  am_sam_tut_10_18_low %>%
  select(site, depth_bin, obs_year, exposure)

is.data.frame(am_sam_tut_10_18_low_env)

write.csv(am_sam_tut_10_18_low_env, "am_sam_tut_10_18_low_env.csv")

# Need to change anything
am_sam_tut_10_18_low_main <- am_sam_tut_10_18_low[, sapply(am_sam_tut_10_18_low, 
                                                           function(col) is.numeric(col) && any(col >= 0.01)), 
                                                  drop = FALSE]

#perform square root transformation
am_sam_tut_10_18_low.sqrt_y <- sqrt(am_sam_tut_10_18_low_main)

#Need to skip the "...h"
#am_sam_tut_simprof1_2018h <- bind_cols(am_sam_tut_simprof1_2018_env)

am_sam_tut_10_18_low.sqrt_y2 <- bind_cols(am_sam_tut_10_18_low_env, 
                                          am_sam_tut_10_18_low.sqrt_y)

am_sam_tut_10_18_low.mat <-
  column_to_rownames(am_sam_tut_10_18_low.sqrt_y2, var = "site")

am_sam_tut_10_18_low.mat2 = data.matrix(am_sam_tut_10_18_low.mat)

is.matrix(am_sam_tut_10_18_low.mat2)

write.csv(am_sam_tut_10_18_low.mat2, "am_sam_tut_10_18_low.mat2.csv")

#convert com to a matrix

#com_simrof1_2018 = am_sam_tut_simprof1_2018i[,4:15] # <- Do I use this?
#env_simprof1_2018 = am_sam_tut_simprof1_2018i[,2:3] # <- Do I use this?

#rownames(distmat) <- am_sam_tut_simprof1_2018_env$site

am_sam_tut_10_18_low.mat2.dist <- vegdist(am_sam_tut_10_18_low.mat2, 
                                          method = "bray")
am_sam_tut_10_18_low.mat2.dist


#Should I do this?
#am_sam_tut_simprof1_2018.jac <- vegdist(am_sam_tut_simprof1_2018i.mat, 
#                                        method = "jaccard", binary = T)
#am_sam_tut_simprof1_2018.jac

# Do not need this
#distmat <- as.matrix(am_sam_tut_simprof1_2018.dist)

#Do I need this?
#distmat2 <- cbind(distmat, am_sam_tut_simprof1_2018_env)

#We can use the simper function in the R package vegan to calculate the contribution 
#of each species to Bray-Curtis dissimilarity between sites in the Plymouth Sound. 
#The simper function will automatically perform pairwise comparisons between sites, 
#and return cumulative contribution scores up to a maximum of 0.7 (70% of total dissimilarity). 

#Can't read output -- what does this do?
#Need further examination
#am_sam_tut_simprof1_2018.simper <- simper(am_sam_tut_simprof1_2018g, 
#                                          am_sam_tut_simprof1_2018_env$site, permutations = 999)

#PERMANOVA
#PERMANOVA (permutational multivariate analysis of variance; Anderson 2001) is non-parametric multivariate 
#statistical test used to quantify the impact of both continuous and categorical variables on dissimilarity 
#between communities. While it is valid to construct a PERMANOVA model which includes continuous variables, 
#in this instance we will use a simple PERMANOVA model to test the effect of sediment type on meiofaunal 
#community composition. The input for PERMANOVA is a dissimilarity matrix (Bray-Curtis dissimilarity in this case), 
#and corresponding environmental data. A resultant p-value < 0.05 indicates that centroid position and/or dispersion 
#differs between the groups in the model.

#As PERMANOVA is affected by both centroid position and dispersion, we perform a homogeneity of dispersions 
#analysis using betadisper to establish whether dispersion is homogeneous between groups (in this case, silt 
#and sand sediment associated communities). We will then perform PERMANOVA analysis using the adonis function in 
#the R package vegan.

# Homogeneity of dispersion test
permutest(betadisper(am_sam_tut_10_18_low.mat2.dist, am_sam_tut_10_18_low_env$obs_year)
)

# PERMANOVA analysis

adonis(am_sam_tut_10_18_low.mat2 ~ obs_year, data = am_sam_tut_10_18_low_env, 
       permutations = 999, 
       method = "bray")

#levels(am_sam_tut_coral_nmds7$exposure)[levels(am_sam_tut_coral_nmds7$exposure)=="low"] <- "Low"
#levels(am_sam_tut_coral_nmds7$exposure)[levels(am_sam_tut_coral_nmds7$exposure)=="high"] <- "High"

# What is going on here?

am_sam_tut_coral_nmds7$depth_bin <- factor(am_sam_tut_coral_nmds7$depth_bin,      # Reordering group factor levels
                                           levels = c("Shallow", "Mid", "Deep")
)

am_sam_tut_coral_nmds7$exposure <- factor(am_sam_tut_coral_nmds7$exposure,      # Reordering group factor levels
                                          levels = c("low", "high")
)

x_labels = c("low" = "Low", "high" = "High")

label_names = c("Shallow" = "Shallow", "Mid" = "Middle", "Deep" = "Deep")

p <- ggplot(am_sam_tut_coral_nmds7, 
            aes(x = exposure, y = coral_sum, fill = obs_year)
) +
  scale_y_continuous(limits = c(0, 1.0), labels = function(x) paste0(x * 100)) +  
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(breaks = c("2010", "2012", "2015", "2016", "2018"),
                    values = c("red", "blue", 
                               "green", "orange", "purple"), 
                    labels = c("2010", "2012", "2015", "2016", "2018")
  ) +
  facet_wrap( ~ depth_bin, dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Tutuila: coral cover between 2010 and 2018")))) +
  ylab(expression(paste("Coral Cover (%)"))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 12, color = "white", hjust = 0.5),
        strip.background = element_rect(fill = "#858585", color = NA),    
        panel.background = element_rect(fill = "#efefef", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        legend.position="right",
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title=element_blank()
  )

p + geom_boxplot()

# Graph the categories (benthic species) that drove the differences in the sites 
# distinguished by the simprof funciton

#Do not need
#am_sam_tut_2018 <-
#  am_sam_tut_coral_nmds7 %>%
#  filter(obs_year == "2018")

am_sam_tut_2018 <- am_sam_tut_simprof1_2018e

am_sam_tut_2018b <- 
  am_sam_tut_2018 %>%
  mutate(simprof = forcats::fct_recode(simprof,
                                       "1" = "blue",
                                       "2" = "darkblue",
                                       "3" = "darkpink1",
                                       "4" = "darkpink2",
                                       "5" = "darkpurple1",
                                       "6" = "green",
                                       "7" = "lightgreen1",
                                       "8" = "lightgreen2",
                                       "9" = "lightgreen3",
                                       "10" = "lightorange",
                                       "11" = "lightpurple2",
                                       "12" = "lightpurple3",
                                       "13" = "limegreen",
                                       "14" = "orange",
                                       "15" = "purple",
                                       "16" = "red",
                                       "17" = "turquoise",
                                       "18" = "turquoise1",
                                       "19" = "turquoise2",
                                       "20" = "yellow"
  )
  )

# 1 Acropora Branching (acbr)

am_sam_tut_acbr_2018 <- 
  am_sam_tut_2018b %>%
  #mutate(simprof = fct_reorder(acbr, simprof)) %>%
  ggplot( aes(x = reorder(simprof, -acbr), y= acbr 
              #fill = simprof
  )
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.0), labels = function(x) paste0(x * 100)) +  
  scale_x_discrete(expand = c(0, 1) 
                   #labels = x_labels
  ) + 
  #scale_fill_manual(breaks = c("2010", "2012", "2015", "2016", "2018"),
  #                  values = c("red", "blue", 
  #                             "green", "orange", "purple"), 
  #                  labels = c("2010", "2012", "2015", "2016", "2018")
  #) +
  #facet_wrap( ~ depth_bin, dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Acropora branching (acbr): American Samoa, Tutuila - 2018")))) +
  ylab(expression(paste("Aerial Coverage (%)"))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 12, color = "white", hjust = 0.5),
        strip.background = element_rect(fill = "#858585", color = NA),    
        panel.background = element_rect(fill = "#efefef", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        legend.position="right",
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title=element_blank()
  )

am_sam_tut_acbr_2018 + geom_boxplot()

# 2 Acropora Tabulate (acta)

am_sam_tut_acta_2018 <- 
  am_sam_tut_2018b %>%
  #mutate(simprof = fct_reorder(acbr, simprof)) %>%
  ggplot( aes(x = reorder(simprof, -acta), y = acta
              #fill = simprof
  )
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.0), labels = function(x) paste0(x * 100)) +  
  scale_x_discrete(expand = c(0, 1) 
                   #labels = x_labels
  ) + 
  #scale_fill_manual(breaks = c("2010", "2012", "2015", "2016", "2018"),
  #                  values = c("red", "blue", 
  #                             "green", "orange", "purple"), 
  #                  labels = c("2010", "2012", "2015", "2016", "2018")
  #) +
  #facet_wrap( ~ depth_bin, dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Acropora tabulate (acta): American Samoa, Tutuila - 2018")))) +
  ylab(expression(paste("Aerial Coverage (%)"))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 12, color = "white", hjust = 0.5),
        strip.background = element_rect(fill = "#858585", color = NA),    
        panel.background = element_rect(fill = "#efefef", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        legend.position="right",
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title=element_blank()
  )

am_sam_tut_acta_2018 + geom_boxplot()

# 3 Acropora Tabulate (assp)

am_sam_tut_acta_2018 <- 
  am_sam_tut_2018b %>%
  #mutate(simprof = fct_reorder(acbr, simprof)) %>%
  ggplot( aes(x = reorder(simprof, -acta), y = acta
              #fill = simprof
  )
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.0), labels = function(x) paste0(x * 100)) +  
  scale_x_discrete(expand = c(0, 1) 
                   #labels = x_labels
  ) + 
  #scale_fill_manual(breaks = c("2010", "2012", "2015", "2016", "2018"),
  #                  values = c("red", "blue", 
  #                             "green", "orange", "purple"), 
  #                  labels = c("2010", "2012", "2015", "2016", "2018")
  #) +
  #facet_wrap( ~ depth_bin, dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Acropora tabulate (acta): American Samoa, Tutuila - 2018")))) +
  ylab(expression(paste("Aerial Coverage (%)"))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 12, color = "white", hjust = 0.5),
        strip.background = element_rect(fill = "#858585", color = NA),    
        panel.background = element_rect(fill = "#efefef", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        legend.position="right",
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title=element_blank()
  )

am_sam_tut_acta_2018 + geom_boxplot()

for(i in 2:ncol(am_sam_tut_2018b)) {                              # Printing ggplot within for-loop
  print(ggplot(am_sam_tut_2018b, aes(x = simprof, y = am_sam_tut_2018b[ , i])) +
          geom_boxplot())
  Sys.sleep(2)
}

am_sam_tut_en_2018 <- 
  am_sam_tut_2018b %>%
  select('simprof',
         'acbr',
         'acta' ,
         'assp',
         'ccah',
         'fasp',
         'favs',
         'gasp',
         'gons',
         'hali',
         'hysp',
         'issp',
         'lept',
         'lobo',
         'moen',
         'pesp',
         'pocs',
         'poma',
         'turs',
         'bgma',
         'brma',
         'ccar',
         'ema',
         'enc',
         'fine',
         'hard',
         'lphy',
         'mass',
         'mofo',
         'octo',
         'paen',
         'pobr',
         'poen',
         'pofo',
         'rdma',
         'rub',
         'sand',
         'tun',
         'turfh',
         'turfr',
         'zo'
  )

am_sam_tut_en_2018b <-
  
  am_sam_tut_en_2018 %>%
  
  pivot_longer(!simprof, names_to = "species", values_to = "cover")

# Helper function
Iris_plot <- function(df = am_sam_tut_en_2018b, cover) {
  ggplot(am_sam_tut_en_2018b, aes(x = simprof, y = !! sym(cover) )) + 
    geom_boxplot(notch = TRUE) +
    theme_classic(base_size = 10)
}

# Main loop through the columns and dataset
for(i in 1:ncol(am_sam_tut_en_2018b)){
  name <- paste0(cover, "_x_Species")
  png(paste0(name, ".png"))
  print(Iris_plot(df = am_sam_tut_en_2018b, y = cover))
  dev.off()
}

ggplot(pivot_longer(basePlot, -cut, names_to="var", values_to="val"),
       aes(cut, val, color=cut)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
  xlab("Diamond Cut") +
  facet_wrap(~var, nrow=2, scales="free") +
  scale_x_discrete(guide=guide_axis(n.dodge=2))


am_sam_tut_en_2018.all <- 
  am_sam_tut_en_2018b %>%
  #mutate(simprof = fct_reorder(acbr, simprof)) %>%
  ggplot( aes(x = reorder(simprof, -cover), y = cover
              #fill = simprof
  )
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.0), labels = function(x) paste0(x * 100)) +  
  #scale_x_discrete(expand = c(0, 1) 
  #labels = x_labels
  #) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)
  ) +
  #scale_fill_manual(breaks = c("2010", "2012", "2015", "2016", "2018"),
  #                  values = c("red", "blue", 
  #                             "green", "orange", "purple"), 
  #                  labels = c("2010", "2012", "2015", "2016", "2018")
  #) +
  #facet_wrap( ~ depth_bin, dir = "v", ncol = 1) +
  #ggtitle(expression(paste(italic(" Acropora tabulate (acta): American Samoa, Tutuila - 2018")))) +
  ylab(expression(paste("Aerial Coverage (%)"))) +
  labs(x = NULL) +
  facet_wrap( ~ species, ncol = 2, scales = "free") +
  theme(strip.text = element_text(size = 6, color = "white", hjust = 0.5),
        strip.background = element_rect(fill = "#858585", color = NA),    
        panel.background = element_rect(fill = "#efefef", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        legend.position="right",
        #plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 5.5),
        legend.title=element_blank()
  )

am_sam_tut_en_2018.all + geom_boxplot()

# t-tests

# 2010, coral cover - low vs. high wave exposure

am_sam_tut_coral_sum_sh_2010 <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010", depth_bin == "Shallow") %>%
  select(exposure, coral_sum)

t.test(am_sam_tut_coral_sum_sh_2010$coral_sum ~ am_sam_tut_coral_sum_sh_2010$exposure, method = "holm")

am_sam_tut_coral_sum_md_2010 <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010", depth_bin == "Mid") %>%
  select(exposure, coral_sum) %>%
  summarize(mean = mean(coral_sum), n = n())

am_sam_tut_coral_sum_sh_2010 <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010", depth_bin == "Deep") %>%
  select(exposure, coral_sum)

# 2010 all depths and exposures

am_sam_tut_coral_sum_2010 <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010") %>%
  select(depth_bin, exposure, coral_sum)

am_sam_tut_coral_sum_2010b <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010") %>%
  select(depth_bin, exposure, coral_sum) %>%
  group_by(depth_bin, exposure) %>%
  summarize(mean = mean(coral_sum), n = n())

# Raw data

am_sam_tut_coral_sum_2010  %>%
  #pivot_longer(cols = north_sh:se_dp) %>%
  #mutate(name = factor(name, levels = c("North Sh", "North Md", "North Dp", 
  #                                    "NW Sh", "NW Md", "NW Dp", 
  #                                    "West Sh", "West Md", "West Dp", 
  #                                    "SE Sh", "SE Md", "SE Dp"))) %>%
  ggplot() + 
  #geom_histogram(mapping = aes(x = coral_sum, y = ..count..), 
  #               bins = 20, alpha = 0.7,
  #               fill = "gray40", size = 0.5) + 
  geom_histogram(mapping = aes(x = coral_sum, y = ..count.., 
                               #color = exposure, 
                               fill = exposure
  ), 
  stat = "bin", bins = 20, size = 0.5,
  alpha = 0.7) + 
  #scale_color_manual(values = c('darkred', 'darkred', 'darkred', 
  #                              'darkblue', 'darkblue', 'darkblue'  
  #'darkgreen', 'darkgreen', 'darkgreen', 
  #'yellow', 'yellow', 'yellow' 
  #)
  #) + 
  scale_fill_manual(values = c('blue', 'red' 
                               #                               'blue', 'blue'  
                               #                               'green', 'green', 'green', 
                               #                               'yellow', 'yellow', 'yellow' 
  )
  ) + 
  guides(color = "none", fill = "none") + 
  labs(x = "Measure", y = "Count", 
       title = "Coral Cover in 2010: Raw Data", 
       subtitle = "Tutuila, American Samoa") + 
  facet_grid(vars(depth_bin), vars(exposure)
  )

# Logit transformation

require(car)

coral_sum.logit <- logit(am_sam_tut_coral_sum_2010$coral_sum)

am_sam_tut_coral_sum_2010.logit <-
  #am_sam_tut_coral_sum_2010 %>%
  bind_cols(am_sam_tut_coral_sum_2010, coral_sum.logit)

am_sam_tut_coral_sum_2010.logit2 <-
  rename(am_sam_tut_coral_sum_2010.logit, coral_sum_logit = ...4)

# Graph here

am_sam_tut_coral_sum_2010.logit2  %>%
  #pivot_longer(cols = north_sh:se_dp) %>%
  #mutate(name = factor(name, levels = c("North Sh", "North Md", "North Dp", 
  #                                    "NW Sh", "NW Md", "NW Dp", 
  #                                    "West Sh", "West Md", "West Dp", 
  #                                    "SE Sh", "SE Md", "SE Dp"))) %>%
  ggplot() + 
  #geom_histogram(mapping = aes(x = coral_sum, y = ..count..), 
  #               bins = 20, alpha = 0.7,
  #               fill = "gray40", size = 0.5) + 
  geom_histogram(mapping = aes(x = coral_sum_logit, y = ..count.., 
                               #color = exposure, 
                               fill = exposure
  ), 
  stat = "bin", bins = 20, size = 0.5,
  alpha = 0.7) + 
  #scale_color_manual(values = c('darkred', 'darkred', 'darkred', 
  #                              'darkblue', 'darkblue', 'darkblue'  
  #'darkgreen', 'darkgreen', 'darkgreen', 
  #'yellow', 'yellow', 'yellow' 
  #)
  #) + 
  scale_fill_manual(values = c('blue', 'red' 
                               #                               'blue', 'blue'  
                               #                               'green', 'green', 'green', 
                               #                               'yellow', 'yellow', 'yellow' 
  )
  ) + 
  guides(color = "none", fill = "none") + 
  labs(x = "Measure", y = "Count", 
       title = "Comparing Reef Sites at Rapa Nui: Sizes of Pocilloporid Colonies", 
       subtitle = "Line transects, 2016; Overall distribution shown in gray") + 
  facet_grid(vars(depth_bin), vars(exposure))

# t test for transformed data

# Shallow

am_sam_tut_coral_sum_sh_2010.logit <-
  am_sam_tut_coral_sum_2010.logit2 %>%
  filter(depth_bin == "Shallow") %>%
  select(depth_bin, exposure, coral_sum_logit)


t.test(am_sam_tut_coral_sum_sh_2010.logit$coral_sum_logit ~ am_sam_tut_coral_sum_sh_2010.logit$exposure, 
       method = "holm")

# Mid

am_sam_tut_coral_sum_md_2010.logit <-
  am_sam_tut_coral_sum_2010.logit2 %>%
  filter(depth_bin == "Mid") %>%
  select(depth_bin, exposure, coral_sum_logit)

t.test(am_sam_tut_coral_sum_md_2010.logit$coral_sum_logit ~ am_sam_tut_coral_sum_md_2010.logit$exposure, 
       method = "holm")

# Deep

am_sam_tut_coral_sum_dp_2010.logit <-
  am_sam_tut_coral_sum_2010.logit2 %>%
  filter(depth_bin == "Deep") %>%
  select(depth_bin, exposure, coral_sum_logit)

t.test(am_sam_tut_coral_sum_dp_2010.logit$coral_sum_logit ~ am_sam_tut_coral_sum_dp_2010.logit$exposure, 
       method = "holm")


# 2010 vs. 2018 Stuff


# Summary

am_sam_tut_coral_sum_2010_2018.sum <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010" | obs_year == "2018") %>%
  select(obs_year, depth_bin, exposure, coral_sum) %>%
  group_by(obs_year, depth_bin, exposure) %>%
  summarize(mean = mean(coral_sum), n = n()
  )

# Logit Transformation

am_sam_tut_coral_sum_2010_2018 <-
  am_sam_tut_coral_nmds7 %>%
  filter(obs_year == "2010" | obs_year == "2018") %>%
  select(obs_year, depth_bin, exposure, coral_sum)

coral_sum_2010_2018.logit <- logit(am_sam_tut_coral_sum_2010_2018$coral_sum)

am_sam_tut_coral_sum_2010_2018.logit <-
  #am_sam_tut_coral_sum_2010 %>%
  bind_cols(am_sam_tut_coral_sum_2010_2018, coral_sum_2010_2018.logit)

am_sam_tut_coral_sum_2010_2018.logit2 <-
  rename(am_sam_tut_coral_sum_2010_2018.logit, coral_sum_logit = ...5)

# Low Exposure
# 2010 2018 Am Sam Shallow - Low

am_sam_tut_coral_sum_low_sh_2010_2018.logit <-
  am_sam_tut_coral_sum_2010_2018.logit2 %>%
  filter(depth_bin == "Shallow", exposure == "low") %>%
  select(obs_year, depth_bin, exposure, coral_sum_logit)

t.test(
  am_sam_tut_coral_sum_low_sh_2010_2018.logit$coral_sum_logit ~ am_sam_tut_coral_sum_low_sh_2010_2018.logit$obs_year, 
  method = "holm")

# 2010 2018 Am Sam Mid - Low

am_sam_tut_coral_sum_low_md_2010_2018.logit <-
  am_sam_tut_coral_sum_2010_2018.logit2 %>%
  filter(depth_bin == "Mid") %>%
  select(obs_year, depth_bin, exposure, coral_sum_logit)

t.test(
  am_sam_tut_coral_sum_low_md_2010_2018.logit$coral_sum_logit ~ am_sam_tut_coral_sum_low_md_2010_2018.logit$obs_year, 
  method = "holm")

# 2010 2018 Am Sam Deep - Low

am_sam_tut_coral_sum_low_dp_2010_2018.logit <-
  am_sam_tut_coral_sum_2010_2018.logit2 %>%
  filter(depth_bin == "Deep") %>%
  select(obs_year, depth_bin, exposure, coral_sum_logit)

t.test(
  am_sam_tut_coral_sum_low_dp_2010_2018.logit$coral_sum_logit ~ am_sam_tut_coral_sum_low_dp_2010_2018.logit$obs_year, 
  method = "holm")

# High Exposure
# 2010 2018 Am Sam Shallow - High

am_sam_tut_coral_sum_high_sh_2010_2018.logit <-
  am_sam_tut_coral_sum_2010_2018.logit2 %>%
  filter(depth_bin == "Shallow", exposure == "high") %>%
  select(obs_year, depth_bin, exposure, coral_sum_logit)

t.test(
  am_sam_tut_coral_sum_high_sh_2010_2018.logit$coral_sum_logit ~ am_sam_tut_coral_sum_high_sh_2010_2018.logit$obs_year, 
  method = "holm")

# 2010 2018 Am Sam Mid - High

am_sam_tut_coral_sum_high_md_2010_2018.logit <-
  am_sam_tut_coral_sum_2010_2018.logit2 %>%
  filter(depth_bin == "Mid") %>%
  select(obs_year, depth_bin, exposure, coral_sum_logit)

t.test(
  am_sam_tut_coral_sum_high_md_2010_2018.logit$coral_sum_logit ~ am_sam_tut_coral_sum_high_md_2010_2018.logit$obs_year, 
  method = "holm")

# 2010 2018 Am Sam Deep - High

am_sam_tut_coral_sum_high_dp_2010_2018.logit <-
  am_sam_tut_coral_sum_2010_2018.logit2 %>%
  filter(depth_bin == "Deep") %>%
  select(obs_year, depth_bin, exposure, coral_sum_logit)

t.test(
  am_sam_tut_coral_sum_high_dp_2010_2018.logit$coral_sum_logit ~ am_sam_tut_coral_sum_high_dp_2010_2018.logit$obs_year, 
  method = "holm")


































































