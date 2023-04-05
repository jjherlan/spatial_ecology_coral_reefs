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
write.csv(rpn_man_sh, 'rpn_man_md.csv')

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

rpn_mtt_sh <- read.csv('rpn_mtt_dp.csv')
rpn_mtt_md <- read.csv('rpn_mtt_dp.csv')
rpn_mtt_dp <- read.csv('rpn_mtt_dp.csv')

rpn_man_sh <- read.csv('rpn_man_sh.csv')
rpn_man_md <- read.csv('rpn_man_md.csv')
rpn_man_dp <- read.csv('rpn_man_dp.csv')

rpn_se_sh <- read.csv('rpn_se_sh.csv')
rpn_se_ms <- read.csv('rpn_se_md.csv')
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
rpn_se_ms
rpn_se_dp

rpn_point_counts <-
  bind_rows(rpn_man_sh,
            rpn_man_md,
            rpn_man_dp,
            
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















