# Convert ANOVA results into dataframes allows for easier name manipulation
a1_new <- data.frame(a1)
a2_new <- data.frame(a2) 

# Putting all into one dataframe/table
anova_results <- data.frame(cbind(c("Species", "Residuals", "Species", "Residuals"), 
                                  rbind(a1_new, a2_new))) 
colnames(anova_results) <- c("", "Sum Sq", "Df", "F value", "Pr(>F)")
row.names(anova_results) <- NULL

# create HTML table using kableExtra
library(kableExtra)
anova_results %>% kable("html", digits=2) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  pack_rows(., "Sepal Length", 1, 2) %>% # groups rows with label
  pack_rows(., "Petal Width", 3, 4) # groups rows with label

anova_results2 <- data.frame(rbind(c("Sum Sq", "Df", "F value", "Pr(>F)", 
                                     "Sum Sq", "Df", "F value", "Pr(>F)"), 
                                   cbind(round(a1_new, 2), round(a2_new,2)))) 
colnames(anova_results2) <- c("", "", "", "","", "", "", "")
row.names(anova_results2)[1] <- ""

anova_results2 %>% kable("html") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  add_header_above(c("", "Sepal Length" = 4, "Petal Width" = 4))