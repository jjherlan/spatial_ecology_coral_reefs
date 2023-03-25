require(tidyverse)
require(gridExtra)
require(grid)
require(lattice)
require(cowplot)

soil <- read.csv('soil_full.csv') %>%
  as_tibble() %>%
  #  mutate(size_cm = area*10000) %>%
  mutate_at(vars(material, range, range_mean), factor) %>%
  group_by(material, range_mean) %>%
  dplyr::summarize(mean = mean(mbc), 
                   sd = sd(mbc), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
  #mutate_at(vars(Site), factor) %>%
  #add_column(
  #  location = c('Anakena', 'Manavai', 'Southeast')
  #) %>%
  

plob_cover.gg <- plob_cover %>%
  mutate(site = factor(site, levels = c('Anakena', 'Motu Tautara', 'Manavai', 'Southeast')),
         depth2 = factor(depth2, levels = c('8 m', '15 m', '25 m')
         )
  )

x_labels = c("HAHTM", "NDM")
#label_names = c("8 m" = "8 m", "15 m" = "15 m", "25 m" = "25 m")

# w <- rep(c('8 m','15 m','25 m'), times = 4))
# df3 <- data.frame(w)
# df3  

# plob_cover.gg$se <- c(0.158, 0.128, 0.174, 0.218, 0.199, 0.161, 
#                    0.0854, 0.107, 0.255, 0.0768, 0.0947, 0.312)
soil.barplot <- ggplot(soil, aes(x = ParentMaterials, y = mean, fill = Depth)) +   
  geom_bar(stat = "identity", width = 0.75, color = "#2b2b2b", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + se), size = 0.75) +
  #scale_y_continuous(limits = c(0.0, 1.0), labels = function(x) paste0(x * 100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  #scale_fill_manual(breaks = c("North", "West",
  #                             "Southeast"),
  #                  values = c("red", "blue", 
  #                             "green"), 
  #                  labels = c("North", "West",
  #                             "Southeast")) +
  #facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" Soil")))) +
  ylab(expression(paste("MBCugCpergDrySoil"))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
        strip.background = element_rect(fill = "#FFFFFF", color = NA),    
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        axis.ticks = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

soil.barplot
