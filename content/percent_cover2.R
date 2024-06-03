library(tidyverse)
#library(ggplot2)
#library(tidyr)
library(patchwork)
library(ggpubr)
library(cowplot)

Fcn.CreateSummary.betareg <- function(object.betareg){
  OUT <- summary(object.betareg)
  tab <- rbind(OUT$coefficients$mean,OUT$coefficients$precision)
  return(tab)
}

p_cover = read_csv('rpn_poci.csv')

p_cover = read_csv('rpn_poci.csv') %>% 
  select(plot_id, Site, pland) %>%
  group_by(Site) %>%
  mutate(
    poci_cover = pland*0.01
  ) %>%
  mutate_at(vars(Site, plot_id), factor)

ggplot(p_cover, aes(x = poci_cover)) +
  geom_histogram(fill = "#333399") + 
  #below here is ylabel, xlabel, and main title
  ylab("Frequency") +
  xlab(NULL) +
  ggtitle(expression("Coral Cover (%)")) +
  theme_bw() +
  facet_wrap(~ Site, ncol = 1) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.y  = element_text(size= 10),
        axis.text.x  = element_text(size = 12), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill = "white"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 12, colour = "#FFFFFF"),
        strip.background = element_rect(fill = '#000066')
  )

p_cover.mean <-
  p_cover %>%
  group_by(Site) %>%
  dplyr::summarise(mean = mean(poci_cover))

poci_cover_summary <-
  p_cover %>%
  as_tibble() %>%
  #  mutate(size_cm = area*10000) %>%
  group_by(Site) %>%
  dplyr::summarize(mean = mean(poci_cover), 
                   sd = sd(poci_cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  mutate_at(vars(Site), factor) %>%
  add_column(
    location = c('Anakena', 'Manavai', 'Southeast'),
    group = c('poci', 'poci', 'poci')
  ) %>%
  mutate_at(vars(location), factor)


poci_cover2 <- 
  poci_cover_summary %>%
  add_column(
    cld = c('a', 'b', 'c')
  ) %>%
  mutate_at(vars(cld), factor)

poci_cover2

x_labels = c("North", "West", "Southeast")

#poci_cover2$location <- factor(poci_cover2$location, levels=c("North", "West", "Southeast"))

poci_cover.ggbarplot <- ggplot(poci_cover2, aes(x = location, y = mean, fill = x_labels)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), size = 0.75) +
  scale_y_continuous(expression(paste("Mean Percent Cover (%)")), limits = c(0, 1), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(limits = c("North", "West", "Southeast"),
                                  values = c("#FFC74E", "#ABC178", "#82A5C0")) +
  #scale_fill_discrete(values = c("#FFC74E", "#82A5C0", "#ABC178"), 
  #                               limits = c("North", "West", "Southeast")) +
  #  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  #ggtitle(expression(paste(italic(" Pocillopora "), "spp."))) +
  ggtitle(expression(paste(" Pocilloporid "))) +
  geom_text(aes(label = cld, y = upper.ci), vjust = -0.5) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
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
        legend.position = 'bottom',
        plot.title = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

poci_cover.ggbarplot

##################################
#Bare Substrate###################
##################################

bare_cover = read_csv('rpn_bare.csv')

bare_cover = read_csv('rpn_bare.csv') %>% 
  select(plot_id, Site, pland) %>%
  group_by(Site) %>%
  mutate(
    bare_cover = pland*0.01
  ) %>%
  mutate_at(vars(Site, plot_id), factor)

ggplot(bare_cover, aes(x = bare_cover)) +
  geom_histogram(fill = "#333399") + 
  #below here is ylabel, xlabel, and main title
  ylab("Frequency") +
  xlab(NULL) +
  ggtitle(expression("Coral Cover (%)")) +
  theme_bw() +
  facet_wrap(~ Site, ncol = 1) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.y  = element_text(size= 10),
        axis.text.x  = element_text(size = 12), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill = "white"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 12, colour = "#FFFFFF"),
        strip.background = element_rect(fill = '#000066')
  )

bare_cover.mean <-
  bare_cover %>%
  group_by(Site) %>%
  dplyr::summarise(mean = mean(bare_cover))

bare_cover_summary <-
  bare_cover %>%
  as_tibble() %>%
  #  mutate(size_cm = area*10000) %>%
  group_by(Site) %>%
  dplyr::summarize(mean = mean(bare_cover), 
                   sd = sd(bare_cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  mutate_at(vars(Site), factor) %>%
  add_column(
    location = c('Anakena', 'Manavai', 'Southeast'),
    group = c('bare', 'bare', 'bare')
  ) %>%
  mutate_at(vars(location), factor)


bare_cover2 <- 
  bare_cover_summary %>%
  add_column(
    cld = c('a', 'b', 'c')
  ) %>%
  mutate_at(vars(cld), factor)

bare_cover2

x_labels = c("North", "West", "Southeast")

bare_cover.ggbarplot <- ggplot(bare_cover2, aes(x = location, y = mean, fill = x_labels)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), size = 0.75) +
  scale_y_continuous(expression(paste("Mean Percent Cover (%)")), limits = c(0, 1),
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) +
  #  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(" Bare Substrate "))) +
  geom_text(aes(label = cld, y = upper.ci), vjust = -0.5) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
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
        legend.position = 'bottom',
        plot.title = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

bare_cover.ggbarplot

##################################
#Porites lobata###################
##################################

plob_cover = read_csv('rpn_plob.csv')

plob_cover = read_csv('rpn_plob.csv') %>% 
  dplyr::select(plot_id, Site, pland) %>%
  group_by(Site) %>%
  mutate(
    plob_cover = pland*0.01
  ) %>%
  mutate_at(vars(Site, plot_id), factor)

ggplot(plob_cover, aes(x = plob_cover)) +
  geom_histogram(fill = "#333399") + 
  #below here is ylabel, xlabel, and main title
  ylab("Frequency") +
  xlab(NULL) +
  ggtitle(expression("Coral Cover (%)")) +
  theme_bw() +
  facet_wrap(~ Site, ncol = 1) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.y  = element_text(size= 10),
        axis.text.x  = element_text(size = 12), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill = "white"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 12, colour = "#FFFFFF"),
        strip.background = element_rect(fill = '#000066')
  )

plob_cover.mean <-
  plob_cover %>%
  group_by(Site) %>%
  dplyr::summarise(mean = mean(plob_cover))

plob_cover_summary <-
  plob_cover %>%
  as_tibble() %>%
  #  mutate(size_cm = area*10000) %>%
  group_by(Site) %>%
  dplyr::summarize(mean = mean(plob_cover), 
                   sd = sd(plob_cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  mutate_at(vars(Site), factor) %>%
  add_column(
    location = c('Anakena', 'Manavai', 'Southeast'),
    group = c("plob", "plob", "plob")
  ) %>%
  mutate_at(vars(location), factor)


plob_cover2 <- 
  plob_cover_summary %>%
  add_column(
    cld = c('a', 'b', 'c')
  ) %>%
  mutate_at(vars(cld), factor)

plob_cover2

x_labels = c("North", "West", "Southeast")

plob_cover.ggbarplot <- ggplot(plob_cover2, aes(x = location, y = mean, fill = x_labels)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), size = 0.75) +
  scale_y_continuous(expression(paste("Mean Percent Cover (%)")), limits = c(0, 1),
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) +
  #facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(" Poritid "))) +
  geom_text(aes(label = cld, y = upper.ci), vjust = -0.5) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
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
        legend.position = 'bottom',
        plot.title = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

plob_cover.ggbarplot

##################################
#Groups ##########################
##################################

groups_percent_cover <- bind_rows(poci_cover2, plob_cover2, bare_cover2)
groups_percent_cover

group = c("Pocillopora", "Porites", "Bare")

cover_main.ggbarplot <- ggplot(groups_percent_cover, aes(x = Site, y = mean, fill = location)) +   
  # geom_col(aes(fill = supp), position = position_dodge(0.8), width = 0.7) +
  geom_bar(stat = "identity", position = position_dodge(0.8), 
           width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), size = 0.75) +
  scale_y_continuous(expression(paste("Mean Percent Cover (%)")), limits = c(0, 1)) + 
  #scale_x_discrete(expand = c(0, 1), labels = Site) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) +
  facet_wrap( ~ group, 
              #labeller = as_labeller(label_names), 
              dir = "v", 
              ncol = 1) + 
  #ggtitle(expression(paste("Bare Substrate"))) +
  geom_text(aes(label = cld, y = upper.ci), vjust = -0.5) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
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
        legend.position = 'right',
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

cover_main.ggbarplot

ggplot(df.summary2, aes(dose, len)) +
  geom_col(aes(fill = supp), position = position_dodge(0.8), width = 0.7)+
  geom_errorbar(
    aes(ymin = len, ymax = len+sd, group = supp),
    width = 0.2, position = position_dodge(0.8)
  )+
  scale_fill_manual(values = c("grey80", "grey30"))
 
poci_size3 <- 
  poci_size2 %>%
  add_column(
    cld = c('a', 'b', 'c')
  ) %>%
  mutate_at(vars(cld), factor)

poci_size3

x_labels = c("North", "West", "Southeast")
# label_names = c("8 m" = "8 m", "15 m" = "15 m", "25 m" = "25 m")

poci_size.gg.barplot <- ggplot(poci_size3, aes(x = location, y = mean, fill = x_labels)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), size = 0.75) +
  scale_y_continuous(expression(paste("Mean Colony Size ("," ", cm^2, ")")), limits = c(0, 300)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) +
  # facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(" Pocilloporid "))) +
  geom_text(aes(label = cld, y = upper.ci), vjust = -0.5) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
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
        legend.position = "bottom",
        plot.title = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

poci_size.gg.barplot

plob_cover.ggbarplot + bare_cover.ggbarplot + poci_cover.ggbarplot + poci_size.gg.barplot

inv.logit(3.1694)

ggarrange(
  plob_cover.ggbarplot, 
  bare_cover.ggbarplot,
  poci_cover.ggbarplot, 
  poci_size.gg.barplot, 
  ncol = 2,
  nrow = 2,
  widths = c(0.25, 0.25, 0.25, 0.25),
  labels = c("A", "B", "C", "D"),
  common.legend = TRUE, 
  legend = "bottom"
)

legend_b <- get_legend(
  poci_cover.ggbarplot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

legend_b <- get_legend(
  poci_cover.ggbarplot)
+ 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

# arrange the three plots in a single row
prow <- plot_grid(
  plob_cover.ggbarplot + theme(legend.position = "none"),
  poci_cover.ggbarplot + theme(legend.position = "none"),
  bare_cover.ggbarplot + theme(legend.position = "none"),
  poci_size.gg.barplot + theme(legend.position = "none"),
  scale = c(0.8, 0.8, 0.8, 0.8),
  align = 'vh',
  labels = c("A", "B", "C", "D"),
  hjust = -1,
  nrow = 2
  #ncol = 1
)

prow

plot_grid(prow, 
          legend_b,
          ncol = 1,
          rel_heights = c(1, 0.1),
          labels = "AUTO", 
          scale = c(0.9, 0.9, 0.9, 0.9)
          )


















