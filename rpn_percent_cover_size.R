poci_cover.ggbarplot <- ggplot(poci_cover2, aes(x = location, y = mean, fill = x_labels)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), size = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) +
  #  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  #ggtitle(expression(paste(italic(" Pocillopora "), "spp."))) +
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
        legend.position = 'none',
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

poci_cover.ggbarplot

x_labels = c("North", "West", "Southeast")
# label_names = c("8 m" = "8 m", "15 m" = "15 m", "25 m" = "25 m")

poci_size.gg.barplot <- ggplot(poci_size3, aes(x = location, y = mean, fill = x_labels)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", size = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), size = 0.75) +
  scale_y_continuous(expression(paste("Mean Colony Size ("," ", cm^2, ")")), limits = c(0, 300)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) +
  # facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  # ggtitle(expression(paste(italic(" Pocillopora "), "spp."))) +
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
        legend.position = "none",
        plot.title = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank())

poci_size.gg.barplot


