library(ggplot2)

ggplot(data = bananas, aes(x = flies, fill = color, group = color)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.5, position = "identity", bins = 30) +
  geom_density(alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("brown", "green", "yellow"))+
  #scale_fill_identity(guide = 'legend') + #instead of scale_fill_manual...
  xlab("Flies per Banana") +
  ylab("Density")