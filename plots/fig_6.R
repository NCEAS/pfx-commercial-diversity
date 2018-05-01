# set user <- "your name"
source("plots/plot-preamble.R")

fig = group_by(cfec, year) %>%
  summarize(fishers=length(unique(p_holder)) / 1000) %>%
  ggplot(aes(year, fishers)) + geom_line(colour = "grey40", size=0.85) +
  theme_sleek() +
  ylab("Participation (1000 permit holders)") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 15), expand = FALSE)
#  geom_point(colour = "dodgerblue4", size=3, alpha=0.5)
height <- 3
pdf("Fig_6.pdf", height = height, width = height * golden_ratio)
fig
dev.off()
