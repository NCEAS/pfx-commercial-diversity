user <- "Sean"
source("plots/plot-preamble.R")

fig = group_by(cfec, year) %>%
  summarize(fishers=length(unique(p_holder))) %>%
  ggplot(aes(year, fishers)) + geom_line(colour = "dodgerblue4", size=1.2) +
  theme_sleek() +
  ylab("Participation") +
  xlab("Year") +
  geom_point(colour = "dodgerblue4", size=3, alpha=0.5)
pdf("Fig_6.pdf", height=4, width=7)
fig
dev.off()
