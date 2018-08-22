library(tidyverse)
# devtools::install_github("ben-williams/FNGr")
# library(FNGr)
theme_set(ggsidekick::theme_sleek())

dat <- read_csv("SubsistenceFigure.csv")

glimpse(dat)

xaxis <- FNGr::tickr(dat, Year, 5)

dat %>%
  dplyr::select(-X13, -X14, -Per.cap.lbs) %>%
  gather(harvest, lbs, -Year) %>%
  ggplot(aes(Year, lbs, fill = harvest)) + geom_col() +
  scale_x_continuous(breaks=xaxis$breaks, labels = xaxis$labels) +
  scale_fill_brewer(palette = 'Spectral', name = "") +
  ylab('Per capita harvest (lbs)') +
  xlab("") +
  coord_cartesian(ylim = c(0, 450), expand = FALSE)

ggsave("Fig_subsistence.pdf", width = 6, height = 3.2)
