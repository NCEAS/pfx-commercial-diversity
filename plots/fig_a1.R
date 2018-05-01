# set user <- "your name"
source("plots/plot-preamble.R")

for(y in 1985:2014) {
  message(y)
  people <- filter(cfec,
    p_fshy %in% c("G 01E","G 34E", "H 01E", "H 34E", "L 21E") & year == y) %>%
    group_by(p_holder) %>%
    select(p_holder, year)
  # for each person find last active year in any fishery
  last <- filter(cfec, p_holder %in% people$p_holder) %>%
    group_by(p_holder) %>%
    summarize(last_year = max(year, na.rm=TRUE))
  people = left_join(people, last, by = "p_holder")

  if(y == 1985) {
    df <- people
  } else {
    df <- dplyr::bind_rows(df, people)
  }
}

fig <- ggplot(filter(df, last_year !=2014),
  aes(year, last_year, group = year)) +
  geom_point(colour = "grey40", position = position_jitter(width = 0.15), alpha = 0.2) +
  theme_sleek() + xlab("Year active in Prince William Sound herring") +
  ylab("Last active year (any fishery)")

pdf("Fig_A1.pdf", height = 3, width = 3 * golden_ratio)
fig
dev.off()
