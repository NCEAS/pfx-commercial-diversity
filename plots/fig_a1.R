user <- "Sean"
source("plots/plot-preamble.R")

for(y in 1985:2014) {
people = filter(cfec, p_fshy %in% c("G 01E","G 34E", "H 01E", "H 34E", "L 21E") & year == y) %>%
  group_by(p_holder) %>%
  select(p_holder, year)
# for each person find last active year in any fishery
last = filter(cfec, p_holder %in% people$p_holder) %>%
  group_by(p_holder) %>%
  summarize(last_year = max(year, na.rm=T))
people = left_join(people, last)

if(y == 1985) {
  df = people
} else {
  df = rbind(df, people)
}
}

library(viridis)
fig = ggplot(filter(df, last_year !=2014),
  aes(year, last_year, group=year)) + geom_violin(fill="dodgerblue4") +
  theme_sleek() + xlab("Year active in Prince William Sound herring") +
  ylab("Last active year (any fishery)")

pdf("Fig_A1.pdf")
fig
dev.off()
