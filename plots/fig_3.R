user <- "Sean"
source("plots/plot-preamble.R")

# Make Figure 3 -- revenue as main axis, permit holders as secondary
sub_1 = filter(cfec, p_fshy %in% c("G 01E","G 34E", "H 01E", "H 34E", "L 21E")) %>%
  mutate(case_study = "PWS herring")
sub_2 = filter(cfec, p_fshy %in% c("B 06B","B 61B")) %>%
  mutate(case_study = "Halibut")
sub_3 = filter(cfec, p_fshy %in% c("S 03T")) %>%
  mutate(case_study = "Bristol Bay Drift Gillnet")
sub_4 = filter(cfec, region %in% c("Kodiak")) %>%
  mutate(case_study = "Kodiak")

totals = rbind(sub_1, sub_2, sub_3, sub_4)

totals$case_study <- factor(totals$case_study, levels = c(
  "PWS herring",
  "Halibut",
  "Bristol Bay Drift Gillnet",
  "Kodiak"
))
totals$case_study <- forcats::fct_recode(totals$case_study,
  `Halibut` = "Halibut",
  `PWS herring` = "PWS herring",
  `BBDG salmon` = "Bristol Bay Drift Gillnet",
  `EVOS commercial (Kodiak only)` = "Kodiak")

fig = group_by(totals, year, case_study) %>%
  summarize(fishers=length(unique(p_holder)),
    rev=sum(g_earn)/1000000) %>%
  ggplot(aes(year, rev)) + geom_line(colour = "dodgerblue1", size=1.2) +
  facet_wrap(~case_study, scale="free_y") +
  theme_sleek() +
  ylab("Revenue (million USD)") +
  xlab("Year") +
  geom_line(aes(y = fishers/10), colour = "dodgerblue4", size=1.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*(10), name = "Participation"))
pdf("Fig_3.pdf")
fig
dev.off()
