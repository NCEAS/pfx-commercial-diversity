user <- "Sean"
source("plots/plot-preamble.R")

sub_1 = filter(cfec, p_fshy %in% c("S 01E", "S 03E")) %>%
  mutate(case_study = "Prince William Sound salmon")
sub_2 = filter(cfec, region %in% c("PWS")) %>%
  mutate(case_study = "Prince William Sound")
sub_3 = filter(cfec, region %in% c("Cook Inlet")) %>%
  mutate(case_study = "Cook Inlet")

totals = rbind(sub_1, sub_2, sub_3)

totals$case_study <- factor(totals$case_study, levels = c(
  "Prince William Sound salmon",
  "Prince William Sound",
  "Cook Inlet"
))

totals$case_study <- forcats::fct_recode(totals$case_study,
  `PWS salmon` = "Prince William Sound salmon",
  `EVOS commercial (PWS)` = "Prince William Sound",
  `EVOS commercial (Cook Inlet)` = "Cook Inlet")

fig = group_by(totals, year, case_study) %>%
  summarize(fishers=length(unique(p_holder)),
    rev=sum(g_earn)/1000000) %>%
  ggplot(aes(year, rev)) + geom_line(colour = "dodgerblue1", size=1.2) +
  facet_wrap(~case_study, scale="free_y", ncol=2, nrow=2) +
  theme_sleek() +
  ylab("Revenue (million USD)") +
  xlab("Year") +
  geom_line(aes(y = fishers/10), colour = "dodgerblue4", size=1.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*(10), name = "Participation"))
pdf("Fig_A5.pdf")
fig
dev.off()
