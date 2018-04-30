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

fig_1 = group_by(totals, year, case_study) %>%
  summarize(fishers=length(unique(p_holder)),
    rev=sum(g_earn)/1000000) %>%
  ggplot(aes(year, rev,group=case_study,colour = case_study)) +
  geom_line(size=1.2) +
  theme_sleek() +
  ylab("Revenue (million USD)") +
  xlab("Year")
fig_2 = group_by(totals, year, case_study) %>%
  summarize(fishers=length(unique(p_holder)),
    rev=sum(g_earn)/1000000) %>%
  ggplot(aes(year, fishers,group=case_study,colour = case_study)) +
  geom_line(size=1.2) +
  theme_sleek() +
  ylab("Participation") +
  xlab("Year")
fig_3 = group_by(totals, year, case_study) %>%
  summarize(fishers=length(unique(p_holder)),
    rev=sum(g_earn)/1000000) %>%
  ggplot(aes(year, rev/fishers,group=case_study,colour = case_study)) +
  geom_line(size=1.2) +
  theme_sleek() +
  ylab("Revenue (million USD) / Participation") +
  xlab("Year")

pdf("Fig_3b.pdf", 7, 7)
gridExtra::grid.arrange(fig_1,fig_2,fig_3,nrow=3)
dev.off()
