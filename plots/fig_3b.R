#  An amalgamation of general diversity plots for each fishery.

library(tidyverse)
library(reshape2)
library(fpc)
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)

#  Eric's data read-in
if (!exists("cfec")) stop("The data frame `cfec` must be loaded first.")

load("/users/eric.ward/documents/CFEC/data/cfec_070616")

#  Jordan's data read-in
#load("../cfec_070616.Rdata")

# Deflate the data per Sean's paper
deflationTable <- read.csv("data/deflation.csv")
# Adjust price and g_earn for inflation
cfec$year <- as.numeric(cfec$year)
cfec$day <- substr(cfec$landdate, 6, 10)
cfec <- inner_join(cfec, deflationTable)
cfec <- mutate(cfec, g_price = g_price / defl, g_earn = g_earn / defl)
cfec$defl <- NULL

#===================================================================================================
#  Get Anne's regions
region <- read.csv("data/regions.csv")
cfec <- cfec %>%
  inner_join(region[,c("stat6","final")]) %>%
  rename(region=final)

cfec$year <- as.numeric(cfec$year)
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

# Make Figure 3 -- revenue as main axis, permit holders as secondary
library(ggsidekick)
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
