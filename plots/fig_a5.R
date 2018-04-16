#  An amalgamation of general diversity plots for each fishery.

library(tidyverse)
library(reshape2)
library(fpc)
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)

#  Eric's data read-in
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
