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
sub_1 = filter(cfec, p_fshy %in% c("G 01E","G 34E", "H 01E", "H 34E", "L 21E")) %>%
  mutate(case_study = "PWS herring")
sub_2 = filter(cfec, p_fshy %in% c("B 06B","B 61B")) %>%
  mutate(case_study = "Halibut")
sub_3 = filter(cfec, p_fshy %in% c("S 03T")) %>%
  mutate(case_study = "Bristol Bay Drift Gillnet")
sub_4 = filter(cfec, region %in% c("Kodiak")) %>%
  mutate(case_study = "Kodiak")

fig = group_by(rbind(sub_1, sub_2, sub_3, sub_4), year, case_study) %>%
  summarize(fishers=length(unique(p_holder)),
    rev=sum(g_earn)/1000000) %>%
  ggplot(aes(year, rev)) + geom_line(colour = "dodgerblue1", size=1.2) +
  facet_wrap(~case_study, scale="free_y") +
  theme_sleek() +
  ylab("Revenue (million USD)") +
  xlab("Year") +
  geom_line(aes(y = fishers/10), colour = "dodgerblue4", size=1.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*(10), name = "Participation"))
pdf("Figure 03.pdf")
fig
dev.off()
