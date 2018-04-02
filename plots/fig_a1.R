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
