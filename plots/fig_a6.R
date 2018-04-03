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

sub_1 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(p_fshy%in%c("S 01E", "S 03E")))>0, 1, 0)) %>%
  filter(pws>0) %>%
  mutate(case_study = "Prince William Sound salmon")
sub_2 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%c("Cook Inlet")))>0, 1, 0)) %>%
  filter(pws>0) %>%
  mutate(case_study = "Cook Inlet")
sub_3 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%c("PWS")))>0, 1, 0)) %>%
  filter(pws>0) %>%
  mutate(case_study = "Prince Willam Sound")

coarse_df = rbind(sub_1, sub_2, sub_3)
coarse_df = coarse_df %>%
  mutate(s = substr(p_fshy, 1, 1))
coarse_df$p_fshy[which(coarse_df$s =="S")] = "S"
coarse_df$p_fshy[which(coarse_df$s =="B")] = "B"
coarse_df$p_fshy[which(coarse_df$s =="C")] = "C"
coarse_df$p_fshy[which(coarse_df$s %in% c("G","H","L"))] = "GHL"
coarse_df$p_fshy[which(coarse_df$s %in% c("K","T","D"))] = "KTD"
coarse_df$p_fshy[which(coarse_df$s %in% c("Q"))] = "Q"
coarse_df$p_fshy[which(coarse_df$s %in% c("M"))] = "M"
coarse_df$p_fshy[which(coarse_df$s %in% c("P"))] = "P"
coarse_df$p_fshy[which(coarse_df$s %in% c("S","B","C","G","H",
  "L","K","T","D","Q","M","P") ==FALSE)] = "O"

summary = group_by(coarse_df, p_holder, year, p_fshy) %>%
  summarize(g = sum(g_earn), case_study = case_study[1]) %>%
  group_by(p_holder, year) %>%
  mutate(totRev = sum(g)) %>%
  group_by(p_holder, year, case_study) %>%
  mutate(g = g / totRev) %>%
  select(-totRev)

# use reshape 2 to cast this to wide format
aql <- melt(summary, id.vars = c("year", "p_fshy",
  "p_holder", "case_study"))
aqw <- dcast(aql, year + p_holder + case_study ~ p_fshy)
# replace NAs w/0s
for(i in 1:ncol(aqw)) {
  aqw[which(is.na(aqw[,i])),i] = 0
}

kss = 0
for(j in 1:15) {
  kss[j] = kmeans(aqw[,-c(1:3)], j)$tot.withinss
}

clust = kmeans(aqw[,-c(1:3)], 10)

#clust = fpc::pamk(aqw[sample(seq(1,nrow(aqw)),size=5000,replace=F),-c(1:3)])# cluster based on subset to pick medoids
medoids = clust$centers

names_medoids = ""
for(i in 1:nrow(medoids)) {
  names_medoids[i] = paste0(colnames(medoids)[which(medoids[i,] >= 0.05)], collapse=":")
}

#clust = kmeans(coarse_df, centers = medoids)

aqw$clusters = clust$cluster
totals = group_by(aqw, clusters, year, case_study) %>%
  summarize(n = n())
totals$group = names_medoids[totals$clusters]

totals = group_by(totals, year, group, case_study) %>%
  summarize(n = sum(n)) %>%
  group_by(year, case_study) %>%
  mutate(n_year = sum(n))

library(viridis)
fig = ggplot(rename(totals, Fisheries=group), aes(year, n/n_year)) +
    geom_area(aes(fill = Fisheries), position = "stack", colour = 1) +
    xlab("Year") +
    ylab("Proportion of people") +
    theme_sleek() + facet_wrap(~case_study, nrow=2,ncol=2) +
  scale_fill_viridis(discrete=TRUE, end=0.9)
pdf("Figure_A6.pdf")
fig
dev.off()
