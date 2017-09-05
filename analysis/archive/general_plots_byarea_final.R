library(dplyr)
library(ggplot2)
library(reshape2)
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)

#  Eric's data read-in
cfec = feather::read_feather("/users/eric.ward/documents/pfx-commercial/portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

cfec$year <- as.numeric(cfec$year)
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}
casestudies = c("PWS", "Cook Inlet", "Kodiak", "Alaska Peninsula")

# subsets by region: kodiak, prince william sound, cook inlet, alaska peninsula
# all fisheries

for(cc in 1:length(casestudies)) {

casestudy = casestudies[cc]
#  Change the "yourfishery" to include whichever fishery(ies) you want. And then the rest of the code should be runnable. I didn't put this in a function because I didn't know how you'd want to output the figures.

mydat <- cfec[which(cfec$region%in%casestudy),]

pdf(paste0("plots/area_",casestudy,"_generalPlots",".pdf"))
#  Create a summary dataset per year.
p1 <- mydat %>% group_by(year) %>%
  summarise(fisheries=length(unique(p_fshy)),permits=length(unique(p_fshy)),fishers=length(unique(p_holder)),rev=sum(g_earn))

#  Active permit holders per year
ggplot(p1,aes(x=year,y=fishers)) + geom_point() + geom_line() +
  theme_sleek() + ylab("Number of active permit holders") + xlab("Year")

#  Gross revenues per year
ggplot(p1,aes(x=year,y=rev)) + geom_point() + geom_line() +
  theme_sleek() + ylab("Gross revenue (USD)") + xlab("Year")

# Identify dominant strategies associated with this permit
g2 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%casestudy))>0, 1, 0)) %>%
  filter(pws>0) %>%
  select(-pws) %>%
  group_by(p_holder, year, p_fshy) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(p_holder, year) %>%
  mutate(totRev = sum(g)) %>%
  group_by(p_holder, year, p_fshy) %>%
  mutate(g = g / totRev) %>%
  select(-totRev)

# use reshape 2 to cast this to wide format
aql <- melt(g2, id.vars = c("year", "p_fshy", "p_holder"))
aqw <- dcast(aql, year + p_holder ~ p_fshy)
# replace NAs w/0s
for(i in 1:ncol(aqw)) {
  aqw[which(is.na(aqw[,i])),i] = 0
}

indx = sample(seq(1,nrow(aqw)),size=1000,replace=F)
sub = aqw[indx,-c(1,2)]

sub = aqw[,-c(1,2)]
coarse_df = aqw[,-c(1,2)]
coarse_df$S = apply(coarse_df[,which(substr(names(sub),1,1)=="S")],1,sum)
coarse_df$B = apply(coarse_df[,which(substr(names(sub),1,1)=="B")],1,sum)
coarse_df$GHL = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("G","H","L"))],1,sum)
coarse_df$KTD = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("K","T","D"))],1,sum)
coarse_df$M = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("M"))],1,sum)
coarse_df$P = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("P"))],1,sum)
coarse_df$OTHER = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("S","B","G","H","L","K","T","D","M","P")==FALSE)],1,sum)
coarse_df = coarse_df[,c("S","B","GHL","KTD","M","P","OTHER")]

clust = fpc::pamk(coarse_df[sample(seq(1,nrow(aqw)),size=1000,replace=F),])# cluster based on subset to pick medoids
medoids = clust$pamobject$medoids

names_medoids = ""
for(i in 1:nrow(medoids)) {
  names_medoids[i] = paste0(colnames(medoids)[which(medoids[i,] > 0.05)], collapse=":")
}

clust = kmeans(coarse_df, centers = medoids)

aqw$clusters = clust$cluster
totals = group_by(aqw, clusters, year) %>%
  summarize(n = n())
totals$group = names_medoids[totals$clusters]

totals = group_by(totals, year, group) %>%
  summarize(n = sum(n)) %>%
  group_by(year) %>%
  mutate(n_year = sum(n))

ggplot(totals, aes(year, n/n_year)) +
  geom_area(aes(fill = group), position = "stack", colour = 1) +
  xlab("Year") +
  ylab("Proportion of active participants") +
  theme_sleek()

dev.off()

}
