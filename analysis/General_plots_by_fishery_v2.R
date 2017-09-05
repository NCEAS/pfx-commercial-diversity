#  An amalgamation of general diversity plots for each fishery.

library(tidyverse)
library(reshape2)
library(fpc)
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)

#  Eric's data read-in
cfec = feather::read_feather("/users/eric.ward/documents/pfx-commercial/portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

#  Jordan's data read-in
load("../cfec_070616.Rdata")

# remove region column
cfec <- cfec[,-which(names(cfec)%in%"region")]
#===================================================================================================
#  Adjust for inflation
# Eric commented this out because the .feather file above has already been corrected
#cpi <- read.csv("data/CPI_2009.csv") %>% filter(year>1984)

#cfec <- cfec %>% left_join(cpi) %>%
#  mutate(g_earn=g_earn/GDPDEF09_base1,
#         g_price=g_price/GDPDEF09_base1) %>%
#  dplyr::select(-GDPDEF09_base1)
#===================================================================================================

#===================================================================================================
#  Get Anne's regions
region <- read.csv("data/regions.csv")
cfec <- cfec %>%
  inner_join(region[,c("stat6","final")]) %>%
  rename(region=final)

casestudies = c("PWS", "Cook Inlet", "Kodiak", "Alaska Peninsula")
#===================================================================================================

cfec$year <- as.numeric(cfec$year)
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

casestudies = c("herring", "pws_seine", "bb_gnet","cucumber", "halibut")

for(cc in 1:length(casestudies)) {

  casestudy = casestudies[cc]
  if(casestudy == "herring") {
    yourfishery <- c("G 01E","G 34E", "H 01E", "H 34E", "L 21E")
  }
  if(casestudy == "pws_seine") {
    yourfishery <- c("S 01E")
  }
  if(casestudy == "bb_gnet") {
    yourfishery <- c("S 03T")
  }
  if(casestudy == "cucumber") {
    yourfishery <- c("Q 11A","Q 11B")
  }
  if(casestudy == "halibut") {
    yourfishery <- c("B 06B","B 61B")
  }
  mydat <- cfec[which(cfec$p_fshy%in%yourfishery),]

  #  Create pdf output filename
  pdf(paste0("plots/",casestudy,"_generalPlots",".pdf"))

  #  Create a summary dataset per year.
  p1 <- mydat %>% group_by(year) %>%
    summarise(fisheries=length(unique(p_fshy)),
              permits=length(unique(p_fshy)),
              fishers=length(unique(p_holder)),
              rev=sum(g_earn))

  # Permit fisheries by year
  #ggplot(p1,aes(x=year,y=fisheries)) + geom_point() + geom_line() + ggtitle(paste("Number of",casestudy,"permits"))

  #  Total permits per year
  #ggplot(p1,aes(x=year,y=permits)) + geom_point() + geom_line()

  #  Active permit holders per year
  print(ggplot(p1,aes(x=year,y=fishers)) +
          geom_point() +
          geom_line() +
          theme_sleek() +
      ylab("Number of active permit holders") +
      xlab("Year"))

  #  Total gross revenues for the fishery per year
  print(ggplot(p1,aes(x=year,y=rev)) +
          geom_point() +
          geom_line() +
          theme_sleek() +
      ylab("Gross revenue (USD)") +
      xlab("Year"))

  # Identify dominant strategies associated with this permit
  g2 = group_by(cfec, p_holder) %>%
    mutate(pws = ifelse(length(which(p_fshy%in%yourfishery))>0, 1, 0)) %>%
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

  print(ggplot(totals, aes(year, n/n_year)) +
          geom_area(aes(fill = group), position = "stack", colour = 1) +
          xlab("Year") +
          ylab("Proportion of active participants") +
          theme_sleek())

  dev.off()
}
