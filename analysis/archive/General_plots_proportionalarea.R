#  This code will create some of the diversity summary metrics by region instead of by fishery. 
#It modifies some of Eric's original code but I wanted to create a separate file because I may be doing things totally differently. 
#This also creates some proportional stacked area plots


library(dplyr)
library(ggplot2)
library(reshape2)

cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

load("//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/AFSC/GOA/AKFIN/Meeting/cfec_070616.Rdata")
#cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
setwd("//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/AFSC/GOA/AKFIN/Meeting")

#Adjust for inflation
cpi <- read.csv("Data/CPI_2009.csv") %>% filter(year>1984)
cfec <- left_join(cfec,cpi) %>% mutate(g_earn=g_earn/GDPDEF09_base1,g_price=g_price/GDPDEF09_base1) %>% dplyr::select(-GDPDEF09_base1)

region <- read.csv("diversity/regions.csv")
cfec <- inner_join(cfec,region[,c("stat6","final")]) %>% rename(region=final)

casestudies = c("PWS", "Cook Inlet", "Kodiak", "Alaska Peninsula")

# subsets by region: kodiak, prince william sound, cook inlet, alaska peninsula
# all fisheries


#------------------------------------------------------------------------------------------------
#  Make proportional stacked area plots by region
#------------------------------------------------------------------------------------------------

# subsets by region: kodiak, prince william sound, cook inlet, alaska peninsula
# all fisheries

regionplot <- function(casestudy) {
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
    summarize(n = sum(n))
  
  pdf(paste0("diversity/",casestudy,"proportionalarea",".pdf"))
  print(totals %>% right_join(expand.grid(year=unique(totals$year),group=unique(totals$group))) %>% replace_na(list(n=0)) %>% 
    ggplot(aes(year, n)) + 
    geom_area(aes(fill = group), position = "fill", colour = 1) + 
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) + 
    xlab("Year") + 
    ylab("People") + 
    theme_bw() + 
    ggtitle(paste(casestudy)))
  dev.off()
}


regionplot("PWS")
regionplot("Cook Inlet")
regionplot("Kodiak")
regionplot("Alaska Peninsula")
