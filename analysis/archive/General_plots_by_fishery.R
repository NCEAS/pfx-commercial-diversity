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
cpi <- read.csv("data/CPI_2009.csv") %>% filter(year>1984)

cfec <- cfec %>% left_join(cpi) %>%
  mutate(g_earn=g_earn/GDPDEF09_base1,
         g_price=g_price/GDPDEF09_base1) %>%
  dplyr::select(-GDPDEF09_base1)
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

casestudies = c("herring", "pws_seine", "bb_gnet","cucumber")

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
  mydat <- cfec[which(cfec$p_fshy%in%yourfishery),]

  #  Create pdf output filename
  pdf(paste0("plots/",casestudy,"_generalPlots",".pdf"))

  #  Create a summary dataset per year.
  p1 <- mydat %>% group_by(year) %>%
    summarise(fisheries=length(unique(p_fshy)),
              permits=length(unique(p_fshy)),
              fishers=length(unique(p_holder)),
              rev=sum(g_earn))

  temp <- cfec %>%
    group_by(year) %>%
    filter(p_fshy %in% yourfishery) %>%
    ungroup %>%
    distinct(year,p_holder) %>%
    left_join(cfec) %>%
    group_by(year) %>%
    summarise(fisheries=length(unique(p_fshy)))

  print(ggplot(temp,aes(x=year,y=fisheries)) +
          geom_point() +
          geom_line() +
          ggtitle(paste(casestudy)) +
          theme_sleek() +
      ylab("Unique permits held") +
      xlab("Year"))

  temp <- cfec %>%
    group_by(year) %>%
    filter(p_fshy %in% yourfishery) %>%
    ungroup %>%
    distinct(year,p_holder) %>%
    left_join(cfec) %>%
    group_by(year,p_holder) %>%
    summarise(lfisheries=length(unique(p_fshy))) %>%
    ungroup %>%
    group_by(year) %>%
    summarise(fisheries=mean(lfisheries))

  print(ggplot(temp,aes(x=year,y=fisheries)) +
          geom_point() +
          geom_line() +
          ggtitle(paste(casestudy)) +
          theme_sleek() +
      ylab("Mean permits per individual") +
      xlab("Year"))

  # Permit fisheries by year
  #ggplot(p1,aes(x=year,y=fisheries)) + geom_point() + geom_line() + ggtitle(paste("Number of",casestudy,"permits"))

  #  Total permits per year
  #ggplot(p1,aes(x=year,y=permits)) + geom_point() + geom_line()

  #  Active permit holders per year
  print(ggplot(p1,aes(x=year,y=fishers)) +
          geom_point() +
          geom_line() +
          ggtitle(paste(casestudy)) +
          theme_sleek() +
      ylab("Active permit holders") +
      xlab("Year"))

  #  Total gross revenues for the fishery per year
  print(ggplot(p1,aes(x=year,y=rev)) +
          geom_point() +
          geom_line() +
          ggtitle(paste(casestudy)) +
          theme_sleek() +
      ylab("Gross revenue (USD)") +
      xlab("Year"))

  #  Gross revenues per permit holder per year - not sure how you want to visualize this. boxplots?
  p2 <- mydat %>%
    group_by(year,p_holder) %>%
    summarise(rev=sum(g_earn),
              pph=length(unique(p_fshy)),
              source=ifelse(cc==1,"herring",
                            ifelse(cc %in% 2:3,"salmon","cukes")))

  print(ggplot(p2,aes(x=factor(year),y=log10(rev))) +
          geom_boxplot(outlier.shape=NA) +
          ggtitle("Log (gross revenues per permit holder per year)") +
          theme_sleek() +
      ylab("Log10 (gross revenue per person) in USD") +
      xlab("Year"))

  temp <- cfec %>%
    group_by(year) %>%
    filter(p_fshy %in% yourfishery) %>%
    ungroup %>%
    distinct(year,p_holder) %>%
    left_join(cfec) %>%
    group_by(year,p_holder) %>%
    summarise(rev=sum(g_earn),source="all")

  temp2 <- temp %>%
    rename(revall=rev) %>% dplyr::select(-source) %>%
    inner_join(p2 %>% dplyr::select(-pph,-source)) %>%
    ungroup %>%
    transmute(revnonfocus=revall-rev,rev,year,p_holder) %>%
    group_by(year) %>%
    summarise_at(vars(-p_holder),sum) %>%
    melt(id.vars="year")


  #  Create a figure that shows the proportion of salmon and non-salmon revenues per year (all fishers combined).
  print(ggplot(temp2,aes(factor(year),value,fill=variable)) +
          geom_bar(stat="identity") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90)) +
          scale_fill_discrete(name="Revenue Source",
                              breaks=c("revnonfocus","rev"),
                              labels=c(ifelse(cc==1,"Non-herring",
                                              ifelse(cc %in% 2:3,"Non-salmon","Non-cuke")),
                                       ifelse(cc==1,"Herring",
                                              ifelse(cc %in% 2:3,"Salmon","Cuke")))) +
          ggtitle(paste("Total revenue per year from",ifelse(cc==1,"Non-herring",
                                                             ifelse(cc %in% 2:3,"Non-salmon","Non-cuke")),"and", ifelse(cc==1,"Herring",
                                                                                                                        ifelse(cc %in% 2:3,"Salmon","Cuke")),"\nfishing for holders of",casestudy,"permits")))

  print(ggplot(bind_rows(p2 %>% dplyr::select(-pph),temp),aes(x=factor(year),y=log10(rev),fill=source)) +
          geom_boxplot(outlier.size=0.5) +
          labs(y="log10(annual revenue per fisher)",
               title=paste("All revenue and",
                           ifelse(cc==1,"herring-only",ifelse(cc %in% 2:3,"salmon-only","cuke-only")),
                           "revenue per fisher for fishers\nthat fished",casestudy,"permits")) +
          scale_fill_discrete(name="Revenue Source") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90)))


  #  Number of permits per permit holder. There are some outliers,
  #so I have created a plus group where anything greater than 5 permits becomes a 5
  p2a <- p2 %>%
    mutate(pph.plus=ifelse(pph>5,5,pph)) %>%
    group_by(year) %>%
    count(pph.plus)

  print(ggplot(p2a,aes(x=year,y=n,linetype=factor(pph.plus))) +
          geom_line() +
          scale_linetype_discrete(guide_legend(title="# of permits")) +
          ggtitle("Number of permits per permit holder") +
          theme_bw())

  #  Number of entrants and quitters per year (will need to delete the first year of the dataset for entrants and the last year for quitters)
  p3 <- mydat %>% group_by(p_holder) %>% summarise(year1=min(year),year2=max(year))

  #  Look at entrants
  p3a <- p3 %>%
    group_by(year1) %>%
    summarise(entrants=n()) %>%
    filter(year1>1985)

  #  Look at quitters.
  p3b <- p3 %>%
    group_by(year2) %>%
    summarise(quitters=n()) %>%
    filter(year2<2014)

  print(ggplot(bind_rows(data.frame(year=p3a$year1,n=p3a$entrants,group="Entrants"),
                         data.frame(year=p3b$year2,n=p3b$quitters,group="Quitters")),
               aes(year,n,linetype=group)) +
          geom_line() +
          geom_point() +
          theme_bw() +
          ylab("Number of permit holders"))

  # Diversity
  g1 = group_by(mydat, p_holder) %>%
    mutate(pws = ifelse(length(which(p_fshy%in%yourfishery))>0, 1, 0)) %>%
    filter(pws>0) %>%
    select(-pws) %>%
    group_by(p_holder, year, p_fshy) %>%
    summarize(g = sum(g_earn)) %>%
    group_by(p_holder, year) %>%
    summarize(div = simp.div(g)) %>%
    group_by(year) %>%
    summarize(mean = mean(div), lower=quantile(div,0.025), upper=quantile(div,0.975)) %>%
    ggplot(aes(year, mean)) + geom_line() + ggtitle(casestudy) + ylab("Mean diversity")
  #print(g1)

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
    summarize(n = sum(n))

  print(ggplot(totals, aes(year, n)) +
          geom_area(aes(fill = group), position = "stack", colour = 1) +
          xlab("Year") +
          ylab("People") +
          theme_bw())

  dev.off()
}
