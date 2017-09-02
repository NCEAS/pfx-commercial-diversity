library(dplyr)
library(knitr)
library(ggplot2)
library(date)


cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")

# convert 6-dig p_fshy codes to 5 digits
# removing blank space - now they're all length(5)
idx = which(nchar(cfec$p_fshy)==6)
cfec$p_fshy[idx] = paste(substr(cfec$p_fshy[idx], 1, 1),
  substr(cfec$p_fshy[idx], 3, 6), sep="")

# remove white space in 2nd position
idx = which(substr(cfec$p_fshy, 2, 2) == " ")
cfec$p_fshy[idx] = paste(substr(cfec$p_fshy[idx], 1, 1),
  substr(cfec$p_fshy[idx], 3, length(cfec$p_fshy[idx])), sep="")

cfec = group_by(cfec, p_holder, year) %>%
  mutate(n = length(unique(p_fshy))) %>%
  filter(n == 1)

cfec = cfec[cfec$p_fshy %in% c("B06B","B61B","C61B","C06B"),]

# Calculate mean and CV of revenue before / after catch shares
cv_people = group_by(cfec, p_holder) %>%
  summarize(mu.pre = mean(g_earn[year%in%seq(1990,1994)], na.rm=T),
    mu.post = mean(g_earn[year%in%seq(1996,2000)], na.rm=T),
    sd.pre = sd(g_earn[year%in%seq(1990,1994)], na.rm=T),
    sd.post = sd(g_earn[year%in%seq(1996,2000)], na.rm=T),
    cv.pre = sd.pre/mu.pre, cv.post=sd.post/mu.post)

hist(cv_people$cv.post-cv_people$cv.pre, 40, col="grey")
hist(cv_people$mu.post-cv_people$mu.pre, 40, col="grey")
# Calculate mean and CV of revenue before / after catch shares
cv_boat = group_by(cfec, cadfg) %>%
  summarize(mu.pre = mean(g_earn[year%in%seq(1990,1994)], na.rm=T),
    mu.post = mean(g_earn[year%in%seq(1996,2000)], na.rm=T),
    sd.pre = sd(g_earn[year%in%seq(1990,1994)], na.rm=T),
    sd.post = sd(g_earn[year%in%seq(1996,2000)], na.rm=T),
    cv.pre = sd.pre/mu.pre, cv.post=sd.post/mu.post)

hist(cv_boat$cv.post-cv_boat$cv.pre, 40, col="grey")
hist(cv_boat$mu.post-cv_boat$mu.pre, 40, col="grey")

par(mfrow = c(2,1), mgp = c(2,1,0), mai = c(0.5,0.5,0.1,0.1))
hist(log(cv_people$cv.post/cv_people$cv.pre), breaks = seq(-8,8,length.out=100), col="grey", xlim=c(-8,8))
hist(log(cv_boat$cv.post/cv_boat$cv.pre), breaks = seq(-8,8,length.out=100), col="grey", xlim=c(-8,8))

par(mfrow = c(2,1), mgp = c(2,1,0), mai = c(0.5,0.5,0.1,0.1))
hist(log(cv_people$mu.post/cv_people$mu.pre), breaks = seq(-8,8,length.out=100), col="grey", xlim=c(-8,8))
hist(log(cv_boat$mu.post/cv_boat$mu.pre), breaks = seq(-8,8,length.out=100), col="grey", xlim=c(-8,8))
