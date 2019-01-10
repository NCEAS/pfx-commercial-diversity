# set user <- "your name"
source("plots/plot-preamble.R")

d <- group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(p_fshy %in%
      c("G 01E", "G 34E", "H 01E", "H 34E", "L 21E"))) > 0, 1, 0)) %>%
  filter(pws > 0) %>%
  mutate(case_study = "PWS herring")

ids_pre_1989 <- d %>%
  filter(region == "PWS") %>%
  filter(year %in% 1985:1988) %>%
  group_by(p_holder) %>%
  summarise(n = length(unique(startdt))) %>%
  filter(n >= 1) %>%
  arrange(-n)

ids_post_1989 <- d %>%
  filter(region == "PWS") %>%
  filter(year >= 1990) %>%
  group_by(p_holder) %>%
  summarise(n = length(unique(startdt))) %>%
  filter(n >= 1) %>%
  arrange(-n)

# a. how many PWS herring fishermen totally left fishing post-PWS herring closure in 1999?
length(ids_pre_1989$p_holder)
length(ids_post_1989$p_holder)
length(ids_pre_1989$p_holder[!ids_pre_1989$p_holder %in% ids_post_1989$p_holder])

# b. for fishermen who only fished for PWS herring prior to the closure and did not quit fishing entirely, what permits were they using after the closure?

ids_pre_1989_only_herring <- d %>%
  filter(region == "PWS") %>%
  filter(year <= 1988) %>%
  group_by(p_holder) %>%
  summarise(not_herring = sum(!p_fshy %in% c("G 01E", "G 34E", "H 01E", "H 34E", "L 21E"))) %>%
  filter(not_herring == 0) %>%
  select(-not_herring)

ids_stuck_arround <-
  ids_pre_1989_only_herring$p_holder[ids_pre_1989_only_herring$p_holder %in%
    ids_post_1989$p_holder]

coarse_df <- filter(d, p_holder %in% ids_stuck_arround) %>%
  filter(region == "PWS") # %>%
  # filter(year >= 1990)

coarse_df <- coarse_df %>%
  mutate(s = substr(p_fshy, 1, 1))
coarse_df$p_fshy[which(coarse_df$s == "S")] <- "S"
coarse_df$p_fshy[which(coarse_df$s == "B")] <- "B"
coarse_df$p_fshy[which(coarse_df$s == "C")] <- "C"
coarse_df$p_fshy[which(coarse_df$s %in% c("G", "H", "L"))] <- "GHL"
coarse_df$p_fshy[which(coarse_df$s %in% c("K", "T", "D"))] <- "KTD"
coarse_df$p_fshy[which(coarse_df$s %in% c("Q"))] <- "Q"
coarse_df$p_fshy[which(coarse_df$s %in% c("M"))] <- "M"
coarse_df$p_fshy[which(coarse_df$s %in% c("P"))] <- "P"
coarse_df$p_fshy[which(coarse_df$s %in% c(
  "S", "B", "C", "G", "H",
  "L", "K", "T", "D", "Q", "M", "P"
) == FALSE)] <- "O"

summary <- group_by(coarse_df, p_holder, year, p_fshy, case_study) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(p_holder, year) %>%
  mutate(totRev = sum(g)) %>%
  group_by(p_holder, year, case_study) %>%
  mutate(g = g / totRev) %>%
  select(-totRev)

# summary2 <- group_by(summary, year, p_holder) %>%


# wide <- reshape2::dcast(summary, year + p_holder ~ p_fshy)

# use reshape 2 to cast this to wide format
aql <- melt(summary, id.vars = c(
  "year", "p_fshy",
  "p_holder", "case_study"
))
aqw <- dcast(aql, year + p_holder + case_study ~ p_fshy)
# replace NAs w/0s
for (i in 1:ncol(aqw)) {
  aqw[which(is.na(aqw[, i])), i] <- 0
}

kss <- 0
# set.seed(28192)
# for (j in seq_len(15)) {
#   kss[j] <- kmeans(aqw[, -c(1:3), drop = FALSE], j, iter.max = 100)$tot.withinss
# }

library(doParallel)
registerDoParallel(cores = parallel::detectCores())

set.seed(25481)
seeds <- sample.int(1e4, size = 50)
out <- plyr::laply(seeds, function(x) {
  set.seed(x)
  kmeans(aqw[, -c(1:3), drop = FALSE], centers = 9, iter.max = 100)$tot.withinss
}, .parallel = TRUE)

# set the seed to one of the global minimums:
set.seed(seeds[[which(out == min(out))[[1]]]])
clust <- kmeans(aqw[, -c(1:3), drop = FALSE], centers = 9, iter.max = 100)

# cluster based on subset to pick medoids
# clust = fpc::pamk(aqw[sample(seq(1,nrow(aqw)),size=5000,replace=F),-c(1:3)])
medoids <- clust$centers

names_medoids <- ""
for (i in seq_len(nrow(medoids))) {
  names_medoids[i] <-
    paste0(colnames(medoids)[which(medoids[i, ] >= 0.1)], collapse = ":")
}

# clust = kmeans(coarse_df, centers = medoids)

aqw$clusters <- clust$cluster
totals <- group_by(aqw, clusters, year, case_study) %>%
  summarize(n = n())
totals$group <- names_medoids[totals$clusters]

totals <- group_by(totals, year, group, case_study) %>%
  summarize(n = sum(n)) %>%
  group_by(year, case_study) %>%
  mutate(n_year = sum(n))

all <- unique(select(totals, year, group, case_study))
all <- expand.grid(year = unique(all$year),
  group = unique(all$group), case_study = unique(all$case_study))

missing <- anti_join(all, totals)
missing$n <- 0
missing$n_year <- 1
plot_dat <- bind_rows(missing, totals)
plot_dat <- arrange(plot_dat, year, group, case_study)

fig <- ggplot(rename(plot_dat, Fisheries = group), aes(year, n / n_year)) +
  geom_area(aes(fill = Fisheries), position = "stack", colour = "grey90",
    lwd = 0.3) +
  xlab("Year") +
  ylab("Proportion of permit holders") +
  theme_sleek() + #facet_wrap(~case_study) +
  viridis::scale_fill_viridis(discrete = TRUE, end = 0.98) +
  coord_cartesian(expand = FALSE) +
  theme(legend.position = "right") +
  theme(strip.text.x = element_text(size = rel(1.0))) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
  geom_vline(xintercept = 1989, lty = 2)

fig

ggsave("Fig_pws_herring.pdf", width = 4.8, height = 3)
