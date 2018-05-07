# set user <- "your name"
source("plots/plot-preamble.R")

# Make Figure 3 -- revenue as main axis, permit holders as secondary
sub_1 = filter(cfec, p_fshy %in% c("G 01E","G 34E", "H 01E", "H 34E", "L 21E")) %>%
  mutate(case_study = "PWS herring")
sub_2 = filter(cfec, p_fshy %in% c("B 06B","B 61B")) %>%
  mutate(case_study = "Halibut")
sub_3 = filter(cfec, p_fshy %in% c("S 03T")) %>%
  mutate(case_study = "Bristol Bay Drift Gillnet")
sub_4 = filter(cfec, region %in% c("Kodiak")) %>%
  mutate(case_study = "Kodiak")

totals = dplyr::bind_rows(sub_1, sub_2, sub_3, sub_4)

totals$case_study <- factor(totals$case_study, levels = c(
  "PWS herring",
  "Halibut",
  "Bristol Bay Drift Gillnet",
  "Kodiak"
))

totals$case_study <- forcats::fct_recode(totals$case_study,
  `Halibut` = "Halibut",
  `PWS herring` = "PWS herring",
  `BBDG salmon` = "Bristol Bay Drift Gillnet",
  `EVOS commercial (Kodiak only)` = "Kodiak")

temp <- group_by(totals, year, case_study) %>%
  summarize(fishers=length(unique(p_holder))/100,
    rev=(sum(g_earn)/1e6)) %>%
  reshape2::melt(id.vars = c("year", "case_study"), variable.name = "rev_or_fishers")

temp_rev <- mutate(temp, case_study = forcats::fct_recode(case_study,
  `(d) Halibut` = "Halibut",
  `(b) PWS herring` = "PWS herring",
  `(f) BBDG salmon` = "BBDG salmon",
  `(h) EVOS commercial (Kodiak)` = "EVOS commercial (Kodiak only)")
)

temp_part <- mutate(temp, case_study = forcats::fct_recode(case_study,
  `(c) Halibut` = "Halibut",
  `(a) PWS herring` = "PWS herring",
  `(e) BBDG salmon` = "BBDG salmon",
  `(g) EVOS commercial (Kodiak)` = "EVOS commercial (Kodiak only)")
)

make_plot <- function(dat, ylab = "") {
  g <- ggplot(dat, aes_string("year", "value")) +
    geom_line(colour = "grey40", lwd = 0.85) +
    theme_sleek() +
    xlab("Year") + ylim(0, NA) +
    guides(colour = FALSE) +
    ylab(ylab) +
    theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
    theme(strip.text.x = element_text(size = rel(1.0)))
  g <- g + facet_wrap(~case_study, scale="free_y", ncol = 1)
  g
}
g1 <- filter(temp_part, rev_or_fishers == "fishers") %>%
  make_plot(ylab = "Participation (100 permit holders)")
g2 <- filter(temp_rev, rev_or_fishers == "rev") %>%
  make_plot(ylab = "Revenue (million USD)")

pdf("Fig_3.pdf", width = 5, height = 5.7)
cowplot::plot_grid(g1, g2, align = "v")
dev.off()
