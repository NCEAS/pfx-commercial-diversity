# set user <- "your name"
source("plots/plot-preamble.R")

sub_1 = filter(cfec, p_fshy %in% c("S 01E", "S 03E")) %>%
  mutate(case_study = "Prince William Sound salmon")
sub_2 = filter(cfec, region %in% c("PWS")) %>%
  mutate(case_study = "Prince William Sound")
sub_3 = filter(cfec, region %in% c("Cook Inlet")) %>%
  mutate(case_study = "Cook Inlet")

totals = dplyr::bind_rows(sub_1, sub_2, sub_3)

totals$case_study <- factor(totals$case_study, levels = c(
  "Prince William Sound salmon",
  "Prince William Sound",
  "Cook Inlet"
))

totals$case_study <- forcats::fct_recode(totals$case_study,
  `PWSPS salmon fishery` = "Prince William Sound salmon",
  `PWS commercial fisheries (EVOS-area)` = "Prince William Sound",
  `Cook Inlet commercial fisheries (EVOS-area)` = "Cook Inlet")

# fig = group_by(totals, year, case_study) %>%
#   summarize(fishers=length(unique(p_holder)),
#     rev=sum(g_earn)/1000000) %>%
#   ggplot(aes(year, rev)) + geom_line(colour = "dodgerblue1", size=1.2) +
#   facet_wrap(~case_study, scale="free_y", ncol=2, nrow=2) +
#   theme_sleek() +
#   ylab("Revenue (million USD)") +
#   xlab("Year") +
#   geom_line(aes(y = fishers/10), colour = "dodgerblue4", size=1.2) +
#   scale_y_continuous(sec.axis = sec_axis(~.*(10), name = "Participation"))
# pdf("Fig_A5.pdf")
# fig
# dev.off()

temp <- group_by(totals, year, case_study) %>%
  summarize(fishers=length(unique(p_holder))/100,
    rev=(sum(g_earn)/1e6)) %>%
  reshape2::melt(id.vars = c("year", "case_study"), variable.name = "rev_or_fishers")

temp_rev <- mutate(temp, case_study = forcats::fct_recode(case_study,
  `(a) PWSPS salmon fishery` = "PWSPS salmon fishery",
  `(c) PWS commercial fisheries\n     (EVOS-area)` = "PWS commercial fisheries (EVOS-area)",
  `(e) Cook Inlet commercial fisheries\n     (EVOS-area)` = "Cook Inlet commercial fisheries (EVOS-area)")
)

temp_part <- mutate(temp, case_study = forcats::fct_recode(case_study,
  `(b) PWSPS salmon fishery` = "PWSPS salmon fishery",
  `(d) PWS commercial fisheries\n     (EVOS-area)` = "PWS commercial fisheries (EVOS-area)",
  `(f) Cook Inlet commercial fisheries\n    (EVOS-area)` = "Cook Inlet commercial fisheries (EVOS-area)")
)

make_plot <- function(dat, ylab = "") {
  ggplot(dat, aes_string("year", "value")) +
    geom_line(colour = "grey40", lwd = 0.85) +
    facet_wrap(~case_study, scale="free_y", ncol = 1) +
    theme_sleek() +
    xlab("Year") + ylim(0, NA) +
    guides(colour = FALSE) +
    ylab(ylab) +
    theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
    theme(strip.text.x = element_text(size = rel(1)))
}
g1 <- filter(temp_part, rev_or_fishers == "fishers") %>%
  make_plot(ylab = "Participation (100 permit holders)")
g2 <- filter(temp_rev, rev_or_fishers == "rev") %>%
  make_plot(ylab = "Revenue (million USD)")

pdf("Fig_A5.pdf", width = 5.5, height = 5.4)
cowplot::plot_grid(g2, g1, align = "v")
dev.off()
