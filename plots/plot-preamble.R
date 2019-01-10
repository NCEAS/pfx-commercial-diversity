library(tidyverse)
library(reshape2)
library(fpc)
# devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)

if (!exists("user"))
  stop("Please define the object `user` as 'Eric', 'Sean', or 'Jordan' ",
    "on the command line to load the correct (confidential) raw data.", call. = FALSE)

if (user[[1]] == "Eric")
  load(here::here("/users/eric.ward/documents/CFEC/data/cfec_070616"))
if (user[[1]] == "Sean")
  load(here::here("../pfx-commercial/data/cfec_070616"))
if (user[[1]] == "Jordan")
  load(here::here("../cfec_070616.Rdata"))

if (!exists("cfec")) stop("The data frame `cfec` must be loaded first.")

# Deflate the data per Sean's paper
deflationTable <- read.csv(here::here("data/deflation.csv"))
# Adjust price and g_earn for inflation
cfec$year <- as.numeric(cfec$year)
cfec$day <- substr(cfec$landdate, 6, 10)
cfec <- inner_join(cfec, deflationTable)
cfec <- mutate(cfec, g_price = g_price / defl, g_earn = g_earn / defl)
cfec$defl <- NULL

#  Get Anne's regions
region <- read.csv(here::here("data/regions.csv"))
cfec <- cfec %>%
  inner_join(region[, c("stat6", "final")]) %>%
  rename(region = final)

cfec$year <- as.numeric(cfec$year)

simp.div <- function(x) {
  1 / sum((x / sum(x))^2)
}

golden_ratio <- 1.618
