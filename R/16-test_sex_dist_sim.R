# Sim --------------------------------------------------------------------------
lnt <- FALSE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-sim_calib_params.R")

control <- control_msm(
  nsteps = 52 * 65,
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  verbose = FALSE
)

param$riskh.start <- 52 * 1e5
param$prep.start <- 52 * 1e5

init <- init_msm(
  prev.ugc = 0.015,
  prev.rct = 0.015,
  prev.rgc = 0.015,
  prev.uct = 0.015
)

sim <- netsim(orig, param, init, control)
df <- as.data.frame(sim)

# Plots ------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_light())

names(df)

ggplot(df, aes(x = time, y = prepCurr)) +
  geom_line()

ggplot(df, aes(x = time, y = incid)) +
  geom_line()

ggplot(df, aes(x = time, y = prev.gc)) +
  geom_line()

ggplot(df, aes(x = time, y = prev.ct)) +
  geom_line()

ggplot(df, aes(x = time, y = ir100.gc)) +
  geom_line()

ggplot(df, aes(x = time, y = ir100.ct)) +
  geom_line()

ggplot(df, aes(x = time, y = prep.rand.stop)) +
  geom_line()

# testing!
ggplot(df, aes(x = time, y = tot.tests)) +
  geom_line()

# diags
ggplot(df, aes(x = time, y = cc.dx)) +
  geom_line()
