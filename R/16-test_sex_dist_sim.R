# Sim --------------------------------------------------------------------------
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-sim_calib_params.R")

control <- control_msm(
  nsteps = 52 * 5,
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  verbose = FALSE
)

param$riskh.start <- 52 * 0
param$prep.start <- 52 * 1
param$rgc.tprob <- param$rgc.tprob / 2.1 # 0.357
param$ugc.tprob <- param$ugc.tprob / 2.1 # 0.248
param$rct.tprob <- param$rct.tprob / 2.3 # 0.3216
param$uct.tprob <- param$uct.tprob / 2.3 # 0.213

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

ggplot(df, aes(x = time, y = ir100.sti.B, col = factor(sim))) +
  geom_line()

ggplot(df, aes(x = time, y = ir100.sti.H, col = factor(sim))) +
  geom_line()

ggplot(df, aes(x = time, y = ir100.sti.W, col = factor(sim))) +
  geom_line()

ggplot(df, aes(x = time, y = ir100.gc, col = factor(sim))) +
  geom_line()

ggplot(df, aes(x = time, y = ir100.ct, col = factor(sim))) +
  geom_line()

options(max.print = 200)
names(df)
options(max.print = 100)

ggplot(df, aes(x = time, y = prepCurr/prepElig)) +
  geom_line()

ggplot(df, aes(x = time, y = incid)) +
  geom_line()

ggplot(df, aes(x = time, y = prev.gc)) +
  geom_line()

ggplot(df, aes(x = time, y = prev.ct)) +
  geom_line()

ggplot(df, aes(x = time, y = incid.gc)) +
  geom_line()

ggplot(df, aes(x = time, y = incid.ct)) +
  geom_line()

ggplot(df, aes(x = time, y = prep.rand.stop)) +
  geom_line()

# testing!
ggplot(df, aes(x = time, y = tot.tests)) +
  geom_line()

# diags
ggplot(df, aes(x = time, y = cc.dx)) +
  geom_line()
