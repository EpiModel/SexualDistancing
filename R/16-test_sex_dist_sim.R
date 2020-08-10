# Sim --------------------------------------------------------------------------
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-sim_calib_params.R")

control <- control_msm(
  nsteps = 52 * 4,
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  verbose = TRUE,
)

param$netresim.form.rr = rep(1, 3)
param$netresim.disl.rr = c(1, 1)

init <- init_msm(
  prev.ugc = 0.015,
  prev.rct = 0.015,
  prev.rgc = 0.015,
  prev.uct = 0.015
)

param$param_updaters <- list(
  list(
    at = 52 * 1, verbose = TRUE,
    param = list(
      netresim.form.rr = rep(2, 3),
      netresim.disl.rr = rep(0.1, 2)
    )
  ),
  list(
    at = 52 * 2, verbose = TRUE,
    param = list(
      netresim.form.rr = rep(10, 3),
      netresim.disl.rr = rep(1, 2)
    )
  )
)

sim <- netsim(orig, param, init, control)
df <- as.data.frame(sim)

plot(df$time, df$incid)
