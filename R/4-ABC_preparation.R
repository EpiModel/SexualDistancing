# In this file we make the preparation required for the ABC calibration

library(EpiABC)
## library(EpiModelHIV)

# Try to create the "abc" folder. If it exists, throw an error
source("R/utils-ABC.R")
create_abc_folder()#force = TRUE)

# Main Model Fx -----------------------------------------------------------

f <- function(x) {
  lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
  source("R/utils-sim_calib_params.R", local = TRUE)

  control <- control_msm(
    nsteps = 52 * 65,
    nsims = 28,
    ncores = 28,
    save.nwstats = FALSE,
    save.clin.hist = FALSE,
    verbose = FALSE
  )

  init <- init_msm(
    prev.ugc = 0.015,
    prev.rct = 0.015,
    prev.rgc = 0.015,
    prev.uct = 0.015
  )

  set.seed(x[1])

  param$rgc.tprob <- x[2]
  param$ugc.tprob <- x[3]
  param$rct.tprob <- x[4]
  param$uct.tprob <- x[5]
  param$rgc.sympt.prob <- x[6]
  param$ugc.sympt.prob <- x[7]
  param$rct.sympt.prob <- x[8]
  param$uct.sympt.prob <- x[9]
  param$rgc.ntx.int <- x[10]
  param$ugc.ntx.int <- x[11]
  param$gc.tx.int <- x[12]
  param$rct.ntx.int <- x[13]
  param$uct.ntx.int <- x[14]
  param$ct.tx.int <- x[15]

 ##  # gaps appendix 9.3 - 9.4 (not explained this way but similar result)
 ##  param$sti.cond.eff <- x[16]
 ##  param$sti.cond.fail <- x[17]
 ##  # gaps appendix 9.2
 ##  param$hiv.rgc.rr <- x[18]
 ##  param$hiv.ugc.rr <- x[19]
 ##  param$hiv.rct.rr <- x[20]
 ##  param$hiv.uct.rr <- x[21]
 ## # if both ct + gc -> log(RRgc) + 0.2 * log(RRct) | swap ct and gc if RRct > RRgc
 ##  param$hiv.dual.rr <- x[22]
 ##
  param$trans.scale <- x[16:18]
  param$gc.sympt.prob.tx <- x[19:21]
  param$ct.sympt.prob.tx <- x[22:24]
  param$gc.asympt.prob.tx <- x[25:27]
  param$ct.asympt.prob.tx <- x[28:30]

  sim <- netsim(orig, param, init, control)
  df <- as.data.frame(sim)

  # Target calculation
  i.prev <- colMeans(tail(df[, c("i.prev.B", "i.prev.H", "i.prev.W")], 52))
  ir100.gc <- mean(tail(df[["ir100.gc"]], 52), na.rm = T)
  ir100.ct <- mean(tail(df[["ir100.ct"]], 52), na.rm = T)
  ir100.sti.B <- mean(tail(df[["ir100.sti.B"]], 52), na.rm = T)
  ir100.sti.H <- mean(tail(df[["ir100.sti.H"]], 52), na.rm = T)
  ir100.sti.W <- mean(tail(df[["ir100.sti.W"]], 52), na.rm = T)

  c(i.prev, ir100.gc, ir100.ct, ir100.sti.B, ir100.sti.H, ir100.sti.W)
}

# ABC Priors and Target Stats ---------------------------------------------

priors <- list(
  c("unif", .3, .6),   # rgc.tprob
  c("unif", .2, .5),   # ugc.tprob
  c("unif", .3, .6),   # rct.tprob
  c("unif", .2, .5),   # uct.tprob
  c("unif", .01, .15),   # rgc.sympt.prob
  c("unif", .6, .95),   # ugc.sympt.prob
  c("unif", .01, .15),   # rct.sympt.prob
  c("unif", .6, .95),   # uct.sympt.prob
  c("unif", 26, 52),   # rgc.ntx.int
  c("unif", 26, 52),   # ugc.ntx.int
  c("unif", 1, 15),   # gc.tx.int
  c("unif", 39, 65),   # rct.ntx.int
  c("unif", 39, 65),   # uct.ntx.int
  c("unif", 1, 15)   # ct.tx.int
  ## c("unif", .5, .8),   # sti.cond.eff
  ## c("unif", .5, .8),   # sti.cond.fail
  ## c("unif", .5, .8),   # hiv.rgc.rr
  ## c("unif", .5, .8),   # hiv.ugc.rr
  ## c("unif", .5, .8),   # hiv.rct.rr
  ## c("unif", .5, .8),   # hiv.uct.rr
  ## c("unif", .5, .8),   # hiv.dual.rr
)
priors <- c(
  priors,
  as.list(rep(list(c("unif", .1, 4)), 3)),  # trans.scale
  as.list(rep(list(c("unif", .8, 1)), 3)), # gc.sympt.prob.tx
  as.list(rep(list(c("unif", .8, 1)), 3)), # ct.sympt.prob.tx
  as.list(rep(list(c("unif", .01, .25)), 3)), # gc.asympt.prob.tx
  as.list(rep(list(c("unif", .01, .25)), 3))  # ct.asympt.prob.tx
)

i.prev <- c(.33, .127, .084) # overall * race_prev / race_freq
ir100.gc <- 4.4
ir100.ct <- 6.6
ir100.sti.B <- 6.4
ir100.sti.H <- 5.4
ir100.sti.W <- 2.4

targets <- c(i.prev, ir100.gc, ir100.ct, ir100.sti.B, ir100.sti.H, ir100.sti.W)

## # test before run
## x <- c(1, vapply(
##   priors,
##   function(x) runif(1, as.numeric(x[2]), as.numeric(x[3])),
##   1
## ))

## f(x)
# Run ABC Prep ------------------------------------------------------------

prep <- abc_smc_prep(
  model = f,
  prior = priors,
  nsims = 400,
  summary_stat_target = targets,
  ncores = 28,
  alpha = 0.3
)

saveRDS(prep, file = "abc/data/abc.prep.rds")

sbatch_master_abc(
  prep,
  nwaves = 10,
  master.file = "abc/master.sh",
  runsim.file = "abc/runsim.sh",
  mem = "150G",
  user = "aleguil",
  ckpt = TRUE
)
