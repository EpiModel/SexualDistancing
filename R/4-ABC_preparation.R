# In this file we make the preparation required for the ABC calibration

library(EpiABC)
## library(EpiModelHIV)

# Try to create the "abc" folder. If it exists, throw an error
source("R/utils-ABC.R")
create_abc_folder(force = TRUE)

# Main Model Fx -----------------------------------------------------------

f <- function(x) {
  lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
  source("R/utils-sim_calib_params.R", local = TRUE)

  control <- control_msm(
    nsteps = 52 * 68,
    ## nsims = 3,
    ## ncores = 3,
    ## save.nwstats = FALSE,
    ## save.clin.hist = FALSE,
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
  param$rgc.ntx.int <- x[6]
  param$ugc.ntx.int <- x[7]
  param$rct.ntx.int <- x[8]
  param$uct.ntx.int <- x[9]
  ## param$gc.tx.int <- x[12]
  ## param$ct.tx.int <- x[15]
#   param$trans.scale  <- x[6:8]
  ## param$rgc.sympt.prob <- x[6]
  ## param$ugc.sympt.prob <- x[7]
  ## param$rct.sympt.prob <- x[8]
  ## param$uct.sympt.prob <- x[9]

  ## ## # gaps appendix 9.3 - 9.4 (not explained this way but similar result)
  ## param$sti.cond.eff <- x[16]
  ## param$sti.cond.fail <- x[17:19]
  ## param$gc.sympt.prob.tx  <- x[20:22]
  ## param$ct.sympt.prob.tx  <- x[23:25]
  ## param$gc.asympt.prob.tx <- x[26:28]
  ## param$ct.asympt.prob.tx <- x[29:31]

  # gaps appendix 9.2
 ##  param$hiv.rgc.rr <- x[18]
 ##  param$hiv.ugc.rr <- x[19]
 ##  param$hiv.rct.rr <- x[20]
 ##  param$hiv.uct.rr <- x[21]
 ## # if both ct + gc -> log(RRgc) + 0.2 * log(RRct) | swap ct and gc if RRct > RRgc
 ##  param$hiv.dual.rr <- x[22]
 ##

  ## param$trans.scale  <- x[28:30]
  sim <- netsim(orig, param, init, control)
  df <- as.data.frame(sim)

  # Target calculation
  i.prev <- colMeans(tail(df[, c("i.prev.B", "i.prev.H", "i.prev.W")], 52*3))
  ir100.gc <- mean(tail(df[["ir100.gc"]], 52*3), na.rm = T)
  ir100.ct <- mean(tail(df[["ir100.ct"]], 52*3), na.rm = T)


  ir100.sti.B <- mean(tail(df[["ir100.sti.B"]], 52*3), na.rm = T) / ir100.sti
  ir100.sti.H <- mean(tail(df[["ir100.sti.H"]], 52*3), na.rm = T) / ir100.sti
  ir100.sti.W <- mean(tail(df[["ir100.sti.W"]], 52*3), na.rm = T) / ir100.sti

  ir100.sti <- ir100.sti.B + ir100.sti.H + ir100.sti.W

  ir100.sti.B <- ir100.sti.B / ir100.sti
  ir100.sti.H <- ir100.sti.H / ir100.sti
  ir100.sti.W <- ir100.sti.W / ir100.sti

  p <- c(
    ## i.prev,
    ir100.gc, ir100.ct, ir100.sti.B, ir100.sti.H, ir100.sti.W)
  names(p) <- c(
    ## "i.prev.B", "i.prev.H", "i.prev.W",
    "ir100.gc", "ir100.ct", "ir100.sti.B", "ir100.sti.H", "ir100.sti.W")

  p
}

# ABC Priors and Target Stats ---------------------------------------------

priors <- list(
  c("unif", .10, .6),   # rgc.tprob
  c("unif", .10, .6),   # ugc.tprob
  c("unif", .10, .6),   # rct.tprob
  c("unif", .10, .6),   # uct.tprob
  c("unif", 26, 52),   # rgc.ntx.int
  c("unif", 26, 52),   # ugc.ntx.int
  c("unif", 39, 65),   # rct.ntx.int
  c("unif", 39, 65)#,   # uct.ntx.int
  ## c("unif", 1, 15),   # gc.tx.int
  ## c("unif", 1, 15),   # ct.tx.int
  
  ## c("unif", 1, 4),   # trans.scale.B
  ## c("unif", .2, .6),   # trans.scale.H
  ## c("unif", .1, .44)   # trans.scale.W

  ## c("unif", .01, .15),   # rgc.sympt.prob
  ## c("unif", .6, .95),   # ugc.sympt.prob
  ## c("unif", .01, .15),   # rct.sympt.prob
  ## c("unif", .6, .95),   # uct.sympt.prob
  ## c("unif", .5, .8)#,   # sti.cond.eff
  ## c("unif", .5, .8),   # hiv.rgc.rr
  ## c("unif", .5, .8),   # hiv.ugc.rr
  ## c("unif", .5, .8),   # hiv.rct.rr
  ## c("unif", .5, .8),   # hiv.uct.rr
  ## c("unif", .5, .8),   # hiv.dual.rr
)
## priors <- c(
##   priors,
##   as.list(rep(list(c("unif", .1, .5)), 3)),  # sti.cond.fail
##   as.list(rep(list(c("unif", .8, 1)), 3)), # gc.sympt.prob.tx
##   as.list(rep(list(c("unif", .8, 1)), 3)), # ct.sympt.prob.tx
##   as.list(rep(list(c("unif", .01, .25)), 3)), # gc.asympt.prob.tx
##   as.list(rep(list(c("unif", .01, .25)), 3))  # ct.asympt.prob.tx
## )

i.prev <- c(.33, .127, .084) # overall * race_prev / race_freq
ir100.gc <- 4.4
ir100.ct <- 6.6

ir100.sti.B <- 6.4
ir100.sti.H <- 5.4
ir100.sti.W <- 2.4

ir100.sti <- ir100.sti.B + ir100.sti.H + ir100.sti.W

ir100.sti.B <- ir100.sti.B / ir100.sti
ir100.sti.H <- ir100.sti.H / ir100.sti
ir100.sti.W <- ir100.sti.W / ir100.sti

targets <- c(
  # i.prev,
  ir100.gc, ir100.ct, ir100.sti.B, ir100.sti.H, ir100.sti.W)

# Run ABC Prep ------------------------------------------------------------

prep <- abc_smc_prep(
  model = f,
  prior = priors,
  nsims = 420,
  summary_stat_target = targets,
  ncores = 28,
  alpha = 0.1
)

saveRDS(prep, file = "abc/data/abc.prep.rds")

## sbatch_master_abc(
##   prep,
##   nwaves = 25,
##   master.file = "abc/master.sh",
##   runsim.file = "abc/runsim.sh",
##   mem = "248G",
##   user = "aleguil",
##   ckpt = TRUE
## )


# test before run
## x <- c(3, vapply(
##   priors,
##   function(x) runif(1, as.numeric(x[2]), as.numeric(x[3])),
##   1
## ))

## df <- f(x)

library(glue)

nwaves <- 25
mem <- "150G"
user <- "aleguil"
master.file <- "abc/master.sh"

cat("#!/bin/bash", file = master.file)
cat("\n\n", file = master.file, append = TRUE)

cat(
  glue("sbatch -p ckpt -A csde-ckpt --array=1-15 --job-name=wave0_{user} --export=ALL,wave=0  --ntasks-per-node=28 --mem={mem} --time=1:00:00 abc/runsim.sh"),
  file = master.file, append = TRUE
)
cat("\n", file = master.file, append = TRUE)

cat(
  glue("sbatch -p ckpt -A csde-ckpt --job-name=process0_{user} --export=ALL,wave=0 --depend=afterany:$(squeue --noheader --Format arrayjobid --name wave0_{user} | uniq) -c 1 --mem=15G --time=1:00:00 abc/runprocess.sh"),
  file = master.file, append = TRUE
  )
cat("\n\n", file = master.file, append = TRUE)


for (i in seq_len(nwaves)) {
  cat(
    glue("sbatch -p ckpt -A csde-ckpt --array=1-14 --job-name=wave{i}_{user} --export=ALL,wave={i} --depend=afterany:$(squeue --noheader --Format arrayjobid --name process{i-1}_{user} | uniq) --ntasks-per-node=28 --mem={mem} --time=1:00:00 abc/runsim.sh"),
         file = master.file, append = TRUE
         )
    cat("\n", file = master.file, append = TRUE)

    cat(
      glue("sbatch -p ckpt -A csde-ckpt --job-name=process{i}_{user} --export=ALL,wave={i} --depend=afterany:$(squeue --noheader --Format arrayjobid --name wave{i}_{user} | uniq) -c 1 --mem=15G --time=1:00:00 abc/runprocess.sh"),
           file = master.file, append = TRUE
           )
      cat("\n\n", file = master.file, append = TRUE)
}
