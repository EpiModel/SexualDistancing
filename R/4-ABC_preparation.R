# In this file we make the preparation required for the ABC calibration

library(EpiABC)
## library(EpiModelHIV)

# Try to create the "abc" folder. If it exists, throw an error
source("R/utils-ABC.R")
create_abc_folder(force = TRUE)

t_names <- c("ir100.sti.B", "ir100.sti.H")
# Main Model Fx -----------------------------------------------------------

f <- function(x) {
  lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
  t_names <- c("ir100.sti.B", "ir100.sti.H")
  source("R/utils-sim_calib_params.R", local = TRUE)

  logit <- function(p) log(p / (1 - p))
  logistic <- function(p) 1 / (1 + exp(-p))

  control <- control_msm(
    nsteps = 52 * 68,
    ## nsims = 3,
    ## ncores = 3,
    ## save.nwstats = FALSE,
    ## save.clin.hist = FALSE,
    verbose = FALSE
  )

  init <- init_msm(
    prev.ugc = 0.05,
    prev.rct = 0.05,
    prev.rgc = 0.05,
    prev.uct = 0.05
  )

  set.seed(x[1])

  # For gc and ct, I use a OR of 1.25 for R vs U infection. This is based on
  # prep disparities apdx 9.3, 9.4 with baseline 0.35, 0.4 and 0.32, 0.4 for GC
  # and CT. (35 and 4 are mean of intervall),
  # I have no info in the targets to distinguish U to R. So no reason to have 2 params
  param$ugc.tprob <- 0.169 # x[2]
  param$rgc.tprob <- logistic(logit(param$rgc.tprob) + log(1.25))
  param$uct.tprob <- 0.126 # x[2]
  param$rct.tprob <- logistic(logit(param$rct.tprob) + log(1.25))

  # Same duration for U and R, as in prep disp
  param$rgc.ntx.int <- 26 # round(x[3], 0)
  param$ugc.ntx.int <- param$rgc.ntx.int
  param$rct.ntx.int <- 32 # round(x[3], 0)
  param$uct.ntx.int <- param$rct.ntx.int

  # One base sympt * OR U>R using apdx 10.2, 10.3
  # exp(logit(0.9) - logit(0.16)) = 47.25
  # exp(logit(0.58) - logit(0.14)) = 8.5
  param$rgc.sympt.prob <- 0.146 # x[4]
  param$ugc.sympt.prob <- logistic(logit(param$ugc.sympt.prob) + log(47.25))
  param$rct.sympt.prob <- 0.382 # x[4]
  param$uct.sympt.prob <- logistic(logit(param$uct.sympt.prob) + log(8.5))

  param$gc.sympt.prob.tx  <- x[2:4]
  param$ct.sympt.prob.tx  <- x[2:4]
  param$gc.asympt.prob.tx <- x[5:7]
  param$ct.asympt.prob.tx <- x[5:7]

  ## ## param$gc.tx.int <- x[12]
  ## param$ct.tx.int <- x[15]
  ##  param$trans.scale  <- x[6:8]

  ## ## # gaps appendix 9.3 - 9.4 (not explained this way but similar result)
  ## param$sti.cond.eff <- x[16]
  ## param$sti.cond.fail <- x[17:19]

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


  ir100.sti.B <- mean(tail(df[["ir100.sti.B"]], 52*3), na.rm = T)
  ir100.sti.H <- mean(tail(df[["ir100.sti.H"]], 52*3), na.rm = T)
  ir100.sti.W <- mean(tail(df[["ir100.sti.W"]], 52*3), na.rm = T)

  ir100.sti <- ir100.sti.B + ir100.sti.H + ir100.sti.W

  ir100.sti.B <- ir100.sti.B / ir100.sti
  ir100.sti.H <- ir100.sti.H / ir100.sti
  ir100.sti.W <- ir100.sti.W / ir100.sti

  p <- c(
    i.prev, ir100.gc, ir100.ct,
    ir100.sti.B, ir100.sti.H, ir100.sti.W
  )
  names(p) <- c(
    "i.prev.B", "i.prev.H", "i.prev.W",
    "ir100.gc", "ir100.ct",
    "ir100.sti.B", "ir100.sti.H", "ir100.sti.W"
  )

  p[t_names]
}

# ABC Priors and Target Stats ---------------------------------------------

## priors <- list(
##   c("unif", .15, .4),     # ugc.tprob (before, 0.1 - 0.6)
##   c("unif", .10, .6),  # rgc.tprob - now with 1.25 OR
##   c("unif", .10, .4),     # uct.tprob
##   c("unif", .10, .6),  # rct.tprob - now with 1.25 OR
##   c("unif", 20, 52),      # rgc.ntx.int  (before 26 - 52)
##   c("unif", 26, 52),   # ugc.ntx.int- now U and R same duration
##   c("unif", 20, 52),       # rct.ntx.int (before 39 - 65)
##   c("unif", 39, 65)    # uct.ntx.int - now U and R same duration
##   c("unif", .01, .2),    # rgc.sympt.prob
##   c("unif", .6, .95),  # ugc.sympt.prob - now with OR 47.25
##   c("unif", .01, .6),     # rct.sympt.prob
##   c("unif", .6, .95)  # uct.sympt.prob - now with OR 8.5
## )

priors <- c(
##   priors,
  as.list(rep(list(c("unif", .8, 1)), 3)),    # gc.sympt.prob.tx
##   as.list(rep(list(c("unif", .8, 1)), 3)),    # ct.sympt.prob.tx
  ## as.list(rep(list(c("unif", .01, .25)), 3)), # gc.asympt.prob.tx
  as.list(rep(list(c("unif", .01, .25)), 3))  # ct.asympt.prob.tx
)
##   ## c("unif", 1, 15),   # gc.tx.int
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
  ## i.prev,
  ## ir100.gc, ir100.ct,
  ir100.sti.B, ir100.sti.H
## , ir100.sti.W
)

# Run ABC Prep ------------------------------------------------------------

prep <- abc_smc_prep(
  model = f,
  prior = priors,
  nsims = 840,
  summary_stat_target = targets,
  ncores = 28,
  alpha = 0.2
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
##  priors,
##  function(x) runif(1, as.numeric(x[2]), as.numeric(x[3])),
##  1
## ))

## df <- f(x)

library(glue)

nwaves <- 15
mem <- "150G"
user <- "aleguil"
master.file <- "abc/master.sh"
partition <- "ckpt" #"csde", #"ckpt",
account <- "csde-ckpt" #"csde", #"csde-ckpt",
after <- "afterany" # afterany (start if previous finished with any status)
batchSize <- prep$batchSize
ncores = prep$ncores

cat("#!/bin/bash", file = master.file)
cat("\n\n", file = master.file, append = TRUE)

cat(
  glue("w0=$(sbatch -p {partition} -A {account} --array=1-{batchSize[1]}",
       " --job-name=wave0_{user} --export=ALL,wave=0  --ntasks-per-node={ncores}",
       " --mem={mem} --time=1:00:00 --parsable abc/runsim.sh)"),
  file = master.file, append = TRUE
)
cat("\n", file = master.file, append = TRUE)

cat(
  glue("p0=$(sbatch -p {partition} -A {account} --job-name=process0_{user}",
       " --export=ALL,wave=0 --depend={after}:$w0 -c 1 --mem=15G", 
       " --time=1:00:00 --parsable abc/runprocess.sh)"),
  file = master.file, append = TRUE
)
cat("\n\n", file = master.file, append = TRUE)


for (i in seq_len(nwaves)) {
  cat(
    glue("w{i}=$(sbatch -p {partition} -A {account} --array=1-{batchSize[2]}", 
         " --job-name=wave{i}_{user} --export=ALL,wave={i}", 
         " --depend={after}:$p{i-1} --ntasks-per-node={ncores} --mem={mem}", 
         " --time=1:00:00 --parsable abc/runsim.sh)"),
    file = master.file, append = TRUE
  )
  cat("\n", file = master.file, append = TRUE)

  cat(
    glue("p{i}=$(sbatch -p {partition} -A {account}", 
         " --job-name=process{i}_{user} --export=ALL,wave={i}", 
         " --depend={after}:$w{i} -c 1 --mem=15G --time=1:00:00", 
         " --parsable abc/runprocess.sh)"),
    file = master.file, append = TRUE
  )
  cat("\n\n", file = master.file, append = TRUE)
}

