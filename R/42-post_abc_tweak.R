library(EpiABC)
library(tidyverse)

partition <- "ckpt" # csde / ckpt

parms_steps <- function(low, high, n) {
  low + (high - low) * c(0, 1:(n - 1)/(n - 1))
}

n_cut <- 4
parms_bounds <- list(
  gc_tprob = c(0.18, 0.195), #15-25
  ## gc_dur = c(20, 50),
  gc_txprob = c(0.9, 0.95), #8-98

  ct_tprob = c(0.16, 0.175),
  ## gc_dur = c(20, 50),
  ct_txprob = c(0.90, 0.95)
  ##
  ## trans.scale1 = c(2.575, 2.7),
  ## trans.scale2 = c(0.35, 0.4),
  ## trans.scale3 = c(0.235, 0.27)
)

lxs <- map_df(parms_bounds, ~ parms_steps(.x[1], .x[2], n_cut))
xs <- lxs %>%
  cross_df() %>%
  asplit(1)

info <- list()
info$root_dir <- paste0("slurm_", partition)

saveRDS(xs, paste0("out/", info$root_dir, "_xs.rds"))

mod_fun <- function(x, sim_num,
                    orig, param, init, control, info, keep = 3 * 52) {
  library(EpiModelHIV)

  logit <- function(p) log(p / (1 - p))
  logistic <- function(p) 1 / (1 + exp(-p))

  # appendix 9.4 I take the mean of each prior dist
  # exp(logit(0.4) - logit(0.35)) = 1.25
  param$ugc.tprob <- x[1] #0.13
  param$rgc.tprob <- logistic(logit(param$ugc.tprob) + log(1.25)) # appendix 9.4
  param$uct.tprob <- x[3] # 0.115
  param$rct.tprob <- logistic(logit(param$uct.tprob) + log(1.25))

  # Same duration for U and R, as in prep disp
  param$rgc.ntx.int <- 26 #round(x[2], 0)
  param$ugc.ntx.int <- param$rgc.ntx.int
  param$rct.ntx.int <- 32 #round(x[2], 0) # round(x[5], 0)
  param$uct.ntx.int <- param$rct.ntx.int

  # One base sympt * OR U>R using apdx 10.2, 10.3
  # exp(logit(0.9) - logit(0.16)) = 47.25
  # exp(logit(0.58) - logit(0.14)) = 8.5
  param$rgc.sympt.prob <- 0.1 
  param$ugc.sympt.prob <- x[2] #0.98 #logistic(logit(param$rgc.sympt.prob) + log(47.25))
  param$rct.sympt.prob <- 0.1
  param$uct.sympt.prob <- x[4] #0.98 #logistic(logit(param$rct.sympt.prob) + log(8.5))

  param$prep.start <- Inf

  param$trans.scale <- c(2.7, 0.35, 0.243) #c(x[1], x[2], x[3])
  # Run Sims
  sim <- netsim(orig, param, init, control)

  # As DF
  df <- as.data.frame(sim)
  df <- df[df$time > max(df$time) - keep,]

  # Save
  saveRDS(df, paste0(info[["root_dir"]], "/out/sim", sim_num, ".rds") )
}


lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-sim_calib_params.R", local = TRUE)

init <- init_msm(
  prev.ugc = 0.05,
  prev.rct = 0.05,
  prev.rgc = 0.05,
  prev.uct = 0.05
)

control <- control_msm(
  nsteps = 75 * 52,
  nsims = 28,
  ncores = 28,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  verbose = FALSE
)

sim_nums <- 1:length(xs)

source("R/utils-slurm_wf.R")
source("R/utils-slurm_sim_funs.R")

slurm_wf_tmpl_dir("inst/slurm_wf/", info$root_dir, force = T)

shared_res <- list(
  partition = partition, #"csde", #"ckpt",
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = 28,
  memory = 5 * 1e3 # in Mb and PER CPU
)

slurm_wf_Map(
  info$root_dir,
  resources = c(shared_res, list(
    job_name = "sexdist_aleguil",
    walltime = 60
  )),
  FUN = mod_fun,
  sim_num = sim_nums,
  x = xs,
  MoreArgs = list(orig = orig, param = param, init = init, control = control,
                  info = info, keep = 52 * 10)
)

## sim_nums <- 1:200
## ## x <- c(0.1, 28, 0.92, 0.1, 26, 0.84)
## x <- c(2.7, 0.35, 0.243)

## slurm_wf_Map(
##   info$root_dir,
##   resources = c(shared_res, list(
##     job_name = "sexdist_aleguil",
##     walltime = 60
##   )),
##   FUN = mod_fun,
##   sim_num = sim_nums,
##   MoreArgs = list(x = x, orig = orig, param = param, init = init, control = control,
##                   info = info, keep = 52 * 10)
## )

## control <- control_msm(
##   nsteps = 75,
##   nsims = 1,
##   ncores = 1,
##   save.nwstats = FALSE,
##   save.clin.hist = FALSE,
##   verbose = FALSE
## )

## mod_fun(x = xs[[1]], sim_num = sim_nums[[1]], orig = orig, param = param,
##         init = init, control = control, info = info, keep = 52 * 10)
