library(EpiABC)
library(tidyverse)

parms_steps <- function(low, high, n) {
  low + (high - low) * c(0, 1:(n - 1)/(n - 1))
}

n_cut <- 5
parms_bounds <- list(
  gc_tprob = c(0.1, 0.3),
  gc_dur = c(20, 36),
  gc_txprob = c(0.6, 0.92)
)

lxs <- map_df(parms_bounds, ~ parms_steps(.x[1], .x[2], n_cut))
xs <- lxs %>%
  cross_df() %>%
  asplit(1)


info <- list()
info$root_dir <- "slurm_wf"

saveRDS(xs, paste0("out/", info$root_dir, "_xs.rds"))

mod_fun <- function(x, sim_num,
                    orig, param, init, control, info, keep = 3 * 52) {
  library(EpiModelHIV)

  logit <- function(p) log(p / (1 - p))
  logistic <- function(p) 1 / (1 + exp(-p))

  param$ugc.tprob <- x[1]
  param$rgc.tprob <- logistic(logit(param$rgc.tprob) + log(1.25))
  ## param$uct.tprob <- x[4]
  ## param$rct.tprob <- logistic(logit(param$rct.tprob) + log(1.25))

  # Same duration for U and R, as in prep disp
  param$rgc.ntx.int <- round(x[2], 0)
  param$ugc.ntx.int <- param$rgc.ntx.int
  ## param$rct.ntx.int <- round(x[5], 0)
  ## param$uct.ntx.int <- param$rct.ntx.int

  # One base sympt * OR U>R using apdx 10.2, 10.3
  # exp(logit(0.9) - logit(0.16)) = 47.25
  # exp(logit(0.58) - logit(0.14)) = 8.5
  param$rgc.sympt.prob <- 0.1  #x[3]
  param$ugc.sympt.prob <- x[3] #logistic(logit(param$ugc.sympt.prob) + log(47.25))
  ## param$rct.sympt.prob <- x[6]
  ## param$uct.sympt.prob <- logistic(logit(param$uct.sympt.prob) + log(8.5))

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
  prev.rct = 0.00,
  prev.rgc = 0.05,
  prev.uct = 0.00
)

control <- control_msm(
  nsteps = 75 * 52,
  nsims = 28,
  ncores = 28,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  verbose = TRUE
)

## out <- get_posterior(wave = 7, input = "out/gc_only")
## x_gc <- out$param[
##   sample(length(out$weights), 10,
##          replace = TRUE, prob = out$weights),
##   ]

## out <- get_posterior(wave = 6, input = "out/ct_only")
## x_ct <- out$param[
##   sample(length(out$weights), 10,
##          replace = TRUE, prob = out$weights),
##   ]

## xs <- asplit(cbind(x_gc, x_ct), 1)

sim_nums <- 1:length(xs)

source("R/utils-slurm_wf.R")
source("R/utils-slurm_sim_funs.R")

slurm_wf_tmpl_dir("inst/slurm_wf/", info$root_dir, force = T)

shared_res <- list(
  partition = "csde", #"csde", #"ckpt",
  account = "csde", #"csde", #"csde-ckpt",
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
