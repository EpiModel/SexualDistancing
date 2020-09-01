library(EpiABC)
library(tidyverse)

partition <- "ckpt" # csde / ckpt

info <- list()
info$root_dir <- paste0("slurm_", partition)

mod_fun <- function(x, sim_num,
                    orig, param, init, control, info, keep = 3 * 52) {
  library(EpiModelHIV)

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

source("R/utils-slurm_wf.R")

slurm_wf_tmpl_dir("inst/slurm_wf/", info$root_dir, force = T)

shared_res <- list(
  partition = partition, #"csde", #"ckpt",
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = 28,
  memory = 5 * 1e3 # in Mb and PER CPU
)

sim_nums <- 1:200
x <- c(2.7, 0.35, 0.243)

slurm_wf_Map(
  info$root_dir,
  resources = c(shared_res, list(
    job_name = "sexdist_aleguil",
    walltime = 60
  )),
  FUN = mod_fun,
  sim_num = sim_nums,
  MoreArgs = list(x = x, orig = orig, param = param, init = init, control = control,
                  info = info, keep = 52 * 10)
)

## control <- control_msm(
##   nsteps = 75,
##   nsims = 1,
##   ncores = 1,
##   save.nwstats = FALSE,
##   save.clin.hist = FALSE,
##   verbose = FALSE
## )

## mod_fun(x = x, sim_num = sim_nums[[1]], orig = orig, param = param,
##         init = init, control = control, info = info, keep = 52 * 10)
