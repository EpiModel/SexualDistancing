source("R/utils-slurm_prep_helpers.R") # requires `purrr`
source("R/utils-slurm_wf.R")
test_simulation <- FALSE

# Set slurm parameters ---------------------------------------------------------
batch_per_set <- 20      # How many 28 replications to do per parameter
steps_to_keep <- 6 * 52 # Steps to keep in the output df. If NULL, return sim obj
partition <- "csde"     # On hyak, either ckpt or csde
job_name <- "SD_3_18"
ssh_host <- "hyak_mox"
ssh_dir <- "gscratch/SexualDistancing/"

# Options passed to slurm_wf
slurm_ressources <- list(
  partition = partition,
  job_name = job_name,
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = 28,
  memory = 5 * 1e3, # in Mb and PER CPU
  walltime = 15
)

# Set orig, param, init, control -----------------------------------------------
#
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-sim_calib_params.R", local = TRUE)

orig <- readRDS("out/est/restart.rds")
## tmp_epi <- readRDS("out/est/tmp_epi.rds")
## orig$epi <- lapply(names(tmp_epi$epi), function(n) orig$epi[[n]])
## names(orig$epi) <- names(tmp_epi$epi)

# run 20 rng years before scenarios
param$prep.start <- orig$control$nsteps + 1 * 52 + 1
param$riskh.start <- param$prep.start - 52
step_ana_start <- param$prep.start + 5 * 52
step_interv_start <- step_ana_start + 1 * 52
step_interv_stop <- step_interv_start + 1.5 * 52
step_ana_stop <- step_interv_stop + 2.5 * 52

control <- control_msm(
  start = orig$control$nsteps + 1,
  nsteps = step_ana_stop,
  nsims = 28,
  ncores = 28,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE
)

# Return all scenarios to normal at timestep 2.5 year after prep_calib
param$param_updaters <- list(
  list(
    at = step_interv_stop,
    param = list(
      netresim.form.rr = rep(1, 3),
      netresim.disl.rr = rep(1, 2),
      prep.start.prob = param$prep.start.prob,
      prep.discont.rate = param$prep.discont.rate,
      tx.halt.part.prob = param$tx.halt.part.prob,
      hiv.test.rate = param$hiv.test.rate,
      gc.sympt.prob.tx = param$gc.sympt.prob.tx,
      gc.asympt.prob.tx = param$gc.asympt.prob.tx,
      ct.sympt.prob.tx = param$ct.sympt.prob.tx,
      ct.asympt.prob.tx = param$ct.asympt.prob.tx
    )
  )
)


# Scenarios --------------------------------------------------------------------
# requires the time variables (step_ana_start, etc)
source("R/utils-scenarios.R")

## scenarios <- sens_scenarios
## scenarios <- scenarios[grepl("comb_[0-9]+_025", names(scenarios))]
scenarios <- scenarios_3_18

# Automatic --------------------------------------------------------------------
#
updaters <- rep(scenarios, batch_per_set)
sim_nums <- seq_along(updaters)

# Required directories
paths <- make_job_paths(job_name, ssh_dir, ssh_host)
# Ensure that no job with this name is present
if (fs::dir_exists(paths$local_job_dir))
  stop("Folder: '", paths$local_job_dir,
       "' exists. Change `job_name` or delete the folder")

info <- list()
info$paths <- paths
info$job_name <- job_name
info$ssh_host <- ssh_host
info$root_dir <- fs::path(paths$jobs_dir, job_name, paths$slurm_wf)
info$df_keep <- steps_to_keep
info$updaterss <- updaters

slurm_wf_tmpl_dir("inst/slurm_wf/", info$root_dir, force = T)

shared_res <- list(
  partition = partition,
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = 28,
  memory = 5 * 1e3 # in Mb and PER CPU
)

slurm_wf_Map(
  info$root_dir,
  resources = slurm_ressources,
  FUN = run_netsim_updaters_fun ,
  sim_num = sim_nums,
  updaters = updaters,
  scenario = names(updaters),
  MoreArgs = list(orig = orig, param = param, init = init, control = control,
                  info = info)
)

if (test_simulation) {
  control$nsims <- 1
  control$ncores <- 1
  control$verbose <- FALSE
  n <- 1

  run_netsim_updaters_fun(
    updaters[[n]], sim_nums[[n]], scenario = names(updaters)[[n]],
    orig, param, init, control, info
  )

  df <- readRDS(fs::path("remote_jobs/", job_name,
                         paste0("slurm/out/df_sim", n, ".rds")))
  tail(df[, 103:138])
  print(names(df), max = 200)
  df <- df %>%
    mutate(
      prep_cov = prepCurr / prepElig ,
      hiv_diag = cc.dx,
      hiv_suppr = cc.vsupp,
      sti_tx = (gc.tx + ct.tx) / (gc + ct),
      sti_inc = ir100.sti,
      hiv_inc = ir100,
      deg_main = main.deg,
      deg_casl = casl.deg,
      deg_inst = inst.deg
    )

  ggplot(df, aes(x = time, y = sti_inc)) +
    geom_line()
}


# Create out dir and save params
fs::dir_create(fs::path(paths$local_out, paths$jobs_dir))
saveRDS(info, fs::path(paths$remote_job_dir, "job_info.rds"))
# move slurm to out and cleanup
fs::file_move(paths$remote_job_dir, fs::path(paths$local_out, paths$jobs_dir))
fs::dir_delete(paths$jobs_dir)


scp_send_script <- c(
  "#!/bin/sh",
  "",
  paste0("ssh ", info$ssh_host, " \"mkdir -p '", info$ssh_host, ":",
         fs::path(paths$ssh_proj, paths$jobs_dir) ,"'\""),
  paste0("rsync -vr --exclude '", "out/*", "' '",
         paths$local_job_dir, "' '",
         info$ssh_host, ":", fs::path(paths$ssh_proj, paths$jobs_dir, "'"))
  )

scp_get_script <- c(
  "#!/bin/sh",
  "",
  paste0("rsync -vur '",
         info$ssh_host, ":", fs::path(paths$ssh_job_dir, paths$slurm_out),
         "' '", paths$local_job_dir, "'")
)

writeLines(scp_send_script, fs::path(paths$local_job_dir, "send_to_ssh.sh"))
writeLines(scp_get_script, fs::path(paths$local_job_dir, "get_from_ssh.sh"))

write(job_name, file = fs::path(paths$local_out, paths$jobs_dir, "last_jobs"),
       append = TRUE)
