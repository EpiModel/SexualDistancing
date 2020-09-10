library(tidyverse)

job_names <- c(
  "SD_025",
  "SD_net_casl_09",
  "SD_base_no_prep",
  "SD_net_casl_05"
)

job <- job_names[[2]]
jobs <- list()
i <- 2

for (job in job_names) {
  jobs[[job]] <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  jobs[[job]]$infos <- infos

  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  jobs[[job]]$data <- tibble()
  btch <- i * 1000
  i <- i + 1

  fs::dir_create(fs::path("out/remote_jobs/", job, "out_df"))
  for (fle in sim_files) {
    btch <- btch + 1
    sim <- readRDS(fle)

    p_sce <- sim$param$param_updaters[2]
    p_sce <- if (is.null(p_sce[[1]])) list() else p_sce

    j <- 1
    while (TRUE) {
      if (identical(infos$updaterss[[j]], p_sce)) {
        scenario <- names(infos$updaterss)[[j]]
        break
      }
      j <- j + 1
    }

    dff <- as_tibble(sim)

    dff <- dff %>%
      filter(time >= 3642) %>%
      mutate(batch = btch, scenario = scenario) %>%
      group_by(scenario, batch, sim, time) %>%
      summarise(
        prep_cov = prepCurr / prepElig ,
        hiv_diag = cc.dx,
        hiv_suppr = cc.vsupp,
        sti_tx = (gc.tx + ct.tx) / (gc + ct),
        sti_inc = ir100.sti,
        sti_gc_inc = ir100.gc,
        sti_ct_inc = ir100.ct,
        hiv_inc = ir100,
        deg_main = main.deg,
        deg_casl = casl.deg,
        deg_inst = inst.deg
      ) %>%
      ungroup() %>%
      fill(sti_inc, hiv_inc)

    jobs[[job]]$data <- bind_rows(jobs[[job]]$data, dff)
    saveRDS(dff, fs::path("out/remote_jobs/", job,
                          paste0("out_df/df_sim", btch, ".rds")))
  }

    saveRDS(jobs[[job]]$data, fs::path("out/remote_jobs/", job, "df_comb_sim.rds"))
}
