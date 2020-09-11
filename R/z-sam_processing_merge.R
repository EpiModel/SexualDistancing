library(tidyverse)

scenario_map <- readRDS("out/scenario_map.rds")

job_names <- c(
  ## "SD_025",
  ## "SD_net_casl_09",
  "SD_base_no_prep"
  ## "SD_net_casl_05"
)

jobs <- list()
i <- 3

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

    scenario <- "base_no_prep"

    dff <- as_tibble(sim)

    dff <- dff %>%
      filter(time >= 3642) %>%
      mutate(batch = btch, scenario = scenario)

    jobs[[job]]$data <- bind_rows(jobs[[job]]$data, dff)
    saveRDS(dff, fs::path("out/remote_jobs/", job,
                          paste0("out_df/df_sim", btch, ".rds")))
  }

    saveRDS(jobs[[job]]$data, fs::path("out/remote_jobs/", job, "df_comb_sim.rds"))
}
