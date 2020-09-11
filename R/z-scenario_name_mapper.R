library(tidyverse)

job_names <- c(
  "SD_025",
  "SD_net_casl_09",
  "SD_base_no_prep",
  "SD_net_casl_05"
)

scenario_map <- list()

for (job in job_names) {
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))

  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")

  fs::dir_create(fs::path("out/remote_jobs/", job, "out_df"))
  for (fle in sim_files) {
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

    scenario_map[[job]][[fle]] <- scenario
  }
}

scenario_map
ll <- scenario_map

for (nms in names(scenario_map)) {
  names(ll[[nms]]) <- fs::path_file(names(ll[[nms]]))
}

saveRDS(ll, "out/scenario_map.rds")
