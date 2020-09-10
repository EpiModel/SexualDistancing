library(tidyverse)

job_names <- c(
  "SD_025",
  "SD_net_casl_09",
  "SD_base_no_prep",
  "SD_net_casl_05",
  "SD_scenario_full"
)

df <- tibble()
for (job in job_names) {
    dff <- readRDS(fs::path("out/remote_jobs/", job, "df_comb_sim.rds"))
    if (job == "SD_base_no_prep")
      dff$scenario <- "base_no_prep"
    df <- bind_rows(df, dff)
}

saveRDS(df, "out/df_comb.rds")

df %>%
group_by(scenario) %>%
  summarise(rep = length(unique(batch))) %>%
  print(n = 100)
