library(data.table)
library(tidyverse)
library(RcppRoll)
theme_set(theme_light())

# One or many job_names
job_names <- c("SD_scenario_debug_small2")
job_last_n <- 1 # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n)) {
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)
}

jobs <- list()

# Read targets
prep_start <- 52 * (65 + 1) + 1
ana_beg <- prep_start + 5 * 52
int_beg <- ana_beg + 1 * 52
int_end <- int_beg + 1.5 * 52
ana_end <- int_end + 2.5 * 52

for (job in job_names) {
  jobs[[job]] <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  jobs[[job]]$infos <- infos

  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  jobs[[job]]$data <- data.table()
  btch <- 0
  for (fle in sim_files) {
    btch <- btch + 1
    sim <- readRDS(fle)
    dff <- if (is.null(infos$df_keep)) as_tibble(sim) else sim

    dff$batch <- btch
    dff <- dff %>%
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

    jobs[[job]]$data <- rbind(jobs[[job]]$data, dff)
  }
}

names(jobs[[1]]$infos$updaters)
jobs[[1]]$infos$updaters[[2]]

df <-as_tibble(jobs[[1]]$data)
print(names(df), max = 200)

my_roll <- function(x) {
  roll_mean(x, n = 4, align = "right", fill = NA)
}

saveRDS(df, "out/remote_jobs/SD_scenario_all_big/df.rds")
## df <- readRDS("out/remote_jobs/SD_scenario_all_big/df.rds")

df_scenar <- df %>%
  group_by(scenario, time) %>%
  summarise(across(-c(batch, sim),
    .fns = list(
      p025 = ~ quantile(.x, probs = 0.025, na.rm = TRUE),
      med = ~ median(.x, na.rm = TRUE),
      p975 = ~ quantile(.x, probs = 0.975, na.rm = TRUE)
    ),
    .names = "{col}__{fn}"
  )) %>%
  mutate(across(starts_with("sti_"), ~ roll_meanr(.x, 4, fill = NA))) %>%
  mutate(across(starts_with("hiv_suppr_"), ~ roll_meanr(.x, 8, fill = NA))) %>%
  mutate(across(starts_with("hiv_inc_"), ~ roll_meanr(.x, 8, fill = NA)))

saveRDS(df_scenar, "out/remote_jobs/SD_scenario_all_big/df_scenar.rds")
## df_scenar <- readRDS("out/remote_jobs/SD_scenario_all_big/df_scenar.rds")

fmtr <- scales::label_number(0.01)

df_scenar25 <- df_scenar %>%
  filter(time == int_end) %>%
  select(-time) %>%
  pivot_longer(cols = -scenario) %>%
  separate(name, sep = "__", into = c("name", "measure")) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(formatted = paste0(
    fmtr(med), " (", fmtr(p025), ", ", fmtr(p975), ")")
    ) %>%
  select(-c(p025, med, p975)) %>%
  pivot_wider(names_from = name, values_from = formatted)

saveRDS(df_scenar25, "out/remote_jobs/SD_scenario_all_big/df_scenar25.rds")

df_scenar50 <- df %>%
  filter(time > ana_beg) %>%
  group_by(scenario, batch, sim) %>%
  summarise(
    hiv_cum_inc = sum(hiv_inc) * 1e5 / 5200,
    sti_cum_inc = sum(sti_inc) * 1e5 / 5200,
    sti_gc_cum_inc = sum(sti_gc_inc) * 1e5 / 5200,
    sti_ct_cum_inc = sum(sti_ct_inc) * 1e5 / 5200
  ) %>%
  group_by(scenario) %>%
  summarise(across(-c(batch, sim),
    .fns = list(
      p025 = ~ quantile(.x, probs = 0.025, na.rm = TRUE),
      med = ~ median(.x, na.rm = TRUE),
      p975 = ~ quantile(.x, probs = 0.975, na.rm = TRUE)
    ),
    .names = "{col}__{fn}"
  )) %>%
  pivot_longer(cols = -scenario) %>%
  separate(name, sep = "__", into = c("name", "measure")) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(formatted = paste0(
    fmtr(med), " (", fmtr(p025), ", ", fmtr(p975), ")")
    ) %>%
  select(-c(p025, med, p975)) %>%
  pivot_wider(names_from = name, values_from = formatted)

saveRDS(df_scenar50, "out/remote_jobs/SD_scenario_all_big/df_scenar50.rds")
