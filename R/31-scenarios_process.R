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
    dff <- if (is.null(infos$df_keep)) as.data.frame(sim) else sim

    setDT(dff)
    dff[, batch := btch]

    jobs[[job]]$data <- rbind(jobs[[job]]$data, dff)
  }
}

names(jobs[[1]]$infos$updaters)
jobs[[1]]$infos$updaters[[2]]

as_tibble(jobs[[1]]$data)
print(names(df), max = 200)

my_roll <- function(x) {
  roll_mean(x, n = 4, align = "right", fill = NA)
}

df <- as_tibble(jobs[[1]]$data) %>%
  ## group_by(scenarios, sim, time) %>%
  group_by(scenario, time) %>%
  summarise(
    prep_cov = prepCurr / prepElig ,
    hiv_diag = cc.dx,
    hiv_suppr = cc.vsupp,
    sti_tx = (gc.tx + ct.tx) / (gc + ct),
    sti_inc = ir100.sti,
    hiv_inc = ir100,
    deg_main = main.deg,
    deg_casl = casl.deg,
    deg_inst = inst.deg
  ) %>%
  ungroup() %>%
  fill(sti_inc, hiv_inc)

df <- readRDS("out/remote_jobs/SD_scenario_all/df.rds")

df_scenar <- df %>%
  group_by(scenario, time) %>%
  summarise(across(
    .fns = list(
      p025 = ~ quantile(.x, probs = 0.025, na.rm = TRUE),
      med = ~ median(.x, na.rm = TRUE),
      p975 = ~ quantile(.x, probs = 0.975, na.rm = TRUE)
    )
  )) %>%
  mutate(across(starts_with("sti_"), ~ roll_meanr(.x, 4, fill = NA))) %>%
  mutate(across(starts_with("hiv_suppr_"), ~ roll_meanr(.x, 8, fill = NA))) %>%
  mutate(across(starts_with("hiv_inc_"), ~ roll_meanr(.x, 8, fill = NA)))


df_scenar %>%
  filter(
    !grepl("comb_", scenario),
    !grepl("ser_", scenario),
    !grepl("net_", scenario),
    ## scenarios == "base",
    time > ana_beg
  ) %>%
  ggplot(aes(x = time, y = deg_inst_med , col = scenario)) +
    geom_line() +
    geom_vline(xintercept = int_beg) +
    geom_vline(xintercept = int_end)


df %>%
  filter(scenarios == "base") %>%
  pivot_longer(cols = -c(scenarios, time)) %>%
  ggplot(aes(x = time, y = value)) +
    geom_line() +
    facet_grid(rows = vars(name), scales = "free")
