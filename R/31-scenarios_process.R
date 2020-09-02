library(data.table)
library(tidyverse)
theme_set(theme_light())

# One or many job_names
job_names <- c("")
job_last_n <- 1 # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n)) {
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)
}

jobs <- list()

# Read targets
prep_start <- 52 * (65 + 5) + 1
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

df <- as_tibble(jobs[[1]]$data)
print(names(df), max = 200)

df <- df %>%
  mutate(scenarios = names(jobs[[1]]$infos$updaters)[batch]) %>%
  group_by(scenarios, sim, time) %>%
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
  ungroup()

# incid (how much pooling) and cumul incid


df %>%
  filter(
    time > ana_beg,
    !grepl("comb_", scenarios)
  ) %>%
  group_by(scenarios, time) %>%
  summarize(y = median(deg_inst, na.rm = TRUE)) %>%
  ggplot(aes(x = time, y = y, col = scenarios)) +
    geom_smooth() +
    geom_vline(xintercept = int_beg) +
    geom_vline(xintercept = int_end)
