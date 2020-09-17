library(data.table)
library(tidyverse)
library(RcppRoll)
theme_set(theme_light())

# One or many job_names
job_names <- c("SD_scenario_full")
job_last_n <- NULL # if not NULL, get last N jobs. Otherwise, use job_names

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

saveRDS(df, fs::path("out/remote_jobs/", job, "/df.rds"))
## df <- readRDS(fs::path("out/remote_jobs/", job, "/df.rds"))
df <- readRDS(fs::path("out/df_comb.rds"))

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

saveRDS(df_scenar, fs::path("out/df_scenar.rds"))
## df_scenar <- readRDS(fs::path("out/df_scenar.rds"))

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

saveRDS(df_scenar25, fs::path("out/df_scenar25.rds"))
## df_scenar25 <- readRDS(fs::path("out/df_scenar25.rds"))

df_scenar50 <- df %>%
  filter(time > ana_beg) %>%
  group_by(scenario, batch, sim) %>%
  summarise(
    hiv_cum_inc = mean(hiv_inc) * 5000,
    sti_cum_inc = mean(sti_inc) * 5000,
    sti_gc_cum_inc = mean(sti_gc_inc) * 5000,
    sti_ct_cum_inc = mean(sti_ct_inc) * 5000
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

saveRDS(df_scenar50, fs::path("out/df_scenar50.rds"))
## df_scenar50 <- readRDS(fs::path("out/df_scenar50.rds"))

df_table <- left_join(df_scenar25, df_scenar50, by = "scenario")


library(tidyverse)
library(viridis)
library(metR)

prep_start <- 52 * (65 + 1) + 1
ana_beg <- prep_start + 5 * 52
int_beg <- ana_beg + 1 * 52
int_end <- int_beg + 1.5 * 52
ana_end <- int_end + 2.5 * 52

df_sensi <- readRDS("analysis/data/df_sensi.rds")

dfs <- df_sensi %>%
  filter(time >= ana_beg) %>%
  group_by(sim, batch, scenario) %>%
  summarise(
    hivCI = sum(hiv_inc),
    stiCI = sum(sti_inc)
  ) %>%
  group_by(scenario) %>%
  summarise(across(c(hivCI, stiCI), median)) %>%
  separate(scenario, into = c(NA, "net", "ser"), "\\D+", remove = FALSE) %>%
  mutate(across(c(net, ser), .fns = as.numeric)) #%>% pivot_longer(c(hivCI, stiCI))


ggplot(dfs, aes(x = ser, y = net, z = stiCI)) +
  geom_contour_fill(na.fill = TRUE) +
  geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
  ## geom_text_contour(stroke = 0.1, size = 3.5) +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("Service Interruption Duration (Months)") +
  ylab("Sexual Distancing Duration (Months)") +
  guides(fill = guide_legend(
    title = "Sexual Distancing Duration (Months):",
    nrow = 1
  ))

gg_sensi_contour <- function(df_sensi, outcome, outcome_label) {
  prep_start <- 52 * (65 + 1) + 1
  ana_beg <- prep_start + 5 * 52

  dfs <- df_sensi %>%
    filter(time >= ana_beg) %>%
    group_by(sim, batch, scenario) %>%
    summarise(
      hivCI = sum(hiv_inc),
      stiCI = sum(sti_inc)
    ) %>%
    group_by(scenario) %>%
    summarise(across(c(hivCI, stiCI), median)) %>%
    separate(scenario, into = c(NA, "net", "ser"), "\\D+", remove = FALSE) %>%
    mutate(across(c(net, ser), .fns = as.numeric))

  p <- ggplot(dfs, aes(x = ser, y = net, z = {{ outcome }})) +
    geom_contour_fill(na.fill = TRUE) +
    geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
    scale_fill_viridis(
      discrete = FALSE,
      alpha = 1,
      option = "D",
      direction = 1,
      name = outcome_label
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    xlab("Service Interruption Duration (Months)") +
    ylab("Sexual Distancing Duration (Months)") +
    theme_classic() +
    theme(legend.position = "top")

  p
}

gg_sensi_contour(df_sensi, hivCI, "HIV Cumulative Incidence")
gg_sensi_contour(df_sensi, stiCI, "STI CumulativeIncidence")
