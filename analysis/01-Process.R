
## Exploratory and Data Processing

fn <- list.files("analysis/data", pattern = "df_sim", full.names = TRUE, recursive = TRUE)
# df1 <- readRDS(fn[1])

# To process only a subset:
## fn <- fn[grepl("comb_\\d+_025", fn)]

btch <- 0
for (i in seq_along(fn)) {
  btch <- btch + 1
  dft <- readRDS(fn[i])
  # dft <- dplyr::select(dft, var.names)
  prep_cov = dft$prepCurr / dft$prepElig
  hiv_diag = dft$cc.dx
  hiv_suppr = dft$cc.vsupp
  sti_tx = (dft$gc.tx + dft$ct.tx) / (dft$gc + dft$ct)
  sti_inc = dft$ir100.sti
  sti_gc_inc = dft$ir100.gc
  sti_ct_inc = dft$ir100.ct
  hiv_inc = dft$ir100
  deg_main = dft$main.deg
  deg_casl = dft$casl.deg
  deg_inst = dft$inst.deg
  sti.n.tx = dft$gc.tx + dft$ct.tx
  sti.n.tot = dft$gc + dft$ct
  dft2 <- data.frame(sim = dft$sim,
                     batch = btch,
                     time = dft$time,
                     scenario = dft$scenario,
                     prep_cov, hiv_diag, hiv_suppr, sti_tx, sti_inc, sti_gc_inc,
                     sti_ct_inc, hiv_inc, deg_main, deg_casl, deg_inst,
                     sti.n.tot, sti.n.tx)
  if (i == 1) {
    df <- dft2
  } else {
    df <- rbind(df, dft2)
  }
  cat(i, "/", length(fn), "...\n ", sep = "")
}
dim(df)

saveRDS(df, file = "analysis/data/df.rds")


df <- readRDS("analysis/data/df.rds")
dim(df)
head(df)
table(df$scenario)
unique(table(df$scenario))











library("RcppRoll")
my_roll <- function(x) {
  roll_mean(x, n = 4, align = "right", fill = NA)
}





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

saveRDS(df_scenar, fs::path("out/remote_jobs/", job, "/df_scenar.rds"))
## df_scenar <- readRDS(fs::path("out/remote_jobs/", job, "/df_scenar.rds"))

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

saveRDS(df_scenar25, fs::path("out/remote_jobs/", job, "/df_scenar25.rds"))
## df_scenar25 <- readRDS(fs::path("out/remote_jobs/", job, "/df_scenar25.rds"))

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

saveRDS(df_scenar50, fs::path("out/remote_jobs/", job, "/df_scenar50.rds"))
## df_scenar50 <- readRDS(fs::path("out/remote_jobs/", job, "/df_scenar50.rds"))

df_table <- left_join(df_scenar25, df_scenar50, by = "scenario")


library(metR)
df_scenar <- readRDS("out/remote_jobs/SD_scenario_all_big/df_scenar.rds")

df_contour <- df_scenar %>%
  filter(
    time == int_end,
    scenario %in% c("base", scenario[grep("comb_", scenario)])) %>%
  select(scenario, sti_inc__med, hiv_inc__med) %>%
  mutate(scenario = if_else(scenario == "base", "base_00_00", scenario)) %>%
  separate(scenario, into = c(NA, "ser", "net"), "_") %>%
  mutate(across(c(ser, net), ~ as.numeric(str_pad(.x, 3, "right", "0"))))

ggplot(df_contour, aes(x = ser, y = net, z = hiv_inc__med)) +
  geom_contour_fill(na.fill = TRUE)
