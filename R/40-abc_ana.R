library(data.table)
library(tidyverse)
library(EpiABC)

## sims <- readRDS("out/only_rprob/abc.wave5.rda")

out <- get_posterior(wave = 7, input = "out/gc_only/")
summary(out, digits = 3)

boxplot(out, type = "stats")

plot(out, type = "stats")

plot(out, type = "param")

boxplot(out, type = "param")

out$param[sample(length(out$weights), 10, replace = TRUE, prob = out$weights),]

s <- readRDS("out/out/abc.wave0.batch0001.rda")
prms <- s$cwave$tab_param

prms[order(prms[, 2], decreasing = F), ]

library(EpiABC)
library(data.table)
library(tidyverse)

# Check sims
slurm_dir <- "slurm_ckpt"
parms <- readRDS(paste0("out/", slurm_dir, "_xs.rds"))
parms_mat <- matrix(flatten_dbl(parms), ncol = length(parms[[1]]), byrow = TRUE)

dt <- data.table()

## for (slurm_dir in c("slurm_ckpt/", "slurm_csde/")) {
  i <- 0
  for ( file in list.files(paste0("out/", slurm_dir), pattern = ".rds") ) {
    s <- readRDS(paste0("out/", slurm_dir, "/", file))
    dtt <- as.data.table(s)
    dtt[, `:=`(batch = str_extract(file, "\\d+"))]
    dt <- rbindlist(list(dt, dtt))
    if (1 %% 10 == 0) gc()
  }
## }

dt <- as_tibble(dt)

extinct <- dt %>%
  filter(time > max(time) - 52 * 3) %>%
  filter(prev.gc == 0 | prev.ct == 0) %>%
  select(batch, sim) %>%
  unique()

dt %>%
  filter(time > 3900 - 52 * 10) %>%
  anti_join(extinct) %>%
  summarize(across(
    c(ir100.gc, ir100.ct),
    list(q1 = ~ quantile(.x, 0.25),
         q2 = median,
         q3 = ~ quantile(.x, 0.75)),
    .names = "{fn}{col}"))

dt %>%
  filter(time > 3900 - 52 * 10) %>%
  ## anti_join(extinct) %>%
  summarize(across(
    starts_with("i.prev.dx."),
    list(q1 = ~ quantile(.x, 0.25),
         q2 = median,
         q3 = ~ quantile(.x, 0.75)),
    .names = "{fn}{col}")) %>%
  as.list()




dt_sum <- dt %>%
  group_by(batch) %>%
  summarize(across(
    c(ir100.gc, ir100.ct),
    list(q1 = ~ quantile(.x, 0.25),
         q2 = median,
         q3 = ~ quantile(.x, 0.75)),
    .names = "{fn}{col}"))

dt_sum %>%
  filter(q2ir100.gc > 0) %>%
  arrange((q2ir100.gc - 12.81)^2) #%>% saveRDS("out/diag/calib_sti_val_gc.rds")

dt_sum %>%
  filter(q2ir100.gc > 0) %>%
  arrange((q2ir100.gc - 12.81)^2) %>%
  pull(batch) %>%
  as.numeric() %>%
  head(10) %>%
  parms_mat[., ] #%>% saveRDS("out/diag/calib_sti_parms_gc.rds")

dt_sum %>%
  filter(q2ir100.ct > 0) %>%
  arrange((q2ir100.ct - 14.59)^2) #%>% saveRDS("out/diag/calib_sti_val_ct.rds")

dt_sum %>%
  filter(q2ir100.ct > 0) %>%
  arrange((q2ir100.ct - 14.59)^2) %>%
  pull(batch) %>%
  as.numeric() %>%
  head(10) %>%
  parms_mat[., ] #%>% saveRDS("out/diag/calib_sti_parms_ct.rds")

summary(dt$ir100.gc) # 4.4 # 12.81
summary(dt$ir100.ct) # 6.6 # 14.59

dt[batch %in% trials[1:3],] %>%
  ggplot(aes(x = time, y = ir100.gc, col = as.factor(sim))) +
    ## geom_line(alpha = 0.5) +
    geom_smooth(alpha = 0.5) +
    facet_grid(cols = vars(batch)) +
    theme(legend.position = NULL)

parms_mat[trials,]
