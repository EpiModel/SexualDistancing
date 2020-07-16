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


# Check sims
slurm_dir <- "slurm"
parms <- readRDS(paste0("out/", slurm_dir, "_xs.rds"))
parms_mat <- matrix(flatten_dbl(parms), ncol = 3, byrow = TRUE)

dt <- data.table()

for (i in 1:125) {
  s <- readRDS(paste0("out/", slurm_dir, "/sim", i, ".rds"))
  dtt <- as.data.table(s)
  dtt[, `:=`(batch = i)]
  dt <- rbindlist(list(dt, dtt))
}

dt_sum <- dt %>%
  group_by(batch) %>%
  summarize(across(
    c(ir100.gc, ir100.ct),
    list(q1 = ~ quantile(.x, 0.25),
         q2 = median,
         q3 = ~ quantile(.x, 0.75)),
    .names = "{fn}{col}"))

dt_sum %>%
  arrange(q3ir100.gc)

trials <- dt_sum %>%
  arrange(q3ir100.gc) %>%
  pull(batch) %>%
  head(10)

summary(dt$ir100.gc) # 4.4
summary(dt$ir100.ct) # 6.6

dt[batch %in% c(106, 1, 26),] %>%
  ggplot(aes(x = time, y = ir100.gc, col = as.factor(sim))) +
    ## geom_line(alpha = 0.5) +
    geom_smooth(alpha = 0.5) +
    facet_grid(cols = vars(batch)) +
    theme(legend.position = NULL)

parms_mat[trials,]
