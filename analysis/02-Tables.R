
library("dplyr")
library("tidyr")
source("analysis/00-fx.R")

# Date targets
prep_start <- 52 * (65 + 1) + 1
ana_beg <- prep_start + 5 * 52
int_beg <- ana_beg + 1 * 52
int_end <- int_beg + 1.5 * 52
ana_end <- int_end + 2.5 * 52

df <- readRDS("~/data/SexDist/df.rds")

## Table 1

scen <- "base"
var <- "hiv_inc"

calc_quants_ir(df, scen = scen, var = var,
               t.start = int_end-8, t.end = int_end,
               qnt.low = 0.25, qnt.high = 0.75)

h1 <- create_var_df(df, scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)

par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(h2[, 1], type = "l", ylim = c(0, 3), col = 2)
draw_quants(h2, col = adjustcolor(2, alpha.f = 0.5))
abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)




## Table 1

load("data/sim.n1000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 1)
ref

cf.sims <- 1001:1012
doParallel::registerDoParallel(parallel::detectCores())
t1set <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = paste0("n", as.character(cf.sims[i])), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 1)
}
doParallel::stopImplicitCluster()

t1set <- do.call("rbind", t1set)
t1 <- full_join(ref, t1set)
t1 <- add_column(t1, scenario = 1000:1012, .before = 1)
t1

write_csv(t1, "data/T1.csv")

