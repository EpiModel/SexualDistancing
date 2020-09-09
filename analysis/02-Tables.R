
library("dplyr")
library("tidyr")
source("analysis/00-fx.R")

# Date targets
prep_start <- 52 * (65 + 1) + 1
ana_beg <- prep_start + 5 * 52
int_beg <- ana_beg + 1 * 52
int_end <- int_beg + 1.5 * 52
ana_end <- int_end + 2.5 * 52

## Table 1

# HIV IR at 2.5 yrs

df <- readRDS("~/data/SexDist/df.rds")

h1 <- create_var_df(df, "base", "hiv_inc")
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h2


par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(h2[, 1], type = "l", ylim = c(0, 3), col = 2)
draw_quants(h2, col = adjustcolor(2, alpha.f = 0.5))



plot(roll_mean(ttrm, n = 4, align = "right"), type = "l", ylim = c(0, 4))


## CombPrev Table 1

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


## CombPrev Table 2

load("data/sim.n2000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 2)
ref

cf.sims <- 2001:2012
doParallel::registerDoParallel(parallel::detectCores())
t2set <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = paste0("n", as.character(cf.sims[i])), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 2)
}
doParallel::stopImplicitCluster()
t2set <- do.call("rbind", t2set)

t2 <- full_join(ref, t2set)
t2 <- add_column(t2, scenario = 2000:2012, .before = 1)
t2

write_csv(t2, "data/T2.csv")


## CombPrev Table 3
# Top half

load("data/sim.n3000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 3)
ref

cf.sims <- 3001:3010
doParallel::registerDoParallel(parallel::detectCores())
t3set <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = paste0("n", as.character(cf.sims[i])), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 3)
}
doParallel::stopImplicitCluster()
t3set <- do.call("rbind", t3set)

t3 <- full_join(ref, t3set)
t3 <- add_column(t3, scenario = c(3000:3010), .before = 1)
t3

write_csv(t3, "data/T3a.csv")

# Bottom half

load("data/sim.n3500.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 3)
ref

cf.sims <- 3501:3508
doParallel::registerDoParallel(parallel::detectCores())
t3bset <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = paste0("n", as.character(cf.sims[i])), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 3)
}
doParallel::stopImplicitCluster()
t3bset <- do.call("rbind", t3bset)

t3b <- full_join(ref, t3bset)
t3b <- add_column(t3b, scenario = 3500:3508, .before = 1)
t3b

write_csv(t3b, "data/T3b.csv")


## Extra Tables Support Figure 1 (Supplemental Table 30)

# prep-linked

load("data/sim.n7000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 1)
ref

cf.sims <- 7001:7003
doParallel::registerDoParallel(parallel::detectCores())
tset <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = paste0("n", as.character(cf.sims[i])), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 1)
}
doParallel::stopImplicitCluster()
tset <- do.call("rbind", tset)
ta <- full_join(ref, tset)
ta <- add_column(ta, scenario = 7000:7003, .before = 1)
ta

write_csv(ta, "data/TS30a.csv")


# prep-unlinked

load("data/sim.n7004.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 1)
ref

cf.sims <- 7005:7007
doParallel::registerDoParallel(parallel::detectCores())
tset <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = as.character(cf.sims[i]), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 1)
}
doParallel::stopImplicitCluster()
tset <- do.call("rbind", tset)
ta <- full_join(ref, tset)
ta <- add_column(ta, scenario = 7004:7007, .before = 1)
ta

write_csv(ta, "data/TS30b.csv")

## Receive from Hyak

system("scp mox:/gscratch/csde/sjenness/combprev/data/*.csv analysis/")
