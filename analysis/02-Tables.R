
source("analysis/00-fx.R")

par(mar = c(3,3,1,1), mgp = c(2,1,0))

# Date targets
prep_start <- 52 * (65 + 1) + 1
ana_beg <- prep_start + 5 * 52
int_beg <- ana_beg + 1 * 52
int_end <- int_beg + 1.5 * 52
ana_end <- int_end + 2.5 * 52

df <- readRDS("~/data/SexDist/df.rds")
df <- filter(df, time >= ana_beg)
table(df$scenario)

## Test outcomes

scen <- "base"
scen <- "ser_prep_09"
var <- "hiv_inc"
roll <- 4

calc_quants_ir(df, scen = scen, var = var,
               t.start = int_end-4, t.end = int_end,
               qnt.low = 0.25, qnt.high = 0.75)

calc_quants_ci(df, scen = scen, var = var,
               t.start = ana_beg, t.end = ana_end,
               qnt.low = 0.25, qnt.high = 0.75, round = 1)

calc_quants_prev(df, scen = scen, var = "prep_cov", at = int_end,
                 mult = 1, round = 2,
                 qnt.low = 0.025, qnt.high = 0.975)

calc_quants_ia(df, base.scen = "base", comp.scen = scen, var = var,
               t.start = ana_beg, t.end = ana_end,
               qnt.low = 0.025, qnt.high = 0.975,
               nsims = 1000, round.nia = 1, round.pia = 1)


## Table 1

scenario_set <- c("base",
                  "net_all_05", "net_all_09",
                  "net_casl_05", "net_casl_09", "net_casl_1",
                  "net_ot_05", "net_ot_09", "net_ot_1")
epi_vars <- c("hiv_inc", "sti_inc", "sti_gc_inc", "sti_ct_inc")
proc_vars <- c("deg_main", "deg_casl", "deg_inst")

qlow <- 0.025
qhigh <- 0.975

t1 <- list()
for (ii in 1:length(scenario_set)) {
  rr <- list()
  for (jj in 1:length(epi_vars)) {
    temp <- calc_quants_ir(df, scen = scenario_set[[ii]], var = epi_vars[[jj]],
                           t.start = int_end-4, t.end = int_end,
                           qnt.low = qlow, qnt.high = qhigh, round = 2)
    rr <- c(rr, temp)
    temp <- calc_quants_ci(df, scenario_set[[ii]], var = epi_vars[[jj]],
                           t.start = ana_beg, t.end = ana_end,
                           qnt.low = qlow, qnt.high = qhigh, round = 1)
    rr <- c(rr, temp)
  }
  for (kk in 1:length(proc_vars)) {
    temp <- calc_quants_prev(df, scen = scenario_set[[ii]],
                            var = proc_vars[[kk]],
                            at = int_end,
                            mult = 1, round = 2,
                            qnt.low = qlow, qnt.high = qhigh)
    rr <- c(rr, temp)
  }
  rr <- do.call("c", rr)
  t1[[ii]] <- rr
}
t1 <- as.data.frame(cbind(scenario_set, do.call("rbind", t1)))

readr::write_csv(t1, "analysis/T1.csv")


## Table 2

scenario_set <- c("base",
                  "ser_all_05", "ser_all_09",
                  "ser_prep_05", "ser_prep_09",
                  "ser_scre_05", "ser_scre_09",
                  "ser_art_05", "ser_art_09",
                  "ser_stitx_05", "ser_stitx_09")
epi_vars <- c("hiv_inc", "sti_inc", "sti_gc_inc", "sti_ct_inc")
proc_vars <- c("prep_cov", "hiv_diag", "hiv_suppr", "sti_tx")

qlow <- 0.025
qhigh <- 0.975

t2 <- list()
for (ii in 1:length(scenario_set)) {
  rr <- list()
  for (jj in 1:length(epi_vars)) {
    temp <- calc_quants_ir(df, scen = scenario_set[[ii]], var = epi_vars[[jj]],
                           t.start = int_end-4, t.end = int_end,
                           qnt.low = qlow, qnt.high = qhigh, round = 2)
    rr <- c(rr, temp)
    temp <- calc_quants_ci(df, scenario_set[[ii]], var = epi_vars[[jj]],
                           t.start = ana_beg, t.end = ana_end,
                           qnt.low = qlow, qnt.high = qhigh, round = 1)
    rr <- c(rr, temp)
  }
  for (kk in 1:length(proc_vars)) {
    temp <- calc_quants_prev(df, scen = scenario_set[[ii]],
                             var = proc_vars[[kk]],
                             at = int_end,
                             mult = 1, round = 2,
                             qnt.low = qlow, qnt.high = qhigh)
    rr <- c(rr, temp)
  }
  rr <- do.call("c", rr)
  t2[[ii]] <- rr
}
t2 <- as.data.frame(cbind(scenario_set, do.call("rbind", t2)))

readr::write_csv(t2, "analysis/T2.csv")
