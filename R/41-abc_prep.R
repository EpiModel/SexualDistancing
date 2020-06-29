library("methods")
suppressMessages(library("EpiABC"))
suppressMessages(library("EpiModelHIV"))

# Main Model Fx -----------------------------------------------------------

f <- function(x) {
  set.seed(x[1])
  require(EpiModelHIV)
  netstats <- readRDS("out/est/netstats.rds")
  epistats <- readRDS("out/est/epistats.rds")
  est <- readRDS("out/est/netest.rds")

  param <- param_msm(netstats = netstats,
                     epistats = epistats,
                     hiv.test.rate = c(x[2], x[3], x[4]),
                     hiv.test.late.prob = c(0, 0, 0),
                     tx.init.prob = c(x[5], x[6], x[7]),
                     tt.part.supp = c(0.35, 0.30, 0.18),
                     tt.full.supp = c(0.24, 0.21, 0.19),
                     tt.dur.supp = c(0.41, 0.49, 0.63),
                     tx.halt.part.prob = c(x[8], x[9], x[10]),
                     tx.halt.full.rr = c(0.8, 0.8, 0.8),
                     tx.halt.dur.rr = c(0.5, 0.5, 0.5),
                     tx.reinit.part.prob = c(x[11], x[12], x[13]),
                     tx.reinit.full.rr = c(1.25, 1.25, 1.25),
                     tx.reinit.dur.rr = c(2.0, 2.0, 2.0),
                     aids.mr = 1/300,
                     trans.scale = 1.10,
                     acts.scale = 1.00,
                     acts.aids.vl = 5.75)
  init <- init_msm(prev.ugc = 0,
                   prev.rct = 0,
                   prev.rgc = 0,
                   prev.uct = 0)

  control <- control_msm(nsteps = 52*60,
                         verbose = FALSE)

  sim <- netsim(est, param, init, control)
  df <- as.data.frame(sim)
  cc.dx <- colMeans(tail(df[, c("cc.dx.B", "cc.dx.H", "cc.dx.W")], 52))
  # cc.dx <- mean(tail(df$cc.dx, 52), na.rm = TRUE)
  cc.linked <- colMeans(tail(df[, c("cc.linked1m.B", "cc.linked1m.H", "cc.linked1m.W")], 52))
  # cc.linked <- mean(tail(df$cc.linked1m, 52), na.rm = TRUE)
  # cc.engaged <- colMeans(tail(df[, c("cc.tx.any1y.B", "cc.tx.any1y.H", "cc.tx.any1y.W")], 52))
  # cc.engaged <- mean(tail(df$cc.tx.any1y, 52), na.rm = TRUE)
  cc.vsupp <- colMeans(tail(df[, c("cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W")], 52))
  # cc.vsupp <- mean(tail(df$cc.vsupp, 52), na.rm = TRUE)
  cc.vsupp.dur <- colMeans(tail(df[, c("cc.vsupp.dur1y.B", "cc.vsupp.dur1y.H", "cc.vsupp.dur1y.W")], 52))
  # cc.vsupp.dur <- mean(tail(df$cc.vsupp.dur1y, 52), na.rm = TRUE)

  out <- c(cc.dx, cc.linked, cc.vsupp, cc.vsupp.dur)
  return(out)
}



# ABC Priors and Target Stats ---------------------------------------------

priors <- list(c("unif", 0.001, 0.02),     # hiv.test.rate
               c("unif", 0.001, 0.02),
               c("unif", 0.001, 0.02),
               c("unif", 1/24, 1/1.1),     # tx.init.prob
               c("unif", 1/24, 1/1.1),
               c("unif", 1/24, 1/1.1),
               c("unif", 1/36, 1/1.1),     # tx.halt.part.prob
               c("unif", 1/36, 1/1.1),
               c("unif", 1/36, 1/1.1),
               c("unif", 1/36, 1/1.1),     # tx.reinit.part.prob
               c("unif", 1/36, 1/1.1),
               c("unif", 1/36, 1/1.1))

# draw <- rep(NA, length(priors))
# for (i in 1:length(priors)) {
#   draw[i] <- runif(1, as.numeric(priors[[i]][2]), as.numeric(priors[[i]][3]))
# }
# x <- c(1, draw)
#
# f1 <- f(x)

cc.dx <- c(0.804, 0.799, 0.88)
# cc.dx <- 0.84
cc.linked <- c(0.62, 0.65, 0.76)
# cc.linked <- 0.68
# cc.engaged <- c(0.76, 0.77, 0.84)
# cc.engaged <- 0.80
cc.vsupp <- c(0.55, 0.60, 0.72)
# cc.vsupp <- 0.63
cc.vsupp.dur <- c(0.41, 0.49, 0.63)
# cc.vsupp.dur <- 0.51
targets <- c(cc.dx, cc.linked, cc.vsupp, cc.vsupp.dur)



# Run ABC Prep ------------------------------------------------------------

prep <- abc_smc_prep(model = f,
                     prior = priors,
                     nsims = 400,
                     summary_stat_target = targets,
                     ncores = 28,
                     alpha = 0.1)
prep
saveRDS(prep, file = "burnin/abc/data/abc.prep.rda")

sbatch_master_abc(prep,
                  nwaves = 25,
                  master.file = "burnin/abc/master.sh",
                  mem = "100G",
                  user = "aleguil",
                  ckpt = TRUE)
