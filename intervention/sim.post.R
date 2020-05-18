
library("parallel")

cl <- makeCluster(parallel::detectCores())
cl

vars <- c("num", "num.B", "num.H", "num.W",
          "i.num", "i.num.B", "i.num.H", "i.num.W",
          "ir100", "ir100.B", "ir100.H", "ir100.W",
          "incid", "incid.B", "incid.H", "incid.W",
          "mean.neg.tests", "mean.neg.tests.B", "mean.neg.tests.H", "mean.neg.tests.W",
          "test.past.year", "test.past.year.B", "test.past.year.H", "test.past.year.W",
          "newDx", "newDx45", "newDx140", "newDx200", "newDx2y",
          "tot.tests", "tot.tests.B", "tot.tests.H", "tot.tests.W",
          "prepCurr", "prepCurr.B", "prepCurr.H", "prepCurr.W",
          "prepElig", "prepElig.B", "prepElig.H", "prepElig.W",
          "cc.dx", "cc.dx.B", "cc.dx.H", "cc.dx.W",
          "cc.dx.delay.int", "cc.dx.delay.int.B", "cc.dx.delay.int.H", "cc.dx.delay.int.W",
          "cc.vsupp", "cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W",
          "cc.vsupp.all", "cc.vsupp.all.B", "cc.vsupp.all.H", "cc.vsupp.all.W",
          "cc.linked1m.int", "cc.linked1m.int.B", "cc.linked1m.int.H", "cc.linked1m.int.W",
          "i.prev.dx", "i.prev.dx.B", "i.prev.dx.H", "i.prev.dx.W",
          "tot.neg.tests", "i.num.dx")

varsF1 <- c("ir100.B", "ir100.H", "ir100.W")
varsF2 <- c("incid", "ir100")

f <- function(x, vars, min.n, nsims) {
  EpiModelHPC::process_simfiles(simno = x, min.n = min.n, nsims = nsims, vars = vars,
                   truncate.at = 52*65, compress = "xz", delete.sub = TRUE)
}

sims <- 7000:7003
clusterApply(cl, sims, f, vars, 36, 1000)

sims <- 5000:5272
clusterApply(cl, sims, f, varsF1, 18, 500)

sims <- 6080:6199
clusterApply(cl, sims, f, varsF2, 4, 112)

sims <- 7200
clusterApply(cl, sims, f, vars, 36, 1000)

sims <- 8004
clusterApply(cl, sims, f, vars, 36, 1000)

sims <- 6500:6507
clusterApply(cl, sims, f, vars, 36, 1000)
