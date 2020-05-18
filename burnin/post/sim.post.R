
library("EpiModelHIV")
sim <- NULL
source("burnin/post/fx.R")

fn <- list.files("burnin/data", pattern = "sim", full.names = TRUE)
cbind(fn)

load(fn[1])
# sim <- truncate_sim(sim, at = 52*60)
df <- as.data.frame(sim, out = "mean")
names(df)



# Single scenario

par(mar = c(3,3,3,2), mgp = c(2,1,0))

plot(sim, y = "i.prev", ylim = 0:1)
mean(tail(df$i.prev, 52))
plot(sim, y = c("i.prev.B", "i.prev.H", "i.prev.W"), legend = TRUE, ylim = c(0, 1))
colMeans(tail(df[, c("i.prev.B", "i.prev.H", "i.prev.W")], 52))

plot(sim, y = "i.prev.dx", ylim = 0:1)
mean(tail(df$i.prev.dx, 52))

plot(sim, y = c("i.prev.dx.B", "i.prev.dx.H", "i.prev.dx.W"), legend = TRUE, ylim = c(0, 0.4))
abline(h = c(0.333, 0.127, 0.084), col = c("steelblue", "firebrick", "seagreen"), lty = 2)
colMeans(tail(df[, c("i.prev.dx.B", "i.prev.dx.H", "i.prev.dx.W")], 52))



plot(sim, y = "ir100")
plot(sim, y = "num")
plot(sim, y = "age.mean")
plot(sim, y = "dep.gen", ylim = c(0, 8))
plot(sim, y = "dep.AIDS", ylim = c(0, 8))

plot(sim, y = "cc.dx", ylim = 0:1)
plot(sim, y = "cc.tx.any1y", mean.col = 2, add = TRUE)
plot(sim, y = "cc.vsupp", mean.col = 3, add = TRUE)
plot(sim, y = "cc.vsupp.dur1y", mean.col = 4, add = TRUE)

plot(sim, y = "cc.dx", ylim = 0:1)
plot(sim, y = "cc.dx.10y", ylim = 0:1, add = TRUE)

plot(sim, y = "prepCurr")

# Diagnosis ---------------------------------------------------------------

plot(sim, y = c("cc.dx.B", "cc.dx.H", "cc.dx.W"), ylim = c(0.5, 1), legend = TRUE,
     main = "Diagnosed")
colMeans(tail(df[, c("cc.dx.B", "cc.dx.H", "cc.dx.W")], 52))
abline(h = c(0.804, 0.799, 0.88), col = c("steelblue", "firebrick", "seagreen"), lty = 2)

# related outcomes
plot(sim, y = c("cc.dx.delay.B", "cc.dx.delay.H", "cc.dx.delay.W"), ylim = c(0, 300),
     legend = TRUE, main = "Weeks between Infection and Diagnosis")
colMeans(tail(df[, c("cc.dx.delay.B", "cc.dx.delay.H", "cc.dx.delay.W")], 52))/52

plot(sim, y = c("cc.test.int.B", "cc.test.int.H", "cc.test.int.W"), ylim = c(0, 400),
     legend = TRUE, main = "Intertest Interval")
colMeans(tail(df[, c("cc.test.int.B", "cc.test.int.H", "cc.test.int.W")], 52))/52

plot(sim, y = "cc.dx.aids")

plot(sim, y = c("cc.dx.aids.B", "cc.dx.aids.H", "cc.dx.aids.W"), ylim = c(0, 0.3))
colMeans(tail(df[, c("cc.dx.aids.B", "cc.dx.aids.H", "cc.dx.aids.W")], 52))

mean(tail(df$cc.test.int), 52)


# Linkage to Care ---------------------------------------------------------

# plot(sim, y = "cc.linked1m")
plot(sim, y = c("cc.linked1m.B", "cc.linked1m.H", "cc.linked1m.W"), ylim = 0:1, legend = TRUE)
colMeans(tail(df[, c("cc.linked1m.B", "cc.linked1m.H", "cc.linked1m.W")], 52))
abline(h = c(0.62, 0.65, 0.76), col = c("steelblue", "firebrick", "seagreen"), lty = 2)



# Engaged in Care ---------------------------------------------------------

# plot(sim, y = "cc.tx.any1y", ylim = 0:1)
plot(sim, y = c("cc.tx.any1y.B", "cc.tx.any1y.H", "cc.tx.any1y.W"), ylim = 0:1, legend = TRUE)
colMeans(tail(df[, c("cc.tx.any1y.B", "cc.tx.any1y.H", "cc.tx.any1y.W")], 52))
abline(h = c(0.76, 0.77, 0.84), col = c("steelblue", "firebrick", "seagreen"), lty = 2)


# Viral Suppression ---------------------------------------------------------

# plot(sim, y = "cc.vsupp", ylim = 0:1)
plot(sim, y = c("cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W"), ylim = 0:1, legend = TRUE)
colMeans(tail(df[, c("cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W")], 52))
abline(h = c(0.55, 0.60, 0.72), col = c("steelblue", "firebrick", "seagreen"), lty = 2)


# Durable Viral Suppression -----------------------------------------------

# plot(sim, y = "cc.vsupp.dur1y", ylim = 0:1)
plot(sim, y = c("cc.vsupp.dur1y.B", "cc.vsupp.dur1y.H", "cc.vsupp.dur1y.W"), ylim = 0:1, legend = TRUE)
colMeans(tail(df[, c("cc.vsupp.dur1y.B", "cc.vsupp.dur1y.H", "cc.vsupp.dur1y.W")], 52))
abline(h = c(0.41, 0.49, 0.63), col = c("steelblue", "firebrick", "seagreen"), lty = 2)


plot(sim, y = "cc.linked1m")

plot(sim, y = "cc.HIV.mr")
mean(tail(df$cc.HIV.mr, 52))

plot(sim, y = "hstage.acute")
plot(sim, y = "hstage.chronic")
plot(sim, y = "hstage.aids")

plot(sim, y = "dep.AIDS")
plot(sim, y = "new.aids.tot")
plot(sim, y = "new.aids.full")
plot(sim, y = "new.aids.part")

plot(sim, y = "dep.HIV")
plot(sim, y = "dep.HIV.old")
mean(tail(df$dep.HIV, 52))
mean(tail(df$dep.HIV.old, 52))

plot(sim, y = "mean.tx.on")
plot(sim, y = "mean.tx.off")

# Comparative -------------------------------------------------------------

par(mar = c(3,3,1,1), mgp = c(2,1,0))
all <- gather_netsim(fn)

plot_netsim_list(all, var = "hstage.aids", ylim = c(0, 1))

plot_netsim_list(all, var = "cc.HIV.mr", ylim = c(0, 0.1))

plot_netsim_list(all, var = "dep.AIDS", ylim = c(0, 2))

plot_netsim_list(all, var = "new.aids.full", ylim = c(0, 1))

plot_netsim_list(all, var = "cc.vsupp", ylim = c(0, 1))
plot_netsim_list(all, var = "cc.vsupp.dur1y", ylim = c(0, 1))

plot_netsim_list(all, var = "mean.tx.on", ylim = c(0, 1000))
plot_netsim_list(all, var = "mean.tx.off", ylim = c(0, 300))

