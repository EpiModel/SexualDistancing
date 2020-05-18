
library("EpiModelHIV")
sim <- NULL

fn <- list.files("burnin/burninFS/data", pattern = "sim", full.names = TRUE)
cbind(fn)

load(fn[1])
# sim <- truncate_sim(sim, at = 52*60)
df <- as.data.frame(sim, out = "mean")
names(df)

par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y = c("i.prev.dx.B", "i.prev.dx.H", "i.prev.dx.W"), legend = TRUE, ylim = c(0, 0.4))
abline(h = c(0.333, 0.127, 0.084), col = c("steelblue", "firebrick", "seagreen"), lty = 2)
x <- round(colMeans(tail(df[, c("i.prev.dx.B", "i.prev.dx.H", "i.prev.dx.W")], 52)), 3)
text(2500, 0.36, x[1], col = "steelblue")
text(2500, 0.18, x[2], col = "firebrick")
text(2500, 0.05, x[3], col = "seagreen")


plot(sim, y = c("cc.dx.B", "cc.dx.H", "cc.dx.W"), ylim = c(0.5, 1), legend = TRUE)
x <- round(colMeans(tail(df[, c("cc.dx.B", "cc.dx.H", "cc.dx.W")], 52)), 3)
abline(h = c(0.804, 0.799, 0.88), col = c("steelblue", "firebrick", "seagreen"), lty = 2)
text(2500, 0.92, x[3], col = "seagreen")
text(2500, 0.85, x[1], col = "steelblue")
text(2500, 0.75, x[2], col = "firebrick")


plot(sim, y = c("cc.linked1m.B", "cc.linked1m.H", "cc.linked1m.W"), ylim = 0:1, legend = TRUE)
abline(h = c(0.62, 0.65, 0.76), col = c("steelblue", "firebrick", "seagreen"), lty = 2)
x <- round(colMeans(tail(df[, c("cc.linked1m.B", "cc.linked1m.H", "cc.linked1m.W")], 52)), 3)
text(2500, 0.59, x[1], col = "steelblue")
text(2500, 0.72, x[2], col = "firebrick")
text(2500, 0.82, x[3], col = "seagreen")


plot(sim, y = c("cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W"), ylim = 0:1, legend = TRUE)
abline(h = c(0.55, 0.60, 0.72), col = c("steelblue", "firebrick", "seagreen"), lty = 2)
x <- round(colMeans(tail(df[, c("cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W")], 52)), 3)
text(2500, 0.50, x[1], col = "steelblue")
text(2500, 0.66, x[2], col = "firebrick")
text(2500, 0.78, x[3], col = "seagreen")
