

## CombPrev Figures

library("EpiModelHIV")
library("ARTnet")
library("dplyr")
library("foreach")

library("ggplot2")
library("viridis")
library("metR")


# Figure 1 ----------------------------------------------------------------

# Run on Hyak

ci_contour_df <- function(sims) {
  doParallel::registerDoParallel(parallel::detectCores() - 1)
  df <- foreach(i = seq_along(sims)) %dopar% {
    fn <- list.files("data/", pattern = as.character(sims[i]), full.names = TRUE)
    load(fn)
    incid <- unname(colSums(sim$epi$incid, na.rm = TRUE))
    new.df <- data.frame(scenario = sims[i],
                         incid = incid,
                         p1 = sim$param$MULT1,
                         p2 = sim$param$MULT2,
                         lnt = sim$param$prep.require.lnt)
    return(new.df)
  }
  doParallel::stopImplicitCluster()
  df <- do.call("rbind", df)
  return(df)
}

fig1 <- ci_contour_df(6000:6721)
table(fig1$p1, fig1$p2, fig1$lnt)

save(fig1, file = "data/Fig1-data.rda")

system("scp mox:/gscratch/csde/sjenness/combprev/data/Fig1-data.rda analysis/")

# Graphics Locally
rm(list = ls())
load("analysis/Fig1-data.rda")

f1a <- filter(fig1, lnt == TRUE)
f1b <- filter(fig1, lnt == FALSE)

f1a.base <- filter(f1a, p1 == 1 & p2 == 1) %>%
  summarise(mi = median(incid)) %>%
  as.numeric
f1a$pia <- (f1a.base - f1a$incid)/f1a.base
f1a[sample(nrow(f1a), 25), ]

f1b.base <- filter(f1b, p1 == 1 & p2 == 1) %>%
  summarise(mi = median(incid)) %>%
  as.numeric
f1b$pia <- (f1b.base - f1b$incid)/f1b.base
f1b[sample(nrow(f1b), 25), ]

loess1a <- loess(pia ~ p1 * p2, data = f1a)
fit1a <- expand.grid(list(p1 = seq(1, 10, 0.1),
                          p2 = seq(1, 10, 0.1)))
fit1a$pia <- as.numeric(predict(loess1a, newdata = fit1a))
head(fit1a, 25)

loess1b <- loess(pia ~ p1 * p2, data = f1b)
fit1b <- expand.grid(list(p1 = seq(1, 10, 0.1),
                          p2 = seq(1, 10, 0.1)))
fit1b$pia <- as.numeric(predict(loess1b, newdata = fit1b))
head(fit1b, 25)

fit1a$LNT = "PrEP Linked"
fit1b$LNT = "PrEP Unlinked"
fit1 <- rbind(fit1a, fit1b)

tail(arrange(filter(fit1, LNT == "PrEP Linked"), pia), 10)
tail(arrange(filter(fit1, LNT == "PrEP Unlinked"), pia), 10)

filter(fit1a, p1 == 10 & p2 == 10)
filter(fit1b, p1 == 10 & p2 == 10)

filter(f1a, p1 == 10 & p2 == 10) %>%
  summarise(mi = mean(pia))

filter(f1b, p1 == 10 & p2 == 10) %>%
  summarise(mi = mean(pia))

names(fit1)[3] <- "PIA"
fit1$PIA <- fit1$PIA * 100
head(fit1, 25)

f1 <- ggplot(fit1, aes(p1, p2)) +
  geom_raster(aes(fill = PIA), interpolate = TRUE) +
  geom_contour(aes(z = PIA), col = "white", alpha = 0.5, lwd = 0.5) +
  geom_text_contour(aes(z = PIA), stroke = 0.1, size = 3.5) +
  theme_minimal() +
  facet_grid(cols = vars(LNT)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Relative Retention", x = "Relative Screening") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1)
f1

ggsave("analysis/fig/Figure1.pdf", device = "pdf", height = 6, width = 10, units = "in")
ggsave("analysis/fig/Figure1.jpg", device = "jpg", height = 6, width = 10, units = "in")


# Figure 2 ----------------------------------------------------------------

load("analysis/data/sim.n8000.rda")
sim.base <- truncate_sim(sim, 2)

load("analysis/data/sim.n8001.rda")
sim.comp <- truncate_sim(sim, 2)

df.base <- as.data.frame(sim.base, out = "mean")
df.comp <- as.data.frame(sim.comp, out = "mean")

save(df.base, df.comp, file = "analysis/Fig2-data.rda")

load("analysis/Fig2-data.rda")

ir.base <- df.base$ir100
ir.comp <- df.comp$ir100

y <- supsmu(1:nrow(df.comp), df.comp$ir100)$y
plot(y, type = "l")
abline(h = c(1.18*0.1, 1.18*0.25), v = 622, lty = 2)

which.max(y <= 1.18*0.1)
622/52
2020 + 622/52

which.max(y <= 1.18*0.25)
208/52
2020 + 208/52

pal <- adjustcolor(RColorBrewer::brewer.pal(3, "Set1"), alpha.f = 0.8)
xs <- 2020 + 1:length(ir.base)/52

# pdf(file = "analysis/fig/Figure2.pdf", height = 6, width = 10)
jpeg(file = "analysis/fig/Figure2.jpg", height = 6, width = 10, res = 200, units = "in")
par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(xs, ir.base, type = "l", ylim = c(0, 1.5), col = pal[2], lwd = 1.2,
     ylab = "Incidence Rate per 100 PYAR", xlab = "Year", font.lab = 2)
lines(xs, ir.comp, col = pal[1], lwd = 1.2)
abline(h = c(1.18*0.1, 1.18*0.25), lty = 2, lwd = 1, col = adjustcolor("black", alpha.f = 0.6))
text(2062, 0.16, "EHE 2030 90% Reduction Target", cex = 0.9)
text(2062, 0.35, "EHE 2025 75% Reduction Target", cex = 0.9)
legend("topright", legend = c("Reference Model", "10x10 Model"), lty = 1, lwd = 2, col = pal[2:1],
       bty = "n", cex = 0.9)
dev.off()

