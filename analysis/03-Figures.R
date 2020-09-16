
## SexDist Figures


# Setup -------------------------------------------------------------------

rm(list=ls())

library("dplyr")
library("ggplot2")
library("viridis")
library("metR")
library("patchwork")

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


# Figure 1 ----------------------------------------------------------------

##  90-90 scenario
jpeg("analysis/Fig1-90.jpeg", height = 6, width = 12, units = "in", res = 300)

par(mar = c(3,3,1,1), mgp = c(2,1,0))
par(mfrow = c(1,2))

## HIV panel
var <- "hiv_inc"
roll <- 8

scen <- "ser_all_09"
h1 <- create_var_df(df, scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
plot(h3[, 1], type = "l", ylim = c(0, 5), col = 2, lwd = 2, lty = 1,
     xlab = "Week", ylab = "HIV IR per 100 PYAR")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

h1 <- create_var_df(df, scen = "base", var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 1, lwd = 2)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))
# abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

scen <- "net_all_09"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 3, lwd = 2, lty = 1)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_09_09"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 4, lwd = 2, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

# legend("topright",
#        legend = c("No Change", "Sexual Distancing Only",
#                   "Service Reduction Only", "Combined"),
#        lwd = 2.5, lty = 1, col = c(1, 3, 2, 4), bty = "n", cex = 0.8)

text(26, 4.9, "COVID Start", cex = 0.8)
text(105, 4.9, "COVID End", cex = 0.8)

## STI panel

var <- "sti_inc"
roll <- 4

scen <- "ser_all_09"
h1 <- create_var_df(df, scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
plot(h3[, 1], type = "l", ylim = c(0, 100), col = 2, lwd = 2,
     xlab = "Week", ylab = "STI IR per 100 PYAR")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

h1 <- create_var_df(df, scen = "base", var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 1, lwd = 2)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))
# abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

scen <- "net_all_09"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 3, lwd = 2)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_09_09"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 4, lwd = 2, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

legend("topright",
       legend = c("No Change", "Sexual Distancing Only",
                  "Service Reduction Only", "Combined"),
       lwd = 2.5, lty = 1, col = c(1, 3, 2, 4), bty = "n", cex = 0.8)

text(26, 96, "COVID Start", cex = 0.8)
text(105, 96, "COVID End", cex = 0.8)

dev.off()



## 50-50 Scenario
jpeg("analysis/Fig1-50.jpeg", height = 6, width = 12, units = "in", res = 300)

par(mar = c(3,3,1,1), mgp = c(2,1,0))
par(mfrow = c(1,2))

## HIV panel
var <- "hiv_inc"
roll <- 8

scen <- "ser_all_05"
h1 <- create_var_df(df, scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
plot(h3[, 1], type = "l", ylim = c(0, 5), col = 2, lwd = 2, lty = 1,
     xlab = "Week", ylab = "HIV IR per 100 PYAR")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

h1 <- create_var_df(df, scen = "base", var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 1, lwd = 2)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))
# abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

scen <- "net_all_05"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 3, lwd = 2, lty = 1)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_05_05"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 4, lwd = 2, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

text(26, 4.9, "COVID Start", cex = 0.8)
text(105, 4.9, "COVID End", cex = 0.8)

## STI panel

var <- "sti_inc"
roll <- 4

scen <- "ser_all_05"
h1 <- create_var_df(df, scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
plot(h3[, 1], type = "l", ylim = c(0, 100), col = 2, lwd = 2,
     xlab = "Week", ylab = "STI IR per 100 PYAR")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

h1 <- create_var_df(df, scen = "base", var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 1, lwd = 2)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))
# abline(v = c(int_beg-ana_beg, int_end-ana_beg), lty = 2)

scen <- "net_all_05"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 3, lwd = 2)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_05_05"
h1 <- create_var_df(df, scen = scen, var)
h2 <- create_quants_df(h1, low = 0.25, high = 0.75)
h3 <- apply_roll(h2, roll)
lines(h3[, 1], type = "l", col = 4, lwd = 2, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

legend("topright",
       legend = c("No Change", "Sexual Distancing Only",
                  "Service Reduction Only", "Combined"),
       lwd = 2.5, lty = 1, col = c(1, 3, 2, 4), bty = "n", cex = 0.8)

text(26, 96, "COVID Start", cex = 0.8)
text(105, 96, "COVID End", cex = 0.8)

dev.off()




# Figure 2 ----------------------------------------------------------------

dfs <- readRDS("~/data/SexDist/df_sensi.rds")
dfs <- filter(dfs, time >= ana_beg)
table(dfs$scenario)


dfs1 <- group_by(dfs, sim, batch, scenario) %>%
  summarise(hivCI = sum(hiv_inc),
            stiCI = sum(sti_inc))

table(dfs1$scenario)

n1 <- strsplit(dfs1$scenario, "_")
net <- rep(NA, length(n1))
ser <- rep(NA, length(n1))

for (i in 1:length(n1)) {
  net[i] <- as.numeric(strsplit(n1[[i]][2], "net")[[1]][2])
  ser[i] <- as.numeric(strsplit(n1[[i]][3], "ser")[[1]][2])
}

dfs1$net <- net
dfs1$ser <- ser

dfs1 <- filter(dfs1, net > 0 & ser > 0)
dfs1$net <- as.factor(dfs1$net)
dfs1$ser <- as.factor(dfs1$ser)


p1 <- ggplot(dfs1, aes(ser, hivCI)) +
  geom_boxplot(aes(fill = net), outlier.shape = NA, alpha = 0.75, position = position_dodge(1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()
p1

# ggplot(dfs1, aes(ser, hivCI)) +
#   geom_violin(aes(fill = net), alpha = 0.75, position = position_dodge(1)) +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw()

# ggsave("analysis/Figure2a.jpeg", device = "jpeg", height = 6, width = 12, units = "in")

p2 <- ggplot(dfs1, aes(ser, stiCI)) +
  geom_boxplot(aes(fill = net), outlier.shape = NA, alpha = 0.75, position = position_dodge(1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()
p2

# ggplot(dfs1, aes(ser, stiCI)) +
#   geom_violin(aes(fill = net), alpha = 0.75, position = position_dodge(1)) +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw()



p1/p2

ggsave("analysis/Figure2.jpeg", device = "jpeg", height = 6, width = 12, units = "in")






fig1 <- ci_contour_df(6000:6721)
table(fig1$p1, fig1$p2, fig1$lnt)

save(fig1, file = "data/Fig1-data.rda")

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


