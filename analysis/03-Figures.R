
## SexDist Figures

# Setup -------------------------------------------------------------------

rm(list = ls())

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


# Figure 1&3 ----------------------------------------------------------------

## 50-50 Scenario, 18m Distancing

df <- readRDS("~/data/SexDist/df.rds")
df <- filter(df, time >= ana_beg)
table(df$scenario)

# pdf("analysis/Fig1.pdf", height = 6, width = 12)
tiff("analysis/Fig1.tiff", height = 6, width = 12, units = "in", res = 500)

par(mar = c(3,3,2,1), mgp = c(2,1,0))
par(mfrow = c(1,2))

## HIV panel
var <- "hiv_inc"
roll <- 10

scen <- "ser_all_05"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
plot(h3[, 1], type = "l", ylim = c(0, 5), col = 2, lwd = 1.25, lty = 1,
     xlab = "Week", ylab = "HIV Incidence per 100 PYAR",
     main = "A. HIV")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = int_beg-ana_beg, lty = 3, lwd = 1.25)
abline(v = int_end-ana_beg, lty = 3, lwd = 1.25)

h1 <- create_var_df(df, scen = "base", var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 1, lwd = 1.25)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))

scen <- "net_all_05"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 3, lwd = 1.25, lty = 1)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_05_05"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 4, lwd = 1.25, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

text(23, 4.9, "COVID Start", cex = 0.85)
text(93, 4.9, "Distancing Ends\n& Services Resume", cex = 0.85)

## STI panel

var <- "sti_inc"
roll <- 2

scen <- "ser_all_05"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
plot(h3[, 1], type = "l", ylim = c(0, 100), col = 2, lwd = 1.25,
     xlab = "Week", ylab = "STI Incidence per 100 PYAR",
     main = "B. Combined STI (NG and CT)")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = int_beg-ana_beg, lty = 3, lwd = 1.25)
abline(v = int_end-ana_beg, lty = 3, lwd = 1.25)

h1 <- create_var_df(df, scen = "base", var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 1, lwd = 1.25)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))

scen <- "net_all_05"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 3, lwd = 1.25)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_05_05"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 4, lwd = 1.25, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

legend("topright",
       legend = c("Base", "Distancing Only",
                  "Services Only", "Combined"),
       lwd = 2.5, lty = 1, col = c(1, 3, 2, 4), bty = "n", cex = 0.9)

dev.off()

## 50-50 Scenario + 3 month distancing

# pdf("analysis/Fig3.pdf", height = 6, width = 12)
tiff("analysis/Fig3.tiff", height = 6, width = 12, units = "in", res = 500)

par(mar = c(3,3,2,1), mgp = c(2,1,0))
par(mfrow = c(1,2))

df <- readRDS("~/data/SexDist/df_318.rds")
df <- filter(df, time >= ana_beg)
table(df$scenario)

## HIV panel
var <- "hiv_inc"
roll <- 10

scen <- "ser_only_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
plot(h3[, 1], type = "l", ylim = c(0, 5), col = 2, lwd = 1.25, lty = 1,
     xlab = "Week", ylab = "HIV Incidence per 100 PYAR",
     main = "A. HIV")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = int_beg-ana_beg, lty = 3, lwd = 1.25)
abline(v = int_end-ana_beg, lty = 3, col = 2, lwd = 1.25)
abline(v = int_beg-ana_beg+(3*4), lty = 3, col = 3, lwd = 1.25)

scen <- "base_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 1, lwd = 1.25)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))

scen <- "net_only_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 3, lwd = 1.25, lty = 1)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 4, lwd = 1.25, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

text(23, 4.9, "COVID Start", cex = 0.85)
text(88, 4.9, "Distancing\nEnds", col = 3, cex = 0.85)
text(152, 4.9, "Services\nResume", col = 2, cex = 0.85)


## STI panel

var <- "sti_inc"
roll <- 4

scen <- "ser_only_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
plot(h3[, 1], type = "l", ylim = c(0, 100), col = 2, lwd = 1.25,
     xlab = "Week", ylab = "STI Incidence per 100 PYAR",
     main = "B. Combined STI (NG and CT)")
draw_quants(h3, col = adjustcolor(2, alpha.f = 0.1))
abline(v = int_beg-ana_beg, lty = 3, lwd = 1.25)
abline(v = int_end-ana_beg, lty = 3, col = 2, lwd = 1.25)
abline(v = int_beg-ana_beg+(3*4), lty = 3, col = 3, lwd = 1.25)

scen <- "base_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 1, lwd = 1.25)
draw_quants(h3, col = adjustcolor(1, alpha.f = 0.1))

scen <- "net_only_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 3, lwd = 1.25, lty = 1)
draw_quants(h3, col = adjustcolor(3, alpha.f = 0.1))

scen <- "comb_318"
h1 <- create_var_df(df, scen, var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
lines(h3[, 1], type = "l", col = 4, lwd = 1.25, lty = 1)
draw_quants(h3, col = adjustcolor(4, alpha.f = 0.1))

legend("topright",
       legend = c("Base", "Distancing Only",
                  "Services Only", "Combined"),
       lwd = 2.5, lty = 1, col = c(1, 3, 2, 4), bty = "n", cex = 0.9)

dev.off()


# Figure 2 ----------------------------------------------------------------

dfs <- readRDS("~/data/SexDist/df_sensi.rds")
dfs <- filter(dfs, time >= ana_beg)
table(dfs$scenario)

dfs1 <- group_by(dfs, sim, batch, scenario) %>%
  summarise(hivCI = sum(hiv_inc)*(1000/5200),
            stiCI = sum(sti_inc)*(1000/5200))

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

dfs1 %>%
  select(net, ser, hivCI, stiCI) %>%
  pivot_longer(c(hivCI, stiCI)) %>%
  ggplot(aes(x = ser, y = value, fill = net)) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.75,
    width = 0.6,
    position = position_dodge(0.8),
  ) +
  facet_grid(
    rows = vars(name), scales = "free_y", switch = "y",
    labeller = function(x) list("name" = c("HIV Cumulative Incidence",
                                           "STI CumulativeIncidence"))
  ) +
  scale_fill_brewer(palette = "Set1") +
  ## ylab("Cumulative Incidence") +
  ylab("") +
  xlab("Service Interruption Duration (Months)") +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "black"),
    legend.margin = margin(0, 0, -5, 0),
    legend.position = "top",
    aspect.ratio = 1/4,
  ) +
  guides(fill = guide_legend(
    title.position = "left",
    title = "Sexual Distancing Duration (Months):",
    nrow = 1
  ))
dfs1

# ggsave("analysis/Fig2.pdf", device = "pdf", height = 6, width = 12, units = "in")
ggsave("analysis/Fig2.tiff", device = "tiff", height = 6, width = 12, units = "in", dpi = 500)


head(dfs1)
d318 <- filter(dfs1, net == 3 & ser == 18)
summary(d318$hivCI)
summary(d318$stiCI)

d183 <- filter(dfs1, net == 18 & ser == 3)
summary(d183$hivCI)
summary(d183$stiCI)


# Fig 2 Contour Plots -----------------------------------------------------

gg_sensi_contour <- function(df_sensi, outcome, outcome_label) {
  prep_start <- 52 * (65 + 1) + 1
  ana_beg <- prep_start + 5 * 52

  dfs <- df_sensi %>%
    filter(time >= ana_beg) %>%
    group_by(sim, batch, scenario) %>%
    summarise(
      hivCI = sum(hiv_inc),
      stiCI = sum(sti_inc)
    ) %>%
    group_by(scenario) %>%
    summarise(across(c(hivCI, stiCI), median)) %>%
    separate(scenario, into = c(NA, "net", "ser"), "\\D+", remove = FALSE) %>%
    mutate(across(c(net, ser), .fns = as.numeric))

  p <- ggplot(dfs, aes(x = ser, y = net, z = {{ outcome }})) +
    geom_contour_fill(na.fill = TRUE) +
    geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
    scale_fill_continuous(type = "viridis", direction = -1, name = "CI") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    xlab("Service Interruption Duration (Months)") +
    ylab("Sexual Distancing Duration (Months)") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      aspect.ratio = 1
    ) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.1)) +
    ggtitle(outcome_label)

  p
}

df_sensi <- readRDS("~/data/SexDist/df_sensi.rds")

gg_sensi_contour(df_sensi, hivCI, "A. HIV Cumulative Incidence")
ggsave("analysis/Figure3_HIV.jpeg", device = "jpeg", height = 6, width = 6, units = "in")
gg_sensi_contour(df_sensi, stiCI, "B. STI CumulativeIncidence")
ggsave("analysis/Figure3_STI.jpeg", device = "jpeg", height = 6, width = 6, units = "in")



# Fig 1 90-90 Scenarios ---------------------------------------------------

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

