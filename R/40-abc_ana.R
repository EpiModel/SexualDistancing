library(tidyverse)
library(EpiABC)

## sims <- readRDS("out/only_rprob/abc.wave5.rda")

out <- get_posterior(wave = 3, input = "out/out")
summary(out, digits = 3)

boxplot(out, type = "stats")

plot(out, type = "stats")

plot(out, type = "param")

boxplot(out, type = "param")


s <- readRDS("out/out/abc.wave0.batch0001.rda")
s$tab_ini

for (f in list.files("out/out")) {
  s <- readRDS(paste0("out/out/", f))
  print(f)
  print(head(s$tab_ini))
}

data_dir <- "out/out/"
wave <- 0
merge_abc(
  wave = wave,
  indir = data_dir,
  outdir = data_dir
)

suppressMessages(library(methods))
suppressMessages(library(EpiABC))
suppressMessages(library(EpiModel))
data_dir <- "out/"

abc_smc_process(
  input = data_dir,
  wave = 0,
  save = TRUE,
  outdir = data_dir
)


data_dir <- "out/out/out"
input <- data_dir
outdir <- data_dir
wave <- 0
save <- TRUE

abc_smc_process(
  input = data_dir,
  wave = 1,
  save = TRUE,
  outdir = data_dir
)
