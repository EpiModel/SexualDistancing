suppressMessages(library(methods))
suppressMessages(library(EpiABC))
suppressMessages(library(EpiModel))

prep <- readRDS("abc/data/abc.prep.rds")

batch <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
wave <- as.numeric(Sys.getenv("wave"))

data_dir <- "abc/out/"
if (wave != 0) prep <- data_dir

abc_smc_wave(
  input = prep,
  wave = wave,
  batch = batch,
  save = TRUE,
  outdir = data_dir
)

