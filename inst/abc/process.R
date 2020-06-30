suppressMessages(library(methods))
suppressMessages(library(EpiABC))
suppressMessages(library(EpiModel))

wave <- as.numeric(Sys.getenv("wave"))
data_dir <- "abc/out/"

merge_abc(
  wave = wave,
  indir = data_dir,
  outdir = data_dir
)

abc_smc_process(
  input = data_dir,
  wave = wave,
  save = TRUE,
  outdir = data_dir
)
