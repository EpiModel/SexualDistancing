library(EpiABC)
library(data.table)
library(tidyverse)

# Check sims
slurm_dir <- "slurm_ckpt"
parms <- readRDS(paste0("out/", slurm_dir, "_xs.rds"))
parms_mat <- matrix(flatten_dbl(parms), ncol = length(parms[[1]]), byrow = TRUE)

dt <- data.table()

## for (slurm_dir in c("slurm_ckpt/", "slurm_csde/")) {
  i <- 0
  for ( file in list.files(paste0("out/", slurm_dir), pattern = ".rds") ) {
    s <- readRDS(paste0("out/", slurm_dir, "/", file))
    dtt <- as.data.table(s)
    dtt[, `:=`(batch = str_extract(file, "\\d+"))]
    dt <- rbindlist(list(dt, dtt))
    if (1 %% 10 == 0) gc()
  }
## }

targets <- c(
  # CombPrev appendix 8.2.2
  i.prev.dx.B = 0.33,
  i.prev.dx.H = 0.127,
  i.prev.dx.W = 0.084,
  #prep_prop = 0.15,
  # google sheet: https://docs.google.com/spreadsheets/d/1GWFrDqvTpdK24f6Lzqg3xdzCobbvOXj7Bpalq7xLUX4/edit?ts=5defba8b#gid=0
  cc.dx.B = 0.804,
  cc.dx.H = 0.799,
  cc.dx.W = 0.88,
  cc.linked1m.B = 0.62,
  cc.linked1m.H = 0.65,
  cc.linked1m.W = 0.76,
  cc.vsupp.B = 0.55,
  cc.vsupp.H = 0.60,
  cc.vsupp.W = 0.72,
  #
  ir100.gc = 12.81,
  ir100.ct = 14.59
)

dt[, lapply(.SD, median), .SDcols = names(targets)][,
  Map(function(x, y) (x - y), .SD, targets),
  .SDcols = names(targets)
  ]

dt_res <- dt[, lapply(.SD, median),
             .SDcols = names(targets), by = c("batch", "sim")]

dt_norm <- dt_res[,
  Map(function(x, y) (x - y), .SD, targets),
  by = c("batch", "sim"), .SDcols = names(targets)
  ][,
  score := sum(.SD)^2,by = c("batch", "sim"), .SDcols = names(targets)
  ][
  order(score)]
