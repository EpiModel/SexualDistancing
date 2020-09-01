library(EpiABC)
library(data.table)
library(tidyverse)

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

dt <- data.table()
dt_tmp <- data.table()

i <- 0
for ( file in fs::dir_ls("out/choose", type = "file", recurse = TRUE)) {
  i <- i + 1
  s <- readRDS(file)
  dtt <- as.data.table(s)
  dtt[, `:=`(batch = file)]

  dt_tmp <- rbindlist(list(dt_tmp, dtt))

  if (i %% 100 == 0) {
    dt_tmp <- dt_tmp[
      time == max(time),
      lapply(.SD, median),
      .SDcols = names(targets), by = c("batch", "sim")
    ]

    dt <- rbindlist(list(dt, dt_tmp))
    dt_tmp <- data.table()

    gc()
  }
}


saveRDS(dt, "out/choose_median.Rds")

dt_norm <- dt[,
  Map(function(x, y) (x - y), .SD, targets),
  by = c("batch", "sim"), .SDcols = names(targets)
  ][,
  score := sum(.SD)^2,by = c("batch", "sim"), .SDcols = names(targets)
  ][
  order(score)]

dt_v <- dt[,
  Map(function(x, y) (x - y), .SD, targets),
  by = c("batch", "sim"), .SDcols = names(targets)
]

dt_s <- dt_v[, lapply(.SD, function(x) x / sd(x)), .SDcols = names(targets)]
dt_s <- cbind(dt_v[,c(1, 2)], dt_s)

dt_score <- dt_s[,
  score := sum(.SD)^2,by = c("batch", "sim"), .SDcols = names(targets)
][
  order(score)]

dt_v[dt_score[, .(batch, sim)], on=c("batch", "sim")]
# out/choose/4/sim21.rds - 15
restart_point <- EpiModel::get_sims(readRDS("out/choose/4/sim21.rds"), 15)



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
  Map(function(x, y) (x - y)/y, .SD, targets),
  by = c("batch", "sim"), .SDcols = names(targets)
  ][,
  score := sum(.SD)^2,by = c("batch", "sim"), .SDcols = names(targets)
  ][
  order(score)]



# Calib new trans_scale

slurm_dir <- "slurm_ckpt"
parms <- readRDS(paste0("out/", slurm_dir, "_xs.rds"))
parms_mat <- matrix(flatten_dbl(parms), ncol = length(parms[[1]]), byrow = TRUE)

dt_res <- dt[, lapply(.SD, median),
             .SDcols = names(targets)[1:6], by = c("batch")]

dt_norm <- dt_res[,
  Map(function(x, y) (x - y), .SD, targets[1:6]),
  by = c("batch"), .SDcols = names(targets)[1:6]
  ][,
  score := sum(.SD^2),by = c("batch"), .SDcols = names(targets)[1:6]
  ][
  order(score)]

parms_mat[c(30, 61, 7, 34, 44, 3, 13, 8, 67, 51), ]
