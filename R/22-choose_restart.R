library(data.table)

# One or many job_names
job_names <- c()
job_last_n <- 1 # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n)) {
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)
}

jobs <- list()

# Read targets
source("R/utils-targets.R")

for (job in job_names) {
  jobs[[job]] <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  jobs[[job]]$infos <- infos

  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  jobs[[job]]$data <- data.table()
  btch <- 0
  for (fle in sim_files) {
    btch <- btch + 1
    sim <- readRDS(fle)
    dff <- if (is.null(infos$df_keep)) as.data.frame(sim) else sim

    setDT(dff)
    dff[, batch := btch]
    dff <- dff[time > max(time) - 52, lapply(.SD, median, na.rm = T),
               .SDcols = names(targets), by = c("batch", "sim")]
    jobs[[job]]$data <- rbind(jobs[[job]]$data, dff)
  }
}

df <- jobs[[1]]$data

dt_norm <- df[, Map(function(x, y) (x - y), .SD, targets),
              by = c("batch", "sim"), .SDcols = names(targets)
             ][,
               score := sum(.SD / sd(.SD))^2,
               by = c("batch", "sim"), .SDcols = names(targets)
             ][order(score)]


sim <- readRDS(fs::path("out/remote_jobs/", job, "out/sim110.rds"))
orig <- EpiModel::get_sims(sim, 24)
saveRDS(orig, "out/est/restart.rds")

df <- as.data.table(orig)
print(names(df), max = 200)
