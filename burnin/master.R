
## build master.sh script ##

library("EpiModelHPC")

# Calibration/Testing -----------------------------------------------------

# Test stability of run
vars <- NULL
sbatch_master(vars = vars,
              master.file = "burnin/master.sh",
              simno.start = 100,
              ckpt = TRUE,
              nsims = 250,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# Big run batch for model selection
sbatch_master(vars = vars,
              master.file = "burnin/master.sh",
              simno.start = 200,
              ckpt = TRUE,
              nsims = 25000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
