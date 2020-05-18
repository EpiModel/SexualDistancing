
##
## Selects the simulations with the smallest deviance from targets for intervention
##

suppressMessages(library("EpiModelHIV"))
suppressMessages(library("tidyverse"))
suppressMessages(library("foreach"))

# List Files
list.files("data/")
fn <- list.files("data/", pattern = "sim", full.names = TRUE)

# Create outcome data frame for comparison
doParallel::registerDoParallel(parallel::detectCores())
tdf <- foreach(i = 1:length(fn)) %dopar% {
  load(fn[i])
  f <- function(j) {
    df <- as.data.frame(x = sim, sim = j)
    df <- select(df, i.prev.dx.B, i.prev.dx.H, i.prev.dx.W)
    df <- tail(df, 52)
    batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3], collapse = "."), j, sep = ".")
    out <- c(batch, colMeans(df))
    return(out)
  }
  t(sapply(1:sim$control$nsims, f))
}

tdf <- data.frame(do.call("rbind", tdf), stringsAsFactors = FALSE)
names(tdf)[1] <- "batch"
tdf[2:4] <- sapply(tdf[2:4], as.numeric)
head(tdf, 25); str(tdf)

save(tdf, file = "data/hold/tdf.rda")

# Simulation selection
targets <- c(0.333, 0.127, 0.084)
tdf$diff <- abs(tdf$i.prev.dx.B - targets[1]) +
            abs(tdf$i.prev.dx.H - targets[2]) +
            abs(tdf$i.prev.dx.W - targets[3])
head(arrange(tdf, diff), 25)

# Verify outcomes
load("data/sim.n200.715.rda")
ls()
s1 <- get_sims(sim, sims = 20)

df <- as.data.frame(s1)
df <- select(df, i.prev.dx.B, i.prev.dx.H, i.prev.dx.W)
df <- tail(df, 52)
df
colMeans(df)

# Save selected simulation
sim <- s1
saveRDS(sim, file = "est/burnin.ATL.3race.rda", compress = "xz")

# Hold the full netsim file
system("mv data/sim.n200.715.rda data/hold/")

# Reinstall EpiModelHIV before running interventions
