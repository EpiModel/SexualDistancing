
##
## Diagnostics for Combination Prevention Model
##

## Packages ##
rm(list = ls())
suppressMessages(library("EpiModelHIV"))

est <- readRDS("est/netest.simple.rda")
netstats <- readRDS("est/netstats.rda")


# Main --------------------------------------------------------------------

fit_main <- est[[1]]

dx_main <- netdx(fit_main, nsims = 10, ncores = 6, nsteps = 500,
                 skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_main)
plot(dx_main)

netstats$main


# Casual ------------------------------------------------------------------

fit_casl <- est[[2]]

dx_casl <- netdx(fit_casl, nsims = 10, ncores = 6, nsteps = 500,
                 skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_casl, digits = 2)
plot(dx_casl)

netstats$casl


# One-Off -----------------------------------------------------------------

fit_inst <- est[[3]]

dx_inst <- netdx(fit_inst, nsims = 10000, dynamic = FALSE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))

print(dx_inst, digits = 2)

plot(dx_inst, sim.lines = TRUE, sim.lwd = 0.05)

netstats$inst
