#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/combprev/est
scp burnin/burnin2/sim.R burnin/burnin2/runsim.sh burnin/burnin2/master.sh mox:/gscratch/csde/sjenness/combprev

# Receive
scp mox:/gscratch/csde/sjenness/combprev/data/sim.n300.rda burnin/burnin2/data

scp mox:/gscratch/csde/sjenness/combprev/est/burnin.ATL.3race.Prep15.rda est/
scp mox:/gscratch/csde/sjenness/combprev/data/hold/tdf2.rda burnin/burnin2/data/
scp mox:/gscratch/csde/sjenness/combprev/data/hold/sim.n500.8.rda burnin/burnin2/data/
