#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/combprev/est

scp burnin/burnin2noLNT/sim.R burnin/burnin2noLNT/runsim.sh burnin/burnin2noLNT/master.sh mox:/gscratch/csde/sjenness/combprev

# Receive
scp mox:/gscratch/csde/sjenness/combprev/data/sim.n400.rda burnin/burnin2noLNT/data

scp mox:/gscratch/csde/sjenness/combprev/est/*.rda est/
