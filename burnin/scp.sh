#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/combprev/est

scp burnin/sim.R burnin/runsim.sh burnin/master.sh mox:/gscratch/csde/sjenness/combprev

# Receive
scp mox:/gscratch/csde/sjenness/combprev/data/hold/sim.n100.rda burnin/data/

scp mox:/gscratch/csde/sjenness/combprev/data/hold/*.rda burnin/data/
scp mox:/gscratch/csde/sjenness/combprev/est/burnin.ATL.3race.rda est/
