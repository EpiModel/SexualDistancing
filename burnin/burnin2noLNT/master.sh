#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s500 --export=ALL,SIMNO=500,NJOBS=9,NSIMS=250 runsim.sh
