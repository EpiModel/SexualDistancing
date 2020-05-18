#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-893 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s200 --export=ALL,SIMNO=200,NJOBS=893,NSIMS=25000 runsim.sh
