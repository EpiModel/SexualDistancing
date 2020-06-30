#!/bin/bash

#SBATCH --nodes=1
#SBATCH -o ./abc/log/%x_%a.out

source loadR.sh
Rscript abc/process.R
