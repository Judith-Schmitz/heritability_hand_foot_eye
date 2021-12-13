#!/bin/bash

#SBATCH --job-name="sequoia"
#SBATCH --export=ALL
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G

mkdir results_sequoia/

Rscript 0_run_sequoia.r


