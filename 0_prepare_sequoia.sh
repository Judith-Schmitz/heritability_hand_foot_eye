#!/bin/bash

#SBATCH --job-name="filter"
#SBATCH --export=ALL
#SBATCH --cpus-per-task=4
#SBATCH --mem=2G

# filter dataset and use new fam file that includes family relationships
~/apps/conda/envs/test_env/bin/plink \
--bed /mnt/shared/projects/uosa/Silvia_Paracchini/shared/shared_data/ALSPAC_joined/joined.bed \
--bim /mnt/shared/projects/uosa/Silvia_Paracchini/shared/shared_data/ALSPAC_joined/joined.bim \
--fam /mnt/shared/projects/uosa/Silvia_Paracchini/shared/shared_data/ALSPAC_joined/new.joined.fam \
--exclude range /mnt/shared/scratch/jschmitz/private/PCA/PCA.exclude.regions.b37.txt \
--indep-pairwise 50 5 0.01 \
--threads 4 \
--maf 0.45 \
--geno 0.01 \
--out data/filtered

# thin dataset to obtain 500 SNPs at random
~/apps/conda/envs/test_env/bin/plink \
--bed /mnt/shared/projects/uosa/Silvia_Paracchini/shared/shared_data/ALSPAC_joined/joined.bed \
--bim /mnt/shared/projects/uosa/Silvia_Paracchini/shared/shared_data/ALSPAC_joined/joined.bim \
--fam /mnt/shared/projects/uosa/Silvia_Paracchini/shared/shared_data/ALSPAC_joined/new.joined.fam \
--extract data/filtered.prune.in \
--make-bed \
--threads 4 \
--thin-count 500 \
--out data/filtered_thinned

# Prepare raw format for Sequoia
~/apps/conda/envs/test_env/bin/plink \
--bfile data/filtered_thinned \
--keep data/keep.fam \
--threads 4 \
--recode A \
--out data/sequoia

