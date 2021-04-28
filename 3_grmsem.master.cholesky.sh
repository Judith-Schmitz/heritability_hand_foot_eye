#! /bin/bash

pheno=hand.foot.eye.resid

cat > results.grmsem/grmsem.${pheno}.cholesky.sh <<EOF1
#!/bin/bash

#SBATCH --job-name="grmsem"
#SBATCH --partition=long
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G

Rscript grmsem.${pheno}.cholesky.R

EOF1

chmod +x results.grmsem/grmsem.${pheno}.cholesky.sh

cat > results.grmsem/grmsem.${pheno}.cholesky.R <<EOF2
# run gsem analysis
library(grmsem)

pheno <- "${pheno}"
print(paste("phenotype:", pheno, sep=" "))

# read GRM
print("read GRM")
mygrm <- "../grm/ALSPAC.${pheno}.grm.bin"
G <- grm.bin.input(mygrm)

# read phenotypes
print("read phenotypes")
input <- "../phenos/grmsem.${pheno}"
myph <- read.table(input, header=F)

# Run a Cholesky model:
print("running Cholesky model")
out <- grmsem.fit(myph, G, LogL = TRUE, estSE = TRUE, cores = 4, compl.ph = FALSE, model = "Cholesky")
print(out)

save(out, file = "out.cholesky.RData")

# This function estimates standardised parameters for a gsem.fit object.
print("estimating standardised parameters")
stout <- grmsem.stpar(out)
print(stout)

# This function estimates genetic and residual variances, and genetic correlations.
print("estimating genetic variance & genetic correlations")
var.out <- grmsem.var(out)
print(var.out)

# This function estimates the bivariate heritability, shown in the off-diagonals.
print("estimating bivariate heritability")
biv <- grmsem.biher(myph, var.out)
print(biv)
EOF2

cd results.grmsem/

sbatch grmsem.${pheno}.cholesky.sh

cd ../