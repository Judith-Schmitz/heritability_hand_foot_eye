#! /bin/bash

for pheno in hand.foot.eye.cat.resid

do 

cat > gsem.${pheno}.sh <<EOF1
#! /bin/bash
#$ -V
#$ -cwd
#$ -pe multi 4

Rscript scripts/gsem.${pheno}.R

EOF1

chmod +x gsem.${pheno}.sh

cat > scripts/gsem.${pheno}.R <<EOF2
# run gsem analysis
library(gsem)

pheno <- "${pheno}"
print(paste("phenotype:", pheno, sep=" "))

# read GRM
print("read GRM")
mygrm <- read.table("data/grm/ALSPAC.all.hand.foot.eye.child.grm", header = FALSE)
G <- grm.input(mygrm)

# read phenotypes
print("read phenotypes")
input <- "data/phenos/gsem.${pheno}"
myph <- read.table(input, header=F)

# Run a Cholesky model:
print("running Cholesky model")
out <- gsem.fit(myph, G, LogL = TRUE, estSE = TRUE, cores = 16, compl.ph = FALSE)
print(out)

# This function estimates standardised parameters for a gsem.fit object.
print("estimating standardised parameters")
stout <- gsem.stpar(out)
print(stout)

# This function estimates genetic and residual variances, and genetic correlations.
print("estimating genetic variance & genetic correlations")
var.out <- gsem.var(out)
print(var.out)

# This function estimates the bivariate heritability, shown in the off-diagonals.
print("estimating bivariate heritability")
biv <- gsem.biher(myph, var.out)
print(biv)
EOF2

qsub gsem.${pheno}.sh

done

