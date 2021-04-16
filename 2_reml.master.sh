#! /bin/bash

# calculate reml (restricted maximum likelihood) analysis
# estimate variance explained by the respective SNPs

group=all.hand.foot.eye

# quantitative phenotypes
for pheno in eye.bottle eye.cat eye.hole foot.cat foot.climb foot.kick foot.pick foot.stamp hand.cat hand.colour hand.cut hand.draw hand.hit hand.hold hand.throw

do

cat > reml.results/${group}/reml.${pheno}.sh <<EOF1
#!/bin/bash

#SBATCH --job-name="gcta.heritability"
#SBATCH --export=ALL
#SBATCH --cpus-per-task=2

~/apps/gcta_1.93.2beta/gcta64 \
--reml \
--grm-bin ~/scratch/reml/reml.GRM/ALSPAC.${group}.child \
--pheno ~/scratch/reml/reml.phenos/${group}/reml.all.${pheno}.resid \
--thread-num 4 \
--out ${pheno}

EOF1

chmod +x reml.results/${group}/reml.${pheno}.sh

cd reml.results/${group}/

sbatch reml.${pheno}.sh

cd ../../

done