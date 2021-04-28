#! /bin/bash

# calculate reml (restricted maximum likelihood) analysis
# estimate variance explained by the respective SNPs

# quantitative phenotypes
for pheno in hand.cat foot.cat eye.cat hand.colour hand.cut hand.draw hand.hit hand.hold hand.throw foot.climb foot.kick foot.pick foot.stamp eye.bottle eye.hole

do

cd results.reml/mixed_vs_right/

cat > reml.${pheno}.mixed.sh <<EOF1
#!/bin/bash

#SBATCH --job-name="gcta.heritability"
#SBATCH --export=ALL
#SBATCH --cpus-per-task=2

~/apps/gcta_1.93.2beta/gcta64 \
--reml \
--grm-bin ~/scratch/family_hand_foot_eye/grm/ALSPAC.hand.foot.eye.resid \
--pheno ~/scratch/family_hand_foot_eye/phenos/reml.${pheno}.mixed \
--covar ~/scratch/family_hand_foot_eye/phenos/reml.covar \
--qcovar ~/scratch/family_hand_foot_eye/phenos/reml.qcovar \
--thread-num 4 \
--out ${pheno}.mixed

EOF1

chmod +x reml.${pheno}.mixed.sh

sbatch reml.${pheno}.mixed.sh

cd ../../

done