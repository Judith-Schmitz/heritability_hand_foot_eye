#! /bin/bash

# calculate reml (restricted maximum likelihood) analysis
# estimate variance explained by the respective SNPs

# quantitative phenotypes
for pheno in hand foot eye hand.colour hand.cut hand.draw hand.hit hand.hold hand.throw foot.climb foot.kick foot.pick foot.stamp eye.bottle eye.hole

do

cd results.reml/left_vs_right/

cat > reml.${pheno}.left.sh <<EOF1
#!/bin/bash

#SBATCH --job-name="gcta.heritability"
#SBATCH --export=ALL
#SBATCH --cpus-per-task=2

~/apps/conda/envs/geno_utils/bin/gcta64 \
--reml \
--grm-bin ~/scratch/family_hand_foot_eye/grm/ALSPAC.hand.foot.eye.resid \
--pheno ~/scratch/family_hand_foot_eye/phenos/reml.${pheno}.left \
--covar ~/scratch/family_hand_foot_eye/phenos/reml.covar \
--qcovar ~/scratch/family_hand_foot_eye/phenos/reml.qcovar \
--thread-num 4 \
--out ${pheno}.left

EOF1

chmod +x reml.${pheno}.left.sh

sbatch reml.${pheno}.left.sh

cd ../../

done