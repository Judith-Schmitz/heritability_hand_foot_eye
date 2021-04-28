#! /bin/bash

# calculate reml (restricted maximum likelihood) analysis
# estimate variance explained by the respective SNPs

# quantitative phenotypes
for pheno in hand.cat foot.cat eye.cat hand.colour hand.cut hand.draw hand.hit hand.hold hand.throw foot.climb foot.kick foot.pick foot.stamp eye.bottle eye.hole

do

cd results.reml/resid/

cat > reml.${pheno}.resid.sh <<EOF1
#!/bin/bash

#SBATCH --job-name="gcta.heritability"
#SBATCH --export=ALL
#SBATCH --cpus-per-task=2

~/apps/gcta_1.93.2beta/gcta64 \
--reml \
--grm-bin ~/scratch/family_hand_foot_eye/grm/ALSPAC.hand.foot.eye.resid \
--pheno ~/scratch/family_hand_foot_eye/phenos/reml.${pheno}.resid \
--thread-num 4 \
--out ${pheno}.resid

EOF1

chmod +x reml.${pheno}.resid.sh

sbatch reml.${pheno}.resid.sh

cd ../../

done