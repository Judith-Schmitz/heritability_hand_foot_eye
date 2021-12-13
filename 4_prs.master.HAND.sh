#! /bin/bash

base=HAND
pheno=hand,foot
phenofile=hand.foot.eye   # hand.foot.eye.resid      #hand.foot.eye
binary=T,T                # F,F                      #T,T

cd results.prs

cat > prs.${base}.${phenofile}.sh <<EOF1
#!/bin/bash

#SBATCH --job-name="PRS"
#SBATCH --export=ALL
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G

Rscript ~/apps/PRSice/PRSice.R --dir . \
    --prsice ~/apps/PRSice/PRSice_linux \
    --base ~/projects/shared/shared_resources/data_PRS/base/${base}.chrpos.gz \
    --snp ID \
    --A1 Allele1 \
    --A2 Allele2 \
    --pvalue P.value \
    --beta \
    --stat Zscore \
    --binary-target ${binary} \
    --type bgen \
    --extract ~/projects/shared/shared_resources/data_PRS/valid/${base}.valid \
    --target-list ~/scratch/PRS/target.bgen.files.gruffalo \
    --geno 0.1 \
    --info 0.9 \
    --maf 0.05 \
    --pheno ~/scratch/family_hand_foot_eye/phenos/sample.${phenofile} \
    --pheno-col ${pheno} \
    --out ~/scratch/family_hand_foot_eye/results.prs/${base}.${phenofile} \
    --allow-inter \
    --thread 8     
    
EOF1

chmod +x prs.${base}.${phenofile}.sh

sbatch prs.${base}.${phenofile}.sh

cd ../


