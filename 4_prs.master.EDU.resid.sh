#! /bin/bash

base=EDU
pheno=hand.cat,foot.cat
phenofile=hand.foot.eye.resid
binary=F,F

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
    --beta \
    --stat Beta \
    --binary-target ${binary} \
    --type bgen \
    --target-list ~/scratch/PRS/target.bgen.files.gruffalo \
    --extract ~/projects/shared/shared_resources/data_PRS/valid/${base}.valid \
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

