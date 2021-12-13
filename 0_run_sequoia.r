# load the package
library(sequoia)  
library(data.table)
library(dplyr)

# load data
Geno <- fread("data/sequoia.raw", header = TRUE) %>%
  filter(FID != "V1")
Geno[is.na(Geno)] <- "-9"

Geno <- GenoConvert(InData = Geno,
                    InFormat = 'raw')

# LifeHistoryData.txt contains IDs, Sex, and Birth Year for each individual
LH <- read.table(file = "data/LifeHistoryData.txt", header = TRUE)
 
# run pedigree reconstruction
ParOUT <- sequoia(GenoM = Geno, 
                  LifeHistData = LH,
                  Module = 'par', # parentage assignment
                  Err = 0.005, # genotyping error rate
                  quiet = "verbose")
# the result is a list with the pedigree, run parameters, and various other elements.   

names(ParOUT)

# Save results
writeSeq(SeqList = ParOUT, GenoM = Geno, folder = "results_sequoia/")         
