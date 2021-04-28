source(file = 'final_scripts/2_data_preparation_genomics_functions.r')


##########################
#
# PART 1: READ ALSPAC

# Read SPSS file
raw.alspac <- read.alspac()

# Apply exclusion criteria (sensory or physical disabilities)
alspac.clean <- clean.alspac(mydata = raw.alspac)

# Normalize data
master <- normalize.alspac(dataset = alspac.clean)


##########################
#
# PART 2: CREATE KEEP FILE FOR PCA AND GRMs

# read unrelated children and fam file
unrelated.children <- read.table("data/unrelated_children.txt")
fam <- read.table("data/data.fam")

# for PCA
keep.all <- master %>%
  filter(ID_1 %in% unrelated.children$V1) %>%
  filter(ID_1 %in% fam$V1) %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(age.weeks.42)) %>%
  dplyr::select(ID_1) %>%
  mutate(ID_2 = ID_1)
#write.table(keep.all, "outputs/keep_files/keep.hand.foot.eye.resid", sep = "\t", quote = F, row.names = F, col.names = F)


##########################
#
# PART 3: CREATE PHENOTYPE FILE FOR GSEM

# Read ID file from GRM
ids.all <- read.table('data/grm.id/ALSPAC.hand.foot.eye.resid.grm.id', h=F)
colnames(ids.all) <- c("ID_1", "ID_2")
ids.all$ID_1 <- as.character(ids.all$ID_1)

# read Principal components
pcs.all <- read.table('data/eigenvec/hand.foot.eye.resid.eigenvec', h=F)
pcs.all <- pcs.all %>%
  dplyr::select(c(V1, V3, V4))
colnames(pcs.all) <- c("ID_1", "pc1", "pc2")
pcs.all$ID_1 <- as.character(pcs.all$ID_1)

mydata <- master %>%
  filter(ID_1 %in% ids.all$ID_1) %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(age.weeks.42))
mydata <- left_join(mydata, pcs.all, by = "ID_1")

rm(raw.alspac, alspac.clean)

### residualising

phenos <- c("hand.cat", "foot.cat", "eye.cat", 
            "hand.draw", "hand.throw", "hand.colour", "hand.hold", "hand.cut", "hand.hit", 
            "foot.kick", "foot.pick", "foot.stamp", "foot.climb",
            "eye.bottle", "eye.hole" )

mydata.resid <- mydata %>%
  dplyr::select(ID_1)

# correct items for age, sex and 2 pcs
for (i in 1:length(phenos)) {
  
  pheno <- phenos[i]
  
  data <- mydata[,c("ID_1", "sex", "age.weeks.42", "pc1", "pc2", pheno)]

  reg <- glm(get(pheno) ~ sex + age.weeks.42 + pc1 + pc2, data = data)
  
  data[which(!is.na(data[,6])), 6] <- rankNorm(residuals(reg))
  
  pheno.bind <- as.data.frame(data[,6])
  
  colnames(pheno.bind) <- pheno
  
  mydata.resid <- cbind(mydata.resid, pheno.bind)
  
}

rm(reg, pheno.bind, i, pheno)

# merge with ids
mydata.resid <- left_join(ids.all, mydata.resid, by = "ID_1") 

myphenos.gsem <- mydata.resid %>%
  dplyr::select(hand.cat, foot.cat, eye.cat)
#write.table(myphenos.gsem, 'outputs/pheno_files_gsem/hand.foot.eye.resid', quote = F, row.names = F, col.names = F)

# univariate reml analyses
for (pheno in phenos) {
  
  myphenos.resid.reml <- mydata.resid %>%
    dplyr::select(ID_1, ID_2, all_of(pheno))
  
  output.resid.reml <- paste0("outputs/pheno_files_reml/reml.", pheno, ".resid")
  
  #write.table(myphenos.resid.reml, output.resid.reml, quote = F, row.names = F, col.names = F)
  
}

# univariate reml analyses: mixed
for (pheno in phenos) {
  
  myphenos.mixed.reml <- mydata %>%
    mutate(ID_2 = ID_1) %>%
    dplyr::select(ID_1, ID_2, all_of(pheno)) 
  
  myphenos.mixed.reml[which(myphenos.mixed.reml[,3] == 2), 3] <- NA
  
  #print(freq(myphenos.mixed.reml[,3]))
  
  output.mixed.reml <- paste0("outputs/pheno_files_reml/reml.", pheno, ".mixed")
  
  #write.table(myphenos.mixed.reml, output.mixed.reml, quote = F, row.names = F, col.names = F)
  
}

# univariate reml analyses: left
for (pheno in phenos) {
  
  myphenos.left.reml <- mydata %>%
    mutate(ID_2 = ID_1) %>%
    dplyr::select(ID_1, ID_2, all_of(pheno)) 
  
  myphenos.left.reml[which(myphenos.left.reml[,3] == 1), 3] <- NA
  myphenos.left.reml[which(myphenos.left.reml[,3] == 2), 3] <- 1
  
  #print(freq(myphenos.left.reml[,3]))
  
  output.left.reml <- paste0("outputs/pheno_files_reml/reml.", pheno, ".left")
  #write.table(myphenos.left.reml, output.left.reml, quote = F, row.names = F, col.names = F)
  
}

# covariate files for left and mixed
mycovar.reml <- mydata %>%
  mutate(ID_2 = ID_1) %>%
  dplyr::select(ID_1, ID_2, sex) 

#write.table(mycovar.reml, "outputs/pheno_files_reml/reml.covar", quote = F, row.names = F, col.names = F)

myqcovar.reml <- mydata %>%
  mutate(ID_2 = ID_1) %>%
  dplyr::select(ID_1, ID_2, age.weeks.42, pc1, pc2) 

#write.table(myqcovar.reml, "outputs/pheno_files_reml/reml.qcovar", quote = F, row.names = F, col.names = F)


##########################
#
# PART 4: CREATE PHENOTYPE FILE FOR PRS

# Read ID file
sample <- read.table('data/data.sample', h=T)
sample <- sample %>%
  filter(ID_1 != 0) %>%
  dplyr::select(ID_1, ID_2, sex, plink_pheno)
sample$ID_1 <- as.character(sample$ID_1)

mydata.prs <- left_join(sample, mydata.resid, by = 'ID_1') %>%
  filter(!is.na(hand.cat) | !is.na(foot.cat) | !is.na(eye.cat)) %>%
  dplyr::select(ID_1, hand.cat, foot.cat, eye.cat)

sample.hand.foot.eye <- left_join(sample[,1:2], mydata.prs, by = 'ID_1') 

colnames(sample.hand.foot.eye) <- c("FID", "IID", "hand.cat", "foot.cat", "eye.cat")

#write.table(sample.hand.foot.eye, 'outputs/pheno_files_prs/sample.hand.foot.eye', quote = F, row.names = F, col.names = T)



##########################
#
# PART 5: BEHAVIOURAL ANALYSIS

color.new <- brewer.pal(n = 10, name = 'RdYlBu')

# check correlations between items
cor.data <- mydata.resid %>%
  dplyr::select(hand.cat, hand.draw, hand.throw, hand.colour, hand.hold, hand.cut, hand.hit, 
                foot.cat, foot.kick, foot.pick, foot.stamp, foot.climb, 
                eye.cat, eye.hole, eye.bottle)

colnames(cor.data) <- c("hand summary", "draw", "throw", "colour", "hold", "cut", "hit", 
                        "foot summary", "kick", "pick", "stamp", "climb", 
                        "eye summary", "hole", "bottle")

# correlation matrix
cor <- rcorr(as.matrix(cor.data), type = "pearson")$r
p.cor <- rcorr(as.matrix(cor.data), type = "pearson")$P

tiff("outputs/plots/FigS1_final.tiff", units="in", width=10, height=10, res=300)
corrplot(cor, 
         type = "lower", 
         tl.col = "black",
         method = "color",
         col = color.new,
         addCoef.col = "white",
         addgrid.col = "dimgrey",
         diag = FALSE,
         p.mat = p.cor, 
         sig.level = .05/(nrow(cor)*ncol(cor)),
         insig = "blank",
         pch.col = "black",
         pch.cex = 2,
         tl.srt = 45)
dev.off()
