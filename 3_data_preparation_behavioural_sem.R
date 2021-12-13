library(foreign)
library(tidyverse)
library(summarytools)
library(dplyr)
library(xlsx)
library(RNOmni) # for rank normalization
library(Hmisc) # for rcorr
library(corrplot) # for corrplot
library(RColorBrewer)

##########################
#
# PART 1: PREPARE OPENMX INPUT FILE

# read data
raw.hk <- read.spss(file = "data/Mydata.sav",
                    use.value.labels = FALSE, # there are levels with undefined labels in the SPSS file
                    to.data.frame = TRUE,
                    use.missings = TRUE)

# the SPSS file does not contain UIDs (which are needed for running the PCA)
# extract UIDs and Subject IDs from the Excel file
id.key <- read.xlsx(file = "data/FootPreference_20191209.xlsx", sheetName = "FootPreference") %>%
  select(UID, SubjectID)

id.key$SubjectID <- trimws(as.character(id.key$SubjectID))
raw.hk$SubjectID <- trimws(as.character(raw.hk$SubjectID))

# join UIDs and phenotypes
# recode phenotypes to Left-Mixed-Right(2-1-0) as in ALSPAC
pheno <- left_join(id.key, raw.hk, by = 'SubjectID') %>% mutate(
  footLMR = ifelse(EHI11 < 0, yes = 2, no = NA),
  footLMR = ifelse(EHI11 == 0, yes = 1, no = footLMR),
  footLMR = ifelse(EHI11 > 0, yes = 0, no = footLMR),
  eyeLMR = ifelse(EHI12 < 0, yes = 2, no = NA),
  eyeLMR = ifelse(EHI12 == 0, yes = 1, no = eyeLMR),
  eyeLMR = ifelse(EHI12 > 0, yes = 0, no = eyeLMR),
  EHI1_new = ifelse(EHI1 < 0, yes = 1, no = NA),
  EHI1_new = ifelse(EHI1 == 0, yes = 2, no = EHI1_new),
  EHI1_new = ifelse(EHI1 > 0, yes = 3, no = EHI1_new),
  EHI2_new = ifelse(EHI2 < 0, yes = 1, no = NA),
  EHI2_new = ifelse(EHI2 == 0, yes = 2, no = EHI2_new),
  EHI2_new = ifelse(EHI2 > 0, yes = 3, no = EHI2_new),
  EHI4_new = ifelse(EHI4 < 0, yes = 1, no = NA),
  EHI4_new = ifelse(EHI4 == 0, yes = 2, no = EHI4_new),
  EHI4_new = ifelse(EHI4 > 0, yes = 3, no = EHI4_new),
  EHI5_new = ifelse(EHI5 < 0, yes = 1, no = NA),
  EHI5_new = ifelse(EHI5 == 0, yes = 2, no = EHI5_new),
  EHI5_new = ifelse(EHI5 > 0, yes = 3, no = EHI5_new),
  EHI6_new = ifelse(EHI6 < 0, yes = 1, no = NA),
  EHI6_new = ifelse(EHI6 == 0, yes = 2, no = EHI6_new),
  EHI6_new = ifelse(EHI6 > 0, yes = 3, no = EHI6_new),
  EHI7_new = ifelse(EHI7 < 0, yes = 1, no = NA),
  EHI7_new = ifelse(EHI7 == 0, yes = 2, no = EHI7_new),
  EHI7_new = ifelse(EHI7 > 0, yes = 3, no = EHI7_new),
) %>% mutate (
  EHI_mean_6 = (EHI1_new + EHI2_new + EHI4_new + EHI5_new + EHI6_new + EHI7_new) / 6
) %>% mutate (
  EHI_LMR_6 = ifelse(EHI_mean_6 <= 1.5, yes = 2, no = NA),
  EHI_LMR_6 = ifelse(EHI_mean_6 <= 2.6 & EHI_mean_6 >= 1.5, yes = 1, no = EHI_LMR_6),
  EHI_LMR_6 = ifelse(EHI_mean_6 > 2.6, yes = 0, no = EHI_LMR_6)
)

# keep only twins
twindata <- pheno %>%
  filter(between(ZygGroup, 1, 5)) %>%
  filter(SubjectID != '0555278LMH01') %>% # 3x same FID - triplets? excluded 1 female, kept 2 male DZ
  select(SubjectID, UID, FID, Gender, ZygGroup, ZygTwin, Ageyr, EHI_LMR_6, footLMR, eyeLMR, EHI_mean_6) %>%
  arrange(desc(Gender)) %>%
  filter(!is.na(EHI_LMR_6)) %>%
  arrange(FID)

round(mean(twindata$Ageyr), 2)
round(sd(twindata$Ageyr), 2)

# create the 'keep file' for pca
keep.hk.twins <- twindata %>%
  select(FID, UID)
#write.table(keep.hk.twins, file = "outputs/keep_files/keep.hongkong.twins", col.names = F, row.names = F, quote = F)


# residualise phenotypes for sex, age and 2 pcs
# read Principal components 
pcs <- read.table('data/hongkong.twins.eigenvec', h=F)
pcs <- pcs %>%
  select(c(V2, V3, V4))
colnames(pcs) <- c("UID", "pc1", "pc2")

twindata <- left_join(twindata, pcs, by = 'UID') %>%
  filter(!is.na(pc1))

phenos <- c("EHI_LMR_6", "footLMR", "eyeLMR" )

twindata.resid <- twindata %>%
  dplyr::select(SubjectID, UID, FID, Gender, ZygGroup, ZygTwin, Ageyr)

for (i in 1:length(phenos)) {
  
  pheno <- phenos[i]
  
  data <- twindata[,c("SubjectID", "Gender", "Ageyr", "pc1", "pc2", pheno)]
  
  reg <- glm(get(pheno) ~ Gender + Ageyr + pc1 + pc2, data = data)
  
  data[which(!is.na(data[,6])), 6] <- RankNorm(residuals(reg))
  
  pheno.bind <- as.data.frame(data[,6])
  
  colnames(pheno.bind) <- pheno
  
  twindata.resid <- cbind(twindata.resid, pheno.bind)
  
}

# create separate datafiles for twin1 and twin2
twin1_indexes <- seq(1, nrow(twindata.resid), 2)
twin2_indexes <- seq(2, nrow(twindata.resid), 2)

data.twin1 <- twindata.resid[c(twin1_indexes),]
data.twin2 <- twindata.resid[c(twin2_indexes),]

# check all is in the correct order
all.equal(data.twin1$FID, data.twin2$FID)
all.equal(data.twin1$Ageyr, data.twin2$Ageyr)
all.equal(data.twin1$ZygGroup, data.twin2$ZygGroup)

add.twin1 <- data.twin1 %>% 
  select(FID, ZygGroup, ZygTwin, Ageyr, Gender, EHI_LMR_6, footLMR, eyeLMR) %>%
  rename("Gender1" = "Gender",
         "ehi_6_resid1" = "EHI_LMR_6",
         "foot1" = "footLMR",
         "eye1" = "eyeLMR")
         
add.twin2 <- data.twin2 %>% 
  select(Gender, EHI_LMR_6, footLMR, eyeLMR) %>%
  rename("Gender2" = "Gender",
         "ehi_6_resid2" = "EHI_LMR_6",
         "foot2" = "footLMR",
         "eye2" = "eyeLMR")

mx.twindata <- cbind(add.twin1, add.twin2)

#write.table(mx.twindata, file = "data/SEM.hongkong.twindata.resid", quote = FALSE, sep = '\t', row.names = FALSE)



##########################
#
# PART 2: HISTOGRAM, FIG S4

tiff("outputs/plots/FigS4_new.tiff", units = "in", width = 9, height = 4, res = 300)
ggplot(data = twindata,
       aes(x = EHI_mean_6)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "gray") +
  xlab("mean handedness score (Hong Kong)") + 
  ylab("count") +
  theme_bw() + 
  theme(plot.title = element_text(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) + 
  # Lines
  geom_vline(xintercept = 1.475, colour = "red", linetype = "longdash") + 
  geom_vline(xintercept = 2.625, colour = "red", linetype = "longdash") + 
  #geom_vline(xintercept = 1.975, colour = "blue", linetype = "longdash") + 
  #geom_vline(xintercept = 2.725, colour = "blue", linetype = "longdash") + 
  # Rectangles
  geom_rect(aes(xmin = 1, xmax = 1.475, ymin = 200, ymax = 250), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 1.475, xmax = 2.625, ymin = 200, ymax = 250), fill = "white", color = "red", size = 1) + 
  geom_rect(aes(xmin = 2.625, xmax = 3, ymin = 200, ymax = 250), fill = "white", color = "red", size = 1) + 
  # Text
  annotate("text", x = 1.2375, y = 225, label = "n left = 20 \n (5.6%)", colour = "red") + 
  annotate("text", x = 2.05, y = 225, label = "n mixed = 37 \n (10.3%)", colour = "red") + 
  annotate("text", x = 2.8125, y = 225, label = "n right = 301 \n (84.1%)", colour = "red")
dev.off()



##########################
#
# PART 3: CORRELATION PLOT, FIG S7

color.new <- brewer.pal(n = 10, name = 'RdYlBu')

# check correlations between items
cor.data <- twindata.resid %>%
  dplyr::select(EHI_LMR_6, footLMR, eyeLMR)

colnames(cor.data) <- c("hand", "foot", "eye")

# correlation matrix
cor <- rcorr(as.matrix(cor.data), type = "pearson")$r
p.cor <- rcorr(as.matrix(cor.data), type = "pearson")$P

tiff("outputs/plots/FigS7_new.tiff", units = "in", width = 7, height = 7, res = 300)
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
         tl.srt = 45,
         cl.pos = "n")
dev.off()

