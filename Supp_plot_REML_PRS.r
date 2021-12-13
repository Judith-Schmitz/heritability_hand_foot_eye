library(tidyverse)
library(summarytools)
library(ggplot2)
library(ggpubr)
library(xlsx)
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)

color.new <- brewer.pal(n = 10, name = 'RdYlBu')

##########################
#
# PART 1: REML (FIGURE 2)

# read results
resid <- read.xlsx("results/2.reml_new/reml.hand.foot.eye_new.xlsx", sheetName = "figure2a")
resid <- resid %>%
  mutate(se.low = heritability - SE,
         se.up = heritability + SE,
         item = factor(item, levels = c("hand", "draw", "throw", "colour", "hold",  "cut", "hit", "foot", "kick", "pick", "stamp", "climb", "eye", "hole", "bottle")),
         group = factor(group, levels = c("summary", "hand", "foot", "eye")))

left <- read.xlsx("results/2.reml_new/reml.hand.foot.eye_new.xlsx", sheetName = "figure2b")
left <- left %>%
  mutate(se.low = heritability - SE,
         se.up = heritability + SE,
         item = factor(item, levels = c("hand", "draw", "throw", "colour", "hold",  "cut", "hit", "foot", "kick", "pick", "stamp", "climb", "eye", "hole", "bottle")),
         group = factor(group, levels = c("summary", "hand", "foot", "eye")))

mixed <- read.xlsx("results/2.reml_new/reml.hand.foot.eye_new.xlsx", sheetName = "figure2c")
mixed <- mixed %>%
  mutate(se.low = heritability - SE,
         se.up = heritability + SE,
         item = factor(item, levels = c("hand", "draw", "throw", "colour", "hold",  "cut", "hit", "foot", "kick", "pick", "stamp", "climb", "eye", "hole", "bottle")),
         group = factor(group, levels = c("summary", "hand", "foot", "eye")))

Fig2 <- ggplot(data = resid, aes(x = item, fill = group, y = heritability)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = pmax(0, se.low), ymax = se.up), width = 0.2, cex = 0.5) + 
  labs(y = "SNP heritability (+/- SE)",
       x = "item (transformed)",
       fill = "phenotype group") + 
  scale_fill_manual(values = c("#FEE090", "#A50026", "#74ADD1", "#313695")) + 
  ylim(0, 0.5) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 9, color = "dimgrey"),
        axis.text.x = element_text(angle = 90, size = 9, margin = margin(10,0,0,0), color = "dimgrey"),
        axis.title.x = element_text(angle = 0, color = "black", size = 9, margin = margin(0,0,0,0), face = "bold"),
        axis.title.y = element_text(angle = 90, color = "black", size = 9, margin = margin(0,10,0,0), face = "bold"),
        legend.title = element_text(size = 8, face = "bold", margin = margin(0,0,5,0)),
        legend.text = element_text(size = 8, color = "dimgrey"))


tiff("outputs/plots/Fig2_new.tiff", units = "cm", width = 14, height = 6.5, res = 600)
Fig2
dev.off()

FigS8a <- ggplot(data = left, aes(x = item, fill = group, y = heritability)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = pmax(0, se.low), ymax = se.up), width = 0.2, cex = 0.5) + 
  labs(y = "SNP heritability (+/- SE)",
       x = "item (left vs right)",
       fill = "phenotype group") + 
  scale_fill_manual(values = c("#FEE090", "#A50026", "#74ADD1", "#313695")) + 
  ylim(0, 0.5) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 9, color = "dimgrey"),
        axis.text.x = element_text(angle = 90, size = 9, margin = margin(10,0,0,0), color = "dimgrey"),
        axis.title.x = element_text(angle = 0, color = "black", size = 9, margin = margin(0,0,0,0), face = "bold"),
        axis.title.y = element_text(angle = 90, color = "black", size = 9, margin = margin(0,10,0,0), face = "bold"),
        legend.position = c(0.85, 0.65),
        legend.title = element_text(size = 8, face = "bold", margin = margin(0,0,5,0)),
        legend.text = element_text(size = 8, color = "dimgrey"))

FigS8b <- ggplot(data = mixed, aes(x = item, fill = group, y = heritability)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = pmax(0, se.low), ymax = se.up), width = 0.2, cex = 0.5) + 
  labs(y = "SNP heritability (+/- SE)",
       x = "item (mixed vs right)",
       fill = "phenotype group") + 
  scale_fill_manual(values = c("#FEE090", "#A50026", "#74ADD1", "#313695")) + 
  ylim(0, 0.5) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 9, color = "dimgrey"),
        axis.text.x = element_text(angle = 90, size = 9, margin = margin(10,0,0,0), color = "dimgrey"),
        axis.title.x = element_text(angle = 0, color = "black", size = 9, margin = margin(0,0,0,0), face = "bold"),
        axis.title.y = element_text(angle = 90, color = "black", size = 9, margin = margin(0,10,0,0), face = "bold"),
        legend.position = "none")

tiff("outputs/plots/FigS8_new.tiff", units = "cm", width = 12, height = 15, res = 600)
ggarrange(FigS8a, FigS8b,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
dev.off()


##########################
#
# PART 2: PRS (TABLE S16)

traits <- c('HAND','ADHD','ASD','BIP','SCZ','EDU','INT')

thresholds <- c(0.00100005, 0.0500001, 0.1, 0.2, 0.3, 0.4, 0.5, 1)

prs.r2 <- data.frame(matrix(nrow = 9, ncol = 14))
prs.p <- data.frame(matrix(nrow = 9, ncol = 14))

supptable <- data.frame()

colnames(prs.r2) <- c('HAND_hand', 'ADHD_hand','ASD_hand','BIP_hand','SCZ_hand','EDU_hand','INT_hand',
                      'HAND_foot', 'ADHD_foot','ASD_foot','BIP_foot','SCZ_foot','EDU_foot','INT_foot')
colnames(prs.p) <- c('HAND_hand', 'ADHD_hand','ASD_hand','BIP_hand','SCZ_hand','EDU_hand','INT_hand',
                     'HAND_foot', 'ADHD_foot','ASD_foot','BIP_foot','SCZ_foot','EDU_foot','INT_foot')

rownames(prs.r2) <- c('best', '0.001', '0.05', '0.1', '0.2', '0.3', '0.4', '0.5', '1')
rownames(prs.p) <- c('best', '0.001', '0.05', '0.1', '0.2', '0.3', '0.4', '0.5', '1')

for (base in traits) {
  
  prs <- read.table(file = paste0("results/5.prs_new/", base, ".hand.foot.eye.resid.prsice"), header = TRUE)
  prs.hand.thresholds <- prs %>% filter(Pheno == 'hand') %>% filter(Threshold %in% thresholds)
  prs.foot.thresholds <- prs %>% filter(Pheno == 'foot') %>% filter(Threshold %in% thresholds)
  prs.hand.best <- prs %>% filter(Pheno == 'hand') %>% filter(R2 == max(R2))
  prs.foot.best <- prs %>% filter(Pheno == 'foot') %>% filter(R2 == max(R2))
  prs.hand <- rbind(prs.hand.best, prs.hand.thresholds)
  prs.foot <- rbind(prs.foot.best, prs.foot.thresholds)
  prs.hand$Set <- base
  prs.foot$Set <- base
  rm(prs, prs.hand.thresholds, prs.foot.thresholds, prs.hand.best, prs.foot.best)
  supptable <- rbind(supptable, prs.hand, prs.foot)

  prs.r2[,paste0(base, '_hand')] <- prs.hand$R2
  prs.r2[,paste0(base, '_foot')] <- prs.foot$R2
  prs.p[,paste0(base, '_hand')] <- prs.hand$P
  prs.p[,paste0(base, '_foot')] <- prs.foot$P
  
}

rm(prs.hand, prs.foot)

prs.r2 <- round(prs.r2*100, 2)

#write.xlsx(supptable, file = "outputs/Table_S16_new.xlsx", sheetName = "cor", append = TRUE)

