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
# PART 1: REML (FIGURE 3)

# read results
her <- read.xlsx("results/2.reml/hand.foot.eye.all.univariate.xlsx", sheetName = "figure")
her <- her %>%
  mutate(se.low = heritability - SE,
         se.up = heritability + SE,
         item = factor(item, levels = c("hand", "draw", "throw", "colour", "hold",  "cut", "hit", "foot", "kick", "pick", "stamp", "climb", "eye", "hole", "bottle")),
         group = factor(group, levels = c("summary", "hand", "foot", "eye")))

tiff("outputs/plots/Fig3_final.tiff", units = "in", width = 8, height = 4, res = 300)
ggplot(data = her, aes(x = item, fill = group, y = heritability)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = pmax(0, se.low), ymax = se.up), width = 0.2, cex = 0.5) + 
  labs(y = "SNP heritability (+/- SE)",
       x = "item",
       fill = "phenotype group") + 
  scale_fill_manual(values = c("#FEE090", "#A50026", "#74ADD1", "#313695")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, color = "dimgrey"),
        axis.text.x = element_text(angle = 45, size=12, margin = margin(10,0,0,0), color = "dimgrey"),
        axis.title.x = element_text(angle = 0, color = "black", size = 12, margin = margin(0,0,0,0), face = "bold"),
        axis.title.y = element_text(angle = 90, color = "black", size = 12, margin = margin(0,10,0,0), face = "bold"),
        legend.title = element_text(size = 12, face = "bold", margin = margin(0,0,5,0)),
        legend.text = element_text(size = 12, color = "dimgrey"))
dev.off()


##########################
#
# PART 2: PRS (TABLE S2, FIGURE S4)

traits <- c('ADHD','ASD','BIP','SCZ','EDU','INT')

thresholds <- c(0.00100005, 0.0500001, 0.1, 0.2, 0.3, 0.4, 0.5, 1)

prs.r2 <- data.frame(matrix(nrow = 9, ncol = 12))
prs.p <- data.frame(matrix(nrow = 9, ncol = 12))

supptable <- data.frame()

colnames(prs.r2) <- c('ADHD_hand','ASD_hand','BIP_hand','SCZ_hand','EDU_hand','INT_hand',
                      'ADHD_foot','ASD_foot','BIP_foot','SCZ_foot','EDU_foot','INT_foot')
colnames(prs.p) <- c('ADHD_hand','ASD_hand','BIP_hand','SCZ_hand','EDU_hand','INT_hand',
                     'ADHD_foot','ASD_foot','BIP_foot','SCZ_foot','EDU_foot','INT_foot')

rownames(prs.r2) <- c('best', '0.001', '0.05', '0.1', '0.2', '0.3', '0.4', '0.5', '1')
rownames(prs.p) <- c('best', '0.001', '0.05', '0.1', '0.2', '0.3', '0.4', '0.5', '1')

for (base in traits) {
  
  prs <- read.table(file = paste0("results/5.prs/laterality/", base, ".hand.foot.eye.prsice"), header = TRUE)
  prs.hand.thresholds <- prs %>% filter(Pheno == 'hand.cat') %>% filter(Threshold %in% thresholds)
  prs.foot.thresholds <- prs %>% filter(Pheno == 'foot.cat') %>% filter(Threshold %in% thresholds)
  prs.hand.best <- prs %>% filter(Pheno == 'hand.cat') %>% filter(R2 == max(R2))
  prs.foot.best <- prs %>% filter(Pheno == 'foot.cat') %>% filter(R2 == max(R2))
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

# FDR correction
prs.p.fdr <- matrix(p.adjust(as.vector(as.matrix(prs.p)), method = 'fdr'), ncol = ncol(prs.p))
supptable$P <- p.adjust(supptable$P, method = 'fdr', n = length(traits)*2*9)

#write.xlsx(supptable, file = "outputs/Table_S2.xlsx", sheetName = "cor", append = TRUE)

colnames(prs.r2) <- c(' ADHD',' ASD',' BIP',' SCZ',' EA',' IQ',
                      ' ADHD',' ASD',' BIP',' SCZ',' EA',' IQ')

tiff("outputs/plots/FigS4_final.tiff", units = "cm", width = 20, height = 18, res = 600)
par(mfrow = c(1,2))
corrplot(as.matrix(prs.r2[1:6]), 
         tl.col = "black",
         method = "color",
         addCoef.col = "white",
         addgrid.col = "dimgrey",
         is.corr = FALSE,
         cl.pos = "n",
         cl.lim = c(0,0.25),
         col = color.new[5:1],
         tl.srt = 45,
         p.mat = as.matrix(prs.p.fdr[,1:6]),
         insig = "pch",
         sig.level = 0.05,
         number.cex = 0.8, 
         tl.cex = 0.8,
         cl.cex = 0.8,
         mar = c(0, 0, 0, 0))
colorlegend(colbar = color.new[5:1], 
            labels = c(0,0.05,0.10,0.15,0.20,0.25), 
            xlim = c(0.5, 6.5), 
            ylim = c(-1, 0), 
            align = "l", 
            cex = 0.8,
            vertical = FALSE)
corrplot(as.matrix(prs.r2[7:12]), 
         tl.col = "black",
         method = "color",
         addCoef.col = "white",
         addgrid.col = "dimgrey",
         is.corr = FALSE,
         cl.pos = "n",
         cl.lim = c(0,0.25),
         col = color.new[6:10],
         tl.srt = 45,
         p.mat = as.matrix(prs.p.fdr[,7:12]),
         insig = "pch",
         sig.level = 0.05,
         number.cex = 0.8, 
         tl.cex = 0.8,
         cl.cex = 0.8,
         mar = c(0, 0, 0, 0))
colorlegend(colbar = color.new[6:10], 
            labels = c(0,0.05,0.10,0.15,0.20,0.25), 
            xlim = c(0.5, 6.5), 
            ylim = c(-1, 0), 
            align = "l", 
            cex = 0.8,
            vertical = FALSE)
mtext(text = expression(bold(paste(italic("p "), "value threshold"))), side = 2, line = 22, at = 5, las = 0, cex = 0.9)
mtext(text = expression(bold("training GWAS")), side = 3, line = 1, at = -0.5, las = 1, cex = 0.9)
mtext(text = expression(bold("hand preference")), side = 3, line = 0, at = -5, las = 1, cex = 0.9)
mtext(text = expression(bold("foot preference")), side = 3, line = 0, at = 3.5, las = 1, cex = 0.9)
dev.off()



