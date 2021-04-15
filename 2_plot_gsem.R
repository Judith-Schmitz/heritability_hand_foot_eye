# Plot GREML results

library(tidyverse)
library(summarytools)
library(ggplot2)
library(ggpubr)
library(xlsx)
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)

display.brewer.pal(n = 10, name = 'RdYlBu')
color.new <- brewer.pal(n = 10, name = 'RdYlBu')

# univariate (heritability)
her <- read.xlsx("results/2.reml/hand.foot.eye.all.univariate.xlsx", sheetName = "figure")
her <- her %>%
  mutate(se.low = heritability - SE,
         se.up = heritability + SE,
         item = factor(item, levels = c("hand", "draw", "throw", "colour", "hold",  "cut", "hit", "foot", "kick", "pick", "stamp", "climb", "eye", "hole", "bottle")),
         group = factor(group, levels = c("summary", "hand", "foot", "eye")))

tiff("outputs/plots/Fig3_SNP_heritability.tiff", units = "in", width = 8, height = 4, res = 300)
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

# test
her <- read.xlsx("results/2.reml/hand.foot.eye.all.univariate.xlsx", sheetName = "test.figure")
her <- her %>%
  mutate(item = factor(item, levels = c("hand", "draw", "throw", "colour", "hold",  "cut", "hit", "foot", "kick", "pick", "stamp", "climb", "eye", "hole", "bottle")),
         group = factor(group, levels = c("summary", "hand", "foot", "eye")),
         test = factor(test, levels = c("original", "cut", "random1", "random2")),
         n = as.numeric(n))

#tiff("outputs/plots/Test.tiff", units = "in", width = 16, height = 4, res = 300)
jpeg(filename = "outputs/plots/Test.jpg", width = 1600, height = 600)
ggplot(data = her, aes(x = item, fill = test, y = heritability)) +
  geom_bar(position="dodge", stat = "identity") + 
  ylim(0, 0.5) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = 0.3, hjust = -0.1, angle = 90) + 
  labs(y = "SNP heritability",
       x = "item",
       fill = "test") + 
  scale_fill_manual(values = c("darkred", "lightblue", "darkblue", "darkgreen")) + 
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


