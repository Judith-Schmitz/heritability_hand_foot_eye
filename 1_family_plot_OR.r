# Plot OR results

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

# read data
or.parents <- read.xlsx("results/1.or/Family_hand_foot_eye_alspac.xlsx", sheetName = "figure1")
or.parents <- or.parents %>%
  mutate(group = factor(group, levels = c("hand", "foot", "eye")),
         condition = factor(condition, levels = c("one mixed-sided parent", 
                                                  "one left-sided parent", 
                                                  "two mixed-sided parents", 
                                                  "one mixed-, one left-sided parent", 
                                                  "two left-sided parents")))

tiff("outputs/plots/Fig1_OR_combined.tiff", units = "in", width = 12, height = 4, res = 300)
ggplot(data = or.parents,
       aes(x = OR, 
           y = condition)) + 
  geom_point(aes(col = group), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = pmin(upper, 10), col = group), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 1, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(group),
             scales = 'free_y') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  scale_x_continuous(breaks = seq(0,10,2)) + 
  labs(x = "OR (left-sidedness)") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank())  
dev.off()



# read data
or <- read.xlsx("results/1.or/Family_hand_foot_eye_alspac.xlsx", sheetName = "figure2")
or <- or %>%
  mutate(group = factor(group, levels = c("hand", "foot", "eye")),
         analysis = factor(analysis, levels = c("maternal/paternal", "same/opposite sex")),
         condition = factor(condition, levels = c("father mixed-sided", "mother mixed-sided", "father left-sided", "mother left-sided", 
                                                  "opposite-sex parent mixed-sided", "same-sex parent mixed-sided", "opposite-sex parent left-sided", "same-sex parent left-sided")))
                                                  
tiff("outputs/plots/Fig2_OR_individual.tiff", units = "in", width = 12, height = 6, res = 300)
ggplot(data = or,
       aes(x = OR, 
           y = condition)) + 
  geom_point(aes(col = group), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = pmin(upper, 10), col = group), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 1, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(group),
             rows = vars(analysis),
             scales = 'free_y') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  scale_x_continuous(breaks = seq(0,4,1)) + 
  labs(x = "OR (left-sidedness)") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_text(size = 16, face = "bold"))  
dev.off()



# read data
or.child.sex <- read.xlsx("results/1.or/Family_hand_foot_eye_alspac.xlsx", sheetName = "figure3")
or.child.sex <- or.child.sex %>%
  mutate(group = factor(group, levels = c("hand", "foot", "eye")),
         child.sex = factor(child.sex, levels = c("females", "males")),
         condition = factor(condition, levels = c("father mixed-sided", "mother mixed-sided", "father left-sided", "mother left-sided")))

tiff("outputs/plots/Fig3_OR_child_sex.tiff", units = "in", width = 12, height = 6, res = 300)
ggplot(data = or.child.sex,
       aes(x = OR, 
           y = condition)) + 
  geom_point(aes(col = group), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = pmin(upper, 10), col = group), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 1, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(group),
             rows = vars(child.sex),
             scales = 'free_y') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  scale_x_continuous(breaks = seq(0,6,1)) + 
  labs(x = "OR (left-sidedness)") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_text(size = 16, face = "bold"))  
dev.off()



# read data
or.child.sex <- read.xlsx("results/1.or/Family_hand_foot_eye_alspac.xlsx", sheetName = "new.figure")
or.child.sex <- or.child.sex %>%
  mutate(group = factor(group, levels = c("hand", "foot", "eye")),
         child.sex = factor(child.sex, levels = c("all", "females", "males")),
         condition = factor(condition, levels = c("father mixed-sided", "mother mixed-sided", "father left-sided", "mother left-sided")))

tiff("outputs/plots/Fig2_OR_child_sex.tiff", units = "in", width = 12, height = 9, res = 300)
ggplot(data = or.child.sex,
       aes(x = OR, 
           y = condition)) + 
  geom_point(aes(col = group), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = pmin(upper, 10), col = group), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 1, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(group),
             rows = vars(child.sex),
             scales = 'free_y') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  scale_x_continuous(breaks = seq(0,6,1)) + 
  labs(x = "OR (left-sidedness)") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_text(size = 16, face = "bold"))  
dev.off()

