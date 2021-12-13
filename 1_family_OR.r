### hand, foot, eye preference in children and parents

source(file = 'final_scripts_R1/1_family_OR_functions.r')


##########################
#
# PART 1: READ, CLEAN, NORMALIZE ALSPAC

# Read SPSS file
raw.alspac <- read.alspac()

# Normalize data
alspac.normal <- normalize.alspac(dataset = raw.alspac) # n = 9964 with information on either hand,foot,eye

# exclusion
#freq(alspac.normal$id.twins)
#freq(alspac.normal$physical.disability)
#freq(alspac.normal$sensory.impairment)

# Apply exclusion criteria
master <- clean.alspac(mydata = alspac.normal) # n = 9755 after exclusion criteria

rm(raw.alspac, alspac.normal)


##########################
#
# PART 2: DATASETS

# create separate datasets for hand, foot, and eye
data.hand <- subset(master, !is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) # n trios = 5028
data.foot <- subset(master, !is.na(foot) & !is.na(m.foot) & !is.na(p.foot)) # n trios = 4960
data.eye <- subset(master, !is.na(eye) & !is.na(m.eye) & !is.na(p.eye)) # n trios = 4762

# read in ids for confirmed paternity
biol.fathers <- read.table("data/child.ids.sequoia.txt")

# read in ids for genotypes
geno <- read.table("data/joined.sample", header = TRUE)

# filter for confirmed paternity and genotypes for both parents
keep.hand <- data.hand %>% 
  filter(ID_2 %in% biol.fathers$V1) %>%
  filter(ID_2 %in% geno$ID_1) %>% 
  filter(!is.na(age.weeks)) %>%
  filter(!is.na(m.age)) %>%
  filter(!is.na(p.age)) %>%
  filter(paste0(ID_1, "M") %in% geno$ID_2) %>% 
  filter(paste0(ID_1, "F") %in% geno$ID_2) %>%        # n trios = 999
  select(ID_1, ID_2) %>%     
  mutate(ID_3 = paste0(ID_1, "M")) %>%    
  mutate(ID_4 = paste0(ID_1, "F")) 
keep.hand.out <- cbind(c(keep.hand$ID_1, keep.hand$ID_1, keep.hand$ID_1), 
                       c(keep.hand$ID_2, keep.hand$ID_3, keep.hand$ID_4))

keep.foot <- data.foot %>% 
  filter(ID_2 %in% biol.fathers$V1) %>%
  filter(ID_2 %in% geno$ID_1) %>% 
  filter(!is.na(age.weeks)) %>%
  filter(!is.na(m.age)) %>%
  filter(!is.na(p.age)) %>%
  filter(paste0(ID_1, "M") %in% geno$ID_2) %>% 
  filter(paste0(ID_1, "F") %in% geno$ID_2) %>%        # n trios = 989
  select(ID_1, ID_2) %>%     
  mutate(ID_3 = paste0(ID_1, "M")) %>%    
  mutate(ID_4 = paste0(ID_1, "F")) 
keep.foot.out <- cbind(c(keep.foot$ID_1, keep.foot$ID_1, keep.foot$ID_1), 
                       c(keep.foot$ID_2, keep.foot$ID_3, keep.foot$ID_4))

keep.eye <- data.eye %>% 
  filter(ID_2 %in% biol.fathers$V1) %>%
  filter(ID_2 %in% geno$ID_1) %>% 
  filter(!is.na(age.weeks)) %>%
  filter(!is.na(m.age)) %>%
  filter(!is.na(p.age)) %>%
  filter(paste0(ID_1, "M") %in% geno$ID_2) %>% 
  filter(paste0(ID_1, "F") %in% geno$ID_2) %>%        # n trios = 954
  select(ID_1, ID_2) %>%     
  mutate(ID_3 = paste0(ID_1, "M")) %>%    
  mutate(ID_4 = paste0(ID_1, "F")) 
keep.eye.out <- cbind(c(keep.eye$ID_1, keep.eye$ID_1, keep.eye$ID_1), 
                      c(keep.eye$ID_2, keep.eye$ID_3, keep.eye$ID_4))

#write.table(keep.hand.out, file = "outputs/keep_files/keep.hand.family", row.names = FALSE, col.names = FALSE, quote = FALSE)
#write.table(keep.foot.out, file = "outputs/keep_files/keep.foot.family", row.names = FALSE, col.names = FALSE, quote = FALSE)
#write.table(keep.eye.out, file = "outputs/keep_files/keep.eye.family", row.names = FALSE, col.names = FALSE, quote = FALSE)

keep.all <- rbind(keep.hand, keep.foot, keep.eye)
keep.all <- keep.all[!duplicated(keep.all$ID_2),]
keep.all.out <- cbind(c(keep.all$ID_1, keep.all$ID_1, keep.all$ID_1), 
                      c(keep.all$ID_2, keep.all$ID_3, keep.all$ID_4))

#write.table(keep.all.out, file = "outputs/keep_files/keep.hand.foot.eye.family", row.names = FALSE, col.names = FALSE, quote = FALSE)

rm(keep.all.out, keep.hand.out, keep.foot.out, keep.eye.out)

data.hand.bio <- subset(master, !is.na(hand) & !is.na(m.hand) & !is.na(p.hand) & ID_2 %in% biol.fathers$V1) # n trios = 1161
data.foot.bio <- subset(master, !is.na(foot) & !is.na(m.foot) & !is.na(p.foot)  & ID_2 %in% biol.fathers$V1) # n trios = 1150
data.eye.bio <- subset(master, !is.na(eye) & !is.na(m.eye) & !is.na(p.eye)  & ID_2 %in% biol.fathers$V1) # n trios = 1105

rm(biol.fathers)



##########################
#
# PART 3: LOGISTIC REGRESSION: PARENTAL EFFECTS

options(digits=2)

phenos <- c("hand", "foot", "eye")

# Fig 1A (all families - R-M-L)

# empty dataframe to fill
results.parents.lmr <- data.frame(matrix(ncol = 9, nrow = 18))
colnames(results.parents.lmr) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.parents.lmr[,1] <- c(rep("hand", 6), rep("foot", 6), rep("eye", 6))
results.parents.lmr[,2] <- rep(c("intercept", 
                        "one mixed-sided parent", 
                        "one left-sided parent", 
                        "two mixed-sided parents", 
                        "one mixed-, one left-sided parent", 
                        "two left-sided parents"), 3)

# run logistic regression analyses and fill dataframe
for (i in 1:3) {
  
  # variables
  pheno <- phenos[i]
  var <- paste0("both.", phenos[i])
  data <- paste0("data.", phenos[i])
  
  # exclude mixed-sided individuals
  data <- get(data) %>%
    filter(get(pheno) != 1) %>%
    mutate(pheno.new = get(pheno))
  
  # model
  glm <- glm(pheno.new ~ get(var), data = data, family = "binomial")
  
  # model summary
  sum <- summary(glm)
  
  # bind model summary and log odds ratios
  or <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm)))
  
  # fill dataframe
  results.parents.lmr[(i*6-5):(i*6),3:9] <- or
  
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 2:6))

}

# Fig 1B (all families - R-L)

# create empty dataframe to fill
results.parents.lr <- data.frame(matrix(ncol = 9, nrow = 9))
colnames(results.parents.lr) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.parents.lr[,1] <- c(rep("hand", 3), rep("foot", 3), rep("eye", 3))
results.parents.lr[,2] <- rep(c("intercept", 
                                 "one left-sided parent", 
                                 "two left-sided parents"), 3)

# run logistic regression analyses and fill dataframe
for (i in 1:3) {
  
  # variables
  pheno <- paste0(phenos[i], ".lr")
  var <- paste0("both.", phenos[i], ".lr")
  data <- paste0("data.", phenos[i])
  
  # model
  glm <- glm(get(pheno) ~ get(var), data = get(data), family = "binomial")
  
  # model summary
  sum <- summary(glm)
  
  # fill dataframe with model summary and log odds ratios
  results.parents.lr[(i*3-2):(i*3),3:9] <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm)))
  
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 2:3))
  
}

# Fig S5A (confirmed paternity - R-M-L)

# create empty dataframe to fill
results.parents.lmr.bio <- data.frame(matrix(ncol = 9, nrow = 18))
colnames(results.parents.lmr.bio) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.parents.lmr.bio[,1] <- c(rep("hand", 6), rep("foot", 6), rep("eye", 6))
results.parents.lmr.bio[,2] <- rep(c("intercept", 
                                     "one mixed-sided parent", 
                                     "one left-sided parent", 
                                     "two mixed-sided parents", 
                                     "one mixed-, one left-sided parent", 
                                     "two left-sided parents"), 3)

for (i in 1:3) {
  
  # variables
  pheno <- phenos[i]
  var <- paste0("both.", phenos[i])
  data <- paste0("data.", phenos[i], ".bio")
  
  # exclude mixed-sided individuals
  data <- get(data) %>%
    filter(get(pheno) != 1) %>%
    mutate(pheno.new = get(pheno))
  
  # model
  glm <- glm(pheno.new ~ get(var), data = data, family = "binomial")
  
  # model summary
  sum <- summary(glm)
  
  # bind summary and log odds ratios
  or <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm)))
  
  # fill dataframe
  results.parents.lmr.bio[(i*6-5):(i*6), 3:9] <- or
  
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 2:6))
  
}

# Fig S5B (confirmed paternity - L-R)

# empty dataframe to fill
results.parents.lr.bio <- data.frame(matrix(ncol = 9, nrow = 9))
colnames(results.parents.lr.bio) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.parents.lr.bio[,1] <- c(rep("hand", 3), rep("foot", 3), rep("eye", 3))
results.parents.lr.bio[,2] <- rep(c("intercept", 
                                "one left-sided parent", 
                                "two left-sided parents"), 3)

for (i in 1:3) {
  
  # variables
  pheno <- paste0(phenos[i], ".lr")
  var <- paste0("both.", phenos[i], ".lr")
  data <- paste0("data.", phenos[i], ".bio")
  
  # model
  glm <- glm(get(pheno) ~ get(var), data = get(data), family = "binomial")
  
  # model summary
  sum <- summary(glm)
  
  # bind summary and odds ratios
  or <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm)))
  
  # fill dataframe
  results.parents.lr.bio[(i*3-2):(i*3), 3:9] <- or
  
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 2:3))
  
}

#write.xlsx(results.parents.lmr, file = "results/1.or/1_OR_parents_log.xlsx", sheetName = "parentsRML", append = TRUE)
#write.xlsx(results.parents.lr, file = "results/1.or/1_OR_parents_log.xlsx", sheetName = "parentsRL", append = TRUE)
#write.xlsx(results.parents.lmr.bio, file = "results/1.or/1_OR_parents_log.xlsx", sheetName = "parentsRML.Bio", append = TRUE)
#write.xlsx(results.parents.lr.bio, file = "results/1.or/1_OR_parents_log.xlsx", sheetName = "parentsRL.Bio", append = TRUE)

### plot Figure 1

results.parents.lmr <- results.parents.lmr %>%
  filter(condition != "intercept") %>%
  mutate(pheno = factor(pheno, levels = c("hand", "foot", "eye")),
         condition = factor(condition, levels = c("one mixed-sided parent", 
                                                  "one left-sided parent", 
                                                  "two mixed-sided parents", 
                                                  "one mixed-, one left-sided parent", 
                                                  "two left-sided parents")))

results.parents.lmr[3,3:9] <- NA

Fig1a <- ggplot(data = results.parents.lmr,
                aes(x = OR, 
                    y = condition)) + 
  geom_point(aes(col = pheno), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, col = pheno), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 0, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(pheno),
             scales = 'free') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  labs(x = "log(OR) left-sidedness R-M-L") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank())  

results.parents.lr <- results.parents.lr %>%
  filter(condition != "intercept") %>%
  mutate(pheno = factor(pheno, levels = c("hand", "foot", "eye")),
         condition = factor(condition, levels = c("one left-sided parent", 
                                                  "two left-sided parents")))

Fig1b <- ggplot(data = results.parents.lr,
                aes(x = OR, 
                    y = condition)) + 
  geom_point(aes(col = pheno), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, col = pheno), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 0, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(pheno),
             scales = 'free') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  labs(x = "log(OR) left-sidedness R-L") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank()) 

#tiff("outputs/plots/Fig1_new.tiff", units = "in", width = 12, height = 6, res = 300)
egg::ggarrange(Fig1a, Fig1b,
               ncol = 1, 
               labels = c("A", "B"),
               heights = c(5, 2))
#dev.off()

### plot Figure S5 (same as Figure 1 for biological parents)

results.parents.lmr.bio <- read.xlsx(file = "results/1.or/1_OR_parents_log.xlsx", sheetName = "parentsRML.Bio")

results.parents.lmr.bio <- results.parents.lmr.bio %>%
  filter(condition != "intercept") %>%
  mutate(pheno = factor(pheno, levels = c("hand", "foot", "eye")),
         condition = factor(condition, levels = c("one mixed-sided parent", 
                                                  "one left-sided parent", 
                                                  "two mixed-sided parents", 
                                                  "one mixed-, one left-sided parent", 
                                                  "two left-sided parents")),
         lower = as.numeric(lower),
         upper = as.numeric(upper))

results.parents.lmr.bio[4,3:9] <- NA

FigS5a <- ggplot(data = results.parents.lmr.bio,
                 aes(x = OR, 
                     y = condition)) + 
  geom_point(aes(col = pheno), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, col = pheno), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 0, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(pheno),
             scales = 'free') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  labs(x = "log(OR) left-sidedness R-M-L") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank(),
        panel.spacing.x = unit(7, "mm"))  

results.parents.lr.bio <- results.parents.lr.bio %>%
  filter(condition != "intercept") %>%
  mutate(pheno = factor(pheno, levels = c("hand", "foot", "eye")),
         condition = factor(condition, levels = c("one left-sided parent", 
                                                  "two left-sided parents")))

FigS5b <- ggplot(data = results.parents.lr.bio,
                 aes(x = OR, 
                     y = condition)) + 
  geom_point(aes(col = pheno), size = 3) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, col = pheno), width = 0.3, cex = 1) + 
  geom_vline(xintercept = 0, linetype = 2, size = 1, color = 'black') +
  facet_grid(cols = vars(pheno),
             scales = 'free') + 
  scale_colour_manual(values = c("#A50026", "#74ADD1", "#313695")) + 
  labs(x = "log(OR) left-sidedness R-L") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank(),
        panel.spacing.x = unit(7, "mm")) 

#tiff("outputs/plots/FigS5_new.tiff", units = "in", width = 12, height = 6, res = 300)
egg::ggarrange(FigS5a, FigS5b,
               ncol = 1, 
               labels = c("A", "B"),
               heights = c(5, 2))
#dev.off()


##########################
#
# PART 4: LOGISTIC REGRESSION: MATERNAL AND PATERNAL EFFECTS

options(digits = 2)

phenos <- c("hand", "foot", "eye")

# All families - R-M-L

# empty dataframes to fill
results.lmr <- data.frame(matrix(ncol = 9, nrow = 30))
colnames(results.lmr) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.lmr[,1] <- c(rep("hand", 10), rep("foot", 10), rep("eye", 10))
results.lmr[,2] <- rep(c("intercept", "m.mixed", "m.left", "p.mixed", "p.left", "sex", "m.mixed.sex", "m.left.sex", "p.mixed.sex", "p.left.sex"), 3)

plot.lmr <- data.frame(matrix(ncol = 6, nrow = 36))
colnames(plot.lmr) <- c("condition", "pheno", "child.sex", "OR", "lower", "upper")
plot.lmr[,1] <- rep(c("mother mixed-sided", "mother left-sided", "father mixed-sided", "father left-sided"), 9)
plot.lmr[,2] <- c(rep("hand", 12), rep("foot", 12), rep("eye", 12))
plot.lmr[,3] <- rep(c(rep("all", 4), rep("females", 4), rep("males", 4)), 3)

# contrast for maternal vs. paternal effects
contrast.lmr <- cbind(0, 1, 1, -1, -1, 0, 1, 1, -1, -1)

for (i in 1:3) {
  
  pheno <- phenos[i]
  var.m <- paste0("m.", phenos[i])
  var.p <- paste0("p.", phenos[i])
  data <- paste0("data.", phenos[i]) 
  
  # exclude mixed-sided individuals
  data <- get(data) %>%
    filter(get(pheno) != 1) %>%
    mutate(pheno.new = get(pheno),
           pheno.new = recode(pheno.new, "2" = "1", "0" = "0"))
  
  glm <- glm(pheno.new ~ get(var.m) + get(var.p) + sex + get(var.m)*sex + get(var.p)*sex, data = data, family = "binomial")
  
  # maternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(2,3,7,8)))
  # paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(4,5,9,10)))
  # compare maternal vs. paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), L = contrast.lmr))

  sum <- summary(glm)
  
  or <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm, method = "se")))
  
  results.lmr[(i*10-9):(i*10),3:9] <- or
  
  glm.m.a <- glm(pheno.new ~ get(var.m), data = data, family = "binomial")
  glm.p.a <- glm(pheno.new ~ get(var.p), data = data, family = "binomial")
  
  glm.m.f <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 2), family = "binomial")
  glm.p.f <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 2), family = "binomial")
  
  glm.m.m <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 1), family = "binomial")
  glm.p.m <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 1), family = "binomial")
  
  fig <- rbind(cbind(OR = coef(glm.m.a), confint(glm.m.a))[2:3,],
               cbind(OR = coef(glm.p.a), confint(glm.p.a))[2:3,],
               cbind(OR = coef(glm.m.f), confint(glm.m.f))[2:3,],
               cbind(OR = coef(glm.p.f), confint(glm.p.f))[2:3,],
               cbind(OR = coef(glm.m.m), confint(glm.m.m))[2:3,],
               cbind(OR = coef(glm.p.m), confint(glm.p.m))[2:3,])
  
  plot.lmr[(i*12-11):(i*12),4:6] <- fig
  
}


# All families - R-L

# empty dataframew to fill
results.lr <- data.frame(matrix(ncol = 9, nrow = 18))
colnames(results.lr) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.lr[,1] <- c(rep("hand", 6), rep("foot", 6), rep("eye", 6))
results.lr[,2] <- rep(c("intercept", "m.left", "p.left", "sex", "m.left.sex", "p.left.sex"), 3)

plot.lr <- data.frame(matrix(ncol = 6, nrow = 18))
colnames(plot.lr) <- c("condition", "pheno", "child.sex", "OR", "lower", "upper")
plot.lr[,1] <- rep(c("mother left-sided", "father left-sided"), 9)
plot.lr[,2] <- c(rep("hand", 6), rep("foot", 6), rep("eye", 6))
plot.lr[,3] <- rep(c(rep("all", 2), rep("females", 2), rep("males", 2)), 3)

# contrast for maternal vs. paternal effects
contrast.lr <- cbind(0, 1, -1, 0, 1, -1)

for (i in 1:3) {
  
  pheno <- paste0(phenos[i], ".lr")
  var.m <- paste0("m.", phenos[i], ".lr")
  var.p <- paste0("p.", phenos[i], ".lr")
  data <- paste0("data.", phenos[i]) 
  
  data <- get(data) %>%
    mutate(pheno.new = ifelse(get(pheno) == 0, yes = 0, no = 1)) 
  
  glm <- glm(pheno.new ~ get(var.m) + get(var.p) + sex + get(var.m)*sex + get(var.p)*sex, data = data, family = "binomial")
  
  # maternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(2,5)))
  # paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(3,6)))
  # compare maternal vs. paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), L = contrast.lr))
  
  sum <- summary(glm)
  
  or <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm, method = "se")))
  
  results.lr[(i*6-5):(i*6),3:9] <- or
  
  glm.m.a <- glm(pheno.new ~ get(var.m), data = data, family = "binomial")
  glm.p.a <- glm(pheno.new ~ get(var.p), data = data, family = "binomial")
  
  glm.m.f <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 2), family = "binomial")
  glm.p.f <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 2), family = "binomial")
  
  glm.m.m <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 1), family = "binomial")
  glm.p.m <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 1), family = "binomial")
  
  fig <- rbind(cbind(OR = coef(glm.m.a), confint(glm.m.a))[2,],
               cbind(OR = coef(glm.p.a), confint(glm.p.a))[2,],
               cbind(OR = coef(glm.m.f), confint(glm.m.f))[2,],
               cbind(OR = coef(glm.p.f), confint(glm.p.f))[2,],
               cbind(OR = coef(glm.m.m), confint(glm.m.m))[2,],
               cbind(OR = coef(glm.p.m), confint(glm.p.m))[2,])
  
  plot.lr[(i*6-5):(i*6),4:6] <- fig
  
}

# Confirmed paternity - L-M-R

# empty dataframes to fill
results.lmr.bio <- data.frame(matrix(ncol = 9, nrow = 30))
colnames(results.lmr.bio) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.lmr.bio[,1] <- c(rep("hand", 10), rep("foot", 10), rep("eye", 10))
results.lmr.bio[,2] <- rep(c("intercept", "m.mixed", "m.left", "p.mixed", "p.left", "sex", "m.mixed.sex", "m.left.sex", "p.mixed.sex", "p.left.sex"), 3)

plot.lmr.bio <- data.frame(matrix(ncol = 6, nrow = 36))
colnames(plot.lmr.bio) <- c("condition", "pheno", "child.sex", "OR", "lower", "upper")
plot.lmr.bio[,1] <- rep(c("mother mixed-sided", "mother left-sided", "father mixed-sided", "father left-sided"), 9)
plot.lmr.bio[,2] <- c(rep("hand", 12), rep("foot", 12), rep("eye", 12))
plot.lmr.bio[,3] <- rep(c(rep("all", 4), rep("females", 4), rep("males", 4)), 3)

for (i in 1:3) {
  
  pheno <- phenos[i]
  var.m <- paste0("m.", phenos[i])
  var.p <- paste0("p.", phenos[i])
  data <- paste0("data.", phenos[i], ".bio") 
  
  # merge left and mixed to non-right
  data <- get(data) %>%
    #mutate(pheno.new = ifelse(get(pheno) == 0, yes = 0, no = 1)) %>%
    filter(get(pheno) != 1) %>%
    mutate(pheno.new = get(pheno))
  
  glm <- glm(pheno.new ~ get(var.m) + get(var.p) + sex + get(var.m)*sex + get(var.p)*sex, data = data, family = "binomial")
  
  # maternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(2,3,7,8)))
  # paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(4,5,9,10)))
  # compare maternal vs. paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), L = contrast.lmr))
  
  sum <- summary(glm)
  
  or <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm, method = "se")))

  results.lmr.bio[(i*10-9):(i*10),3:9] <- or
  
  glm.m.a <- glm(pheno.new ~ get(var.m), data = data, family = "binomial")
  glm.p.a <- glm(pheno.new ~ get(var.p), data = data, family = "binomial")
  
  glm.m.f <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 2), family = "binomial")
  glm.p.f <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 2), family = "binomial")
  
  glm.m.m <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 1), family = "binomial")
  glm.p.m <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 1), family = "binomial")
  
  fig <- rbind(cbind(OR = coef(glm.m.a), confint(glm.m.a))[2:3,],
               cbind(OR = coef(glm.p.a), confint(glm.p.a))[2:3,],
               cbind(OR = coef(glm.m.f), confint(glm.m.f))[2:3,],
               cbind(OR = coef(glm.p.f), confint(glm.p.f))[2:3,],
               cbind(OR = coef(glm.m.m), confint(glm.m.m))[2:3,],
               cbind(OR = coef(glm.p.m), confint(glm.p.m))[2:3,])
  
  plot.lmr.bio[(i*12-11):(i*12),4:6] <- fig
  
}

# Confirmed paternity - L-R

# empty dataframes to fill
results.lr.bio <- data.frame(matrix(ncol = 9, nrow = 18))
colnames(results.lr.bio) <- c("pheno", "condition", "estimate", "se", "z", "p", "OR", "lower", "upper")
results.lr.bio[,1] <- c(rep("hand", 6), rep("foot", 6), rep("eye", 6))
results.lr.bio[,2] <- rep(c("intercept", "m.left", "p.left", "sex", "m.left.sex", "p.left.sex"), 3)

plot.lr.bio <- data.frame(matrix(ncol = 6, nrow = 18))
colnames(plot.lr.bio) <- c("condition", "pheno", "child.sex", "OR", "lower", "upper")
plot.lr.bio[,1] <- rep(c("mother left-sided", "father left-sided"), 9)
plot.lr.bio[,2] <- c(rep("hand", 6), rep("foot", 6), rep("eye", 6))
plot.lr.bio[,3] <- rep(c(rep("all", 2), rep("females", 2), rep("males", 2)), 3)

for (i in 1:3) {
  
  pheno <- paste0(phenos[i], ".lr")
  var.m <- paste0("m.", phenos[i], ".lr")
  var.p <- paste0("p.", phenos[i], ".lr")
  data <- paste0("data.", phenos[i], ".bio") 
  
  data <- get(data) %>%
    mutate(pheno.new = ifelse(get(pheno) == 0, yes = 0, no = 1)) 
  
  glm <- glm(pheno.new ~ get(var.m) + get(var.p) + sex + get(var.m)*sex + get(var.p)*sex, data = data, family = "binomial")
  
  # maternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(2,5)))
  # paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(3,6)))
  # compare maternal vs. paternal effect
  print(wald.test(b = coef(glm), Sigma = vcov(glm), L = contrast.lr))
  
  sum <- summary(glm)
  
  or <- cbind(sum$coefficients, cbind(OR = coef(glm), confint(glm, method = "se")))

  results.lr.bio[(i*6-5):(i*6),3:9] <- or
  
  glm.m.a <- glm(pheno.new ~ get(var.m), data = data, family = "binomial")
  glm.p.a <- glm(pheno.new ~ get(var.p), data = data, family = "binomial")
  
  glm.m.f <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 2), family = "binomial")
  glm.p.f <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 2), family = "binomial")
  
  glm.m.m <- glm(pheno.new ~ get(var.m), data = subset(data, sex == 1), family = "binomial")
  glm.p.m <- glm(pheno.new ~ get(var.p), data = subset(data, sex == 1), family = "binomial")
  
  fig <- rbind(cbind(OR = coef(glm.m.a), confint(glm.m.a))[2,],
               cbind(OR = coef(glm.p.a), confint(glm.p.a))[2,],
               cbind(OR = coef(glm.m.f), confint(glm.m.f))[2,],
               cbind(OR = coef(glm.p.f), confint(glm.p.f))[2,],
               cbind(OR = coef(glm.m.m), confint(glm.m.m))[2,],
               cbind(OR = coef(glm.p.m), confint(glm.p.m))[2,])
  
  plot.lr.bio[(i*6-5):(i*6),4:6] <- fig
  
}

#write.xlsx(results.lmr, file = "results/1.or/2_OR_maternal_paternal_log.xlsx", sheetName = "parentsRML", append = TRUE)
#write.xlsx(results.lr, file = "results/1.or/2_OR_maternal_paternal_log.xlsx", sheetName = "parentsRL", append = TRUE)
#write.xlsx(results.lmr.bio, file = "results/1.or/2_OR_maternal_paternal_log.xlsx", sheetName = "parentsRML.Bio", append = TRUE)
#write.xlsx(results.lr.bio, file = "results/1.or/2_OR_maternal_paternal_log.xlsx", sheetName = "parentsRL.Bio", append = TRUE)

rm(fig, glm, glm.m.a, glm.m.f, glm.m.m, glm.p.a, glm.p.f, glm.p.m, or, sum, i, pheno, phenos, var.m, var.p, data)
rm(FigS5a, FigS5b, plot.lmr.bio, plot.lr.bio, results.lmr.bio, results.lr.bio)



##########################
#
# PART 5: HERITABILITY: GRM

# Read ID file from GRM
ids.all <- read.table('data/grm.id/ALSPAC.hand.foot.eye.family.grm.id', h = FALSE)
colnames(ids.all) <- c("ID_1", "ID_2")
ids.all$ID_1 <- as.character(ids.all$ID_1)

# read Principal components
pcs.all <- read.table('data/eigenvec/hand.foot.eye.family.eigenvec', h = FALSE)
pcs.all <- pcs.all %>%
  dplyr::select(V2, V3, V4)
colnames(pcs.all) <- c("ID_2", "pc1", "pc2")
pcs.all$ID_2 <- as.character(pcs.all$ID_2)

mydata <- master %>%
  filter(ID_2 %in% keep.all$ID_2) %>%
  select(ID_1, ID_2, sex, age.weeks, hand, foot, eye, m.age, m.hand, m.foot, m.eye, p.age, p.hand, p.foot, p.eye) %>%
  mutate(m.age = m.age * 52,
         p.age = p.age * 52,
         ID_3 = paste0(ID_1, "M"),
         ID_4 = paste0(ID_1, "F"))

mydata.long <- data.frame(c(mydata$ID_2, mydata$ID_3, mydata$ID_4),
                     c(mydata$age.weeks, mydata$m.age, mydata$p.age),
                     c(mydata$sex, rep(2, 1000), rep(1, 1000)),
                     c(mydata$hand, mydata$m.hand, mydata$p.hand),
                     c(mydata$foot, mydata$m.foot, mydata$p.foot),
                     c(mydata$eye, mydata$m.eye, mydata$p.eye))
colnames(mydata.long) <- c("ID_2", "age", "sex", "hand", "foot", "eye")

mydata.long <- left_join(mydata.long, pcs.all, by = "ID_2")

mydata.resid <- mydata.long %>%
  dplyr::select(ID_2)

phenos <- c("hand", "foot", "eye")

mydata.long <- mydata.long %>%
  mutate(hand = as.numeric(as.character(hand)),
         foot = as.numeric(as.character(foot)),
         eye = as.numeric(as.character(eye)))

# correct items for age, sex and 2 pcs
for (i in 1:length(phenos)) {
  
  pheno <- phenos[i]
  
  data <- mydata.long[,c("ID_2", "sex", "age", "pc1", "pc2", pheno)]
  
  reg <- glm(get(pheno) ~ sex + age + pc1 + pc2, data = data)
  
  data[which(!is.na(data[,6])), 6] <- RankNorm(residuals(reg))
  
  pheno.bind <- as.data.frame(data[,6])
  
  colnames(pheno.bind) <- pheno
  
  mydata.resid <- cbind(mydata.resid, pheno.bind)
  
}

rm(reg, pheno.bind, i, pheno)

# merge with ids
mydata.resid <- left_join(ids.all, mydata.resid, by = "ID_2") 

# univariate reml analyses
for (pheno in phenos) {
  
  myphenos.resid.reml <- mydata.resid %>%
    dplyr::select(ID_1, ID_2, all_of(pheno))
  
  output.resid.reml <- paste0("outputs/pheno_files_reml/reml.", pheno, ".family")
  
  #write.table(myphenos.resid.reml, output.resid.reml, quote = F, row.names = F, col.names = F)
  
}



##########################
#
# PART 6: HERITABILITY: MID-PARENTAL REGRESSION

join <- data.frame(mydata.long$ID_2)
colnames(join) <- "ID_2"

mydata.reform <- left_join(join, mydata.resid, by = "ID_2") 

mydata.wide <- data.frame(mydata.reform$ID_1[1:1000],
                          mydata.reform$ID_2[1:1000],
                          mydata.reform$hand[1:1000],
                          mydata.reform$hand[1001:2000],
                          mydata.reform$hand[2001:3000],
                          mydata.reform$foot[1:1000],
                          mydata.reform$foot[1001:2000],
                          mydata.reform$foot[2001:3000],
                          mydata.reform$eye[1:1000],
                          mydata.reform$eye[1001:2000],
                          mydata.reform$eye[2001:3000])
  
colnames(mydata.wide) <- c("ID_1", "ID_2", "hand", "m.hand", "p.hand", "foot", "m.foot", "p.foot", "eye", "m.eye", "p.eye")

mydata.wide <- mydata.wide %>%
  mutate(hand.parent = (m.hand + p.hand) / 2,
         foot.parent = (m.foot + p.foot) / 2,
         eye.parent = (m.eye + p.eye) / 2)

her.hand <- glm(hand ~ hand.parent, data = mydata.wide)
her.foot <- glm(foot ~ foot.parent, data = mydata.wide)
her.eye <- glm(eye ~ eye.parent, data = mydata.wide)

summary(her.hand)
confint(her.hand)
summary(her.foot)
confint(her.foot)
summary(her.eye)
confint(her.eye)



##########################
#
# PART 7: CHECK RELIABILITY OF HAND PREFERENCE

writing.hand <- master %>%
  filter(!is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) %>%
  filter(hand == 0 | hand == 2) %>%
  filter(hand.1 == 1 | hand.1 == 3) %>%
  dplyr::select(ID_1, sex, writing.hand, age.weeks.7, age.weeks, hand.1) %>%
  filter(!is.na(writing.hand)) %>%
  filter(!is.na(hand.1)) %>%
  filter(writing.hand != 8)

# association with writing hand at age 7
writing.hand$hand.1 <- as.numeric(recode(writing.hand$hand.1, "3" = "1", "1" = "0"))
writing.hand$writing.hand <- as.numeric(recode(writing.hand$writing.hand, "1" = "1", "2" = "0"))
freq(writing.hand$writing.hand)
freq(writing.hand$hand.1)

round(mean(writing.hand$age.weeks.7, na.rm = TRUE)/52,2)
round(sd(writing.hand$age.weeks.7, na.rm = TRUE)/52,2)

# Correlation
cor <- cor.test(x = writing.hand$writing.hand, y = writing.hand$hand.1, method = "spearman")

ci <- spearman.ci(writing.hand$writing.hand, writing.hand$hand.1)

cor.results <- data.frame(matrix(ncol = 6, nrow = 1))
colnames(cor.results) <- c("n", "S", "cor", "ci_lower", "ci_upper", "p")

cor.results[1,] <- c(nrow(writing.hand), cor$statistic, cor$estimate, ci$conf.int[1], ci$conf.int[2], cor$p.value)

cor.results$cor <- as.numeric(cor.results$cor)

# Crosstable
CrossTable(writing.hand$hand.1, writing.hand$writing.hand)

