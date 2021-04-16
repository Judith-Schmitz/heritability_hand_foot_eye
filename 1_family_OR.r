### hand, foot, eye preference in children and parents

source(file = 'scripts_final/1_family_OR_functions.r')


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

# Test if all is correct
#all.equal(alspac.normal$hand, alspac.normal$hand.cat)
#all.equal(alspac.normal$foot, alspac.normal$foot.cat)
#all.equal(alspac.normal$eye, alspac.normal$eye.cat)
#all.equal(alspac.normal$m.hand, alspac.normal$m.hand.cat)
#all.equal(alspac.normal$m.foot, alspac.normal$m.foot.cat)

# Apply exclusion criteria
master <- clean.alspac(mydata = alspac.normal) # n = 9755 after exclusion criteria

# descriptive
#round(mean(master$age.weeks, na.rm = TRUE)/52,2)
#round(sd(master$age.weeks, na.rm = TRUE)/52,2)
#freq(master$sex)


##########################
#
# PART 2: DATASETS

# create separate datasets for hand, foot, and eye

# hand
data.hand <- master %>%
  dplyr::select(ID_1, sex, age.weeks, m.age, p.age, hand, m.hand, p.hand, both.hand) %>%
  filter(!is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) %>%
  filter(hand == 0 | hand == 2)

# descriptives: sex
freq(data.hand$sex)

# foot
data.foot <- master %>%
  dplyr::select(ID_1, sex, age.weeks, m.age, p.age, foot, m.foot, p.foot, both.foot) %>%
  filter(!is.na(foot) & !is.na(m.foot) & !is.na(p.foot)) %>%
  filter(foot == 0 | foot == 2)

# descriptives: sex and age
freq(data.foot$sex)
round(mean(data.foot$m.age, na.rm = TRUE),2)
round(sd(data.foot$m.age, na.rm = TRUE),2)
round(mean(data.foot$p.age, na.rm = TRUE),2)
round(sd(data.foot$p.age, na.rm = TRUE),2)

# eye
data.eye <- master %>%
  dplyr::select(ID_1, sex, age.weeks, m.age, p.age, eye, m.eye, p.eye, both.eye) %>%
  filter(!is.na(eye) & !is.na(m.eye) & !is.na(p.eye)) %>%
  filter(eye == 0 | eye == 2)

# descriptives: sex
freq(data.eye$sex)


##########################
#
# PART 3: LOGISTIC REGRESSION ANALYSES

options(digits=2)

## Parental effects

# empty dataframe to fill
results.parents <- data.frame(matrix(ncol = 5, nrow = 15))
colnames(results.parents) <- c("condition", "group", "OR", "lower", "upper")
results.parents[,1] <- rep(c("one mixed-sided parent", 
                             "one left-sided parent", 
                             "two mixed-sided parents", 
                             "one mixed-, one left-sided parent", 
                             "two left-sided parents"), 3)
results.parents[,2] <- c(rep("hand", 5), rep("foot", 5), rep("eye", 5))

phenos <- c("hand", "foot", "eye")

# run logistic regression analyses and fill dataframe
for (i in 1:3) {
  
  pheno <- phenos[i]
  var <- paste0("both.", phenos[i])
  data <- paste0("data.", phenos[i])
  
  glm <- glm(get(pheno) ~ get(var), data = get(data), family = "binomial")
  
  results.parents[(i*5-4):(i*5),3:5] <- exp(cbind(OR = coef(glm), confint(glm)))[2:6,]

}

results.parents <- results.parents %>%
  mutate(group = factor(group, levels = c("hand", "foot", "eye")),
         condition = factor(condition, levels = c("one mixed-sided parent", 
                                                  "one left-sided parent", 
                                                  "two mixed-sided parents", 
                                                  "one mixed-, one left-sided parent", 
                                                  "two left-sided parents")))

# plot Figure 1
tiff("outputs/plots/Fig1_final.tiff", units = "in", width = 12, height = 4, res = 300)
ggplot(data = results.parents,
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


## Maternal and paternal effects

# empty dataframe to fill
results.mp <- data.frame(matrix(ncol = 6, nrow = 36))
colnames(results.mp) <- c("condition", "child.sex", "group", "OR", "lower", "upper")
results.mp[,1] <- rep(c("mother mixed-sided", "mother left-sided", "father mixed-sided", "father left-sided"), 9)
results.mp[,2] <- c(rep("all", 12), rep("females", 12), rep("males", 12))
results.mp[,3] <- rep(c(rep("hand", 4), rep("foot", 4), rep("eye", 4)), 3)

# Fig 2 upper panel

for (i in 1:3) {
  
  pheno <- phenos[i]
  var.m <- paste0("m.", phenos[i])
  var.p <- paste0("p.", phenos[i])
  data <- paste0("data.", phenos[i])
  
  glm.m <- glm(get(pheno) ~ get(var.m), data = get(data), family = "binomial")
  glm.p <- glm(get(pheno) ~ get(var.p), data = get(data), family = "binomial")
  
  or <- rbind(exp(cbind(OR = coef(glm.m), confint(glm.m)))[2:3,],
              exp(cbind(OR = coef(glm.p), confint(glm.p)))[2:3,])
  
  results.mp[(i*4-3):(i*4),4:6] <- or
  
}

## Fig2 middle panel (female children)

data.hand.fem <- data.hand %>%
  filter(sex == 2)

data.foot.fem <- data.foot %>%
  filter(sex == 2)

data.eye.fem <- data.eye %>%
  filter(sex == 2)

for (i in 1:3) {
  
  pheno <- phenos[i]
  var.m <- paste0("m.", phenos[i])
  var.p <- paste0("p.", phenos[i])
  data <- paste0("data.", phenos[i], ".fem")
  
  glm.m <- glm(get(pheno) ~ get(var.m), data = get(data), family = "binomial")
  glm.p <- glm(get(pheno) ~ get(var.p), data = get(data), family = "binomial")
  
  or <- rbind(exp(cbind(OR = coef(glm.m), confint(glm.m)))[2:3,],
              exp(cbind(OR = coef(glm.p), confint(glm.p)))[2:3,])
  
  results.mp[(i*4-3+12):(i*4+12),4:6] <- or
  
}

## Fig2 lower panel (male children)

data.hand.male <- data.hand %>%
  filter(sex == 1)

data.foot.male <- data.foot %>%
  filter(sex == 1)

data.eye.male <- data.eye %>%
  filter(sex == 1)

for (i in 1:3) {
  
  pheno <- phenos[i]
  var.m <- paste0("m.", phenos[i])
  var.p <- paste0("p.", phenos[i])
  data <- paste0("data.", phenos[i], ".male")
  
  glm.m <- glm(get(pheno) ~ get(var.m), data = get(data), family = "binomial")
  glm.p <- glm(get(pheno) ~ get(var.p), data = get(data), family = "binomial")
  
  or <- rbind(exp(cbind(OR = coef(glm.m), confint(glm.m)))[2:3,],
              exp(cbind(OR = coef(glm.p), confint(glm.p)))[2:3,])
  
  results.mp[(i*4-3+24):(i*4+24),4:6] <- or
  
}

results.mp <- results.mp %>%
  mutate(group = factor(group, levels = c("hand", "foot", "eye")),
         child.sex = factor(child.sex, levels = c("all", "females", "males")),
         condition = factor(condition, levels = c("father mixed-sided", "mother mixed-sided", "father left-sided", "mother left-sided")))

# plot Figure 2
tiff("outputs/plots/Fig2_final.tiff", units = "in", width = 12, height = 9, res = 300)
ggplot(data = results.mp,
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


##########################
#
# PART 4: CHECK RELIABILITY OF HAND PREFERENCE

writing.hand <- master %>%
  filter(!is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) %>%
  filter(hand == 0 | hand == 2) %>%
  filter(hand.1 == 1 | hand.1 == 3) %>%
  dplyr::select(ID_1, sex, writing.hand, age.weeks.7, age.weeks, hand.1) %>%
  filter(!is.na(writing.hand)) %>%
  filter(!is.na(hand.1)) %>%
  filter(writing.hand != 8)

# correlation with writing hand at age 7
writing.hand$hand.1 <- as.factor(recode(writing.hand$hand.1, "3" = "0", "1" = "1"))
writing.hand$writing.hand <- as.factor(recode(writing.hand$writing.hand, "1" = "0", "2" = "1"))
freq(writing.hand$writing.hand)
freq(writing.hand$hand.1)

chisq.hand <- chisq.test(x = writing.hand$writing.hand, y = writing.hand$hand.1)
chisq.hand

chisq.hand$observed

round(mean(writing.hand$age.weeks.7, na.rm = TRUE)/52,2)
round(sd(writing.hand$age.weeks.7, na.rm = TRUE)/52,2)

