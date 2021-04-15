### hand, foot, eye preference in children and parents

source(file = 'scripts/1_family_hand_eye_foot_functions.r')


##########################
#
# PART 1: READ, CLEAN, NORMALIZE ALSPAC

# Read SPSS file
raw.alspac <- read.alspac()

# Normalize data
alspac.normal <- normalize.alspac(dataset = raw.alspac) # n = 9964 with information on either hand,foot,eye

# exclusion
freq(alspac.normal$id.twins)
freq(alspac.normal$physical.disability)
freq(alspac.normal$sensory.impairment)

# Apply exclusion criteria
master <- clean.alspac.eye.foot.hand(mydata = alspac.normal) # n = 9755 after exclusion criteria

# Test if all is correct
all.equal(master$hand, master$hand.cat)
all.equal(master$foot, master$foot.cat)
all.equal(master$eye, master$eye.cat)
all.equal(master$m.hand, master$m.hand.cat)
all.equal(master$m.foot, master$m.foot.cat)

# descriptive
round(mean(master$age.weeks, na.rm = TRUE)/52,2)
round(sd(master$age.weeks, na.rm = TRUE)/52,2)
freq(master$gender)

# mixed vs ambidextrous parents
#source(file = 'scripts/1_mixed_ambidextrous_parents.r')

writing.hand <- master %>%
  filter(!is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) %>%
  filter(hand == 1 | hand == 3) %>%
  dplyr::select(ID_1, gender, writing.hand, age.weeks.7, age.weeks, hand.1) %>%
  filter(!is.na(writing.hand))

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


##########################
#
# PART 2: LOGISTIC REGRESSION ANALYSES

logreg.hand <- master %>%
  dplyr::select(ID_1, gender, age.weeks, m.age, p.age, hand, m.hand, p.hand, same.sex.hand, opp.sex.hand, both.hand) %>%
  filter(!is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) %>%
  filter(hand == 1 | hand == 3)

freq(logreg.hand$gender)
round(mean(logreg.hand$age.weeks, na.rm = TRUE)/52,2)
round(sd(logreg.hand$age.weeks, na.rm = TRUE)/52,2)
round(mean(logreg.hand$m.age, na.rm = TRUE),2)
round(sd(logreg.hand$m.age, na.rm = TRUE),2)
round(mean(logreg.hand$p.age, na.rm = TRUE),2)
round(sd(logreg.hand$p.age, na.rm = TRUE),2)

logreg.hand$hand <- as.factor(recode(logreg.hand$hand, "3" = "0", "1" = "1"))
logreg.hand$m.hand <- as.factor(recode(logreg.hand$m.hand, "3" = "0", "2" = "1", "1" = "2"))
logreg.hand$p.hand <- as.factor(recode(logreg.hand$p.hand, "3" = "0", "2" = "1", "1" = "2"))
logreg.hand$same.sex.hand <- as.factor(recode(logreg.hand$same.sex.hand, "3" = "0", "2" = "1", "1" = "2"))
logreg.hand$opp.sex.hand <- as.factor(recode(logreg.hand$opp.sex.hand, "3" = "0", "2" = "1", "1" = "2"))


logreg.foot <- master %>%
  dplyr::select(ID_1, gender, age.weeks, m.age, p.age, foot, m.foot, p.foot, same.sex.foot, opp.sex.foot, both.foot) %>%
  filter(!is.na(foot) & !is.na(m.foot) & !is.na(p.foot)) %>%
  filter(foot == 1 | foot == 3)

freq(logreg.foot$gender)
round(mean(logreg.foot$age.weeks, na.rm = TRUE)/52,2)
round(sd(logreg.foot$age.weeks, na.rm = TRUE)/52,2)
round(mean(logreg.foot$m.age, na.rm = TRUE),2)
round(sd(logreg.foot$m.age, na.rm = TRUE),2)
round(mean(logreg.foot$p.age, na.rm = TRUE),2)
round(sd(logreg.foot$p.age, na.rm = TRUE),2)

logreg.foot$foot <- as.factor(recode(logreg.foot$foot, "3" = "0", "1" = "1"))
logreg.foot$m.foot <- as.factor(recode(logreg.foot$m.foot, "3" = "0", "2" = "1", "1" = "2"))
logreg.foot$p.foot <- as.factor(recode(logreg.foot$p.foot, "3" = "0", "2" = "1", "1" = "2"))
logreg.foot$same.sex.foot <- as.factor(recode(logreg.foot$same.sex.foot, "3" = "0", "2" = "1", "1" = "2"))
logreg.foot$opp.sex.foot <- as.factor(recode(logreg.foot$opp.sex.foot, "3" = "0", "2" = "1", "1" = "2"))


logreg.eye <- master %>%
  dplyr::select(ID_1, gender, age.weeks, m.age, p.age, eye, m.eye, p.eye, same.sex.eye, opp.sex.eye, both.eye) %>%
  filter(!is.na(eye) & !is.na(m.eye) & !is.na(p.eye)) %>%
  filter(eye == 1 | eye == 3)

freq(logreg.eye$gender)
round(mean(logreg.eye$age.weeks, na.rm = TRUE)/52,2)
round(sd(logreg.eye$age.weeks, na.rm = TRUE)/52,2)
round(mean(logreg.eye$m.age, na.rm = TRUE),2)
round(sd(logreg.eye$m.age, na.rm = TRUE),2)
round(mean(logreg.eye$p.age, na.rm = TRUE),2)
round(sd(logreg.eye$p.age, na.rm = TRUE),2)

logreg.eye$eye <- as.factor(recode(logreg.eye$eye, "3" = "0", "1" = "1"))
logreg.eye$m.eye <- as.factor(recode(logreg.eye$m.eye, "3" = "0", "2" = "1", "1" = "2"))
logreg.eye$p.eye <- as.factor(recode(logreg.eye$p.eye, "3" = "0", "2" = "1", "1" = "2"))
logreg.eye$same.sex.eye <- as.factor(recode(logreg.eye$same.sex.eye, "3" = "0", "2" = "1", "1" = "2"))
logreg.eye$opp.sex.eye <- as.factor(recode(logreg.eye$opp.sex.eye, "3" = "0", "2" = "1", "1" = "2"))


# L-M-R (3 x 3)
table(logreg.hand$hand, logreg.hand$m.hand)
table(logreg.foot$foot, logreg.foot$m.foot)
table(logreg.eye$eye, logreg.eye$m.eye)
table(logreg.hand$hand, logreg.hand$p.hand)
table(logreg.foot$foot, logreg.foot$p.foot)
table(logreg.eye$eye, logreg.eye$p.eye)
table(logreg.hand$hand, logreg.hand$both.hand)
table(logreg.foot$foot, logreg.foot$both.foot)
table(logreg.eye$eye, logreg.eye$both.eye)


# hand

options(digits=2)

hand.mother <- glm(hand ~ m.hand, data = logreg.hand, family = "binomial")
hand.mother.or <-exp(cbind(OR = coef(hand.mother), confint(hand.mother)))
cbind(summary(hand.mother)$coefficients, hand.mother.or)

hand.father <- glm(hand ~ p.hand, data = logreg.hand, family = "binomial")
hand.father.or <- exp(cbind(OR = coef(hand.father), confint(hand.father)))
cbind(summary(hand.father)$coefficients, hand.father.or)

hand.same <- glm(hand ~ same.sex.hand, data = logreg.hand, family = "binomial")
hand.same.or <- exp(cbind(OR = coef(hand.same), confint(hand.same)))
cbind(summary(hand.same)$coefficients, hand.same.or)

hand.opp <- glm(hand ~ opp.sex.hand, data = logreg.hand, family = "binomial")
hand.opp.or <- exp(cbind(OR = coef(hand.opp), confint(hand.opp)))
cbind(summary(hand.opp)$coefficients, hand.opp.or)

hand.parents <- glm(hand ~ both.hand, data = logreg.hand, family = "binomial")
hand.parents.or <- exp(cbind(OR = coef(hand.parents), confint(hand.parents)))
cbind(summary(hand.parents)$coefficients, hand.parents.or)

# foot
foot.mother <- glm(foot ~ m.foot, data = logreg.foot, family = "binomial")
foot.mother.or <- exp(cbind(OR = coef(foot.mother), confint(foot.mother)))
cbind(summary(foot.mother)$coefficients, foot.mother.or)

foot.father <- glm(foot ~ p.foot, data = logreg.foot, family = "binomial")
foot.father.or <- exp(cbind(OR = coef(foot.father), confint(foot.father)))
cbind(summary(foot.father)$coefficients, foot.father.or)

foot.same <- glm(foot ~ same.sex.foot, data = logreg.foot, family = "binomial")
foot.same.or <- exp(cbind(OR = coef(foot.same), confint(foot.same)))
cbind(summary(foot.same)$coefficients, foot.same.or)

foot.opp <- glm(foot ~ opp.sex.foot, data = logreg.foot, family = "binomial")
foot.opp.or <- exp(cbind(OR = coef(foot.opp), confint(foot.opp)))
cbind(summary(foot.opp)$coefficients, foot.opp.or)

foot.parents <- glm(foot ~ both.foot, data = logreg.foot, family = "binomial")
foot.parents.or <- exp(cbind(OR = coef(foot.parents), confint(foot.parents)))
cbind(summary(foot.parents)$coefficients, foot.parents.or)

# eye
eye.mother <- glm(eye ~ m.eye, data = logreg.eye, family = "binomial")
eye.mother.or <- exp(cbind(OR = coef(eye.mother), confint(eye.mother)))
cbind(summary(eye.mother)$coefficients, eye.mother.or)

eye.father <- glm(eye ~ p.eye, data = logreg.eye, family = "binomial")
eye.father.or <- exp(cbind(OR = coef(eye.father), confint(eye.father)))
cbind(summary(eye.father)$coefficients, eye.father.or)

eye.same <- glm(eye ~ same.sex.eye, data = logreg.eye, family = "binomial")
eye.same.or <- exp(cbind(OR = coef(eye.same), confint(eye.same)))
cbind(summary(eye.same)$coefficients, eye.same.or)

eye.opp <- glm(eye ~ opp.sex.eye, data = logreg.eye, family = "binomial")
eye.opp.or <- exp(cbind(OR = coef(eye.opp), confint(eye.opp)))
cbind(summary(eye.opp)$coefficients, eye.opp.or)

eye.parents <- glm(eye ~ both.eye, data = logreg.eye, family = "binomial")
eye.parents.or <- exp(cbind(OR = coef(eye.parents), confint(eye.parents)))
cbind(summary(eye.parents)$coefficients, eye.parents.or)

# sex-specific

# hand female

logreg.hand.fem <- logreg.hand %>%
  filter(gender == 2)

hand.fem.mother <- glm(hand ~ m.hand, data = logreg.hand.fem, family = "binomial")
hand.fem.mother.or <-exp(cbind(OR = coef(hand.fem.mother), confint(hand.fem.mother)))
cbind(summary(hand.fem.mother)$coefficients, hand.fem.mother.or)

hand.fem.father <- glm(hand ~ p.hand, data = logreg.hand.fem, family = "binomial")
hand.fem.father.or <- exp(cbind(OR = coef(hand.fem.father), confint(hand.fem.father)))
cbind(summary(hand.fem.father)$coefficients, hand.fem.father.or)

hand.fem.parents <- glm(hand ~ both.hand, data = logreg.hand.fem, family = "binomial")
hand.fem.parents.or <- exp(cbind(OR = coef(hand.fem.parents), confint(hand.fem.parents)))
cbind(summary(hand.fem.parents)$coefficients, hand.fem.parents.or)

table(logreg.hand.fem$hand, logreg.hand.fem$m.hand)
table(logreg.hand.fem$hand, logreg.hand.fem$p.hand)
table(logreg.hand.fem$hand, logreg.hand.fem$both.hand)

# hand male

logreg.hand.male <- logreg.hand %>%
  filter(gender == 1)

hand.male.mother <- glm(hand ~ m.hand, data = logreg.hand.male, family = "binomial")
hand.male.mother.or <-exp(cbind(OR = coef(hand.male.mother), confint(hand.male.mother)))
cbind(summary(hand.male.mother)$coefficients, hand.male.mother.or)

hand.male.father <- glm(hand ~ p.hand, data = logreg.hand.male, family = "binomial")
hand.male.father.or <- exp(cbind(OR = coef(hand.male.father), confint(hand.male.father)))
cbind(summary(hand.male.father)$coefficients, hand.male.father.or)

hand.male.parents <- glm(hand ~ both.hand, data = logreg.hand.male, family = "binomial")
hand.male.parents.or <- exp(cbind(OR = coef(hand.male.parents), confint(hand.male.parents)))
cbind(summary(hand.male.parents)$coefficients, hand.male.parents.or)

table(logreg.hand.male$hand, logreg.hand.male$m.hand)
table(logreg.hand.male$hand, logreg.hand.male$p.hand)
table(logreg.hand.male$hand, logreg.hand.male$both.hand)

# foot female

logreg.foot.fem <- logreg.foot %>%
  filter(gender == 2)

foot.fem.mother <- glm(foot ~ m.foot, data = logreg.foot.fem, family = "binomial")
foot.fem.mother.or <-exp(cbind(OR = coef(foot.fem.mother), confint(foot.fem.mother)))
cbind(summary(foot.fem.mother)$coefficients, foot.fem.mother.or)

foot.fem.father <- glm(foot ~ p.foot, data = logreg.foot.fem, family = "binomial")
foot.fem.father.or <- exp(cbind(OR = coef(foot.fem.father), confint(foot.fem.father)))
cbind(summary(foot.fem.father)$coefficients, foot.fem.father.or)

foot.fem.parents <- glm(foot ~ both.foot, data = logreg.foot.fem, family = "binomial")
foot.fem.parents.or <- exp(cbind(OR = coef(foot.fem.parents), confint(foot.fem.parents)))
cbind(summary(foot.fem.parents)$coefficients, foot.fem.parents.or)

table(logreg.foot.fem$foot, logreg.foot.fem$m.foot)
table(logreg.foot.fem$foot, logreg.foot.fem$p.foot)
table(logreg.foot.fem$foot, logreg.foot.fem$both.foot)

# foot male

logreg.foot.male <- logreg.foot %>%
  filter(gender == 1)

foot.male.mother <- glm(foot ~ m.foot, data = logreg.foot.male, family = "binomial")
foot.male.mother.or <-exp(cbind(OR = coef(foot.male.mother), confint(foot.male.mother)))
cbind(summary(foot.male.mother)$coefficients, foot.male.mother.or)

foot.male.father <- glm(foot ~ p.foot, data = logreg.foot.male, family = "binomial")
foot.male.father.or <- exp(cbind(OR = coef(foot.male.father), confint(foot.male.father)))
cbind(summary(foot.male.father)$coefficients, foot.male.father.or)

foot.male.parents <- glm(foot ~ both.foot, data = logreg.foot.male, family = "binomial")
foot.male.parents.or <- exp(cbind(OR = coef(foot.male.parents), confint(foot.male.parents)))
cbind(summary(foot.male.parents)$coefficients, foot.male.parents.or)

table(logreg.foot.male$foot, logreg.foot.male$m.foot)
table(logreg.foot.male$foot, logreg.foot.male$p.foot)
table(logreg.foot.male$foot, logreg.foot.male$both.foot)


# eye female

logreg.eye.fem <- logreg.eye %>%
  filter(gender == 2)

eye.fem.mother <- glm(eye ~ m.eye, data = logreg.eye.fem, family = "binomial")
eye.fem.mother.or <-exp(cbind(OR = coef(eye.fem.mother), confint(eye.fem.mother)))
cbind(summary(eye.fem.mother)$coefficients, eye.fem.mother.or)

eye.fem.father <- glm(eye ~ p.eye, data = logreg.eye.fem, family = "binomial")
eye.fem.father.or <- exp(cbind(OR = coef(eye.fem.father), confint(eye.fem.father)))
cbind(summary(eye.fem.father)$coefficients, eye.fem.father.or)

eye.fem.parents <- glm(eye ~ both.eye, data = logreg.eye.fem, family = "binomial")
eye.fem.parents.or <- exp(cbind(OR = coef(eye.fem.parents), confint(eye.fem.parents)))
cbind(summary(eye.fem.parents)$coefficients, eye.fem.parents.or)

table(logreg.eye.fem$eye, logreg.eye.fem$m.eye)
table(logreg.eye.fem$eye, logreg.eye.fem$p.eye)
table(logreg.eye.fem$eye, logreg.eye.fem$both.eye)

# eye male

logreg.eye.male <- logreg.eye %>%
  filter(gender == 1)

eye.male.mother <- glm(eye ~ m.eye, data = logreg.eye.male, family = "binomial")
eye.male.mother.or <-exp(cbind(OR = coef(eye.male.mother), confint(eye.male.mother)))
cbind(summary(eye.male.mother)$coefficients, eye.male.mother.or)

eye.male.father <- glm(eye ~ p.eye, data = logreg.eye.male, family = "binomial")
eye.male.father.or <- exp(cbind(OR = coef(eye.male.father), confint(eye.male.father)))
cbind(summary(eye.male.father)$coefficients, eye.male.father.or)

eye.male.parents <- glm(eye ~ both.eye, data = logreg.eye.male, family = "binomial")
eye.male.parents.or <- exp(cbind(OR = coef(eye.male.parents), confint(eye.male.parents)))
cbind(summary(eye.male.parents)$coefficients, eye.male.parents.or)

table(logreg.eye.male$eye, logreg.eye.male$m.eye)
table(logreg.eye.male$eye, logreg.eye.male$p.eye)
table(logreg.eye.male$eye, logreg.eye.male$both.eye)






