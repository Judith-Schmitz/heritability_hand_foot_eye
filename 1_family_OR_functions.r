
library(foreign) # for read.spss()
library(tidyverse) # for read.spss()
library(xlsx) # export dataframes to xlsx files
library(summarytools)
library(BBmisc) # for z standardisation
library(aod) # for logistic regression
library(ggpubr)
library(RVAideMemoire) # for speakman.rank
library(gmodels) # for CrossTable
library(RNOmni) # for rank normalization

read.alspac <- function(mypath = 'C:\\Users\\Judith\\OneDrive - University of St Andrews\\St Andrews\\projects\\gwas\\data_preparation\\data\\Paracchini_12Feb20.sav') {
  
  # Purpose:  Import an Alspac dataset from SPSS and rename the columns to a clearer syntax 
  #           
  # Inputs:   path: .sav location in the file system 
  #
  # Output:   one dataframe with the Alspac data
  #
  
  default.opt <- options()
  options(warn = -1) # Avoid warnings "record type 7, subtype 22" of read.spss() function
  
  input.alspac <- read.spss(file = mypath,
                            use.value.labels = FALSE, # there are levels with undefined labels in the SPSS file
                            to.data.frame = TRUE,
                            use.missings = TRUE)
  
  # Remove labels because missForest() cannot process numeric variables with labels
  for (var in colnames(input.alspac)) {
    attr(input.alspac[,deparse(as.name(var))], "value.labels") <- NULL
  }
  
  # Choose variables
  alspac <- input.alspac[,c('cid_417b', # unique pregnancy identifier
                            'qlet', # Identify children from multiple births
                            'kz021', # sex; 1 = male, 2 = female, -1 = not known
                            'c804', # child's ethnic background
                            
                            # Child
                            'kj999', # age at completion in weeks
                            'kj999a', # age at completion in months
                            'kj660', # Hand CH Draws With (1 left, 2 right, 3 either)
                            'kj661', # Hand CH Uses to Throw a Ball
                            'kj662', # Hand CH Uses to Colour in
                            'kj663', # Hand CH Uses to Hold Toothbrush
                            'kj664', # Hand CH Uses to Cut Things W Knife
                            'kj665', # Hand CH Uses to Hit Things
                            'kj670', # Foot CH Uses to to Kick Ball
                            'kj671', # Foot Used by CH to Pick up Stone
                            'kj672', # Foot CH Uses to Stamp on Something
                            'kj673', # Foot CH Uses to Climb a Step
                            'kj680', # Eye CH Uses to Look Through Hole
                            'kj681', # Eye CH Uses to Look Through Bottle
                            'kj685', # Handedness 42 mths
                            'kj686', # Footedness at 42 months
                            'kj687', # Eyedness at 42 months
                            
                            'sa036a', # Child has ever had sensory impairment (visual) (2=no)
                            'sa037a', # Child has ever had pysical disabilities (2=no)
                            
                            # Mother
                            'h992', # DV: Age of mother at completion (years)
                            'h120a', # B1a: Hand used by mum to write
                            'h120b', # B1b: Hand used by mum to draw
                            'h120c', # B1c: Hand used by mum to throw a ball
                            'h120d', # B1d: Hand used by mum to hold racket
                            'h120e', # B1e: Hand used by mum to clean teeth
                            'h120f', # B1f: Hand used by mum to cut with knife
                            'h120g', # B1g: Hand used by mum for hammering
                            'h120h', # B1h: Hand used by mum to strike a match
                            'h120i', # B1i: Hand used by mum to use eraser
                            'h120j', # B1j: Hand used by mum to deal cards
                            'h120k', # B1k: Hand used by mum to thread needle
                            'h122a', # B2a: Foot used by mum to kick a ball
                            'h122b', # B2b: Foot used by mum to pick up pebble
                            'h122c', # B2c: Foot used by mum to step on insect
                            'h122d', # B2d: Foot used by mum to step onto chair
                            'h124a', # B3a: Eye used by mum to look in telescope
                            'h124b', # B3b: Eye used by mum to look into dark bottle
                            'h121a', # Handedness score grouped, 1=left, 2=mixed, 3=right
                            'h123a', # Footedness score grouped
                            'h125', # Eyedness score, 1=right, 5=left
                            
                            # Father
                            'pf9996a', # DV: Age of respondent at completion of questionnaire (years)
                            'pf2000', #B1a: Hand which partner uses normally to write
                            'pf2001', #B1b: Hand which partner uses to draw
                            'pf2002', #B1c: Hand which partner uses to throw a ball
                            'pf2003', #B1d: Hand which partner uses to hold a racket or bat
                            'pf2004', #B1e: Hand which partner uses to hold a toothbrush to clean teeth
                            'pf2005', #B1f: Hand which partner uses to hold a knife to cut things
                            'pf2006', #B1g: Hand which partner uses to hold a hammer when driving a nail
                            'pf2007', #B1h: Hand which partner uses to hold a match to strike it
                            'pf2008', #B1i: Hand which partner uses to hold a rubber to rub out a mark on paper
                            'pf2009', #B1j: Hand which partner uses to deal a pack of cards
                            'pf2010', #B1k: Hand which partner uses to hold the thread when threading a needle
                            'pf2020', #B2a: Foot which partner uses to kick a ball to someone
                            'pf2021', #B2b: Foot which partner uses to pick up a pebble with their toes
                            'pf2022', #B2c: Foot which partner uses to step on an insect or something similar
                            'pf2023', #B2d: Foot which partner puts on a chair first if you had to step onto the chair
                            'pf2030', #B3a: Eye which partner uses to look through a telescope
                            'pf2031', #B3b: Eye which partner uses to look into a dark bottle to see how full it was
                            
                            'f7ws013', # Hand he or she used to write with, 1 = right, 2 = left
                            'f7003b' # age in weeks
                            
  )]
  
  # Change names
  names(alspac) <- c("id", #cidB823
                     "id.twins", #qlet
                     "sex", #kz021
                     "ethnic", #c804
                     
                     # Child
                     "age.weeks", #kj999
                     "age.months", #kj999a
                     "hand.1", #kj660
                     "hand.2", #kj661
                     "hand.3", #kj662
                     "hand.4", #kj663
                     "hand.5", #kj664
                     "hand.6", #kj665
                     "foot.1", #kj670
                     "foot.2", #kj671
                     "foot.3", #kj672
                     "foot.4", #kj673
                     "eye.1", #kj680
                     "eye.2", #kj681
                     "hand.cat", #kj685
                     "foot.cat", #kj686
                     "eye.cat", #kj687
                     
                     "sensory.impairment",
                     "physical.disability",
                     
                     # Mother
                     "m.age", #h992
                     "m.hand.1", #h120a
                     "m.hand.2", #h120b
                     "m.hand.3", #h120c
                     "m.hand.4", #h120d
                     "m.hand.5", #h120e
                     "m.hand.6", #h120f
                     "m.hand.7", #h120g
                     "m.hand.8", #h120h
                     "m.hand.9", #h120i
                     "m.hand.10", #h120j
                     "m.hand.11", #h120k
                     "m.foot.1", #h122a
                     "m.foot.2", #h122b
                     "m.foot.3", #h122c
                     "m.foot.4", #h122d
                     "m.eye.1", #h124a
                     "m.eye.2", #h124b
                     "m.hand.cat", #h121a
                     "m.foot.cat", #h123a
                     "m.eye", #h125
                     
                     # Father
                     "p.age", #pf9996a
                     "p.hand.1", #pf2000
                     "p.hand.2", #pf2001
                     "p.hand.3", #pf2002
                     "p.hand.4", #pf2003
                     "p.hand.5", #pf2004
                     "p.hand.6", #pf2005
                     "p.hand.7", #pf2006
                     "p.hand.8", #pf2007
                     "p.hand.9", #pf2008
                     "p.hand.10", #pf2009
                     "p.hand.11", #pf2010
                     "p.foot.1", #pf2020
                     "p.foot.2", #pf2021
                     "p.foot.3", #pf2022
                     "p.foot.4", #pf2023
                     "p.eye.1", #pf2030
                     "p.eye.2", #pf2030
                     
                     "writing.hand",
                     "age.weeks.7"
                     
  )
  
  options(default.opt) #Set back the default values
  
  return(alspac)
}


normalize.alspac <- function(dataset) {
  
  # Purpose:  Generate mean values across laterality items
  #
  # Inputs:   dataset: Alspac dataframe with single items
  #
  # Output:   A normalized Alspac dataframe
  #
  
  norm.data <- dataset %>% mutate(
    
    # Mutate categorical variables as factors
    sex = as.factor(sex),
    id.twins = as.character(id.twins),
    #writing.hand = ifelse(writing.hand == 8, NA, writing.hand),
    ID_1 = id,
    ID_2 = as.character(paste0(id, id.twins))) %>% 
    mutate_if(.predicate = is.factor, .funs = funs('levels<-'(., trimws(levels(.)))) #Trim factor-levels' string
              
    ) %>% mutate ( 
      
      # before: 1=left, 3=either, 2=right
      # after: 1=left, 2=either, 3=right
      
      # Child
      hand.1 = as.numeric(recode(hand.1, '3'='2', '2'='3', '1'='1')),
      hand.2 = as.numeric(recode(hand.2, '3'='2', '2'='3', '1'='1')),
      hand.3 = as.numeric(recode(hand.3, '3'='2', '2'='3', '1'='1')),
      hand.4 = as.numeric(recode(hand.4, '3'='2', '2'='3', '1'='1')),
      hand.5 = as.numeric(recode(hand.5, '3'='2', '2'='3', '1'='1')),
      hand.6 = as.numeric(recode(hand.6, '3'='2', '2'='3', '1'='1')),
      foot.1 = as.numeric(recode(foot.1, '3'='2', '2'='3', '1'='1')),
      foot.2 = as.numeric(recode(foot.2, '3'='2', '2'='3', '1'='1')),
      foot.3 = as.numeric(recode(foot.3, '3'='2', '2'='3', '1'='1')),
      foot.4 = as.numeric(recode(foot.4, '3'='2', '2'='3', '1'='1')),
      eye.1 = as.numeric(recode(eye.1, '3'='2', '2'='3', '1'='1')),
      eye.2 = as.numeric(recode(eye.2, '3'='2', '2'='3', '1'='1')),
      
      # Mother
      m.hand.1 = as.numeric(recode(m.hand.1, '3'='2', '2'='3', '1'='1')),
      m.hand.2 = as.numeric(recode(m.hand.2, '3'='2', '2'='3', '1'='1')),
      m.hand.3 = as.numeric(recode(m.hand.3, '3'='2', '2'='3', '1'='1')),
      m.hand.4 = as.numeric(recode(m.hand.4, '3'='2', '2'='3', '1'='1')),
      m.hand.5 = as.numeric(recode(m.hand.5, '3'='2', '2'='3', '1'='1')),
      m.hand.6 = as.numeric(recode(m.hand.6, '3'='2', '2'='3', '1'='1')),
      m.hand.7 = as.numeric(recode(m.hand.7, '3'='2', '2'='3', '1'='1')),
      m.hand.8 = as.numeric(recode(m.hand.8, '3'='2', '2'='3', '1'='1')),
      m.hand.9 = as.numeric(recode(m.hand.9, '3'='2', '2'='3', '1'='1')),
      m.hand.10 = as.numeric(recode(m.hand.10, '3'='2', '2'='3', '1'='1')),
      m.hand.11 = as.numeric(recode(m.hand.11, '3'='2', '2'='3', '1'='1')),
      m.foot.1 = as.numeric(recode(m.foot.1, '3'='2', '2'='3', '1'='1')),
      m.foot.2 = as.numeric(recode(m.foot.2, '3'='2', '2'='3', '1'='1')),
      m.foot.3 = as.numeric(recode(m.foot.3, '3'='2', '2'='3', '1'='1')),
      m.foot.4 = as.numeric(recode(m.foot.4, '3'='2', '2'='3', '1'='1')),
      m.eye.1 = as.numeric(recode(m.eye.1, '3'='2', '2'='3', '1'='1')),
      m.eye.2 = as.numeric(recode(m.eye.2, '3'='2', '2'='3', '1'='1')),
      
      # Father
      p.hand.1 = as.numeric(recode(p.hand.1, '3'='2', '2'='3', '1'='1')),
      p.hand.2 = as.numeric(recode(p.hand.2, '3'='2', '2'='3', '1'='1')),
      p.hand.3 = as.numeric(recode(p.hand.3, '3'='2', '2'='3', '1'='1')),
      p.hand.4 = as.numeric(recode(p.hand.4, '3'='2', '2'='3', '1'='1')),
      p.hand.5 = as.numeric(recode(p.hand.5, '3'='2', '2'='3', '1'='1')),
      p.hand.6 = as.numeric(recode(p.hand.6, '3'='2', '2'='3', '1'='1')),
      p.hand.7 = as.numeric(recode(p.hand.7, '3'='2', '2'='3', '1'='1')),
      p.hand.8 = as.numeric(recode(p.hand.8, '3'='2', '2'='3', '1'='1')),
      p.hand.9 = as.numeric(recode(p.hand.9, '3'='2', '2'='3', '1'='1')),
      p.hand.10 = as.numeric(recode(p.hand.10, '3'='2', '2'='3', '1'='1')),
      p.hand.11 = as.numeric(recode(p.hand.11, '3'='2', '2'='3', '1'='1')),
      p.foot.1 = as.numeric(recode(p.foot.1, '3'='2', '2'='3', '1'='1')),
      p.foot.2 = as.numeric(recode(p.foot.2, '3'='2', '2'='3', '1'='1')),
      p.foot.3 = as.numeric(recode(p.foot.3, '3'='2', '2'='3', '1'='1')),
      p.foot.4 = as.numeric(recode(p.foot.4, '3'='2', '2'='3', '1'='1')),
      p.eye.1 = as.numeric(recode(p.eye.1, '3'='2', '2'='3', '1'='1')),
      p.eye.2 = as.numeric(recode(p.eye.2, '3'='2', '2'='3', '1'='1'))
      
    ) %>% mutate(
      
      hand.sum = rowSums(.[,7:12], na.rm = TRUE),
      foot.sum = rowSums(.[,13:16], na.rm = TRUE),
      eye.sum = rowSums(.[,17:18], na.rm = TRUE),
      m.hand.sum = rowSums(.[,25:35], na.rm = TRUE),
      m.foot.sum = rowSums(.[,36:39], na.rm = TRUE),
      m.eye.sum = rowSums(.[,40:41], na.rm = TRUE),
      p.hand.sum = rowSums(.[,46:56], na.rm = TRUE),
      p.foot.sum = rowSums(.[,57:60], na.rm = TRUE),
      p.eye.sum = rowSums(.[,61:62], na.rm = TRUE)
      
    ) %>% mutate(
      
      hand.mean = rowMeans(.[,7:12], na.rm = TRUE),
      foot.mean = rowMeans(.[,13:16], na.rm = TRUE),
      eye.mean = rowMeans(.[,17:18], na.rm = TRUE),
      m.hand.mean = rowMeans(.[,25:35], na.rm = TRUE),
      m.foot.mean = rowMeans(.[,36:39], na.rm = TRUE),
      m.eye.mean = rowMeans(.[,40:41], na.rm = TRUE),
      p.hand.mean = rowMeans(.[,46:56], na.rm = TRUE),
      p.foot.mean = rowMeans(.[,57:60], na.rm = TRUE),
      p.eye.mean = rowMeans(.[,61:62], na.rm = TRUE)
      
    ) %>% mutate(
      
      hand = ifelse(test = hand.mean < 1.5, yes = 1, no = 0),
      hand = ifelse(test = hand.mean <= 2.6 & hand.mean >= 1.5, yes = 2, no = hand),
      hand = ifelse(test = hand.mean > 2.6, yes = 3, no = hand),
      foot = ifelse(test = foot.mean < 1.5, yes = 1, no = 0),
      foot = ifelse(test = foot.mean < 2.6 & foot.mean >= 1.5, yes = 2, no = foot),
      foot = ifelse(test = foot.mean >= 2.6, yes = 3, no = foot),
      eye = ifelse(test = eye.mean < 1.5, yes = 1, no = 0),
      eye = ifelse(test = eye.mean < 2.6 & eye.mean >= 1.5, yes = 2, no = eye),
      eye = ifelse(test = eye.mean >= 2.6, yes = 3, no = eye),
      
      m.hand = ifelse(test = m.hand.mean < 1.5, yes = 1, no = 0),
      m.hand = ifelse(test = m.hand.mean <= 2.6 & m.hand.mean >= 1.5, yes = 2, no = m.hand),
      m.hand = ifelse(test = m.hand.mean > 2.6, yes = 3, no = m.hand),
      m.foot = ifelse(test = m.foot.mean < 1.5, yes = 1, no = 0),
      m.foot = ifelse(test = m.foot.mean < 2.6 & m.foot.mean >= 1.5, yes = 2, no = m.foot),
      m.foot = ifelse(test = m.foot.mean >= 2.6, yes = 3, no = m.foot),
      m.eye = ifelse(test = m.eye.mean < 1.5, yes = 1, no = 0),
      m.eye = ifelse(test = m.eye.mean < 2.6 & m.eye.mean >= 1.5, yes = 2, no = m.eye),
      m.eye = ifelse(test = m.eye.mean >= 2.6, yes = 3, no = m.eye),
      
      p.hand = ifelse(test = p.hand.mean < 1.5, yes = 1, no = 0),
      p.hand = ifelse(test = p.hand.mean <= 2.6 & p.hand.mean >= 1.5, yes = 2, no = p.hand),
      p.hand = ifelse(test = p.hand.mean > 2.6, yes = 3, no = p.hand),
      p.foot = ifelse(test = p.foot.mean < 1.5, yes = 1, no = 0),
      p.foot = ifelse(test = p.foot.mean < 2.6 & p.foot.mean >= 1.5, yes = 2, no = p.foot),
      p.foot = ifelse(test = p.foot.mean >= 2.6, yes = 3, no = p.foot),
      p.eye = ifelse(test = p.eye.mean < 1.5, yes = 1, no = 0),
      p.eye = ifelse(test = p.eye.mean < 2.6 & p.eye.mean >= 1.5, yes = 2, no = p.eye),
      p.eye = ifelse(test = p.eye.mean >= 2.6, yes = 3, no = p.eye),
      
    ) %>% mutate(
      
      both.hand = ifelse(test = m.hand == 3 & p.hand == 3, yes = 0, no = -9),
      both.hand = ifelse(test = m.hand == 3 & p.hand == 2, yes = 1, no = both.hand),
      both.hand = ifelse(test = m.hand == 2 & p.hand == 3, yes = 1, no = both.hand),
      both.hand = ifelse(test = m.hand == 3 & p.hand == 1, yes = 2, no = both.hand),
      both.hand = ifelse(test = m.hand == 1 & p.hand == 3, yes = 2, no = both.hand),
      both.hand = ifelse(test = m.hand == 2 & p.hand == 2, yes = 3, no = both.hand),
      both.hand = ifelse(test = m.hand == 2 & p.hand == 1, yes = 4, no = both.hand),
      both.hand = ifelse(test = m.hand == 1 & p.hand == 2, yes = 4, no = both.hand),
      both.hand = ifelse(test = m.hand == 1 & p.hand == 1, yes = 5, no = both.hand),
      
      both.foot = ifelse(test = m.foot == 3 & p.foot == 3, yes = 0, no = -9),
      both.foot = ifelse(test = m.foot == 3 & p.foot == 2, yes = 1, no = both.foot),
      both.foot = ifelse(test = m.foot == 2 & p.foot == 3, yes = 1, no = both.foot),
      both.foot = ifelse(test = m.foot == 3 & p.foot == 1, yes = 2, no = both.foot),
      both.foot = ifelse(test = m.foot == 1 & p.foot == 3, yes = 2, no = both.foot),
      both.foot = ifelse(test = m.foot == 2 & p.foot == 2, yes = 3, no = both.foot),
      both.foot = ifelse(test = m.foot == 2 & p.foot == 1, yes = 4, no = both.foot),
      both.foot = ifelse(test = m.foot == 1 & p.foot == 2, yes = 4, no = both.foot),
      both.foot = ifelse(test = m.foot == 1 & p.foot == 1, yes = 5, no = both.foot),
      
      both.eye = ifelse(test = m.eye == 3 & p.eye == 3, yes = 0, no = -9),
      both.eye = ifelse(test = m.eye == 3 & p.eye == 2, yes = 1, no = both.eye),
      both.eye = ifelse(test = m.eye == 2 & p.eye == 3, yes = 1, no = both.eye),
      both.eye = ifelse(test = m.eye == 3 & p.eye == 1, yes = 2, no = both.eye),
      both.eye = ifelse(test = m.eye == 1 & p.eye == 3, yes = 2, no = both.eye),
      both.eye = ifelse(test = m.eye == 2 & p.eye == 2, yes = 3, no = both.eye),
      both.eye = ifelse(test = m.eye == 2 & p.eye == 1, yes = 4, no = both.eye),
      both.eye = ifelse(test = m.eye == 1 & p.eye == 2, yes = 4, no = both.eye),
      both.eye = ifelse(test = m.eye == 1 & p.eye == 1, yes = 5, no = both.eye)
      
    ) %>% mutate(
      
      hand.lr = ifelse(test = hand.mean <= 2, yes = 1, no = NA),
      foot.lr = ifelse(test = foot.mean <= 2, yes = 1, no = NA),
      eye.lr = ifelse(test = eye.mean <= 2, yes = 1, no = NA),
      
      m.hand.lr = ifelse(test = m.hand.mean <= 2, yes = 1, no = NA),
      m.foot.lr = ifelse(test = m.foot.mean <= 2, yes = 1, no = NA),
      m.eye.lr = ifelse(test = m.eye.mean <= 2, yes = 1, no = NA),
      
      p.hand.lr = ifelse(test = p.hand.mean <= 2, yes = 1, no = NA),
      p.foot.lr = ifelse(test = p.foot.mean <= 2, yes = 1, no = NA),
      p.eye.lr = ifelse(test = p.eye.mean <= 2, yes = 1, no = NA)

    ) %>% mutate(
      
      hand.lr = ifelse(test = hand.mean > 2, yes = 0, no = hand.lr),
      foot.lr = ifelse(test = foot.mean > 2, yes = 0, no = foot.lr),
      eye.lr = ifelse(test = eye.mean > 2, yes = 0, no = eye.lr),
      
      m.hand.lr = ifelse(test = m.hand.mean > 2, yes = 0, no = m.hand.lr),
      m.foot.lr = ifelse(test = m.foot.mean > 2, yes = 0, no = m.foot.lr),
      m.eye.lr = ifelse(test = m.eye.mean > 2, yes = 0, no = m.eye.lr),
      
      p.hand.lr = ifelse(test = p.hand.mean > 2, yes = 0, no = p.hand.lr),
      p.foot.lr = ifelse(test = p.foot.mean > 2, yes = 0, no = p.foot.lr),
      p.eye.lr = ifelse(test = p.eye.mean > 2, yes = 0, no = p.eye.lr)
      
      
    ) %>% mutate(
      
      both.hand.lr = ifelse(test = p.hand.lr == 1 & m.hand.lr == 1, yes = 2, no = NA),
      both.hand.lr = ifelse(test = p.hand.lr == 1 & m.hand.lr == 0, yes = 1, no = both.hand.lr),
      both.hand.lr = ifelse(test = p.hand.lr == 0 & m.hand.lr == 1, yes = 1, no = both.hand.lr),
      both.hand.lr = ifelse(test = p.hand.lr == 0 & m.hand.lr == 0, yes = 0, no = both.hand.lr),
      
      both.foot.lr = ifelse(test = p.foot.lr == 1 & m.foot.lr == 1, yes = 2, no = NA),
      both.foot.lr = ifelse(test = p.foot.lr == 1 & m.foot.lr == 0, yes = 1, no = both.foot.lr),
      both.foot.lr = ifelse(test = p.foot.lr == 0 & m.foot.lr == 1, yes = 1, no = both.foot.lr),
      both.foot.lr = ifelse(test = p.foot.lr == 0 & m.foot.lr == 0, yes = 0, no = both.foot.lr),
      
      both.eye.lr = ifelse(test = p.eye.lr == 1 & m.eye.lr == 1, yes = 2, no = NA),
      both.eye.lr = ifelse(test = p.eye.lr == 1 & m.eye.lr == 0, yes = 1, no = both.eye.lr),
      both.eye.lr = ifelse(test = p.eye.lr == 0 & m.eye.lr == 1, yes = 1, no = both.eye.lr),
      both.eye.lr = ifelse(test = p.eye.lr == 0 & m.eye.lr == 0, yes = 0, no = both.eye.lr),
      
    ) %>% dplyr::select(ID_1,
                        ID_2,
                        id.twins,
                        physical.disability,
                        sensory.impairment,
                        sex,
                        # Child
                        age.weeks,
                        hand,
                        hand.cat,
                        hand.1,
                        hand.2,
                        hand.3,
                        hand.4,
                        hand.5,
                        hand.6,
                        foot,
                        foot.cat,
                        foot.1,
                        foot.2,
                        foot.3,
                        foot.4,
                        eye,
                        eye.cat,
                        eye.1,
                        eye.2,
                        # Mother
                        m.age,
                        m.hand,
                        m.hand.cat,
                        m.hand.1,
                        m.hand.2,
                        m.hand.3,
                        m.hand.4,
                        m.hand.5,
                        m.hand.6,
                        m.hand.7,
                        m.hand.8,
                        m.hand.9,
                        m.hand.10,
                        m.hand.11,
                        m.foot,
                        m.foot.cat,
                        m.foot.1,
                        m.foot.2,
                        m.foot.3,
                        m.foot.4,
                        m.eye,
                        m.eye.1,
                        m.eye.2,
                        # Father
                        p.age,
                        p.hand,
                        p.hand.1,
                        p.hand.2,
                        p.hand.3,
                        p.hand.4,
                        p.hand.5,
                        p.hand.6,
                        p.hand.7,
                        p.hand.8,
                        p.hand.9,
                        p.hand.10,
                        p.hand.11,
                        p.foot,
                        p.foot.1,
                        p.foot.2,
                        p.foot.3,
                        p.foot.4,
                        p.eye,
                        p.eye.1,
                        p.eye.2,
                        # Both
                        both.hand,
                        both.foot,
                        both.eye,
                        # Writing hand
                        writing.hand,
                        age.weeks.7,
                        # LR
                        hand.lr,
                        foot.lr,
                        eye.lr,
                        m.hand.lr,
                        m.foot.lr,
                        m.eye.lr,
                        p.hand.lr,
                        p.foot.lr,
                        p.eye.lr,
                        both.hand.lr,
                        both.foot.lr,
                        both.eye.lr,
                        # Mean
                        hand.mean,
                        m.hand.mean,
                        p.hand.mean,
                        foot.mean,
                        m.foot.mean,
                        p.foot.mean,
                        eye.mean,
                        m.eye.mean,
                        p.eye.mean) %>%
    
    filter(!is.na(hand) | !is.na(foot) | !is.na(eye))
  
  return(norm.data)
  
}


clean.alspac <- function(mydata) {
  
  # Purpose:  Filter and recode items
  #
  # Inputs:   mydata: an Alspac dataframe
  #
  # Output:   A cleaned Alspac dataframe
  #
  
  working.data <- mydata %>% 
    # hand/foot
    filter(id.twins == "A") %>%
    filter(is.na(physical.disability) | physical.disability != 1 ) %>%
    # eye
    filter(is.na(sensory.impairment) | sensory.impairment != 1 ) %>% mutate(
    
    hand = as.factor(recode(hand, "3" = "0", "2" = "1", "1" = "2")),
    m.hand = as.factor(recode(m.hand, "3" = "0", "2" = "1", "1" = "2")),
    p.hand = as.factor(recode(p.hand, "3" = "0", "2" = "1", "1" = "2")),
    
    foot = as.factor(recode(foot, "3" = "0", "2" = "1", "1" = "2")),
    m.foot = as.factor(recode(m.foot, "3" = "0", "2" = "1", "1" = "2")),
    p.foot = as.factor(recode(p.foot, "3" = "0", "2" = "1", "1" = "2")),
    
    eye = as.factor(recode(eye, "3" = "0", "2" = "1", "1" = "2")),
    m.eye = as.factor(recode(m.eye, "3" = "0", "2" = "1", "1" = "2")),
    p.eye = as.factor(recode(p.eye, "3" = "0", "2" = "1", "1" = "2")),
    
    both.hand = as.factor(both.hand),
    both.foot = as.factor(both.foot),
    both.eye = as.factor(both.eye),
    
    both.hand.lr = as.factor(both.hand.lr),
    both.foot.lr = as.factor(both.foot.lr),
    both.eye.lr = as.factor(both.eye.lr)
    
    ) %>% select(
      -c(physical.disability, sensory.impairment)
    )
  
  working.data[working.data == -9999] <- NA
  
  return(working.data)
  
}
