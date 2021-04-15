
library(foreign) # for read.spss()
library(tidyverse) # for read.spss()
library(xlsx) # export dataframes to xlsx files
library(summarytools)
library(BBmisc) # for z standardisation
library(RNOmni) # for rank normalization
library(Hmisc) # for rcorr
library(corrplot) # for corrplot
library(RColorBrewer)

read.alspac <- function(mypath = 'C:\\Users\\Judith\\sciebo\\St Andrews\\projects\\gwas\\data_preparation\\data\\Paracchini_03Apr19.sav') { #Paracchini_16Jan19.sav

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
                            'kz021', # Gender; 1 = male, 2 = female, -1 = not known
                            'c804', # child's ethnic background

                            # Cognitive measures
                            'f7003b', # Age (weeks) at Focus @ 7 visit
                            'f7ws076', # Reading score: Word: F7
                            'f7ws116', # Spelling score (no. correct): Word: 
                            
                            'f8003b', # Age (weeks) at Focus @ 8 visit
                            'f8ws112', # WISC total IQ (age corrected) - age 8
                            'f8ws110', # WISC verbal IQ (age corrected) - age 8
                            'f8ws111', # WISC performance IQ (age corrected) - age 8
                            'f8sl105', # nonword rep
                            'f8sl040', # WOLD comprehension
                            
                            'f9003b', # Age (weeks) at F9 visit
                            'f9mw031', # reading score at age 9

                            # Laterality
                            'kj999', # age at completion in weeks
                            
                            'kjhand', # Handedness 42 mths
                            'kjfoot', # Footedness at 42 months
                            'kjeyed', # Eyedness at 42 months
                            
                            'kj660', # hand used for drawing
                            'kj661', # hand used for throwing
                            'kj662', # hand used to colour in 
                            'kj663', # hand used to hold toothbrush
                            'kj664', # hand used to cut
                            'kj665', # hand used to hit
                            
                            'kj670', # foot used to kick
                            'kj671', # foot used to pick up pebble
                            'kj672', # foot used to stamp
                            'kj673', # foot used to climb
                           
                            'kj680', # eye used to look through hole
                            'kj681', # eye used to look through bottle
                            
                            # Control variables child
                            'sa036a', # Child has ever had sensory impairment (visual) (2=no)
                            'sa037a', # Child has ever had physical disabilities (2=no)
                            
                            'f7ws013' # Hand he or she used to write with, 1 = right, 2 = left
  )]
  
  # Change names
  names(alspac) <- c("id", #cidB823
                     "id.twins", #qlet
                     "sex", #kz021
                     "ethnic", #c804

                     "age.weeks.7", #f7003b at 7
                     "read.7", #f7ws076
                     "spell.7", #f7ws116
                     
                     "age.weeks.8", #f8003b
                     "total.iq.8", #f8ws112 at 8
                     "verbal.iq.8", #f8ws110 at 8
                     "performance.iq.8", #f8ws111 at 8
                     "short.term.mem.8", #f8sl105
                     "listening.comp.8", #f8sl040
                     
                     "age.weeks.9", #f9003b
                     "read.9", #f9mw031
                    
                     "age.weeks.42", #kj999

                     "hand.cat", #kjhand
                     "foot.cat", #kjfoot
                     "eye.cat", #kjeye
                     
                     "hand.draw", #kj660
                     "hand.throw", #kj661
                     "hand.colour", #kj662
                     "hand.hold", #kj663
                     "hand.cut", #kj664
                     "hand.hit", #kj665
                     
                     "foot.kick", #kj670
                     "foot.pick", #kj671
                     "foot.stamp", #kj672
                     "foot.climb", #kj673
                     
                     "eye.hole", #kj680
                     "eye.bottle", #kj681

                     "sensory.impairment",
                     "physical.disability",
                     
                     "writing.hand"

  )
  
  options(default.opt) #Set back the default values
  
  return(alspac)
}


clean.alspac <- function(mydata) {
  
  working.data <- mydata %>% 
    filter(is.na(physical.disability) | physical.disability != 1 ) %>%
    filter(is.na(sensory.impairment) | sensory.impairment != 1 ) %>%
    # Mutate categorical variables as factors
    mutate(sex = as.factor(sex),
           id.twins = as.character(id.twins),
           ID_1 = as.character(paste0(id, id.twins))) %>% 
    mutate_if(.predicate = is.factor, .funs = funs('levels<-'(., trimws(levels(.))))) #Trim factor-levels' string
  
  working.data[working.data == -9999] <- NA
  
  return(working.data)
  
}


normalize.alspac <- function(dataset) {

  norm.data <- dataset %>% mutate(
    
    # before: 1 left, 2 mixed, 3 right
    # new: 2 left, 1 mixed, 0 right
    hand.cat = as.numeric(recode(hand.cat, '3'='0', '2'='1', '1'='2')),
    foot.cat = as.numeric(recode(foot.cat, '3'='0', '2'='1', '1'='2')),
    eye.cat = as.numeric(recode(eye.cat, '3'='0', '2'='1', '1'='2')), 
    # before: 1 left, 3 either, 2 right
    # new: 2 left, 1 mixed, 0 right  
    hand.draw = as.numeric(recode(hand.draw, '3'='1', '2'='0', '1'='2')), 
    hand.throw = as.numeric(recode(hand.throw, '3'='1', '2'='0', '1'='2')), 
    hand.colour = as.numeric(recode(hand.colour, '3'='1', '2'='0', '1'='2')), 
    hand.hold = as.numeric(recode(hand.hold, '3'='1', '2'='0', '1'='2')), 
    hand.cut = as.numeric(recode(hand.cut, '3'='1', '2'='0', '1'='2')), 
    hand.hit = as.numeric(recode(hand.hit, '3'='1', '2'='0', '1'='2')), 
    foot.kick = as.numeric(recode(foot.kick, '3'='1', '2'='0', '1'='2')), 
    foot.pick = as.numeric(recode(foot.pick, '3'='1', '2'='0', '1'='2')), 
    foot.stamp = as.numeric(recode(foot.stamp, '3'='1', '2'='0', '1'='2')), 
    foot.climb = as.numeric(recode(foot.climb, '3'='1', '2'='0', '1'='2')), 
    eye.hole = as.numeric(recode(eye.hole, '3'='1', '2'='0', '1'='2')), 
    eye.bottle = as.numeric(recode(eye.bottle, '3'='1', '2'='0', '1'='2'))
    
    ) %>% dplyr::select(
           ID_1,
           id,
           id.twins,
           sex,
           writing.hand,
           # cognitive measures
           age.weeks.7,
           read.7,
           spell.7,
           age.weeks.8,
           total.iq.8,
           verbal.iq.8,
           performance.iq.8,
           short.term.mem.8,
           listening.comp.8,
           age.weeks.9,
           read.9,
           # laterality
           age.weeks.42,
           hand.cat,
           foot.cat,
           eye.cat,
           hand.draw, 
           hand.throw, 
           hand.colour, 
           hand.hold, 
           hand.cut, 
           hand.hit, 
           foot.kick, 
           foot.pick, 
           foot.stamp, 
           foot.climb, 
           eye.hole, 
           eye.bottle)

  return(norm.data)

}

