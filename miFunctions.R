# ------------------------------------------------------------------------------
# Program: miFunctions.R  
#  Author: Hermine Maes
#    Date: 02 25 2016 
#
# Set of my options & functions used in basic twin methodology scripts
#   Email: hmaes@vcu.edu
# -------|---------|---------|---------|---------|---------|---------|---------|

# Options
mxOption( NULL, "Default optimizer", "NPSOL" )
#mxOption( NULL, "Checkpoint Prefix", filename )
mxOption( NULL, "Checkpoint Units", "iterations" )
mxOption( NULL, "Checkpoint Count", 1 )
options(width=120)
options(digits=8)
mxVersion()

# Functions to assign labels
labLower  <- function(lab,nv) { paste(lab,rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="_") }
labSdiag  <- function(lab,nv) { paste(lab,rev(nv+1-sequence(1:(nv-1))),rep(1:(nv-1),(nv-1):1),sep="_") }
labFullSq <- function(lab,nv) { paste(lab,1:nv,rep(1:nv,each=nv),sep="_") }
labDiag   <- function(lab,nv) { paste(lab,1:nv,1:nv,sep="_") } 
labSymm   <- function(lab,nv) { paste(lab,rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="_") }
labFull   <- function(lab,nr,nc) { paste(lab,1:nr,rep(1:nc,each=nr),sep="_") }

# Functions to assign values
valDiag   <- function(dim, valD) {
valF      <- diag(valD,dim,dim)        # values for diagonal of covariance matrix
valF
}
valODiag  <- function(dim, valD, valOD) {
valF      <- diag(valD,dim,dim)        # values for diagonal of covariance matrix
valF[lower.tri(valF)] <- valOD       # values for below diagonal elements 
valF[upper.tri(valF)] <- valOD       # values for above diagonal elements
valF
}
valLUDiag <- function(dim, valD, valLD, valUD) {
valF      <- diag(valD,dim,dim)        # values for diagonal of covariance matrix
valF[lower.tri(valF)] <- valLD       # values for below diagonal elements 
valF[upper.tri(valF)] <- valUD       # values for above diagonal elements
valF
}

myMean    <- function(x) { mean(as.numeric(x),na.rm=TRUE) }
myCov     <- function(x) { cov(as.numeric(x),use="complete.obs") }
myCor     <- function(x) { cor(as.numeric(x),use="everything") }

# Functions to generate output

fitGofs   <- function(fit) {
          summ <- summary(fit)
          cat(paste("Mx:", fit$name,"  os=", summ$ob,"  ns=", summ$nu,"   ep=", summ$es,
                    "   co=", sum(summ$cons),"  df=", summ$de, "  ll=", round(summ$Mi,4), 
                    "  cpu=", round(summ$cpu,4),"  opt=", summ$op,"  ver=", summ$mx,
                    "  stc=", fit$output$status$code, "\n",sep=""))
}

fitGofS   <- function(fit) {
          summ <- summary(fit)
          cat(paste("Mx:", fit$name,"  #statistics=", summ$ob,"  #records=", summ$nu,"   #parameters=", summ$es,
                    "   #constraints=", sum(summ$cons),"  df=", summ$de, "  -2LL=", round(summ$Mi,4), 
                    "  cpu=", round(summ$cpu,4),"  optim=", summ$op,"  version=", summ$mx,
                    "  code=", fit$output$status$code, "\n",sep=""))
}

fitEsts   <- function(fit) {
	 print(round(fit$output$estimate,4)) 
	 print(round(fit$VC$result,4))
	 round(fit$output$confidenceIntervals,4)
}

fitEstS   <- function(fit) {
	 print(round(fit$output$estimate,4)) 
	 print(round(fit$VCf$result,4))
	 print(round(fit$VCm$result,4))
	 round(fit$output$confidenceIntervals,4)
}

		 
fitExpc   <- function(fit) {
print(round(fit$MZ$meanMZ$values,4))
print(round(fit$DZ$meanDZ$values,4))
print(round(fit$MZ$covMZ$values,4))
print(round(fit$DZ$covDZ$values,4)) }

fitExpb   <- function(fit) {
print(round(fit$MZ$threMZ$values,4))
print(round(fit$DZ$threDZ$values,4))
print(round(fit$MZ$corMZ$values,4))
print(round(fit$DZ$corDZ$values,4)) }

fitExpo   <- function(fit) {
print(round(fit$MZ$thinMZ$values,4))
print(round(fit$DZ$thinDZ$values,4))
print(round(fit$MZ$threMZ$values,4))
print(round(fit$DZ$threDZ$values,4))
print(round(fit$MZ$corMZ$values,4))
print(round(fit$DZ$corDZ$values,4)) }

fitExpm   <- function(fit) {
print(round(fit$MZ$thinMZ$values,4))
print(round(fit$DZ$thinDZ$values,4))
print(round(fit$MZ$threMZ$values,4))
print(round(fit$DZ$threDZ$values,4))
print(round(fit$MZ$corMZ$values,4))
print(round(fit$DZ$corDZ$values,4))
print(round(fit$MZ$meanMZ$values,4))
print(round(fit$DZ$meanDZ$values,4))
print(round(fit$MZ$covMZ$values,4))
print(round(fit$DZ$covDZ$values,4)) }
