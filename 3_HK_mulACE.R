# ------------------------------------------------------------------------------
# Twin Multivariate ACE model to estimate causes of variation across multiple groups
# Matrix style model - Raw data - Continuous data
#
# This program was adapted from the script 'mulACEc.R' by Hermine Maes on
# THE 2020 INTERNATIONAL WORKSHOP ON STATISTICAL GENETIC METHODS FOR HUMAN COMPLEX TRAITS
# -------|---------|---------|---------|---------|---------|---------|---------|

# Load Libraries & Options
library(OpenMx)
library(psych)
source("miFunctions.R")

# Create Output 
filename    <- "mulACEc"
sink(paste(filename,".Ro",sep=""), append=FALSE, split=TRUE)

# ------------------------------------------------------------------------------

# Load Data
FootData <- read.table(file="HKTWINS", header=TRUE, sep="\t")

# This is the PegQ score using 5-trial means
FootData[,'X1'] <- FootData[,'ehi_resid_61']
FootData[,'X2'] <- FootData[,'ehi_resid_62']

FootData[,'Y1'] <- FootData[,'foot1']
FootData[,'Y2'] <- FootData[,'foot2']

FootData[,'Z1'] <- FootData[,'eye1']
FootData[,'Z2'] <- FootData[,'eye2']


describe(FootData[,c('X1','X2')])
describe(FootData[,c('Y1','Y2')])
describe(FootData[,c('Z1','Z2')])

table(FootData$Y1)

# Select Variables for Analysis
vars      <- c('X','Y', 'Z')           # list of variables names
nv        <- 3                         # number of variables
ntv       <- nv*2                      # number of total variables
selVars   <- paste(vars,c(rep("1",nv),rep("2",nv)),sep="")

# Select Data for Analysis
mzData    <- subset(FootData, ZygTwin==1, selVars)
dzData    <- subset(FootData, ZygTwin==2, selVars)

# Generate Descriptive Statistics
colMeans(mzData,na.rm=TRUE)
colMeans(dzData,na.rm=TRUE)
cov(mzData,use="complete")
cov(dzData,use="complete")
cor(mzData,use="complete")
cor(dzData,use="complete")


# Set Starting Values 
svMe      <- c(0.1,0.1,0.1)                   # start value for means
svVa      <- c(1,1,1)                  # start value for variances

#svMe      <- c(-1,1)                   # start value for means for EHI Blom score
#svVa      <- c(80,10)                  # start value for variances for EHI Blom score
svVas     <- diag(svVa,ntv,ntv)        # assign start values to diagonal of matrix
lbVa      <- .0001                    # start value for lower bounds
lbVas     <- diag(lbVa,ntv,ntv)        # assign lower bounds values to diagonal of matrix


# Generate Descriptive Statistics
round(colMeans(mzData,na.rm=TRUE),4)
round(colMeans(dzData,na.rm=TRUE),4)
round(cov(mzData,use="complete"),4)
round(cov(dzData,use="complete"),4)
round(cor(mzData,use="complete"),4)
round(cor(dzData,use="complete"),4)


# Create Labels
labMeMZ   <- paste("meanMZ",selVars,sep="_")
labMeDZ   <- paste("meanDZ",selVars,sep="_")
labMeZ    <- paste("meanZ",selVars,sep="_")
labCvMZ   <- labLower("covMZ",ntv)
labCvDZ   <- labLower("covDZ",ntv)
labCvZ    <- labLower("covZ",ntv)
labVaMZ   <- labDiag("covMZ",ntv)
labVaDZ   <- labDiag("covDZ",ntv)
labVaZ    <- labDiag("covZ",ntv)

# ------------------------------------------------------------------------------
# PREPARE MODEL

# Saturated Model
# Create Algebra for expected Mean Matrices
meanMZ    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labMeMZ, name="meanMZ" )
meanDZ    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labMeDZ, name="meanDZ" )

# Create Algebra for expected Variance/Covariance Matrices
covMZ     <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=labCvMZ, name="covMZ" )
covDZ     <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=labCvDZ, name="covDZ" )

# Create Data Objects for Multiple Groups
dataMZ    <- mxData( observed=mzData, type="raw" )
dataDZ    <- mxData( observed=dzData, type="raw" )

# Create Expectation Objects for Multiple Groups
expMZ     <- mxExpectationNormal( covariance="covMZ", means="meanMZ", dimnames=selVars )
expDZ     <- mxExpectationNormal( covariance="covDZ", means="meanDZ", dimnames=selVars )
funML     <- mxFitFunctionML()

# Create Model Objects for Multiple Groups
modelMZ   <- mxModel( "MZ", meanMZ, covMZ, dataMZ, expMZ, funML )
modelDZ   <- mxModel( "DZ", meanDZ, covDZ, dataDZ, expDZ, funML )
multi     <- mxFitFunctionMultigroup( c("MZ","DZ") )

# Create Confidence Interval Objects
ciCov     <- mxCI( c('MZ.covMZ','DZ.covDZ') )
ciMean    <- mxCI( c('MZ.meanMZ','DZ.meanDZ') )

# Build Saturated Model with Confidence Intervals
modelSAT  <- mxModel( "mulSATc", modelMZ, modelDZ, multi, ciCov, ciMean )

# ------------------------------------------------------------------------------
# RUN MODEL

# Run Saturated Model
fitSAT    <- mxRun( modelSAT, intervals=F )
fit       <- mxTryHard( fitSAT )
sumSAT    <- summary( fitSAT )
fitGofs(fitSAT)
mxGetExpected(fitSAT, c("means","covariance"))


# ------------------------------------------------------------------------------
#
# Cholesky ACE Models Start Here
#
# ------------------------------------------------------------------------------

# Set Starting Values 
svMe      <- round(colMeans(mzData,na.rm=TRUE)[1:nv],1)           # start value for means
svPa      <- .6                        # start value for path coefficient
svPe      <- .6                        # start value for path coefficient for e
lbPa      <- .0001                     # lower bound for path coefficient
lbPaD     <- valDiagLU(lbPa,-10,NA,nv) # lower bound for diagonal, lower & upper elements of covariance matrix

# ------------------------------------------------------------------------------
# PREPARE MODEL

# ACE Model
# Create Algebra for expected Mean Matrices
meanG     <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labVars("mean",vars), name="meanG" )

# Create Matrices for Path Coefficients
pathA     <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), labels=labLower("a",nv), lbound=lbPaD, name="a" ) 
pathC     <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), labels=labLower("c",nv), lbound=lbPaD, name="c" )
pathE     <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPe,nv), labels=labLower("e",nv), lbound=lbPaD, name="e" )
	
# Create Algebra for Variance Components
covA      <- mxAlgebra( expression=a %*% t(a), name="A" )
covC      <- mxAlgebra( expression=c %*% t(c), name="C" ) 
covE      <- mxAlgebra( expression=e %*% t(e), name="E" )

# Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covP      <- mxAlgebra( expression= A+C+E, name="V" )
covMZ     <- mxAlgebra( expression= A+C, name="cMZ" )
covDZ     <- mxAlgebra( expression= 0.5%x%A+ C, name="cDZ" )
expCovMZ  <- mxAlgebra( expression= rbind( cbind(V, cMZ), cbind(t(cMZ), V)), name="expCovMZ" )
expCovDZ  <- mxAlgebra( expression= rbind( cbind(V, cDZ), cbind(t(cDZ), V)), name="expCovDZ" )

# Create Data Objects for Multiple Groups
dataMZ    <- mxData( observed=mzData, type="raw" )
dataDZ    <- mxData( observed=dzData, type="raw" )

# Create Expectation Objects for Multiple Groups
expMZ     <- mxExpectationNormal( covariance="expCovMZ", means="meanG", dimnames=selVars )
expDZ     <- mxExpectationNormal( covariance="expCovDZ", means="meanG", dimnames=selVars )
funML     <- mxFitFunctionML()

# Create Model Objects for Multiple Groups
pars      <- list(meanG, pathA, pathC, pathE, covA, covC, covE, covP )
modelMZ   <- mxModel( name="MZ", pars, covMZ, expCovMZ, dataMZ, expMZ, funML )
modelDZ   <- mxModel( name="DZ", pars, covDZ, expCovDZ, dataDZ, expDZ, funML )
multi     <- mxFitFunctionMultigroup( c("MZ","DZ") )

# Create Algebra for Standardization
matI      <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
invSD     <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")

# Calculate genetic and environmental correlations
corA      <- mxAlgebra( expression=cov2cor(A), name ="rA" )
corC      <- mxAlgebra( expression=cov2cor(C), name ="rC" )
corE      <- mxAlgebra( expression=cov2cor(E), name ="rE" )

# Build Model with Confidence Intervals
calc      <- list( matI, invSD, corA, corC, corE )
modelACE  <- mxModel( "mulACEc", pars, modelMZ, modelDZ, multi, calc )

# ------------------------------------------------------------------------------
# RUN MODEL

# Run ACE Model
fitACE    <- mxRun( modelACE, intervals=F )
sumACE    <- summary( fitACE )
parameterSpecifications(fitACE)

# Compare with Saturated Model
mxCompare( fit, fitACE )

# Print Goodness-of-fit Statistics & Parameter Estimates
fitGofs(fitACE)
fitEsts(fitACE)

# ------------------------------------------------------------------------------
# RUN SUBMODELS

# Run AE model
modelAE   <- mxModel( fitACE, name="mulAE" )
modelAE   <- omxSetParameters( modelAE, labels=labLower("c",nv), free=FALSE, values=0 )
fitAE     <- mxRun( modelAE, intervals=T )
mxCompare( fitACE, fitAE )
fitGofs(fitAE)
fitEsts(fitAE)

# Run AE1 model Drop a22 a32 a33
modelAE1   <- omxSetParameters( modelAE, labels=c("a22","a32", "a33"), free=FALSE, values=0 )
fitAE1     <- mxRun( modelAE1, intervals=T )
mxCompare( fitAE, fitAE1 )
fitGofs(fitAE1)
fitEsts(fitAE1)

# Run AE2 model Drop a31 a22 a32 a33
modelAE2   <- omxSetParameters( modelAE, labels=c("a31", "a22","a32", "a33"), free=FALSE, values=0 )
fitAE2     <- mxRun( modelAE2, intervals=T )
mxCompare( fitAE1, fitAE2 )

# Run AE3 model Drop a31 a21
modelAE3   <- omxSetParameters( modelAE, labels=c("a31", "a21"), free=FALSE, values=0 )
fitAE3     <- mxRun( modelAE3, intervals=T )
mxCompare( fitAE, fitAE3 )
fitGofs(fitAE3)
fitEsts(fitAE3)


# Run CE model
modelCE   <- mxModel( fitACE, name="mulCEc" )
modelCE   <- omxSetParameters( modelCE, labels=labLower("a",nv), free=FALSE, values=0 )
fitCE     <- mxRun( modelCE, intervals=T )
mxCompare( fitACE, fitCE )
fitGofs(fitCE)


# Run E model
modelE    <- mxModel( fitAE, name="mulEc" )
modelE    <- omxSetParameters( modelE, labels=labLower("a",nv), free=FALSE, values=0 )
fitE      <- mxRun( modelE, intervals=T )
mxCompare( fitAE, fitE )
fitGofs(fitE)

# Print Comparative Fit Statistics
mxCompare( fitACE, nested <- list(fitACE, fitAE1, fitAE2, fitAE3, fitCE, fitE) )

# ------------------------------------------------------------------------------                                           
sink()
save.image(paste(filename,".Ri",sep=""))

