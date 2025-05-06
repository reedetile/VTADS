#Description-----------------------------------------
#Modelling of disease dynamics
#  18 Apr 2025
#RCS

#Initialize -----------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(unmarked)

# Load functions--------------------------------------


# Global Variables------------------------------------
RV <- # read in disease data
BioDiv <- # read in biodiversity data
# Program Body------------------------------------------
# Note on naming models = There are 3 parameters, psi, theta, and p
# Each parameter could be either constant, affected by alpha diversity, or effected by beta diversity
# 3 parameters * 3 possible covariates = 27 possible models
# Each model was named based on the parameter ~ covariate inputs.
# For example, the null model is named NullNullNull because all there parameters are constant. the 2nd model is named
# NullNullAlpha because it includes an effect of alpha on detection/intensity. And so on.
# Psi ~ 1
NullNullNull <- goccu(psiformula = ~1, phiformula = ~1, pformula = ~1,data = ) #null model
NullNullAlpha <- goccu(psiformula = ~1, phiformula = ~1, pformula = ~alpha,data = )
NullNullBeta <- goccu(psiformula = ~1, phiformula = ~1, pformula = ~beta,data = )
NullAlphaNull <- goccu(psiformula = ~1, phiformula = ~alpha, pformula = ~1,data = )
NullAlphaAlpha <- goccu(psiformula = ~1, phiformula = ~alpha, pformula = ~alpha,data = )
NullAlphaBeta <- goccu(psiformula = ~1, phiformula = ~alpha, pformula = ~beta,data = )
NullBetaNull <- goccu(psiformula = ~1, phiformula = ~beta, pformula = ~1,data = ) 
NullBetaAlpha <- goccu(psiformula = ~1, phiformula = ~beta, pformula = ~alpha,data = )
NullBetaBeta <- goccu(psiformula = ~1, phiformula = ~beta, pformula = ~beta,data = )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha, phiformula = ~1, pformula = ~1,data = )
AlphaNullAlpha <- goccu(psiformula = ~alpha, phiformula = ~1, pformula = ~alpha,data = )
AlphaNullBeta <- goccu(psiformula = ~alpha, phiformula = ~1, pformula = ~beta,data = )
AlphaAlphaNull <- goccu(psiformula = ~alpha, phiformula = ~alpha, pformula = ~1,data = )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha, phiformula = ~alpha, pformula = ~alpha,data = )
AlphaAlphaBeta <- goccu(psiformula = ~alpha, phiformula = ~alpha, pformula = ~beta,data = )
AlphaBetaNull <- goccu(psiformula = ~alpha, phiformula = ~beta, pformula = ~1,data = )
AlphaBetaAlpha <- goccu(psiformula = ~alpha, phiformula = ~beta, pformula = ~alpha,data = )
AlphaBetaBeta <- goccu(psiformula = ~alpha, phiformula = ~beta, pformula = ~beta,data = )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta, phiformula = ~1, pformula = ~1,data = )
BetaNullAlpha <- goccu(psiformula = ~beta, phiformula = ~1, pformula = ~alpha,data = )
BetaNullBeta <- goccu(psiformula = ~beta, phiformula = ~1, pformula = ~beta,data = )
BetaAlphaNull <- goccu(psiformula = ~beta, phiformula = ~alpha, pformula = ~1,data = )
BetaAlphaAlpha <- goccu(psiformula = ~beta, phiformula = ~alpha, pformula = ~alpha,data = )
BetaAlphaBeta <- goccu(psiformula = ~beta, phiformula = ~alpha, pformula = ~beta,data = )
BetaBetaNull <- goccu(psiformula = ~beta, phiformula = ~beta, pformula = ~1,data = )
BetaBetaAlpha <- goccu(psiformula = ~beta, phiformula = ~beta, pformula = ~alpha,data = )
BetaBetaBeta <- goccu(psiformula = ~beta, phiformula = ~beta, pformula = ~beta,data = )

# Do model selection
fitlist.psi <- fitList(NullNullNull,
                       NullNullAlpha,
                       NullNullBeta,
                       NullAlphaNull,
                       NullAlphaAlpha,
                       NullAlphaBeta,
                       NullBetaNull,
                       NullBetaAlpha,
                       NullBetaBeta,
                       AlphaNullNull,
                       AlphaNullAlpha,
                       AlphaNullBeta,
                       AlphaAlphaNull,
                       AlphaAlphaAlpha,
                       AlphaAlphaBeta,
                       AlphaBetaNull,
                       AlphaBetaAlpha,
                       AlphaBetaBeta,
                       BetaNullNull,
                       BetaNullAlpha,
                       BetaNullBeta,
                       BetaAlphaNull,
                       BetaAlphaAlpha,
                       BetaAlphaBeta,
                       BetaBetaNull,
                       BetaBetaAlpha,
                       BetaBetaBeta)

modSel(fitlist.psi)





