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
BD <- # read in disease data
BioDiv <- # read in biodiversity data
# Program Body------------------------------------------
# Note on naming models = There are 3 parameters, psi, theta, and p
# Each parameter could be either constant, affected by alpha diversity, or effected by beta diversity
# 3 parameters * 3 possible covariates = 27 possible models
# Each model was named based on the parameter ~ covariate inputs.
# For example, the null model is named NullNullNull because all there parameters are constant. the 2nd model is named
# NullNullAlpha because it includes an effect of alpha on detection/intensity. And so on.

# If no temp included
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




# If temp included
#
# Psi ~ Temp
NullNullNull <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~Temp,data = ) # Really a temp model
NullNullAlpha <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
NullNullBeta <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~beta+ Temp,data = )
NullAlphaNull <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
NullAlphaAlpha <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
NullAlphaBeta <- goccu(psiformula = ~Temp, phiformula = ~alpha + Temp, pformula = ~beta+ Temp,data = )
NullBetaNull <- goccu(psiformula = ~Temp, phiformula = ~beta+ Temp, pformula = ~Temp,data = ) 
NullBetaAlpha <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
NullBetaBeta <- goccu(psiformula = ~Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )
#Psi ~ alpha
AlphaNullNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
AlphaNullAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
AlphaNullBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = )
AlphaAlphaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
AlphaAlphaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
AlphaAlphaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = )
AlphaBetaNull <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = )
AlphaBetaAlpha <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
AlphaBetaBeta <- goccu(psiformula = ~alpha + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )
# Psi ~ beta
BetaNullNull <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
BetaNullAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
BetaNullBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~Temp, pformula = ~beta + Temp,data = )
BetaAlphaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
BetaAlphaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
BetaAlphaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~alpha + Temp, pformula = ~beta + Temp,data = )
BetaBetaNull <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~Temp,data = )
BetaBetaAlpha <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~alpha + Temp,data = )
BetaBetaBeta <- goccu(psiformula = ~beta + Temp, phiformula = ~beta + Temp, pformula = ~beta + Temp,data = )

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

# If only want to explore Alpha*NumPonds, with temperature
NullNullNull <- goccu(psiformula = ~Temp, phiformula = ~Temp, pformula = ~Temp,data = ) # Really a temp model

# Effects of alpha + temp ONLY
AlphaNullNull <- goccu(psiformula = ~ alpha + Temp, phiformula = ~Temp, pformula = ~Temp,data = )
NullAlphaNull <- goccu(psiformula = ~ Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
NullNullAlpha <- goccu(psiformula = ~ Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
AlphaAlphaNull <- goccu(psiformula = ~ alpha + Temp, phiformula = ~alpha + Temp, pformula = ~Temp,data = )
NullAlphaAlpha <- goccu(psiformula = ~ Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )
AlphaNullAlpha <- goccu(psiformula = ~ alpha + Temp, phiformula = ~Temp, pformula = ~alpha + Temp,data = )
AlphaAlphaAlpha <- goccu(psiformula = ~ alpha + Temp, phiformula = ~alpha + Temp, pformula = ~alpha + Temp,data = )

# Effects of alpha + NumPonds + Temp
AlphaPondsNullNull <- goccu(psiformula = ~ alpha + Ponds + Temp, phiformula = ~Temp, pformula = ~Temp,data = )





