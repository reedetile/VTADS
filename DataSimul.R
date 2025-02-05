#Description-----------------------------------------
# Simulating data for mock analysis of VTADS
#  30 Dec 2024
#RCS

#Initialize -----------------------------------------
rm(list=ls())
library(unmarked)
library(MuMIn)
library(ggplot2)
# Load functions--------------------------------------


# Global Variables-------------------------------------


# Program Body------------------------------------------
# Initialize variables
set.seed(1234)
ponds <- 24 #number of sites
frogs <- 2 #number of frogs PER VISIT
visits <- 3 #number of visits to each site
tests <- 3 #number of tests per frog
total_frogs <- frogs*visits
total_tests <- frogs*visits*tests #total number of tests for each pond for the season

# Create an empty dataframe for the detection data
det <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
site_covs <- data.frame(temp = rnorm(ponds),betadiv = rnorm(ponds))

umf <- unmarkedFrameGOccu(y = det, siteCovs = site_covs, numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~temp, phiformula =~ betadiv, pformula =~ 1)
coeff <- list(psi = c(0,-2), 
              phi = c(0,5), 
              det = 0)
sim <- simulate(umf, model = goccu,  psiformula = ~temp, phiformula = ~betadiv, pformula =~ 1, coefs = coeff)
head(sim[[1]])
tempXpsiphiXbeta <- goccu(psiformula = ~temp, phiformula = ~betadiv, pformula =~ 1, data = sim[[1]])
summary(tempXpsiphiXbeta)
temp <- data.frame(temp = seq(from = -2, to = 2, by = 0.1))
temppredict <- unmarked::predict(tempXpsiphiXbeta, type = "psi", newdata = temp, appendData = T)
ggplot(data = temppredict, aes(x= temp, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Temperature") + 
  ylab("Psi")+
  theme_classic()


betadiv <- data.frame(betadiv = seq(from = -2, to = 2, by = 0.1))
betapredict <- unmarked::predict(tempXpsiphiXbeta, type = "phi", newdata = betadiv, appendData = T)
ggplot(data = betapredict, aes(x= betadiv, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Beta diversity") + 
  ylab("Psi")+
  theme_classic()
