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
coeff <- list(psi = c(0,-0.7), 
              phi = c(0,-0.9), 
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
  ylab("Prevalence")+
  theme_classic()


# Okay it works! so lets simulate our full model

# Lets simulate and effect of beta on occupancy, Spp on theta, and alpha on detection
# but we'll fit + dredge the full model and see if it picks the top model
# Create an empty dataframe for the detection data
det <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
site_covs <- data.frame(betadiv = rnorm(ponds),
                        alpha = rnorm(ponds))
Spp <-  data.frame(matrix(sample(c("Spp1","Spp2","Spp3","Spp4"), size = total_frogs*ponds, replace = T), nrow = ponds, ncol = total_frogs))
Spp <-  data.frame(lapply(Spp,as.factor))

avail_covs <- list(betadiv = data.frame(data = matrix(rep(site_covs$betadiv, each = total_frogs), nrow = ponds, ncol = total_frogs, byrow = T)),
                   alpha = data.frame(data = matrix(rep(site_covs$alpha, each = total_frogs), nrow = ponds, ncol = total_frogs, byrow = T)),
                   R0P = data.frame(data = matrix(rep(site_covs$R0P, each = total_frogs), nrow = ponds, ncol = total_frogs, byrow = T))) 
names(avail_covs$R0P) <- c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6")
Spp_dets <- NULL
for(i in 1:ncol(Spp)){
  Spp_temp <- matrix(rep(Spp[,i], each = tests), nrow = ponds, ncol = tests, byrow = T)
  Spp_dets <- cbind(Spp_dets, Spp_temp)
}
Spp_dets <- data.frame(lapply(data.frame(Spp_dets),as.factor))    
    
test_covs <- list(betadiv = data.frame(data = matrix(rep(site_covs$betadiv, each = total_tests), nrow = ponds, ncol = total_tests, byrow = T)),
                  alpha = data.frame(data = matrix(rep(site_covs$alpha, each = total_tests), nrow = ponds, ncol = total_tests, byrow = T)),
                  R0P = data.frame(data = matrix(rep(site_covs$R0P, each = total_tests), nrow = ponds, ncol = total_tests, byrow = T)),
                  Spp = Spp_dets)
for(i in 1:length(test_covs)){
  for (j in 1:ncol(test_covs$betadiv)){
    names(test_covs[[i]])[[j]] <- paste("test",i, sep = "")
  }
}
umf <- unmarkedFrameGOccu(y = det, siteCovs = site_covs, obsCovs = test_covs, yearlySiteCovs = avail_covs, numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~betadiv, phiformula =~ Spp, pformula =~ alpha)
coeff <- list(psi = c(0,-0.7), 
              phi = c(0,-0.9,0.8,0.5), 
              det = c(0,0.75))
sim2 <- simulate(umf, model = goccu,  psiformula = ~betadiv, phiformula = ~Spp, pformula =~ alpha, coefs = coeff)

#null model
null.mod <- goccu(psiformula = ~1, phiformula = ~1, pformula = ~1,data = sim2[[1]])
#Models for psi
psi.beta <- goccu(psiformula = ~betadiv, phiformula = ~1, pformula = ~1, data = sim2[[1]])
psi.alpha <- goccu(psiformula = ~alpha, phiformula = ~1, pformula =~ 1, data = sim2[[1]])
psi.R0P <- goccu(psiformula = ~R0P, phiformula = ~1, pformula =~ 1, data = sim2[[1]])

fitlist.psi <- fitList("nullpsi" = null.mod,
                   "betapsi" = psi.beta,
                   "alphapsi" = psi.alpha,
                   "R0Ppsi" = psi.R0P)

modSel(fitlist.psi)
summary(psi.beta)

#Models for theta
theta.beta <- goccu(psiformula = ~1, phiformula = ~betadiv, pformula = ~1, data = sim2[[1]])
theta.alpha <- goccu(psiformula = ~1, phiformula = ~alpha, pformula = ~1, data = sim2[[1]])
theta.R0P <- goccu(psiformula = ~1, phiformula = ~R0P, pformula = ~1, data = sim2[[1]])
theta.spp <- goccu(psiformula = ~1, phiformula = ~Spp, pformula = ~1, data = sim2[[1]])

fitlist.theta <- fitList("beta" = theta.beta,
                         "alpha" = theta.alpha,
                         "R0P" = theta.R0P,
                         "Spp" = theta.spp)
modSel(fitlist.theta)

###########################################################################################
# Creating actual simulations based on potential hypotheses
##########################################################################################

### First we'll need to create a full dataframe of psi, theta, and p covariates

#Theta
