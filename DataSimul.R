#Description-----------------------------------------
# Simulating data for mock analysis of VTADS
#  30 Dec 2024
#RCS

#Initialize -----------------------------------------
rm(list=ls())
library(unmarked)
library(MuMIn)
library(ggplot2)
library(patchwork)
# Load functions--------------------------------------


# Global Variables-------------------------------------
repo <- getwd()
graphs <- paste(repo, "/Graphs", sep = "")

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

# Lets simulate and effect of beta on occupancy and on theta, and alpha on detection
# but we'll fit + dredge the full model and see if it picks the top model
# Create an empty dataframe for the detection data
det <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
site_covs <- data.frame(betadiv = rnorm(ponds),
                        alpha = rnorm(ponds))
beta <- rnorm(ponds)
beta_effect <-  data.frame(matrix(rep(beta, each = total_frogs), nrow = ponds, ncol = total_frogs, byrow = T))

avail_covs <- list(betadiv = data.frame(data = matrix(rep(site_covs$betadiv, each = total_frogs),
                                                      nrow = ponds, ncol = total_frogs, byrow = T)),
                   alpha = data.frame(data = matrix(rep(site_covs$alpha, each = total_frogs), 
                                                    nrow = ponds, ncol = total_frogs, byrow = T))) 
names(avail_covs$betadiv) <- c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6")
beta_dets <- NULL
for(i in 1:ncol(beta_effect)){
  beta_temp <- matrix(rep(beta_effect[,i], each = tests), nrow = ponds, ncol = tests, byrow = T)
  beta_dets <- cbind(beta_dets, beta_temp)
}
theta_dets <- data.frame(beta_dets)    
    
test_covs <- list(betadiv = data.frame(data = matrix(rep(site_covs$betadiv, each = total_tests), 
                                                     nrow = ponds, ncol = total_tests, byrow = T)),
                  alpha = data.frame(data = matrix(rep(site_covs$alpha, each = total_tests), 
                                                   nrow = ponds, ncol = total_tests, byrow = T)),
                  beta_dets = beta_dets)
for(i in 1:length(test_covs)){
  for (j in 1:ncol(test_covs$betadiv)){
    names(test_covs[[i]])[[j]] <- paste("test",i, sep = "")
  }
}
umf <- unmarkedFrameGOccu(y = det, siteCovs = site_covs, obsCovs = test_covs, yearlySiteCovs = avail_covs, 
                          numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~betadiv, phiformula =~ betadiv, pformula =~ alpha)
coeff <- list(psi = c(0,-0.7), 
              phi = c(0,-0.9), 
              det = c(0,-0.75))
sim2 <- simulate(umf, model = goccu,  psiformula = ~betadiv, phiformula = ~betadiv, pformula =~ alpha, coefs = coeff)

#null model
null.mod <- goccu(psiformula = ~1, phiformula = ~1, pformula = ~1,data = sim2[[1]])
#Models for psi
psi.beta <- goccu(psiformula = ~betadiv, phiformula = ~1, pformula = ~1, data = sim2[[1]])
psi.alpha <- goccu(psiformula = ~alpha, phiformula = ~1, pformula =~ 1, data = sim2[[1]])

fitlist.psi <- fitList("nullpsi" = null.mod,
                   "betapsi" = psi.beta,
                   "alphapsi" = psi.alpha)

modSel(fitlist.psi)
summary(psi.beta)

#Models for theta
theta.beta <- goccu(psiformula = ~1, phiformula = ~betadiv, pformula = ~1, data = sim2[[1]])
theta.alpha <- goccu(psiformula = ~1, phiformula = ~alpha, pformula = ~1, data = sim2[[1]])

fitlist.theta <- fitList("null" = null.mod,
                         "beta" = theta.beta,
                         "alpha" = theta.alpha)
modSel(fitlist.theta)

# models for p
p.beta <- goccu(psiformula = ~1, phiformula = ~1, pformula = ~betadiv, data = sim2[[1]])
p.alpha <- goccu(psiformula = ~1, phiformula = ~1, pformula = ~alpha, data = sim2[[1]])

fitlist.theta <- fitList("null" = null.mod,
                         "beta" = p.beta,
                         "alpha" = p.alpha)
modSel(fitlist.theta)

# Eventually I'm going to need to create all 27 possible models (so far... could do an interaction of beta + alpha?)
# Will also need to learn how to do model avg in unmarked
# However for right now I just want to do some plotting. So let plot the following

#Create df's for predictions
alpha <- data.frame(alpha = seq(from = -2, to = 2, by = 0.1))
beta <- data.frame(betadiv = seq(from = -2, to = 2, by = 0.1))

# psi ~ alpha

alphapredict <- unmarked::predict(psi.alpha, type = "psi", newdata = alpha, appendData = T)
PsiXAlpha <- ggplot(data = alphapredict, aes(x= alpha, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Alpha diversity") +
  ylab("Psi")+
  theme_classic()
PsiXAlpha
# psi ~ beta
betapredict <- unmarked::predict(psi.beta, type = "psi", newdata = beta, appendData = T)
PsiXBeta <- ggplot(data = betapredict, aes(x= betadiv, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Beta diversity") +
  ylab("Psi")+
  theme_classic()
PsiXBeta

# theta ~ alpha
alpha_theta_predict <- unmarked::predict(theta.alpha, type = "phi", newdata = alpha, appendData = T)
thetaXAlpha <- ggplot(data = alpha_theta_predict, aes(x= alpha, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Alpha diversity") +
  ylab("Prevalence")+
  theme_classic()
thetaXAlpha
# theta ~ beta
beta_theta_predict <- unmarked::predict(theta.beta, type = "phi", newdata = beta, appendData = T)
thetaXBeta <- ggplot(data = beta_theta_predict, aes(x= betadiv, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Beta diversity") +
  ylab("Prevalence")+
  theme_classic()
thetaXBeta
# p ~ alpha
alpha_p_predict <- unmarked::predict(p.alpha, type = "det", newdata = alpha, appendData = T)
pXAlpha <- ggplot(data = alpha_p_predict, aes(x= alpha, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Alpha diversity") +
  ylab("Infection intensity")+
  theme_classic()
pXAlpha
# p ~ beta
beta_p_predict <- unmarked::predict(p.beta, type = "det", newdata = beta, appendData = T)
pXBeta <- ggplot(data = beta_p_predict, aes(x= betadiv, y= Predicted))+
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_colour_brewer(palette = "Set1") +
  xlab("Beta diversity") +
  ylab("Infection Intensity")+
  theme_classic()
pXBeta

# put them all together!
biodivPlots <- (PsiXAlpha + thetaXAlpha + pXAlpha)/(PsiXBeta + thetaXBeta + pXBeta)
biodivPlots
setwd(graphs)
ggsave(filename = "biodivPlots.png", plot = biodivPlots)
