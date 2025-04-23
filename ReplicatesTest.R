# Description
# A script to run simulations and compare the output of triplicate/duplicate/single swab tests

#Initialize -----------------------------------------
rm(list=ls())
library(unmarked)
library(tidyverse)
# Load functions--------------------------------------


# Global Variables-------------------------------------
repo <- getwd()
graphs <- paste(repo, "/Graphs", sep = "")

# Program Body------------------------------------------

# General variables
set.seed(1234)
ponds <- 24 #number of sites
frogs <- 2 #number of frogs PER VISIT
visits <- 3 #number of visits to each site
theta <- seq(0.1,0.9, by = 0.1)
#BD vars
psi_bd <- log(0.5/(1-0.5))
theta_bd <- log(0.5/(1-0.5))
p_bd <- 2.23 #this was just taken on the log scale, Estimated from Dan's data

#RV vars
psi_rv <- log(0.5 / (1 - 0.5))
theta_rv <- log(0.5 / (1 - 0.5))
p_rv <- log(0.78 / (1 - 0.78)) #0.78 is based on gray 2012
### BD 
## Triplicate
# Initialize variables
tests <- 3 #number of tests per frog
total_frogs <- frogs*visits
total_tests <- frogs*visits*tests #total number of tests for each pond for the season

# Create an empty dataframe for the detection data
det_bd <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
# Note including any occ, prev, or det covars... yet
umf <- unmarkedFrameGOccu(y = det_bd, numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~1, phiformula =~ 1, pformula =~ 1)

sims <- vector("list",length = length(theta))
for (i in 1:length(theta)) {
  theta_i <- log(theta[[i]]/(1 - theta[[i]]))
  coeff <- list(psi = psi_bd,
                phi = theta_i,
                det = p_bd,
                det = p_bd)
  sim <- simulate(umf, 
                  model = goccu,  
                  psiformula = ~1, 
                  phiformula = ~1, 
                  pformula =~ 1,
                  coefs = coeff)
  
  sims[[i]] <- sim

}
prev_triplicate <- data.frame(matrix(nrow = length(sims), ncol = 2))
names(prev_triplicate) <- c("Prev","beta")
for(i in 1:length(sims)){
  triplicate <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sims[[i]][[1]])
  prev_triplicate[i,2] <- summary(triplicate)[[2]]$Estimate
  prev_triplicate[i,1] <- theta[[i]]
}
prev_triplicate$est <- plogis(prev_triplicate$beta)
prev_triplicate$error <- abs(prev_triplicate$Prev - prev_triplicate$est)

coeff <- list(psi = psi_bd, 
              phi = theta_bd, 
              det = p_bd)

sim <- simulate(umf, 
                model = goccu,  
                psiformula = ~1, 
                phiformula = ~1, 
                pformula =~ 1,
                nsim = 100,
                coefs = coeff)
head(sim[[1]])
det_triplicate <- data.frame(matrix(nrow = 100, ncol = 2))
names(det_triplicate) <- c("Sim","Det")
for (i in 1:nrow(det_triplicate)) {
  triplicate <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sim[[i]])
  det_triplicate[i,1] <- i
  det_triplicate[i,2] <- summary(triplicate)[[3]]$Estimate
}
mean(det_triplicate$Det)
## Duplicate
tests <- 2 #number of tests per frog
total_frogs <- frogs*visits
total_tests <- frogs*visits*tests #total number of tests for each pond for the season

# Create an empty dataframe for the detection data
det_bd <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
# Note including any occ, prev, or det covars... yet
umf <- unmarkedFrameGOccu(y = det_bd, numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~1, phiformula =~ 1, pformula =~ 1)

sims <- vector("list",length = length(theta))
for (i in 1:length(theta)) {
  theta_i <- log(theta[[i]]/(1 - theta[[i]]))
  coeff <- list(psi = psi_bd,
                phi = theta_i,
                det = p_bd,
                det = p_bd)
  sim <- simulate(umf, 
                  model = goccu,  
                  psiformula = ~1, 
                  phiformula = ~1, 
                  pformula =~ 1,
                  coefs = coeff)
  
  sims[[i]] <- sim
  
}
prev_duplicate <- data.frame(matrix(nrow = length(sims), ncol = 2))
names(prev_duplicate) <- c("Prev","beta")
for(i in 1:length(sims)){
  duplicate <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sims[[i]][[1]])
  prev_duplicate[i,2] <- summary(duplicate)[[2]]$Estimate
  prev_duplicate[i,1] <- theta[[i]]
}
prev_duplicate$est <- plogis(prev_duplicate$beta)
prev_duplicate$error <- abs(prev_duplicate$Prev - prev_duplicate$est)




coeff <- list(psi = psi_bd, 
              phi = theta_bd, 
              det = p_bd)
sim <- simulate(umf, 
                model = goccu,  
                psiformula = ~1, 
                phiformula = ~1, 
                pformula =~ 1,
                nsim = 100,
                coefs = coeff)
head(sim[[1]])
det_duplicate <- data.frame(matrix(nrow = 100, ncol = 2))
names(det_duplicate) <- c("Sim","Det")
for (i in 1:nrow(det_duplicate)) {
  duplicate <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sim[[i]])
  det_duplicate[i,1] <- i
  det_duplicate[i,2] <- summary(duplicate)[[3]]$Estimate
}
mean(det_duplicate$Det)

## Singlicate
tests <- 1 #number of tests per frog
total_frogs <- frogs*visits
total_tests <- frogs*visits*tests #total number of tests for each pond for the season

# Create an empty dataframe for the detection data
det_bd <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
# Note including any occ, prev, or det covars... yet
umf <- unmarkedFrameGOccu(y = det_bd, numPrimary = total_frogs)

sims <- vector("list",length = length(theta))
for (i in 1:length(theta)) {
  theta_i <- log(theta[[i]]/(1 - theta[[i]]))
  coeff <- list(psi = psi_bd,
                phi = theta_i,
                det = p_bd,
                det = p_bd)
  sim <- simulate(umf, 
                  model = goccu,  
                  psiformula = ~1, 
                  phiformula = ~1, 
                  pformula =~ 1,
                  coefs = coeff)
  
  sims[[i]] <- sim
  
}
prev_single <- data.frame(matrix(nrow = length(sims), ncol = 2))
names(prev_single) <- c("Prev","beta")
for(i in 1:length(sims)){
  single <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sims[[i]][[1]])
  prev_single[i,2] <- summary(single)[[2]]$Estimate
  prev_single[i,1] <- theta[[i]]
}
prev_single$est <- plogis(prev_single$beta)
prev_single$error <- abs(prev_single$Prev - prev_single$est)






head(umf)
simulate(umf, model = goccu,  psiformula = ~1, phiformula =~ 1, pformula =~ 1)
coeff <- list(psi = psi_bd, 
              phi = theta_bd, 
              det = p_bd)
sim <- simulate(umf, 
                model = goccu,  
                psiformula = ~1, 
                phiformula = ~1, 
                pformula =~ 1,
                nsim = 100,
                coefs = coeff)
head(sim[[1]])
det_single <- data.frame(matrix(nrow = 100, ncol = 2))
names(det_single) <- c("Sim","Det")
for (i in 1:nrow(det_single)) {
  single <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sim[[i]])
  det_single[i,1] <- i
  det_single[i,2] <- summary(single)[[3]]$Estimate
}
mean(det_single$Det)

# Lets just compare them all really quick
plogis(mean(det_triplicate$Det)) # mean =0.91
plogis(mean(det_duplicate$Det)) # mean = 0.91
plogis(mean(det_single$Det)) # mean = 0.65

# Main takeaway: Could drop down to duplicate without losing any information.
# Single is a drastic drop in detection prob


### RV
## Triplicate
# Initialize variables
tests <- 3 #number of tests per frog
total_frogs <- frogs*visits
total_tests <- frogs*visits*tests 
#total number of tests for each pond for the season


# Create an empty dataframe for the detection data
det_rv <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
# Note including any occ, prev, or det covars... yet
umf <- unmarkedFrameGOccu(y = det_rv, numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~1, phiformula =~ 1, pformula =~ 1)
coeff <- list(psi = psi_rv, 
              phi = theta_rv, 
              det = p_rv)
sim <- simulate(umf, 
                model = goccu,  
                psiformula = ~1, 
                phiformula = ~1, 
                pformula =~ 1,
                nsim = 100,
                coefs = coeff)
head(sim[[1]])
det_triplicate <- data.frame(matrix(nrow = 100, ncol = 2))
names(det_triplicate) <- c("Sim","Det")
for (i in 1:nrow(det_triplicate)) {
  triplicate <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sim[[i]])
  det_triplicate[i,1] <- i
  det_triplicate[i,2] <- summary(triplicate)[[3]]$Estimate
}
mean(det_triplicate$Det)

## Duplicate
# Initialize variables
tests <- 2 #number of tests per frog
total_frogs <- frogs*visits
total_tests <- frogs*visits*tests 
#total number of tests for each pond for the season


# Create an empty dataframe for the detection data
det_rv <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
# Note including any occ, prev, or det covars... yet
umf <- unmarkedFrameGOccu(y = det_rv, numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~1, phiformula =~ 1, pformula =~ 1)
coeff <- list(psi = psi_rv, 
              phi = theta_rv, 
              det = p_rv)
sim <- simulate(umf, 
                model = goccu,  
                psiformula = ~1, 
                phiformula = ~1, 
                pformula =~ 1,
                nsim = 100,
                coefs = coeff)
head(sim[[1]])
det_duplicate <- data.frame(matrix(nrow = 100, ncol = 2))
names(det_duplicate) <- c("Sim","Det")
for (i in 1:nrow(det_duplicate)) {
  duplicate <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sim[[i]])
  det_duplicate[i,1] <- i
  det_duplicate[i,2] <- summary(duplicate)[[3]]$Estimate
}
mean(det_duplicate$Det)

## Duplicate
# Initialize variables
tests <- 1 #number of tests per frog
total_frogs <- frogs*visits
total_tests <- frogs*visits*tests 
#total number of tests for each pond for the season

# Create an empty dataframe for the detection data
det_rv <- matrix(NA, nrow = ponds, ncol = total_tests)

# Create a site lvl covar
# Note including any occ, prev, or det covars... yet
umf <- unmarkedFrameGOccu(y = det_rv, numPrimary = total_frogs)
head(umf)
simulate(umf, model = goccu,  psiformula = ~1, phiformula =~ 1, pformula =~ 1)
coeff <- list(psi = psi_rv, 
              phi = theta_rv, 
              det = p_rv)
sim <- simulate(umf, 
                model = goccu,  
                psiformula = ~1, 
                phiformula = ~1, 
                pformula =~ 1,
                nsim = 100,
                coefs = coeff)
head(sim[[1]])
det_single <- data.frame(matrix(nrow = 100, ncol = 2))
names(det_single) <- c("Sim","Det")
for (i in 1:nrow(det_single)) {
  single <- goccu(psiformula = ~1, phiformula = ~1, pformula =~ 1, data = sim[[i]])
  det_single[i,1] <- i
  det_single[i,2] <- summary(single)[[3]]$Estimate
}
mean(det_single$Det)

#comparing det for trip, dup, and single
plogis(mean(det_triplicate$Det)) # 0.78 pron
plogis(mean(det_duplicate$Det)) # 0.77 prob
plogis(mean(det_single$Det)) # 0.62 prob

# Again, can drop to duplicate without loss of info
# Single will result in lost of info


# comparison of prevalence errors
prev_comp <- data.frame(Prev = prev_triplicate$Prev, 
                        Triplicate = prev_triplicate$error,
                        Duplicate = prev_duplicate$error,
                        Single = prev_single$error)
