#Description-----------------------------------------
# Simulating data for mock analysis of VTADS
#  30 Dec 2024
#RCS

#Initialize -----------------------------------------

# Load functions--------------------------------------


# Global Variables-------------------------------------


# Program Body------------------------------------------
# Initialize variables#
set.seed(1234)
sites <- 12
ponds <- 24
visits <- 3
frogs_per_pond <- visits * 12
tests_per_frog <-  3
tests_per_pond <- frogs_per_pond * tests_per_frog

#creating occupancy data#
#Create fake site level covar
temp <- rnorm(n = ponds, mean = 30, sd = 2)
error <- rnorm(ponds)
corr <- 0.75
var <- corr * temp + sqrt(1 - corr^2) * rnorm(ponds)
psi <- pnorm(var, mean = 0, sd = 1)



occupancy <- rbinom(n = ponds, size = 1, prob = 0.75)
frog_pos <- data.frame(matrix(nrow = ponds, ncol = frogs_per_pond))
test_pos <-  data.frame(matrix(nrow = ponds, ncol = tests_per_pond))

for (i in 1:length(occupancy)) {
  frog_pos[i,1:ncol(frog_pos)] <- if(occupancy[[i]] == 1){
    rbinom(n = frogs_per_pond, size = 1, prob = 0.65)} else{
      rep(0, times = frogs_per_pond)}
}

for (i in 1:length(occupancy)) {
  for(j in 1:ncol(frog_pos)) {
    assign(paste("frog",j,sep = "_"), if(frog_pos[i,j] == 1){
      rbinom(n = tests_per_frog, size = 1, prob = 0.95)} else{
        rep(0, times = tests_per_frog)})
    det <- unlist(lapply(objects(pattern = "frog_\\d+"), get))
    test_pos[i,1:ncol(test_pos)] <- det 
  }
}
