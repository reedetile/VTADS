#Description-----------------------------------------
# Simulating data for mock analysis of VTADS
#  30 Dec 2024
#RCS

#Initialize -----------------------------------------

# Load functions--------------------------------------


# Global Variables-------------------------------------


# Program Body------------------------------------------

set.seed(1234)
sites <- 12
ponds <- 24
seasons <- 2
visits <- 3
seasonsXvisits <- seasons * visits
num_frogs <- ponds*seasonsXvisits*12
num_tests <- num_frogs * 3
occupancy <- rbinom(n = ponds*seasons, size = 1, prob = 0.75)
rep(occupancy, each = 
df <- data.frame(matrix(nrow =  num_tests, ncol = 7))
names(df) <- c("Season","Site","Pond","Visit","Occupancy","Prevalence","Detection")
