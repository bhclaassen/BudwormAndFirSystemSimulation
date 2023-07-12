## ------------------------------------------------------------------------
## Budworm Simulation
## Thinking in Systems, pg 92
## June 2023
## Author: Ben Claassen
## ------------------------------------------------------------------------
 

# -------------------------------------------------------------------------
## ~SUMMARY NOTES AFTER COMPLETION~
##    - I did not capture the exactly correct dynamics, BUT got the boom and bust
##    - Takes 3 years for the predators to catch up to the worm boom
##    - But the worm boom was caused by a fluctuation of the predators:worms ratio, not the surplus of fir

##    - This code does not include:
##          - winter/spring dynamics worm die-off
##          - accurate rates for most constants
##          - affects of spraying, because underlying dynamics not accurate enough yet
##          - other trees to harbor worm population
##          - fractions of fir is odd
# -------------------------------------------------------------------------



# Libraries ---------------------------------------------------------------
library(tidyverse)

# Assumptions and constants -----------------------------------------------
# Each worm can create 150 new worms <https://apps.fs.usda.gov/r6_decaid/views/western_spruce_budworm.html>
rate_wormReproduction <- 150
# Each predator (bird) lays 5 eggs <invented>
rate_predatorReproduction <- 5
# Each fir creates 0.2 more firs <invented>
rate_firReproduction <- 0.2


# 1 fir is killed by 10,000 worms <invented>
rate_firDeath_worm <- 10000

# 1,000 worms are eaten per predator <invented>
rate_wormDeath_predators <- 1000



# -------------------------------------------------------------------------


forest <- as.data.frame(
  matrix(0, 101, 7)
)

names(forest) <- c('year', 'start_worms', 'start_predators', 'start_fir', 'end_worms', 'end_predators', 'end_fir')

forest[,1] <- 0:100 # set time periods
forest[1,2] <- 0 # start simulation with 100 worms
forest[1,3] <- 0 # start simulation with 1 predator
forest[1,4] <- 0 # start simulation with 1 fir

forest[1,5] <- 100 # start simulation with 100 worms
forest[1,6] <- 1 # start simulation with 1 predator
forest[1,7] <- 1 # start simulation with 1 fir

head(forest)



# Fill out [forest] matrix ----------------------------------------------------
# Set current year to work with
# x=4
for(x in 2:101) {
  previous <- forest[(x-1),]
  current <- forest[x,]
  previous
  current
  
  # Worms reproduce at beginning of year
  current$start_worms <- previous$end_worms * rate_wormReproduction + previous$end_worms
  # Predators reproduce
  current$start_predators <- previous$end_predators * rate_predatorReproduction + previous$end_predators
  # Firs reproduce
  current$start_fir <- previous$end_fir * rate_firReproduction + previous$end_fir
  
  # Predators eat worms or starve
  if(current$start_worms >= current$start_predators * rate_wormDeath_predators) {
    current$end_worms <- current$start_worms - (current$start_predators * rate_wormDeath_predators)
    if(current$end_worms < 1) {
      current$end_worms <- 1
    }
    
    current$end_predators <- current$start_predators
  } else {
    current$end_predators <- floor(current$start_worms / rate_wormDeath_predators)
    if(current$end_predators < 1) {
      current$end_predators <- 1
    }
    
    current$end_worms <- current$start_worms - (current$end_predators * rate_wormDeath_predators)
    if(current$end_worms < 1) {
      current$end_worms <- 1
    }
  }
  # Worms eat fir or starve
  if(current$start_fir >= current$end_worms / rate_firDeath_worm) {
    current$end_worms <- current$end_worms
  } else {
    current$end_worms <- current$start_fir * rate_firDeath_worm
  }
  
  # Fir die
  current$end_fir <- current$start_fir - floor(current$end_worms / rate_firDeath_worm)
  if(current$end_fir < 0) {
      current$end_fir <- 1
    }
  ##
  current
  forest[x,] <- current
  head(forest)
}

head(forest)
# forest %>% clipr::write_clip()

plot(type = 'l', forest$year, forest$start_worms)
plot(type = 'l', forest$year, forest$end_worms)

