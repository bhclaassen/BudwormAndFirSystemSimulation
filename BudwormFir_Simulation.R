## ------------------------------------------------------------------------
## Budworm Simulation
## Thinking in Systems, pg 92
## Author: Ben Claassen
## ------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------
library(tidyverse)

# Assumptions and constants -----------------------------------------------
# Each worm can create 150 new worms
# <https://apps.fs.usda.gov/r6_decaid/views/western_spruce_budworm.html>
rate_wormReproduction <- 150
# Each predator (bird) lays 6 eggs <mostly invented>
# <https://www.10000birds.com/spruce-budworms-and-the-warblers-that-eat-them.htm>
# <https://en.wikipedia.org/wiki/Tennessee_warbler>
rate_predatorReproduction <- 6 
# Each fir creates 2 more firs each year <invented>
# Firs crowd out other trees overtime, so we can model their numbers expanding
#      even while ignoring other species
rate_firReproduction <- 2


# 1 fir is killed by 10,000 worms <invented>
rate_firDeath_worm <- 10000

# 1,000 worms are eaten per predator <invented>
rate_wormDeath_predators <- 1000




# Process -----------------------------------------------------------------

## Chronological steps
# (1) Trees, worms, and predators reproduce
# (2) Predators eat worms in the spring. If they cannot find enough worms then they starve to death
# (3) Worms eat trees in the summer. If they cannot find enough trees then they starve to death
# (4) Trees that have too many worms do not survive the winter

## Other assumptions
# - Worms are assumed to saturate trees before moving on
# - If any of the three populations die off, one (1) moves in to the area at the start of the next year
# - No bounds on the area
# - No member of a population dies from any other cause than starvation or predation



# Initialize storage matrix -----------------------------------------------


forest <- as.data.frame(
  matrix(0, 101, 7)
)

names(forest) <- c('year', 'start_worms', 'start_predators', 'start_fir', 'end_worms', 'end_predators', 'end_fir')

forest[,1] <- 0:100 # Set time periods
forest[1,2] <- 0 # Set start value to 0
forest[1,3] <- 0 # Set start value to 0
forest[1,4] <- 0 # Set start value to 0

forest[1,5] <- 100 # Start simulation with 100 worms
forest[1,6] <- 1 # Start simulation with 1 predator
forest[1,7] <- 10 # Start simulation with 10 fir

head(forest)



# Fill out [forest] matrix ----------------------------------------------------
# x=2
for(x in 2:101) {
  previous <- forest[(x-1),]
  current <- forest[x,]
  previous
  current
  
  # If any of the three populations has dropped to 0, then a new 1 moves into the area before reproduction
  
  if(current$start_worms <= 0) {
    current$start_worms <- 1
  }
  if(current$start_predators <= 0) {
    current$start_predators <- 1
  }
  if(current$start_fir <= 0) {
    current$start_fir <- 1
  }
  
  
  # Worms reproduce at beginning of year
  current$start_worms <- (previous$end_worms * rate_wormReproduction) + previous$end_worms
  # Predators reproduce
  current$start_predators <- (previous$end_predators * rate_predatorReproduction) + previous$end_predators
  # Firs reproduce
  current$start_fir <- floor(previous$end_fir * rate_firReproduction) + previous$end_fir
  
  # Predators eat worms or starve
  if(current$start_worms >= current$start_predators * rate_wormDeath_predators) { # If there are enough worms to feed all predators, then...
    current$end_predators <- current$start_predators # ...all predators survive
    current$end_worms <- current$start_worms - (current$start_predators * rate_wormDeath_predators)# ... and worms are reduced proportional to the number of predators
  } else { # Else, all worms are eaten, and the remaining predators starve
    current$end_predators <- floor(current$start_worms / rate_wormDeath_predators) # Set predators to the max whole number that can find enough worms to eat
    current$end_worms <- 0
  }
  
  

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

  
  # if(current$start_worms >= current$start_predators * rate_wormDeath_predators) {
  #   current$end_worms <- current$start_worms - (current$start_predators * rate_wormDeath_predators)
  #   if(current$end_worms < 1) {
  #     current$end_worms <- 1
  #   }
  #   
  #   current$end_predators <- current$start_predators
  # } else {
  #   current$end_predators <- floor(current$start_worms / rate_wormDeath_predators)
  #   if(current$end_predators < 1) {
  #     current$end_predators <- 1
  #   }
  #   
  #   current$end_worms <- current$start_worms - (current$end_predators * rate_wormDeath_predators)
  #   if(current$end_worms < 1) {
  #     current$end_worms <- 1
  #   }
  # }
  
  
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

