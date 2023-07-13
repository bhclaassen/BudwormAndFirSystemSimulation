## ------------------------------------------------------------------------
## Budworm Simulation
## Thinking in Systems, pg 92
## Author: Ben Claassen
## ------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------
library(tidyverse)
options(scipen = 999) # Option for printing in scientific notation

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

# If firs die off, it takes 2 years for a new tree to reach maturity <invented>
rate_firTimeToMaturity <- 2

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
# - A single fir can only host as many worms as it is ultimately killed by
# - If trees or worms die off, one (1) moves in to the area at the start of the next year
# - Predators move in if there are enough worms to feed one (1)
# - Predators have as many young as can be supported by the budworm population, up to 
#      their max (set in parameters)
# - No bounds on the area
# - Budworms only live one year
# - No fir or predator dies from any other cause than starvation or predation



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
  current <- forest[x,] # Set the 'current' period to be {x}
  previous <- forest[(x-1),] # Set the 'previous' period to by the (current - 1)
  
  # print(previous)
  # print(current)
  
  
  # If worm population has dropped to 0, then a new 1 moves into the area before reproduction
  if(previous$end_worms == 0) {
    previous$end_worms <- 1
  }
  
  
  ## Worms eggs hatch at beginning of year (NOTE: All previous-year budworms have died)
  current$start_worms <- (previous$end_worms * rate_wormReproduction)
  # Predators reproduce or arrive
  current$start_predators <- (previous$end_predators * rate_predatorReproduction) + previous$end_predators
  # If previous end predators is zero, and there are enough worms to support one predator,
  #     it moves in. Else keep predators at zero. Do not reproduce at beginning of year
  if(previous$end_predators == 0 & current$start_worms >= rate_wormDeath_predators) {
    previous$end_predators <- 1
  }
  # Firs reproduce
  current$start_fir <- (previous$end_fir * rate_firReproduction) + previous$end_fir

  # If fir population is zero, then add one tree if enough time has passed
  if( all.equal( rep(0, length((x - rate_firTimeToMaturity):x) ),
    c(forest[(x - rate_firTimeToMaturity):x,]$end_fir)) ) {
    previous$end_fir <- 1
  }
  
  ## Predators eat worms or starve
  # If there are enough worms to feed all predators, then...
  if(current$start_worms >= current$start_predators * rate_wormDeath_predators) {
    # ...all predators survive...
    current$end_predators <- current$start_predators
    # ...and worms are reduced proportional to the number of predators
    current$end_worms <- current$start_worms - (current$start_predators * rate_wormDeath_predators)
  } else { # Else...
    # ...all worms are eaten...
    current$end_worms <- 0
    # ...and the remaining predators starve.
    # Set predators to the max whole number that can find enough worms to eat
    current$end_predators <- floor(current$start_worms / rate_wormDeath_predators)
  }
  
  
  ## Worms eat fir or starve
  # If there are not enough fir trees to feed all worms, then...
  if(current$start_fir <= current$end_worms / rate_firDeath_worm) {
    # ... excess worms starve to death
    # Set the number of surviving worms to the number that are sustained by all current starting fir trees
    current$end_worms <- current$start_fir * rate_firDeath_worm
  } # Else, all worms survive, i.e. numbers set by predation remain
  

  ## Fir that are saturated die
  # If there are not enough fir trees to feed all worms, then...
  if(current$start_fir <= current$end_worms / rate_firDeath_worm) {
    current$end_fir <- 0
  } else { # Else...
    # ...all fir trees that are saturated with worms die
    current$end_fir <- current$start_fir - floor(current$end_worms / rate_firDeath_worm)
  }

  
  # Assign the current row back to the storage matrix
  forest[x,] <- current
  
  # print(current)
  # print(head(forest))
}

head(forest)
# forest %>% clipr::write_clip()

plot(type = 'l', forest$year, forest$start_worms)
plot(type = 'l', forest$year, forest$end_worms)

