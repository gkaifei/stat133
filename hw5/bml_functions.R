#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  data_sample <- sample(0:2, prob=c(1-p,p/2,p/2), size = r*c, replace = TRUE)
  m <- matrix(data = data_sample, nrow = r, ncol= c, byrow = FALSE)
  return(m)
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  m.original <- m
  m.1 <- m[ , c(ncol(m), 1:ncol(m)-1)]
  m.position1 <- (m == 0 & m.1 == 1) * 1
  m.moving1 <- m + m.position1
  m.position1.moved <- m.position1 [ ,c(2:ncol(m.position1),1)]
  m <- m.moving1 - m.position1.moved
  
  m.2 <- m[c(2:nrow(m),1), ]
  m.position2 <- (m == 0 & m.2 == 2) * 2
  m.moving2 <- m + m.position2
  m.position2.moved <- m.position2[c(nrow(m.position2),1:nrow(m.position2)-1), ]
  m <- m.moving2 - m.position2.moved
  
  grid.new <- any(m != m.original)
   return(list(m, grid.new))
}



#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r,c,p){
  m <- bml.init(r,c,p)
  i <- 1
  while (i <= 1000){
    m <- bml.step(m)[[1]]
    grid.new <- bml.step(m)[[2]]
    if (grid.new == FALSE){
      summary <- list("Gridlocked Matrix" =m, "Total Steps" = i, "Traffic Density" = p)
      return(summary)
      break}
    else {i <- i + 1}
  }
   return(list("Free Flowing Matrix" = m, "Total Steps" = i-1, "Traffic Density" = p))   
}




