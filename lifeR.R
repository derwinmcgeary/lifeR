######################################################3
# Game of Life in R
# Unit testing is in the other file
# I have set the tests to be the only thing which runs by default,
# which means that "Source on Save" runs them every time I save in RStudio
# Use > runSim() to play it
# For those not used to R, the last thing returned in a function is the return value of the function
# So createRandomGrid() returns a matrix, for example

createRandomGrid <- function(width=10, height=width, density=0.2) {
  # rbinom is one of R's inbuilt distributions, returning 1 with p=density
  population <- rbinom(width*height,1,density)
  matrix(population,height,width)
}

createEmptyGrid <- function(width=10, height=width) {
  population <- 0
  matrix(population,height,width) 
}

killCell <- function(grid,x,y) {
  grid[y,x] <- 0
  grid
}

quickenCell <- function(grid,x,y){
  grid[y,x] <- 1
  grid
}

isAlive <- function(grid,x,y) {
  as.logical(grid[y,x])
}

getNeighbours <- function(grid,x,y){
  # This function is pretty dense - all the max/min stuff is for handling edges and corners
  # Essentially, take the maximum of zero and x-1, and the minimum of our board size and x+1
  # and do the same for the y coords. Skip the middle bit which is our target cell
  nbrs <- vector()
  for(i in max(0,x-1):min(dim(grid)[2],x+1)) {
    for(j in max(0,y-1):min(dim(grid)[1],y+1)) {
      if(x!=i||y!=j){
      nbrs <- c(nbrs,isAlive(grid,i,j))}
    }
  }
  nbrs
}

nLiveNeighbours <- function(grid,x,y){
  sum(getNeighbours(grid,x,y))
}

doGeneration <- function(grid){
#  We generate a new dead grid, then only switch on the cells which should be alive
#  So this is a cruelty-free function with no killing and only two if statements
  newgrid <- createEmptyGrid(dim(grid)[1],dim(grid)[2])
  for(x in 1:dim(grid)[1]) {
    for(y in 1:dim(grid)[2]) {
      if(isAlive(grid,x,y) && (nLiveNeighbours(grid,x,y) > 1) && (nLiveNeighbours(grid,x,y) < 4)) newgrid[y,x] <- 1
      if(!isAlive(grid,x,y) && nLiveNeighbours(grid,x,y)==3) newgrid[y,x] <- 1
    }
  }
  newgrid
}

runSim <- function(width=10,height=width,density=0.2) {
  grid <- createRandomGrid(width,height,density)
  for(i in 1:1000){
  newgrid <- doGeneration(grid)
  print(grid)
  Sys.sleep(0.3)
  if(identical(grid,newgrid)) break
  grid <- newgrid
  }
}

testsuite.c2f <- defineTestSuite("c2f", 
                                 dirs=file.path("./"), 
                                 testFileRegexp = "^runit.+\\.R", 
                                 testFuncRegexp = "^test.+", 
                                 rngKind="Marsaglia-Multicarry", 
                                 rngNormalKind="Kinderman-Ramage")

printTextProtocol(runTestSuite(testsuite.c2f))