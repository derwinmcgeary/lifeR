library(RUnit)

test.createRandomGrid <- function(){
  checkEquals(dim(createRandomGrid())[1],10)
  checkEquals(dim(createRandomGrid())[2],10)
  checkEquals(dim(createRandomGrid(5))[1],5)
  checkEquals(dim(createRandomGrid(5))[2],5)
  checkEquals(dim(createRandomGrid(5,3))[1],3)
  checkEquals(dim(createRandomGrid(5,3))[2],5)
  checkException(createRandomGrid(-1,7))
  
  checkTrue(sum(createRandomGrid())>0, "RandomGrid is empty!")
}

test.killCell <- function(){
  xgrid <- createRandomGrid()
  xgrid <- killCell(xgrid,5,5)
  checkTrue(!isAlive(xgrid,5,5))
}

test.quickenCell <- function(){
  xgrid <- createRandomGrid()
  xgrid <- quickenCell(xgrid,5,5)
  checkTrue(isAlive(xgrid,5,5))
}

test.getNeighbours <- function() {
  xgrid <- createRandomGrid()
  checkEquals(length(getNeighbours(xgrid,5,5)),8)
  checkEquals(length(getNeighbours(xgrid,1,1)),3)
  checkEquals(length(getNeighbours(xgrid,1,5)),5)
  checkEquals(length(getNeighbours(xgrid,5,1)),5)
}

test.nLiveNeighbours <- function() {
  xgrid <- createEmptyGrid()
  xgrid <- quickenCell(xgrid,4,4)
  xgrid <- quickenCell(xgrid,6,6)
  checkEquals(nLiveNeighbours(xgrid,5,5),2)
  xgrid <- quickenCell(xgrid,6,5)
  xgrid <- killCell(xgrid,6,6)
  checkEquals(nLiveNeighbours(xgrid,5,5),2)  
}

test.doGeneration <- function() {
  xgrid <- createEmptyGrid()
  xgrid <- quickenCell(xgrid,4,4)
  xgrid <- quickenCell(xgrid,5,5)
  xgrid <- quickenCell(xgrid,6,6)
  xgrid <- doGeneration(xgrid)
  checkEquals(nLiveNeighbours(xgrid,5,5),0)
  checkTrue(isAlive(xgrid,5,5))
}