generateStartPoints <- function(popCount, dim, min, max) {
  lista = list(popCount);
  for(i in 1:popCount) {
    x = runif(dim, min, max);
    lista[i] =  list(list(coordinates=x, quality=NA));
  }

  return(lista);
}

initialization<-function(startPoints) {
  return (startPoints)
}

getBoundedEvaluation <- function (evaluation, point) {
#TODO: implement and add at least sanity test
#      make it used in meteheuristic when evaluation list
}

termination <- function (evaluation, point) {
#TODO: implement and test - add parameter known global optimum, acceptable delta and max steps
#                         - don't decrement max steps: in the initialization set new parameter: currentCounter
#                         - true if: close enough to optimum, too many steps
}

updatePointVelocity <- function(velocity, bestLocalCoordinates, bestGlobalCoordinates){
  #TODO: implement main evaluating function
  #add dependence on weigths
  #add weigths to properties
  #must return list of new velocities
  retun (0);
}
