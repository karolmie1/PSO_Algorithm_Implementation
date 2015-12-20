generateStartPoints <- function(popCount, dim, min, max) {
  lista = list(popCount);
  for(i in 1:popCount) {
    x = runif(dim, min, max);
    lista[i] =  list(list(coordinates=x, quality=NA));
  }

  return(lista);
}

initialization<-function(startPoints) {return (startPoints)}

getBoundedEvaluation <- function (evaluation, point) {
#TODO: implement and add at least sanity test
#      make it used in meteheuristic when evaluation list
}

#' Terminates execution of algorithm after number of evaluatons exceeds certain treshold
#'
#' @param history A history.
#' @param model A model.
#' @return decision if algorithm has to be stopped.
terminateByEvaluationsCount <- function (history, model) {
  return(length(history) > app.maxEvaluations);
}
