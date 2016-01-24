#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

#### TO BE DEFINED BY THE USER
getBestPoint <- function(points) {
  bestPoint = points[[1]];
  for(point in points) {
    if( point$quality > bestPoint$quality) {
      bestPoint = point;
    }
  }
  return(bestPoint);
}

#TODO: this is biased by distance from zero, it should be biased by the middle point between extremes: implementation and tests
normalizeBySquaring <- function(velocity, center) {
  return(sign(velocity)*sqrt(abs(velocity)));
}

#Initial Velocity should be random, and should be adequate to search space size
#This function provides such constrained initial velocities.
generateInitVelocity <- function(particlesCount, dimCount) {
  center = 0;
  max = normalizeBySquaring(psoProps.initSpawnArea, center);
  min = normalizeBySquaring(-psoProps.initSpawnArea, center);
  return(replicate(particlesCount, list(runif(dimCount, min, max))));
}

#Model structure:
#list of:
# - best point globally known
#     - coordinates
#     - quality
# - list of points
#     - vector of best known coordinates for point
#     - vector of current coordinates
#     - vector of velocity
#     - quality
initModel<-function(history) {
  particlesCount = length(history);
  dimCount = length(history[[1]]$coordinates);
  particles = list(velocities = generateInitVelocity(particlesCount , dimCount), positions = history, bestPositions = history);
  newModel = list(particles = particles, bestPosition = getBestPoint(history));
  return(newModel);
}

#selection of a LIST of points from the history
selection<-function(history, model){
  return(historyPop(history, length(model$particles$positions)));
}

#update of a model based on a LIST of points
modelUpdate<-function(selectedPoints, oldModel)
{
  newModel <- oldModel;
  newModel$particles$positions <- selectedPoints;

  for( i in 1:length(newModel$particles$velocities)) {
    #updating Local bests
    if(newModel$particles$positions[[i]]$quality < newModel$particles$bestPosition[[i]]$quality) {
      newModel$particles$bestPosition[[i]] <- newModel$particles$positions[[i]];
    }

    #updating global best
    if(newModel$particles$positions[[i]]$quality < newModel$bestPosition$quality){
      newModel$bestPosition <- newModel$particles$positions[[i]]
    }

    #updating velocities according to selectedPoints list
    newModel$particles$velocities[[i]] <- updatePointVelocity(newModel$particles$velocities[[i]], selectedPoints[[i]]$coordinates, newModel$particles$bestPositions[[i]], newModel$bestPosition);

  }
  return (newModel)
}

updatePointVelocity <- function(velocity, coordinates, bestLocalCoordinates, bestGlobalCoordinates){
#TODO add tests


  for(i in 1:length(velocity)){
    velocity[[i]] <- velocity [[i]] * (psoProps.explorationVsExploitaitionRatio) + (1-psoProps.explorationVsExploitaitionRatio)*(
      (1 - psoProps.localBestVsGlobalBestRatio) * runif(1,0,1)*(bestLocalCoordinates$coordinates[[i]] - coordinates[[i]]) +
        psoProps.localBestVsGlobalBestRatio * runif(1,0,1)*(bestGlobalCoordinates$coordinates[[i]] - coordinates[[i]])
    );
  }
  return (velocity);
}

#generation of a LIST of new points
variation<-function(selectedPoints, model){
  for(i in 1:length(selectedPoints)) {
    selectedPoints[[i]]$coordinates <- selectedPoints[[i]]$coordinates + model$particles$velocities[[i]];
  }

  return(selectedPoints);
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel)
{
   selectedPoints<-selection(history, oldModel)
   newModel<-modelUpdate(selectedPoints, oldModel)
   newPoints<-variation(selectedPoints, newModel)
   return (list(newPoints=newPoints,newModel=newModel))
}

#' Main function that runs metaheuristic PSO algorithm implementation.
#'
#' @param initialization A method that constructs history from start points
#' @param startPoints A group of start Points
#' @param termination A method that stops algorithm after certain condition is met
#' @param evaluation A method that counts the quality of the points
#' @param initSpawnArea A radius according to which initial velocities of particles are generated
#' @param maxEvaluations Maximum number of evaluations
#' @param localBestVsGlobalBestRatio A number between 0 and 1 telling how strongly particle will be pulled against best known points
#' @param explorationVsExploitaitionRatio A number between 0 and 1 telling if particles will explore or exploit more
#' @return history of the run
metaheuristicRun <- function(initialization, startPoints, termination, evaluation, initSpawnArea = 10, maxEvaluations = 100, localBestVsGlobalBestRatio = 0.75, explorationVsExploitaitionRatio = 0.9)
{
  psoProps.initSpawnArea <<- initSpawnArea;
  psoProps.maxEvaluations <<- maxEvaluations;
  psoProps.localBestVsGlobalBestRatio <<- localBestVsGlobalBestRatio;
  psoProps.explorationVsExploitaitionRatio <<- explorationVsExploitaitionRatio ;

   history<-initialization(startPoints)
   history<-evaluateList(history,evaluation)
   model<-initModel(history)
   while (!termination(history,model))
   {
      aa<-aggregatedOperator(history, model)
      aa$newPoints<-evaluateList(aa$newPoints, evaluation)
      history<-historyPush(history,aa$newPoints)
      model<-aa$newModel
   }
   return(history)
}

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points))
    points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points)
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
  newHistory<-c(oldHistory,newPoints)
  return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
  stop=length(history)
  start=max(stop-number+1,1)
  return(history[start:stop])
}

####  THAT'S ALL FOLKS
