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
  center = (app.maxCoord + app.minCoord)/2;
  max = normalizeBySquaring(app.maxCoord, center);
  min = normalizeBySquaring(app.minCoord, center);
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
  return(model$particles$positions);
}

#update of a model based on a LIST of points
modelUpdate<-function(selectedPoints, oldModel)
{
  newModel <- oldModel;
  #for every particle
  i = 1;
  for(velocity in newModel$particles$velocities){
    #updating velocities according to selectedPoints list
    newModel$particles$velocities[[i]] <- updatePointVelocity(velocity, selectedPoints[[i]]$coordinates, newModel$particles$bestPositions[[i]], newModel$bestPosition);

    #updating Local bests
    if(newModel$particles$positions[[i]]$quality > newModel$particles$bestPositions[[i]]$quality) {
      newModel$particles$bestPositions[[i]] <- newModel$particles$positions[[i]];
    }

    #updating global best
    if(newModel$particles$positions[[i]]$quality > newModel$bestPosition$quality){
      newModel$bestPosition <- newModel$particles$positions[[i]]
    }

    #iterate to next element
    i = i + 1;
  }
  return (newModel)
}

#generation of a LIST of new points
variation<-function(selectedPoints, model){
  i = 1;
  for(velocity in model$particles$velocities){
    j = 1;

    #updating positions according to models velocities
    for(v in velocity){
      selectedPoints[[i]]$coordinates[j] <- selectedPoints[[i]]$coordinates[j]+v;
      j = j + 1;
    }

    #updating qualities
    selectedPoints[[i]]$quality <- evaluate(selectedPoints[[i]]$coordinates);

    i = i + 1;
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

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
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

evaluate <- function(coordinates){
  #TODO change this sum to accual evaulating function
  quality <- sum(coordinates);
  return (quality);
}


updatePointVelocity <- function(velocity, coordinates, bestLocalCoordinates, bestGlobalCoordinates){
  #TODO: implement main evaluating function
  #add dependence on weigths(from properties)
  #and depancence on randoms
  #must return list of new velocities
  i = 1;
  for(v in velocity){
    velocity[[i]] <-  velocity[[i]]+0.5
    i = i+1;
  }
  return (velocity);
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
