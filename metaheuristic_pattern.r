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
#TODO: finish to comply with model
  newModel = list(point=history, best=getBestPoint(history));
   return(newModel);
}

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
   #TODO: implement and test - extract list of current coordinates from model
   return(model);
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel)
{
   #TODO: implement and test - substitute current coordinates
   #                         - count new velocities
   #                         - update global best
   return (oldModel) 
}

#generation of a LIST of new points
#to be defined
variation<-function(selectedPoints, model)
{
  #TODO: implement and test - count new point coordinates: use function described in documentation
  #                         - add to properties files learning attributes
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

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points))
     points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points) 
}


####  THAT'S ALL FOLKS
