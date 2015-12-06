#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################




#### TO BE DEFINED BY THE USER

initModel<-function(history) {
   return(history[1])
}
#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
   #select a number of points from the history using the 
   #method's parameters and the current state of the model
   return(model);
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel, evaluation)
{
   #take a look at the list of selectedPoints and 
   #on the current state of the model, update it 
   #and then return
   #print("before")
   #print(oldModel)
   #print("selected")
   #print(selectedPoints)
   for (i in 1:length(selectedPoints))
   {
     quality = evaluation(oldModel[[1]]$coordinates)
     qualityDwa = evaluation(selectedPoints[[i]]$coordinates)
 #    print(qualityDwa)
     if(evaluation(selectedPoints[[i]]$coordinates) > quality)
     {
         oldModel = list(list(coordinates = selectedPoints[[i]]$coordinates))
#	 print("after")
#	 print(oldModel)
     }
   }

#print("end")
#print(oldModel)
   return (oldModel) 
}

#generation of a LIST of new points
#to be defined
variation<-function(selectedPoints, model)
{
x = list(coordinates = model[[1]]$coordinates+1)
y = list(coordinates = model[[1]]$coordinates-1)

   return(list(y, x));
   #generate the list of newPoints and then  
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel,evaluation)
{

   selectedPoints<-selection(history, oldModel)
   newPoints<-variation(selectedPoints,oldModel )
   #print("newPoints")
   #print(newPoints)
   newModel<-modelUpdate(newPoints, oldModel,evaluation)
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
      aa<-aggregatedOperator(history, model,evaluation)
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
