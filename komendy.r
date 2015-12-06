source("workspace/PSO_Algorithm_Implementation/metaheuristic_pattern.r")

initialization<-function(startPoints) {return (startPoints)}
x = list(coordinates = 2)
startPoints = list(x)
counter = 10
termination <- function (history, model) { 
	counter <<- counter - 1 
	return(counter<=0)
}

evaluation <- function(x = 1) {
	return( -((x-3)^2) + 1)
}

metaheuristicRun(initialization, startPoints, termination, evaluation)

