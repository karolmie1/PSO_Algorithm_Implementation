#' A simple quadratic function
#'
#' @param x A point to be evaluated
#' @return quality of the point
jongFunct = function(x){
  return(sum(x^2))
  }

#' Flat function with one deep, but small peek, implemented only for 2 dimentions
#'
#' @param x A point to be evaluated
#' @return quality of the point
easomFunct = function(z) {
  x= z[1]
  y=z[2]
 return(-cos(x)*cos(y)*exp(-((x-pi)^2 + (y-pi)^2)))
}

#' Very spiky function, best results with larger search area (-600 600)
#'
#' @param x A point to be evaluated
#' @return quality of the point
griewankFunct = function(z) {
  product=1;
  suma=0;

  for (i in 1:length(z)) {
    suma= suma+z[i]^2;
    product= product*cos(z[i]/sqrt(i));
  }

  return(1 + 1/4000*suma - product)
}

#' Function with easy to find local optimum, but hard global optimum to converge too, only 2 dimentions
#'
#' @param x A point to be evaluated
#' @return quality of the point
rosenbrockFunct = function (z) {
  x= z[1]
  y= z[2]
  return((1-x)^2 + 100*(y - x^2)^2)
}

#' Finds the best point in history
#' @param x A history
#' @return best point in history
minHistory <-function (x) {
  best = x[[1]];
  for(i in 1:length(x)){
    if(x[[i]]$quality < best$quality) {
      best = x[[i]];
    }
  }
  return (best);
}

#' Plots the graph with the function trace from history
#'
#' @param z A history
#' @param drawLines - should draw lines or not
#' @return quality of best point
trace <-function (z, drawLines = FALSE) {
  x =vector();
  y =vector();
  for(i in 1:length(z)){
    x[i] = z[[i]]$coordinates[[1]];
    y[i] = z[[i]]$coordinates[[2]];
  }

  plot(x,y);

  if(drawLines == TRUE) {
   lines(x, y, type='l');
  }
  z = minHistory(history);
  points(z$coordinates[[1]],z$coordinates[[2]], col="darkolivegreen1", pch=19);

  return(z$quality);
}


#' Plots the change of quality in time
#'
#' @param history A history for chaining.
plotQuality <- function(history, fName) {
  y <- list();
  x <- 1:length(history);
  for(point in history) {
    y <- c(y, point$quality);
  }
  plot(x, y, xlab="steps", ylab="quality");
  title(main = fName);
}

#' Private function for benchmark
benchmarkCore <- function(populationCount, dimentions, fnc, runs, initSpawnArea, maxEvaluations, c, d) {

  result = vector(length = runs);
  for(i in 1:runs) {
    history = metaheuristicRun(initialization, generateStartPoints(populationCount,dimentions,-600,600), termination, fnc, initSpawnArea, maxEvaluations, c, d)
    result[i] = minHistory(history)$quality;
  }

  return(mean(result))
}

benchmark <- function (fnc) {
  maxEvaluations = 200;
  c = 0.3;
  d = 0.7;
  populationCount = 5;
  dimentions = 2;
  runs = 100;
  initSpawnArea = 10;

  x<- list();
  y<- vector();
  for(weight in seq(0.01, 0.99, 0.1)) {
    print(c("progress: ", weight))
  #for(populationCount in 1:10) {
  #print(c("progress: ", populationCount))
    weigths  <<-  c(weight, 1-weight);

    #benchmarking start
    result = benchmarkCore(populationCount, dimentions, fnc, runs, initSpawnArea, maxEvaluations, c, d);
    y = append(y, result);
    #benchmarking end


    #x = append(x, populationCount);
    x = append(x, weight);


  }

  plot(x, y, xlab="", ylab=paste(c("quality  (", runs ," runs)"), collapse=""));
}




testJong <- function (x, y) {

  return(testScenario(x, y, jongFunct, "Jong's Function"));
}

testEasom <- function (x, y) {
  testScenario(x,y, easomFunct, "Easom Function");
}

testGriewank <- function (x,y) {
  testScenario(x, y, griewankFunct, "Griewank Function");
}

testRosenbrock <- function (x,y) {
  testScenario(x,y, rosenbrockFunct,"Rosenbrock Function");
}

testScenario <- function (x, y, fnc, name) {
  history = metaheuristicRun(initialization, generateStartPoints(2,2,-10,10), termination, fnc, localBestVsGlobalBestRatio = y, explorationVsExploitaitionRatio = x)
  plotQuality(history, name)
  return(history)
}
