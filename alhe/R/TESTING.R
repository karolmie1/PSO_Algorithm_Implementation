jogFunct = function(x){
  return(sum(x^2))
  }

#only in 2d
easomFunct = function(z) {
  x= z[1]
  y=z[2]
 return(-cos(x)*cos(y)*exp(-((x-pi)^2 + (y-pi)^2)))
}

#best -600 600
griewankFunct = function(z) {
  product=1;
  suma=0;

  for (i in 1:length(z)) {
    suma= suma+z[i]^2;
    product= product*cos(z[i]/sqrt(i));
  }

  return(1 + 1/4000*suma - product)
}

#only 2d
rosenbrockFunct = function (z) {
  x= z[1]
  y=z[2]
  return((1-x)^2 + 100*(y - x^2)^2)
}

bidi = function(x,y) {
  return(griewankFunct(c(x,y)))
}

testScenario <- function (x, fnc, name) {
  weigths  <<-  c(x, 1-x);
  app.maxEvaluations <<- 1000;
  history = metaheuristicRun(initialization, generateStartPoints(3,2,-5,5), termination, fnc)
  plotQuality(history, name)
  return(history)
}

testJog <- function (x) {

  testScenario(x, jogFunct, "Jog's Function");
}

testEasom <- function (x) {
  testScenario(x, easomFunct, "Easom Function");
}

testGriewank <- function (x) {
  testScenario(x, griewankFunct, "Griewank Function");
}

testRosenbrock <- function (x) {
  testScenario(x, rosenbrockFunct,"Rosenbrock Function");
}

minHistory <-function (x) {
  z =vector();
  for(i in 1:length(x)){
    z[i] = x[[i]]$quality;
  }
 return (min(z))
}

trace <-function (z) {
  x =vector();
  y =vector();
  for(i in 1:length(z)){
    x[i] = z[[i]]$coordinates[[1]];
    y[i] = z[[i]]$coordinates[[2]];
  }

  plot(x,y)
  minHistory(history)
}
