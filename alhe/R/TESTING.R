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
  app.maxEvaluations <<- 100;
  history = metaheuristicRun(initialization, generateStartPoints(1,2,-10,10), termination, fnc)
  plotQuality(history, name)
  return(history)
}

benchmarkCore <- function(populationCount, dimentions, fnc, runs) {

  result = vector(length = runs);
  for(i in 1:runs) {
    history = metaheuristicRun(initialization, generateStartPoints(populationCount,dimentions,-10,10), termination, fnc)
    result[i] = minHistory(history);
  }

  return(mean(result))
}

benchmark <- function (fnc) {
  app.maxEvaluations <<- 100;
  weight=0.3
  populationCount = 3;
  dimentions = 2;

  runs = 100;

  x<- list();
  y<- vector();
  for(weight in seq(0.01, 0.99, 0.02)) {
    print(c("progress: ", weight))
  #for(populationCount in 1:6) {

    weigths  <<-  c(weight, 1-weight);

    #benchmarking start
    result = benchmarkCore(populationCount, dimentions, fnc, runs);
    y = append(y, result);
    #benchmarking end

    #x = append(x, populationCount);
    x = append(x, weight);


  }

  plot(x, y, xlab="weights", ylab=paste(c("quality  (", runs ," runs)"), collapse=""));
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
  z =vector(length = length(x));
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
  lines(x, y, type='l');
  minHistory(history)
}
