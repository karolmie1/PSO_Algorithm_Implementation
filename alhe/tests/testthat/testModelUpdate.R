test_that("Method correctly updates global max", {
  #for every particle check if global best after 3 updates
  #is actually greater than any of particle's qualities
  particlesCount <- 5;
  dimCount <- 3;

  x <- generateStartPoints(particlesCount, dimCount, 5, 5);
  x <- evaluateList(x, sum);
  model <- initModel(x);
  history <- initialization(x);

  for(i in 1:3) {
    selectedPoints <- selection(history, model);
    model <- modelUpdate(selectedPoints,model);
    result <- variation(selectedPoints, model);
    result <-evaluateList(result, sum)
    history<-historyPush(history, result);
  }

  globalBest = model$bestPosition$quality;
  for(p in model$particles$positions){
    expect_that(p$quality, is_more_than(globalBest - 0.1));
  }
})

test_that("Method correctly updates Local max", {
  #for every particle check if local best after 3 updates
  #is actually greater than any of particle's accual qualities
  particlesCount = 5;
  dimCount = 5;

  x <- generateStartPoints(particlesCount, dimCount, 5, 5);
  x <- evaluateList(x, sum);
  model <- initModel(x);
  history <- initialization(x);

  for(i in 1:3) {
    selectedPoints <- selection(history, model);
    model <- modelUpdate(selectedPoints,model);
    result <- variation(selectedPoints, model);
    result <-evaluateList(result, sum)
    history<-historyPush(history, result);
  }

  model <- modelUpdate(selectedPoints,model);
  for(i in 1:length(model$particles$positions)) {
    expect_that(model$particles$positions[[i]]$quality, is_more_than(model$particles$bestPosition[[i]]$quality - 0.1));
  }
})


