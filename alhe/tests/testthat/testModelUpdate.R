test_that("Method correctly updates global max", {
  #for every particle check if global best after 3 updates
  #is actually greater than any of particle's qualities
  particlesCount <- 5;
  dimCount <- 3;
  x <- generateStartPoints(particlesCount, dimCount, 5, 5);
  model <- initModel(evaluateList(x, sum));

  selectedPoints <- selection(NaN, model);
  model$particles$positions <- variation(selectedPoints, model);
  model <- modelUpdate(selectedPoints,model);

  selectedPoints <- selection(NaN, model);
  model$particles$positions <- variation(selectedPoints, model);
  model <- modelUpdate(selectedPoints,model);

  selectedPoints <- selection(NaN, model);
  model$particles$positions <- variation(selectedPoints, model);
  model <- modelUpdate(selectedPoints,model);

  globalBest = model$bestPosition$quality;
  pos = model$particles$positions
  for(p in pos){
    expect_that(p$quality <= globalBest,is_true());
  }
})

test_that("Method correctly updates Local max", {
  #for every particle check if local best after 3 updates
  #is actually greater than any of particle's accual qualities
  particlesCount = 5;
  dimCount = 5;
  x = generateStartPoints(particlesCount, dimCount, 5, 5);
  model = initModel(evaluateList(x, sum));

  selectedPoints <- selection(NaN, model);
  model$particles$positions <- variation(selectedPoints, model);
  model <- modelUpdate(selectedPoints,model);

  selectedPoints <- selection(NaN, model);
  model$particles$positions <- variation(selectedPoints, model);
  model <- modelUpdate(selectedPoints,model);


  selectedPoints <- selection(NaN, model);
  model$particles$positions <- variation(selectedPoints, model);
  model <- modelUpdate(selectedPoints,model);


  pos = model$particles$positions
  i = 1;
  for(p in pos){
    expect_that(p$quality <= model$particles$bestPositions[[i]]$quality,is_true());
    i = i+1;
  }
})


