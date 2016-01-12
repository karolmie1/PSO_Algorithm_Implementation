test_that("Points are changing", {
  #for every particle check if global best after 3 updates
  #is actually greater than any of particle's qualities
  particlesCount <- 5;
  dimCount <- 3;
  x <- generateStartPoints(particlesCount, dimCount, 5, 5);
  model <- initModel(evaluateList(x, sum));

  startingPoints <- selection(NaN, model);

  selectedPoints <- selection(NaN, model);
  model <- modelUpdate(selectedPoints,model);
  model$particles$positions <- variation(selectedPoints, model);

  selectedPoints <- selection(NaN, model);
  model <- modelUpdate(selectedPoints,model);
  model$particles$positions <- variation(selectedPoints, model);

  selectedPoints <- selection(NaN, model);
  model <- modelUpdate(selectedPoints,model);
  model$particles$positions <- variation(selectedPoints, model);

  endingPoints <- selection(NaN, model);

  i = 1;
  for(point in startingPoints){
    j = 1;
    for(coords in point$coordinates){
      expect_that(endingPoints[[i]]$coordinates[[j]] == coords, is_false());
    j = j+1;
    }
  i = i+1;
  }
})
