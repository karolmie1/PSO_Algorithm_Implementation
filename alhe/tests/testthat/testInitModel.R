test_that("Model initializes properly", {
  particlesCount = 5;
  dimCount = 3;
  x = generateStartPoints(particlesCount, dimCount, 5, 5);
  model = initModel(evaluateList(x, sum));

  expect_that(length(model$particles$velocities), equals(particlesCount));
  expect_that(length(model$particles$positions), equals(particlesCount));
  expect_that(length(model$particles$bestPositions), equals(particlesCount));

  expect_that(length(model$particles$velocities[[2]]), equals(dimCount));
  expect_that(length(model$particles$positions[[2]]$coordinates), equals(dimCount));
  expect_that(length(model$particles$bestPositions[[2]]$coordinates), equals(dimCount));
})

test_that("Evaluation method works", {
  x = generateStartPoints(2, 2, 5, 5);
  result = evaluateList(x, sum);

  expect_that(result[[1]]$quality, equals(10));
  expect_that(result[[2]]$quality, equals(10));
})

test_that("Best point gets best point", {
  x = generateStartPoints(2, 2, 6, 6);
  x[[1]]$coordinates[1] = 3;
  result = getBestPoint(evaluateList(x, sum));

  expect_that(result$quality, equals(12));
})

test_that("Normalizing don't increase numbers", {
  #check negative numbers
  expect_that(normalizeBySquaring(-10,0) < 0, is_true());
  expect_that(normalizeBySquaring(-10,0) > -10, is_true());

  #check positive numbers
  expect_that(normalizeBySquaring(10,0) > 0, is_true());
  expect_that(normalizeBySquaring(10,0) < 10, is_true());

  #check corner case: zero
  expect_that(normalizeBySquaring(0,0) == 0, is_true());
})

test_that("Init Velocity generates arrays with desired dimentions", {
  result = generateInitVelocity(3, 4);
  expect_that(length(result), equals(3));
  expect_that(length(result[[1]]), equals(4));
})

test_that("Selection returns what was placed into model", {
  particlesCount = 5;
  dimCount = 3;


  x = evaluateList(generateStartPoints(particlesCount, dimCount, 5, 5), sum);
  model = initModel(x);
  history<-initialization(x);

  expect_that(selection(history, model), equals(x));
})

