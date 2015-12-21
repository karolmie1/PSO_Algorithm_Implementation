test_that("Generating start points works", {
  popCount = 3;
  dim = 2;
  x = generateStartPoints(popCount, dim, 0, 0);
  temp = list(coordinates=vector(mode = "integer", length = dim), quality=NA);
  result = rep(list(temp), popCount);
  expect_that(x, equals(result));
})

test_that("TerminationByEvaluationsCount return proper values depending on different history length", {
  popCount = 3;
  notEnoughEvaluations = popCount - 1;
  enoughEvaluations = popCount + 4;
  history = generateStartPoints(popCount, 2, 0, 0);

  expect_that(terminateByEvaluationsCount(history, NaN, notEnoughEvaluations), is_true());
  expect_that(terminateByEvaluationsCount(history, NaN, enoughEvaluations), is_false());
})
