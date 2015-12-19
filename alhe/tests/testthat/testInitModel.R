test_that("Model initializes properly", {
  #TODO: init model test
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
