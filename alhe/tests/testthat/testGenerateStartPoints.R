test_that("Generating start points works", {
  popCount = 3;
  dim = 2;
  x = generateStartPoints(popCount, dim, 0, 0);
  temp = list(coordinates=vector(mode = "integer", length = dim), quality=NA);
  result = rep(list(temp), popCount);
  expect_that(x, equals(result));
})
