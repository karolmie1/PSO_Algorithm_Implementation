source(utils.getDir('utils.R'));

test.generateStartPointsSanity <- function() {
  popCount = 3;
  dim = 2;

  x = generateStartPoints(popCount, dim, 0, 0);
  temp = list(coordinates=vector(mode = "integer", length = dim), quality=NA);
  result = rep(list(temp), popCount);
  checkEquals(x, result);
}
