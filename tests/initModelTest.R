source(utils.getDir('metaheuristic_pattern.r'));
source(utils.getDir('utils.R'));

test.initModelTest <- function() {
  #TODO: init model test
}

test.evaluateListTest <- function() {
  x = generateStartPoints(2, 2, 5, 5);
  
  result = evaluateList(x, sum);

  checkEquals(result[[1]]$quality, 10);
  checkEquals(result[[2]]$quality, 10);  
}

test.getBestPointTest <- function() {
  x = generateStartPoints(2, 2, 6, 6);
  x[[1]]$coordinates[1] = 3;
  temp = evaluateList(x, sum);

  result = getBestPoint(temp);
  checkEquals(result$quality, 12);
}