[![Build Status](https://travis-ci.org/karolmie1/PSO_Algorithm_Implementation.svg)](https://travis-ci.org/karolmie1/PSO_Algorithm_Implementation)

Description:
-------

Nice implementation of Particle Swarm Optimization Algorithm

Requirements:
- devtools
- testthat

How to build it:
-------

```
devtools::install()
```

How to test it:
-------

```
devtools::test()
```

How to use it:
-------

You can run main function:

metaheuristicRun <- function(initialization, startPoints, termination, evaluation, initSpawnArea = 10, maxEvaluations = 100, localBestVsGlobalBestRatio = 0.75, explorationVsExploitaitionRatio = 0.9)


We've supplied some helpful utils for you to fill some of parameters. Using them sample run would be for example:

metaheuristicRun(initialization, generateStartPoints(2,2,-10,10), termination, jongFunct, 0.75, 0.9)


As for functions to use, we've supplied implementation of some in test utilites:

https://github.com/karolmie1/PSO_Algorithm_Implementation/blob/master/alhe/R/testUtitities.R


Try running those commands:

history = testJong(0.85, 0.7)

trace(history);
