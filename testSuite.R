library('RUnit');

#Get path starting from current working directory
utils.getDir <- function(path) {
  return(paste(dirname(sys.frame(1)$ofile), path, sep='/'));
}

#Suite initialization
test.suite <- defineTestSuite("AllTests",
                              dirs = file.path(utils.getDir('tests')),
                              testFileRegexp = '*\\.R');


test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)