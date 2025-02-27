ClosestMatch2 <- function(string, stringVector) {
  library(stringdist)
  stringVector[amatch(string, stringVector, maxDist = Inf)]
}
