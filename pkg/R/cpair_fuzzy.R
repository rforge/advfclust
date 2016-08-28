#' Pairing fuzzy cluster
#' @param x an fuzzycluster object
#' @param y an fuzzycluster object
.fuzzy.cPair<-function(x,y){
  if(!is(x,"fuzzycluster")||!is(y,"fuzzycluster"))
    stop("Both x and y must be fuzzy cluster result")
  result<-
    new("fuzzycluster_list",pair=list(x,y))
}