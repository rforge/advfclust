#' Print Fuzzy Clustering Result
#' @description Print Fuzzy Clustering
#' @param x fuzzy clustering object
#' @param ... another paramater
#' @import knitr
#' @export
print.fuzzyResult<-function(x,...){
  cat(paste("Function call:",x@call.func,sep=" "))
  cat(paste("\n"),x@method.fuzzy,sep="")
  cat(paste("\nFunction objective:",x@func.obj,sep=" "))

  cat("\nMembership & Label Matrix:")
  member.matrix<-x@partition
  colnames(member.matrix)<-paste("Cluster ",
                                 c(1:ncol(member.matrix)),
                                 sep="")
  rownames(member.matrix)<-paste("Obs ",
                                 c(1:nrow(member.matrix)),
                                 sep="")
  Label<-x@label
  member.matrix<-cbind(member.matrix,Label)
  print(kable(member.matrix,digits = 3))
  center<-x@centroid
  cat("\nCentroid:")
  rownames(center)<-paste("Cluster ",
                          c(1:nrow(center)),
                          sep="")
  print(kable(center,digits = 3))

}
