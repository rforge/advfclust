#' Membership
#' @import methods

initMembership<-function(){

  #' @name membership-class
  #' @rdname membership-class
  #' @slot member membership matrix
  #' @slot hard.label vector of hard hard.labeling
  #' @exportClass membership
  setClass("membership",
           representation = representation(member="matrix",
                                           hard.label="vector"),
           prototype = list(member=matrix(),
                            hard.label=vector()))

  setMethod("is.na","membership",function(x) FALSE)

  #' @name membership-class
  #' @rdname membership-class
  #' @exportMethod member
  if(!isGeneric("member")){
    if(is.function("member"))
      fun<-member else fun<-function(object) standardGeneric("member")
    setGeneric("member",fun)
  }

  #' @rdname membership-class
  setMethod("member","membership",function(object) object@member)

  #' @name membership-class
  #' @rdname membership-class
  #' @exportMethod hard.label
  if(!isGeneric("hard.hard.label")){
    if(is.function("hard.label"))
      fun<-hard.label else fun<-function(object) standardGeneric("hard.label")
      setGeneric("hard.label",fun)
  }

  #' @rdname membership-class
  setMethod("hard.label","membership",function(object) object@hard.label)

  setValidity("membership",
              function(object){
                if(length(hard.label(object))!= nrow(member(object)))
                  return("hard.label Mismatch\n")
                if(any(member(object)<0 ||member(object)>1))
                  return("Constraint of membership matrix violated\n")
                rowSum.membership<-rowSums(member(object))
                if(any(rowSum.membership!=1))
                  return("Constraint of membership matrix violated\n")
                if(any(!is.numeric(member(object))))
                  return("Not Numeric")
                if(anyNA(object))
                  return("Missing value on membership detected\n")
              })

  setMethod("show","membership",function(object){
    print.membership(object)
  })

}
