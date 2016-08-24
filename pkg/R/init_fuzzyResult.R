#' Fuzzy Result
#' @import methods
#' @name fuzzyResult-class
#' @rdname fuzzyResult-class
#' @slot partition membership matrix
#' @slot label vector of hard partition
#' @slot centroid centroid matrix
#' @slot distance distance matrix
#' @slot func.obj function objective
#' @slot call.func called function
#' @slot fuzzyfier fuzzyness parameter
#' @slot method.fuzzy method of fuzzy clustering used
#' @exportClass fuzzyResult

initFuzzyResult<-function(){
  setClass("fuzzyResult",
           representation= representation(partition="matrix",
                                          label="vector",
                                          centroid="matrix",
                                          distance="matrix",
                                          func.obj="numeric",
                                          call.func="character",
                                          fuzzyfier="numeric",
                                          method.fuzzy="character"),
           prototype = list(partition=matrix(),
                            label=vector(),
                            centroid=matrix(),
                            distance=matrix(),
                            func.obj=numeric(),
                            call.func=character(),
                            fuzzyfier=numeric(),
                            method.fuzzy=character())
  )

  setMethod("is.na","fuzzyResult",function(x) FALSE)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod partition
  setGeneric("partition",
             function(object)
               standardGeneric("partition"))

  #' @rdname fuzzyResult-class
  #' @aliases partition, fuzzyResult-method
  setMethod("partition","fuzzyResult",function(object) object@partition)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod label
  setGeneric("label",
             function(object)
               standardGeneric("label"))

  #' @rdname fuzzyResult-class
  #' @aliases label, fuzzyResult-method
  setMethod("label","fuzzyResult",function(object) object@label)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod centroid
  setGeneric("centroid",
             function(object)
               standardGeneric("centroid"))

  #' @rdname fuzzyResult-class
  #' @aliases centroid, fuzzyResult-method
  setMethod("centroid","fuzzyResult",function(object) object@centroid)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod distance
    setGeneric("distance",
               function(object)
                 standardGeneric("distance"))

  #' @rdname fuzzyResult-class
  #' @aliases distance, fuzzyResult-method
  setMethod("distance","fuzzyResult",function(object) object@distance)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod func.obj
    setGeneric("func.obj",
               function(object)
                 standardGeneric("func.obj"))

  #' @rdname fuzzyResult-class
  #' @aliases func.obj, fuzzyResult-method
  setMethod("func.obj","fuzzyResult",function(object) object@func.obj)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod call.func
    setGeneric("call.func",
               function(object)
                 standardGeneric("call.func"))

  #' @rdname fuzzyResult-class
  #' @aliases call.func, fuzzyResult-method
  setMethod("call.func","fuzzyResult",function(object) object@call.func)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod fuzzyfier
  setGeneric("fuzzyfier",
             function(object)
               standardGeneric("fuzzyfier"))

  #' @rdname fuzzyResult-class
  #' @aliases fuzzyfier, fuzzyResult-method
  setMethod("fuzzyfier","fuzzyResult",function(object) object@fuzzyfier)

  #' @name fuzzyResult-class
  #' @rdname fuzzyResult-class
  #' @exportMethod method.fuzzy
    setGeneric("method.fuzzy",
               function(object)
                 standardGeneric("method.fuzzy"))

  #' @rdname fuzzyResult-class
  #' @aliases method.fuzzy, fuzzyResult-method
  setMethod("method.fuzzy","fuzzyResult",function(object) object@method.fuzzy)

  setMethod("show","fuzzyResult",function(object){
    print.fuzzyResult(object)
  })
}
