#' Validation Class
#' @import methods
#' @name validation-class
#' @rdname validation-class
#' @slot PC Partition Coefficient index
#' @slot MPC Modified Partition Coefficient index
#' @slot CE Classification Entropy index
#' @slot S Separation index
#' @slot XB Xie Beni index
#' @slot Kwon Kwon index
#' @slot Tang Tang index
#' @exportClass validation
#'
initValidation<-function(){
  setClass("validation",
           representation = representation(PC="numeric",
                                           MPC="numeric",
                                           CE="numeric",
                                           S="numeric",
                                           XB="numeric",
                                           Kwon="numeric",
                                           Tang="numeric"
           ),
           prototype = list(PC=numeric(),
                            MPC=numeric(),
                            CE=numeric(),
                            S=numeric(),
                            XB=numeric(),
                            Kwon=numeric(),
                            Tang=numeric()))

  #' @name validation-class
  #' @rdname validation-class
  #' @exportMethod PC
  setGeneric("PC",function(object) standardGeneric("PC"))

  #' @rdname validation-class
  #' @aliases PC, validation-method
  setMethod("PC","validation",function(object) object@PC)

  #' @name validation-class
  #' @rdname validation-class
  #' @exportMethod MPC
  setGeneric("MPC",function(object) standardGeneric("MPC"))


  #' @rdname validation-class
  #' @aliases MPC, validation-method
  setMethod("MPC","validation",function(object) object@MPC)

  #' @name validation-class
  #' @rdname validation-class
  #' @exportMethod CE
  setGeneric("CE",function(object) standardGeneric("CE"))

  #' @rdname validation-class
  #' @aliases CE, validation-method
  setMethod("CE","validation",function(object) object@CE)

  #' @name validation-class
  #' @rdname validation-class
  #' @exportMethod XB
  setGeneric("XB",function(object) standardGeneric("XB"))


  #' @rdname validation-class
  #' @aliases XB, validation-method
  setMethod("XB","validation",function(object) object@XB)

  #' @name validation-class
  #' @rdname validation-class
  #' @exportMethod S
  setGeneric("S",function(object) standardGeneric("S"))

  #' @rdname validation-class
  #' @aliases S, validation-method
  setMethod("S","validation",function(object) object@S)

  #' @name validation-class
  #' @rdname validation-class
  #' @exportMethod Kwon
  setGeneric("Kwon",function(object) standardGeneric("Kwon"))


  #' @rdname validation-class
  #' @aliases Kwon, validation-method
  setMethod("Kwon","validation",function(object) object@Kwon)

  #' @name validation-class
  #' @rdname Tang-method
  #' @exportMethod Tang
  setGeneric("Tang",  function(object) standardGeneric("Tang"))


  #' @rdname validation-class
  #' @aliases Tang, validation-method
  setMethod("Tang","validation",function(object) object@Tang)

  setMethod("show","validation",function(object){
    print.validation(object)
  })
}
