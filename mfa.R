#'
#' @title Multiple Factor Analysis (MFA) 
#' @author Yulin Chen, Stephanie Wuerth, Eren Bilir, Jim Oreluk
#' 
setClass(
  Class = 'mfa', 
    representation = representation(
    data = c('matrix', 'data.frame'),
    sets = 'list',
    ncomps = 'integer',
    center = c('logical', 'vector'),
    scale = c('logical', 'vector')
  ),
  validity = function(object) {
    
    # data
    if (!is.matrix(object@data) | 
        !is.data.frame(object@data) ) {
      stop("'data' must be a matrix or data.frame containing the data set")
    }
    
    # sets
    if (!is.list(object@sets)) {
      stop("'sets' must be a list containing vectors indicating the sets of variables")
    }
    
    # ncomps
    if (!is.integer(object@ncomps)) {
      stop("'ncomps' must be a integer indicating the number of components")
    }
    
    # Center
    if (!is.logical(object@center) | 
        !is.vector(object@center) | 
        length(object@center) != object@ncomps ) {
      stop("'center' must be a logical or numeric 
           vector equal to the number of active variables")
    }
    
    # Scale
    if (!is.logical(object@scale) | 
        !is.vector(object@scale) | 
        length(object@scale) != object@ncomps ) {
      stop("'scale' must be a logical or numeric 
           vector equal to the number of active variables")
    }
  },
  prototype = prototype(
    ncomps = NULL
  )
)

# Print Method
setMethod(
  'print',
  signature = 'mfa', 
  function(x, ...) {
    cat('object "mfa"\n')
    cat('sets: ')
    print(x@sets)
    cat('ncomps: ')
    print(x@ncomps)
    cat('Center: ')
    print(x@center)
    cat('scale: ')
    print(x@scale)
  }
)
