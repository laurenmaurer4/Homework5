## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)


setValidity(
  Class = "sparse_numeric",
  method = function(object){

## Check value validity
    check <- any(object@value == 0)
    if(check)
      return("Some values are equal to zero")
    check <- is.numeric(object@value)
    if(!check)
      return("Some values are not numeric")

## Check pos validity
    check <- length(object@pos) == length(object@value)
    if(!check)
      return("pos and value must be the same length")
    check <- is.integer(object@pos)
    if(!check)
      return("pos must be an integer vector")
    check <- all(object@pos >= 1L & object@pos <= object@length)
    if(!check)
      return("Some positions are outside the valid range")
    check <- !any(duplicated(object@pos))
    if(!check)
      return("pos must not contain duplicates")

## Check length
    check <- length(object@length) == 1L && is.integer(object@length)
    if(!check)
      return("length must be a single integer")
    check <- object@length >= 0L
    if(!check)
      return("length must be >0")

    TRUE

  }
)

## Set generic functions
setGeneric("sparse_add",
           function(x, y, ...) {
             standardGeneric("sparse_add")
           })
setGeneric("sparse_mult",
           function(x, y, ...) {
             standardGeneric("sparse_mult")
           })
setGeneric("sparse_sub",
           function(x, y, ...) {
             standardGeneric("sparse_sub")
           })
setGeneric("sparse_crossprod",
           function(x, y, ...) {
             standardGeneric("sparse_crossprod")
           })


## Addition
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  add_vals <- vx + vy

  keep <- which(add_vals != 0)
  if(length(keep) == 0L){
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length)
  }
  else {
    new("sparse_numeric",
        value = as.numeric(add_vals[keep]),
        pos = as.integer(allpos[keep]),
        length = x@length)
  }
})



## Multiplication
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  mult_vals <- vx * vy

  keep <- which(mult_vals != 0)
  if(length(keep) == 0L){
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length)
  }
  else {
    new("sparse_numeric",
        value = as.numeric(mult_vals[keep]),
        pos = as.integer(allpos[keep]),
        length = x@length)
  }
})

## Subtraction
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  sub_vals <- vx - vy

  keep <- which(sub_vals != 0)
  if(length(keep) == 0L){
    new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length)
  }
  else {
    new("sparse_numeric",
        value = as.numeric(sub_vals[keep]),
        pos = as.integer(allpos[keep]),
        length = x@length)
  }
})



## Cross product
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x,y, ...){
  if(x@length != y@length)
    stop("Sparse vectors must have the same length")

  allpos <- sort(unique(c(x@pos, y@pos)))
  vx <- numeric(length(allpos))
  vy <- numeric(length(allpos))

  vx[match(x@pos, allpos)] <- x@value
  vy[match(y@pos, allpos)] <- y@value

  crossprod_vals <- sum(vx * vy)

  crossprod_vals

})


## Implement +, -, *
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


setAs("numeric", "sparse_numeric",
      function(from){
        pos <- which(from != 0)
        value <- from[pos]
        len <- length(from)
        new("sparse_numeric", value = value, pos = as.integer(pos), length = as.integer(len))
      })

setAs("sparse_numeric", "numeric",
      function(from){
        out <- numeric(from@length)
        out[from@pos] <- from@value
        out
      })

setMethod("show", "sparse_numeric",
          function(object){
            cat("sparse_numeric vector\n")
            cat("Length:", object@length, "\n")
            cat("Positions:", object@pos, "\n")
            cat("Values:", object@value, "\n")
          })

setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...){
            if(x@length != y@length)
              stop("Vectors must have the same length")


            allpos <- sort(unique(c(x@pos, y@pos)))
            vx <- numeric(length(allpos))
            vy <- numeric(length(allpos))

            vx[match(x@pos, allpos)] <- x@value
            vy[match(y@pos, allpos)] <- y@value

            plot(allpos, vx, type = "p", col = "lightblue",
                 xlab = "Position", ylab = "Value", main = "Sparse Vector Comparison", ...)
            points(allpos, vy, col = "darkgreen")

            legend("topright", legend = c("x", "y"),
                   col = c("lightblue", "darkgreen"))
          })


setMethod("summary", "sparse_numeric",
          function(object) {
            cat("Summary for sparse_numeric\n")
            cat("Length:", object@length, "\n")
            cat("Nonzero count:", length(object@value), "\n")
            cat("Min value:", min(object@value), "\n")
            cat("Max value:", max(object@value), "\n")
            cat("Mean (nonzero):", mean(object@value), "\n")
          })

