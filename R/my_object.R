#' Constructor for a `my_object` S3 class
#'
#' This function creates an object of class `my_object` from an integer vector.
#'
#' @param x An integer vector.
#'
#' @returns An object of class \code{my_object}.
#' @export
#'
#' @examples
#' obj <- new_my_object(c(1L, 2L, 3L, 10L))
#' obj
new_my_object <- function(x) {
  stopifnot(is.integer(x))
  structure(list(data = x), class = "my_object")
}


#' Print method for my_object
#'
#' @param x An object of class \code{my_object}.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns \code{x}.
#' @export
#'
#' @examples
#' obj <- new_my_object(c(1L, 5L, 10L))
#' print(obj)
print.my_object <- function(x, ...) {
  print(x$data)
  invisible(x)
}


#' Summary method for my_object
#'
#' @param object An object of class \code{my_object}.
#' @param ... Additional arguments (ignored).
#'
#' @returns A summary of the data inside the object.
#' @export
#'
#' @examples
#' obj <- new_my_object(c(1L, 2L, 2L, 3L, 4L, 10L))
#' summary(obj)
summary.my_object <- function(object, ...) {
  stats <- summary(object$data)
  class(stats) <- "summary.my_object"
  return(stats)
}


#' Plot method for my_object
#'
#' @param x An object of class \code{my_object}.
#' @param ... Additional graphical parameters passed to \code{plot}.
#'
#' @returns A plot of the integer data.
#' @export
#'
#' @examples
#' counts <- rpois(50, lambda = 3)
#' obj_counts <- new_my_object(as.integer(counts))
#' plot(obj_counts, col = "seagreen")
plot.my_object <- function(x, ...) {
  hist(x$data, main = "Plot of my_object", ...)
}

