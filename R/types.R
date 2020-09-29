# This file holds type handling functions that are useful in tests and such.
# They are mostly for internal use, so are not exported.


#' Helper function for building list types
#'
#' The "list" here is a homogenous vector, like the Haskell or lisp list, not
#' the named, heterogenous lists of R. So `mlc_list(mlc_integer)` would be the
#' R version of the Haskell type `[Int]`, in this case, simply a numeric
#' vector.
#'
#' @param x The type parameter for a list
#' @examples
#' # parse a JSON list into R
#' unpack("[1,2,3]", mlc_list(mlc_integer))
#'
#' # write an R vector to JSON
#' pack(c(1,2,3), mlc_list(mlc_integer))
.mlc_list <- function(x){
  list(list=x)
}

#' Helper function for building tuple types
#'
#' A tuple corresponds to an unnamed R list.
#'
#' @param x The type parameter for a list
#' @examples
#' # parse a JSON list into R
#' unpack("[1,2,3]", mlc_tuple(mlc_integer))
#'
#' # write an R list to JSON
#' pack(list(1,2,3), mlc_tuple(mlc_integer))
.mlc_tuple <- function(...){
  list(tuple=list(...))
}

#' An R scalar integer type
.mlc_integer <- list("integer")

#' An R scalar numeric type
.mlc_numeric <- list("numeric")
