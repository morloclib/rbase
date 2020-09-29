context("serialization.R")

mlc_list <- function(x){
  list(list=x)
}

mlc_tuple <- function(...){
  list(tuple=list(...))
}

mlc_integer <- list("integer")
mlc_numeric <- list("numeric")

test_that(
  "test serialization",
  {
    expect_identical(
      unpack("[1,2,3]", mlc_list(mlc_integer)),
      as.integer(c(1,2,3))
    )
  }
)
