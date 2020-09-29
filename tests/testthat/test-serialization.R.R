context("serialization.R")

test_that(
  "test serialization",
  {
    expect_identical(
      unpack("[1,2,3]", list(list=list("integer"))),
      as.integer(c(1,2,3))
    )
  }
)
