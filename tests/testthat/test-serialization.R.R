context("serialization.R")

test_that(
  "test serialization",
  {
    expect_identical(
      unpack("[1,2,3]", .mlc_list(.mlc_integer)),
      as.integer(c(1,2,3))
    )
    expect_identical(
      unpack("[[1,2],[3,4]]", .mlc_list(.mlc_tuple("integer", "integer"))),
      list(as.integer(c(1,2)), as.integer(c(3,4)))
    )
  }
)
